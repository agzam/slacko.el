;;; slacko-creds.el --- Extract Slack credentials from local app data -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 17, 2026
;; Keywords: comm tools
;; Homepage: https://github.com/agzam/slacko.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extract Slack API tokens and session cookies directly from the Slack
;; desktop app's local data.  Discovers workspaces from the app's state,
;; decrypts the session cookie from the Cookies SQLite database, then
;; fetches each workspace's API token from its HTML page.  Caches
;; results in a GPG-encrypted file in netrc format for `auth-source'.
;;
;; Currently macOS only.  The cookie decryption relies on:
;; - `security' (Keychain access)
;; - `openssl' (PBKDF2 key derivation + AES-128-CBC decryption)
;; - `sqlite3' (reading the Cookies database)
;;
;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'password-cache)
(require 'seq)
(require 'url)
(require 'url-vars)

;;; Customizable Variables

(defgroup slacko-creds nil
  "Slack credential extraction and caching."
  :group 'slacko
  :prefix "slacko-creds-")

(defcustom slacko-creds-gpg-file
  (expand-file-name ".slacko-creds.gpg" user-emacs-directory)
  "GPG-encrypted file to cache credentials in netrc format."
  :type 'file
  :group 'slacko-creds)

(defcustom slacko-creds-slack-data-dir
  (pcase system-type
    ('darwin (expand-file-name "~/Library/Application Support/Slack/")))
  "Path to the Slack desktop app's data directory."
  :type 'directory
  :group 'slacko-creds)

(defcustom slacko-creds-keychain-service "Slack Safe Storage"
  "Keychain service name for the Slack cookie encryption key."
  :type 'string
  :group 'slacko-creds)

(defcustom slacko-creds-gpg-key nil
  "GPG key ID to encrypt the credentials file.
When nil, auto-detected from the default secret key."
  :type '(choice string (const nil))
  :group 'slacko-creds)

(defun slacko-creds--gpg-key ()
  "Return the GPG key to use for encryption.
Uses `slacko-creds-gpg-key' if set, otherwise auto-detects."
  (or slacko-creds-gpg-key
      (let ((output (string-trim
                     (shell-command-to-string
                      "gpg --list-secret-keys --keyid-format long 2>/dev/null | grep '^sec' | head -1 | sed 's|.*/\\([A-F0-9]*\\) .*|\\1|'"))))
        (if (string-empty-p output)
            (user-error "No GPG secret key found.  Set `slacko-creds-gpg-key'")
          output))))

;;; Workspace discovery

(defconst slacko-creds--generic-slack-hosts
  '("app" "files" "s" "pp" "api" "edgeapi" "slack-edge")
  "Slack subdomains that are not real workspaces.")

(defconst slacko-creds--host-regexp
  "\\`[a-z0-9-]+\\(?:\\.[a-z0-9-]+\\)*\\.slack\\.com\\'"
  "Hosts the session cookie may be sent to.")

(defun slacko-creds--root-state ()
  "The Slack app's storage/root-state.json, parsed, or nil."
  (let ((file (expand-file-name "storage/root-state.json"
                                slacko-creds-slack-data-dir)))
    (when (file-readable-p file)
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-key-type 'symbol)
                (json-array-type 'list))
            (json-read-file file))
        ((json-error file-error) nil)))))

(defun slacko-creds--workspace-host (workspace)
  "Host of WORKSPACE, an entry of root-state.json's workspaces.
The url wins over the domain: an Enterprise Grid org lives at a host of
its own, such as acme.enterprise.slack.com."
  (let ((url (alist-get 'url workspace))
        (domain (alist-get 'domain workspace)))
    (cond ((and (stringp url)
                (string-match "\\`https?://\\([^/]+\\)" url))
           (match-string 1 url))
          ((stringp domain)
           (concat domain ".slack.com")))))

(defun slacko-creds--root-state-hosts ()
  "Hosts of the workspaces listed in the Slack app's storage/root-state.json."
  (seq-keep (lambda (entry) (slacko-creds--workspace-host (cdr entry)))
            (alist-get 'workspaces (slacko-creds--root-state))))

(defun slacko-creds--root-state-orgs ()
  "The orgs the Slack app lists, the one selected in it first, as plists.
Each has :id, :name and :host.  An Enterprise Grid org is one entry, at
its own host, however many of its workspaces are signed in."
  (let* ((state (slacko-creds--root-state))
         (selected (alist-get 'selectedWorkspaceId
                              (alist-get 'workspacesMeta state)))
         (rank (pcase-lambda (`(,id . ,workspace))
                 (let ((order (alist-get 'order workspace)))
                   (cond ((equal (symbol-name id) selected) -1)
                         ((numberp order) order)
                         (t most-positive-fixnum))))))
    (seq-keep (pcase-lambda (`(,id . ,workspace))
                (when-let* ((host (slacko-creds--workspace-host workspace)))
                  (list :id (symbol-name id)
                        :name (or (alist-get 'name workspace) host)
                        :host host)))
              (seq-sort-by rank #'< (alist-get 'workspaces state)))))

(defun slacko-creds--discover-workspaces ()
  "Hosts of the workspaces the Slack app is signed into.
IndexedDB is scanned only when root-state.json names none."
  (or (slacko-creds--root-state-hosts)
      (slacko-creds--indexeddb-hosts)))

(defun slacko-creds--indexeddb-hosts ()
  "Find workspace hostnames from Slack's IndexedDB files.
Returns a list of hostnames like (\"foo.slack.com\" \"bar.slack.com\")."
  (let* ((idb-dir (expand-file-name "IndexedDB/" slacko-creds-slack-data-dir))
         (cmd (format "rg -aoN --no-filename '%s' %s 2>/dev/null | sort -u"
                      "[a-z0-9-]+\\.slack\\.com"
                      (shell-quote-argument idb-dir)))
         (output (string-trim (shell-command-to-string cmd))))
    (when (and output (not (string-empty-p output)))
      (cl-remove-if
       (lambda (host)
         (member (car (split-string host "\\.")) slacko-creds--generic-slack-hosts))
       (split-string output "\n" t)))))

(defun slacko-creds--extract-token-from-html (host cookie)
  "Fetch HOST's homepage with COOKIE and extract the api_token.
Returns the xoxc token string or nil.  HOST can come from any pasted
link, and the cookie goes wherever it points, so it must be a Slack host."
  (when (string-match-p slacko-creds--host-regexp host)
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Cookie" . ,(format "d=%s;" cookie))))
           (url-cookie-storage nil)
           (url-cookie-secure-storage nil)
           (buf (url-retrieve-synchronously
                 (format "https://%s/" host) t nil 15)))
      (when buf
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (when (re-search-forward
                     "\"api_token\":\"\\(xoxc-[^\"]+\\)\"" nil t)
                (match-string 1)))
          (kill-buffer buf))))))

;;; Cookie decryption (Cookies SQLite + Keychain + OpenSSL)

(defun slacko-creds--get-keychain-password ()
  "Get the Slack Safe Storage password from macOS Keychain."
  (let ((output (string-trim
                 (shell-command-to-string
                  (format "security find-generic-password -s %s -w 2>/dev/null"
                          (shell-quote-argument slacko-creds-keychain-service))))))
    (if (string-empty-p output)
        (error "Could not retrieve Slack keychain password")
      output)))

(defun slacko-creds--decrypt-cookie ()
  "Decrypt the Slack `d' cookie from the Cookies SQLite database.
Returns the cookie value string or nil."
  (let* ((cookies-db (expand-file-name "Cookies" slacko-creds-slack-data-dir))
         (tmp-db (make-temp-file "slack-cookies-" nil ".db"))
         (tmp-enc (make-temp-file "slack-cookie-" nil ".bin"))
         (tmp-dec (make-temp-file "slack-cookie-dec-" nil ".bin")))
    (unwind-protect
        (progn
          ;; Copy the Cookies DB to avoid locking issues with the Slack app
          (copy-file cookies-db tmp-db t)

          ;; Extract encrypted blob, strip v10 prefix (3 bytes)
          (shell-command-to-string
           (format "sqlite3 %s \"SELECT writefile('%s', substr(encrypted_value, 4)) FROM cookies WHERE name='d' LIMIT 1;\""
                   (shell-quote-argument tmp-db) tmp-enc))

          (when (and (file-exists-p tmp-enc)
                     (> (file-attribute-size (file-attributes tmp-enc)) 0))
            (let* ((pass (slacko-creds--get-keychain-password))
                   ;; Derive AES key: PBKDF2(password, salt='saltysalt', iter=1003, SHA1, keylen=16)
                   (salt-hex "73616c747973616c74") ; "saltysalt" in hex
                   (key-hex (string-trim
                             (shell-command-to-string
                              (format "openssl kdf -keylen 16 -kdfopt digest:SHA1 -kdfopt 'pass:%s' -kdfopt hexsalt:%s -kdfopt iter:1003 -binary PBKDF2 | xxd -p"
                                      pass salt-hex))))
                   (iv-hex "20202020202020202020202020202020"))

              ;; Decrypt
              (shell-command-to-string
               (format "openssl enc -aes-128-cbc -d -K %s -iv %s -nopad -in %s -out %s 2>/dev/null"
                       key-hex iv-hex
                       (shell-quote-argument tmp-enc)
                       (shell-quote-argument tmp-dec)))

              ;; Skip 32-byte domain hash prefix, strip PKCS7 padding
              (let ((raw (string-trim
                          (shell-command-to-string
                           (format "dd if=%s bs=1 skip=32 2>/dev/null | perl -pe 's/[\\x01-\\x10]+$//'"
                                   (shell-quote-argument tmp-dec))))))
                (when (string-prefix-p "xoxd-" raw)
                  raw)))))
      ;; Cleanup
      (delete-file tmp-db)
      (delete-file tmp-enc)
      (when (file-exists-p tmp-dec)
        (delete-file tmp-dec)))))

;;; GPG file management

(defun slacko-creds--read-gpg-file ()
  "Read the current contents of the credentials GPG file."
  (when (file-exists-p slacko-creds-gpg-file)
    (with-temp-buffer
      (insert-file-contents slacko-creds-gpg-file)
      (buffer-string))))

(defun slacko-creds--cached-hosts ()
  "Hosts that have a token in the credentials GPG file."
  (when-let* ((contents (slacko-creds--read-gpg-file)))
    (let ((start 0) hosts)
      (while (string-match "^machine \\(\\S-+\\) login token " contents start)
        (push (match-string 1 contents) hosts)
        (setq start (match-end 0)))
      (nreverse hosts))))

(defun slacko-creds--update-gpg-entry (contents host login password)
  "Update or add a netrc entry in CONTENTS for HOST, LOGIN with PASSWORD.
Returns the updated string."
  (let* ((lines (split-string (or contents "") "\n" nil))
         (pattern (format "machine %s login %s " host login))
         (new-line (format "machine %s login %s password %s" host login password))
         (found nil)
         (updated (mapcar (lambda (line)
                            (if (string-prefix-p pattern line)
                                (progn (setq found t) new-line)
                              line))
                          lines)))
    (if found
        (string-join updated "\n")
      (let ((result (string-trim (or contents ""))))
        (if (string-empty-p result)
            new-line
          (concat result "\n" new-line))))))

(defun slacko-creds--save-to-gpg (entries)
  "Save credential ENTRIES to the GPG file.
ENTRIES is a list of (host token cookie) triples."
  (let ((contents (slacko-creds--read-gpg-file)))
    (dolist (entry entries)
      (let ((host (nth 0 entry))
            (token (nth 1 entry))
            (cookie (nth 2 entry)))
        (setq contents (slacko-creds--update-gpg-entry contents host "token" token))
        (setq contents (slacko-creds--update-gpg-entry contents host "cookie" cookie))))
    (unless (string-suffix-p "\n" contents)
      (setq contents (concat contents "\n")))
    ;; Write via gpg CLI directly - bypasses EPA and its dialogs
    (let ((tmp (make-temp-file "slacko-creds-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert contents))
            (set-file-modes tmp #o600)
            (when (file-exists-p slacko-creds-gpg-file)
              (delete-file slacko-creds-gpg-file))
            (let ((exit-code
                   (call-process
                    "gpg" nil nil nil
                    "--batch" "--yes" "--quiet"
                    "--recipient" (slacko-creds--gpg-key)
                    "--output" (expand-file-name slacko-creds-gpg-file)
                    "--encrypt" tmp)))
              (unless (zerop exit-code)
                (error "GPG encryption failed (exit %d)" exit-code))))
        (when (file-exists-p tmp)
          (delete-file tmp))))
    (message "Slack credentials saved to %s" slacko-creds-gpg-file)))

(defvar slacko-creds--last-refresh-time nil
  "Time `slacko-creds-refresh' last ran, or nil.")

(defun slacko-creds--refresh-due-p ()
  "Non-nil when `slacko-creds-refresh' has not run in the last minute.
A refresh that failed counts too: several requests rejected at once
would otherwise each wait for one."
  (or (null slacko-creds--last-refresh-time)
      (< 60 (float-time (time-since slacko-creds--last-refresh-time)))))

(defvar slacko-creds--legacy-gpg-file
  (expand-file-name ".slack-creds.gpg" user-emacs-directory)
  "Legacy GPG file path from before the rename to slacko.")

;;; Main entry point

(defun slacko-creds--clear-cache ()
  "Clear auth-source caches for Slack credentials.
Clears both `password-data', where auth-source stores search results,
and `auth-source-netrc-cache', where parsed file contents are cached
by mtime."
  ;; 1. Clear search-result cache in password-data.
  ;;    Keys are cons cells: (auth-source . (:host HOST :user KIND ...))
  (when (and (boundp 'password-data)
             (hash-table-p password-data))
    (let ((keys-to-remove '()))
      (maphash (lambda (k _v)
                 (when (and (consp k)
                            (eq (car k) 'auth-source)
                            (let ((spec (cdr k)))
                              (or (and (plist-member spec :host)
                                       (let ((h (plist-get spec :host)))
                                         (and (stringp h)
                                              (string-match-p "slack\\.com" h))))
                                  ;; Also match host-less searches (e.g. :user "token")
                                  ;; that would have hit our GPG file
                                  (and (not (plist-member spec :host))
                                       (plist-member spec :user)
                                       (member (plist-get spec :user)
                                               '("token" "cookie"))))))
                   (push k keys-to-remove)))
               password-data)
      (dolist (k keys-to-remove)
        (password-cache-remove k))))
  ;; 2. Clear the netrc file-content cache so auth-source re-reads the GPG file
  ;;    instead of relying on mtime comparison (which can miss same-second writes).
  (when (boundp 'auth-source-netrc-cache)
    (setq auth-source-netrc-cache
          (cl-remove-if (lambda (entry)
                          (or (string= (car entry) slacko-creds-gpg-file)
                              (string= (car entry) slacko-creds--legacy-gpg-file)))
                        auth-source-netrc-cache))))

;;;###autoload
(defun slacko-creds-refresh (&optional host)
  "Extract Slack credentials from the local app and cache them.
Covers the workspaces the app lists, those already cached, and HOST when
given: the app does not list every workspace of an Enterprise Grid org.
Tokens come off each workspace's HTML page, read with the session cookie."
  (interactive)
  (setq slacko-creds--last-refresh-time (current-time))
  (message "Extracting Slack credentials...")
  (let ((hosts (delete-dups (append (slacko-creds--discover-workspaces)
                                    (slacko-creds--cached-hosts)
                                    (and host (list host)))))
        (cookie (slacko-creds--decrypt-cookie))
        (entries '()))
    (unless hosts
      (user-error "No workspaces found.  Is the Slack app running and logged in?"))
    (unless cookie
      (user-error "Could not decrypt the Slack session cookie"))
    (message "Found %d workspace(s), cookie decrypted. Fetching tokens..."
             (length hosts))
    (dolist (h hosts)
      (let ((token (slacko-creds--extract-token-from-html h cookie)))
        (if token
            (progn
              (push (list h token cookie) entries)
              (message "  ✓ %s" h))
          (message "  ✗ %s - could not extract token" h))))
    (if entries
        (progn
          (slacko-creds--save-to-gpg entries)
          (slacko-creds--clear-cache)
          (message "Done. %d workspace(s) updated." (length entries)))
      (error "No valid credentials found"))))

(defun slacko-creds--auth-source-get (host kind)
  "Look up credential for HOST and KIND from the GPG file via auth-source."
  (let* ((auth-sources (append (list slacko-creds-gpg-file)
                               (when (and (not (string= slacko-creds-gpg-file
                                                        slacko-creds--legacy-gpg-file))
                                          (file-exists-p slacko-creds--legacy-gpg-file))
                                 (list slacko-creds--legacy-gpg-file))))
         (auth-source-cache-expiry nil)
         (found (car (auth-source-search :host host :user kind :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun slacko-creds-get (host kind)
  "Get cached credential for HOST (e.g. \"qlikdev.slack.com\").
KIND is either \"token\" or \"cookie\".
Reads from the GPG credentials file via `auth-source'.
If not found and credentials haven't been refreshed recently,
automatically runs `slacko-creds-refresh' and retries."
  (or (slacko-creds--auth-source-get host kind)
      (when (slacko-creds--refresh-due-p)
        (message "No %s for %s, refreshing credentials..." kind host)
        (slacko-creds-refresh host)
        (slacko-creds--auth-source-get host kind))))

;;; API Requests

(defun slacko-creds--auth-headers (host)
  "Request headers carrying HOST's API token and session cookie."
  (let ((token (slacko-creds-get host "token"))
        (cookie (slacko-creds-get host "cookie")))
    (unless token
      (error "No credentials for %s.  Is this workspace logged in?" host))
    (unless cookie
      (error "No cookie for %s.  Is this workspace logged in?" host))
    `(("Authorization" . ,(format "Bearer %s" token))
      ("Cookie" . ,(format "d=%s;" cookie))
      ("Content-Type" . "application/json"))))

(defun slacko-creds--api-url (endpoint params)
  "URL of Slack API ENDPOINT carrying the query PARAMS alist."
  (concat "https://slack.com/api/" endpoint
          (when params (concat "?" (url-build-query-string params)))))

(defun slacko-creds--read-response ()
  "Parse the JSON body of the HTTP response in the current buffer.
Returns nil when there is no body or it does not parse, which is what a
request cancelled mid-flight leaves behind.  `url' delivers the body
undecoded, so anything outside ASCII arrives mangled unless it is
decoded here."
  (goto-char (point-min))
  (when (re-search-forward "^\r?$" nil t)
    (forward-line 1)
    (let* ((raw (buffer-substring-no-properties (point) (point-max)))
           (body (if enable-multibyte-characters
                     raw
                   (decode-coding-string raw 'utf-8)))
           (json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol))
      (unless (string-empty-p body)
        (condition-case nil
            (json-read-from-string body)
          (error nil))))))

(defconst slacko-creds--token-errors
  '("invalid_auth" "not_authed" "token_revoked" "token_expired" "account_inactive")
  "Slack API errors that mean the credentials sent no longer work.")

(defun slacko-creds--renewed-headers (host headers response)
  "Headers to resend a request to HOST with, when RESPONSE rejected HEADERS.
Nil for any other RESPONSE, and when the credentials, even after a
refresh at most once a minute, are still the ones Slack rejected."
  (when (member (alist-get 'error response) slacko-creds--token-errors)
    (when (and (equal headers (slacko-creds--auth-headers host))
               (slacko-creds--refresh-due-p))
      (message "Slack rejected the credentials for %s, refreshing..." host)
      (slacko-creds-refresh host))
    (let ((fresh (slacko-creds--auth-headers host)))
      (unless (equal fresh headers)
        fresh))))

(defun slacko-creds--send (endpoint params headers &optional post)
  "Send PARAMS to Slack API ENDPOINT with request HEADERS.
A GET carries them in the URL; with POST they go in a form body, which
is how Slack's methods that write take them.  Returns the parsed JSON
response or nil."
  (let* ((url-request-method (if post "POST" "GET"))
         (url-request-extra-headers
          (if post
              (cons '("Content-Type" . "application/x-www-form-urlencoded")
                    (assoc-delete-all "Content-Type" (copy-alist headers)))
            headers))
         (url-request-data (when post (url-build-query-string params)))
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (buf (url-retrieve-synchronously
               (slacko-creds--api-url endpoint (unless post params)) t nil 15)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (slacko-creds--read-response))
        (kill-buffer buf)))))

(defun slacko-creds--get-async (endpoint params headers callback)
  "Send a GET to Slack API ENDPOINT with query PARAMS and request HEADERS.
CALLBACK receives the parsed JSON response, or nil when the request
failed.  Returns the request buffer."
  (let* ((url-request-method "GET")
         (url-request-extra-headers headers)
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil))
    (url-retrieve
     (slacko-creds--api-url endpoint params)
     (lambda (status)
       (let ((buf (current-buffer))
             (data (unless (plist-get status :error)
                     (slacko-creds--read-response))))
         (when (buffer-live-p buf)
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf)))
         (funcall callback data)))
     nil t t)))

(defun slacko-creds-api-request (host endpoint params &optional post)
  "Make a synchronous authenticated request to Slack API ENDPOINT on HOST.
HOST is a workspace domain such as \"foo.slack.com\", ENDPOINT a method
such as \"users.info\", PARAMS an alist of parameters, sent as a form
body when POST is non-nil.  Returns the parsed JSON response or nil."
  (let* ((headers (slacko-creds--auth-headers host))
         (response (slacko-creds--send endpoint params headers post)))
    (if-let* ((renewed (slacko-creds--renewed-headers host headers response)))
        (slacko-creds--send endpoint params renewed post)
      response)))

(defun slacko-creds-api-request-async (host endpoint params callback)
  "Make an asynchronous authenticated Slack API request.
HOST, ENDPOINT and PARAMS are as in `slacko-creds-api-request'.
CALLBACK receives the parsed JSON response, or nil when the request
failed.  Returns the request buffer, which a caller that no longer
wants the response can kill."
  (let ((headers (slacko-creds--auth-headers host)))
    (slacko-creds--get-async
     endpoint params headers
     (lambda (data)
       (if-let* ((renewed
                  ;; an error here would never reach CALLBACK, and its
                  ;; caller would wait for an answer forever
                  (condition-case err
                      (slacko-creds--renewed-headers host headers data)
                    (error
                     (message "Slack credentials refresh failed: %s"
                              (error-message-string err))
                     nil))))
           (slacko-creds--get-async endpoint params renewed callback)
         (funcall callback data))))))

(defun slacko-creds-async-all (fns callback)
  "Call each of FNS at once, then CALLBACK with their results, in order.
Each of FNS takes the function to hand its result to, such as the
callback of `slacko-creds-api-request-async'.  CALLBACK runs once,
when the last result is in, and at once when FNS is empty."
  (if (null fns)
      (funcall callback nil)
    (let ((results (make-vector (length fns) nil))
          (pending (length fns)))
      (seq-do-indexed
       (lambda (fn index)
         (funcall fn (lambda (result)
                       (aset results index result)
                       ;; results arrive one at a time and in any order,
                       ;; so only a shared count can tell the last one
                       (setq pending (1- pending))
                       (when (= 0 pending)
                         (funcall callback (append results nil))))))
       fns))))

(provide 'slacko-creds)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-creds.el ends here
