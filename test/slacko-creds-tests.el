;;; slacko-creds-tests.el --- tests for slacko-creds -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for request building and response parsing.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-creds)

(defun slacko-creds-tests--response-buffer (body)
  "Buffer holding an HTTP response with BODY, as `url' delivers it.
Undecoded bytes in a unibyte buffer, headers separated by CRLF."
  (let ((buf (generate-new-buffer " *slacko-creds-test-response*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
      (insert (encode-coding-string body 'utf-8)))
    buf))

(describe "slacko-creds--api-url"
  (it "builds the endpoint URL with query parameters"
    (expect (slacko-creds--api-url "search.messages" '((query "hello") (page "1")))
            :to-equal "https://slack.com/api/search.messages?query=hello&page=1"))

  (it "escapes parameter values"
    ;; which characters `url-hexify-string' spares differs between Emacs
    ;; versions, so the escaping is checked by what it means, not by how
    ;; it is spelled
    (let* ((url (slacko-creds--api-url "search.messages" '((query "in:#dev a b"))))
           (query (cadr (split-string url "[?]"))))
      (expect (string-match-p "[ #]" query) :to-be nil)
      (expect (url-unhex-string (cadr (split-string query "=")))
              :to-equal "in:#dev a b"))))

(describe "slacko-creds--auth-headers"
  (it "carries the token and the session cookie"
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (format "fake-%s" kind)))
    (let ((headers (slacko-creds--auth-headers "team.slack.com")))
      (expect (alist-get "Authorization" headers nil nil #'equal)
              :to-equal "Bearer fake-token")
      (expect (alist-get "Cookie" headers nil nil #'equal)
              :to-equal "d=fake-cookie;")))

  (it "errors when the workspace has no token"
    (spy-on 'slacko-creds-get :and-return-value nil)
    (expect (slacko-creds--auth-headers "team.slack.com") :to-throw 'error)))

(describe "slacko-creds--read-response"
  (it "parses the body as an alist"
    (let ((buf (slacko-creds-tests--response-buffer
                "{\"ok\":true,\"messages\":{\"total\":2}}")))
      (unwind-protect
          (let ((parsed (with-current-buffer buf (slacko-creds--read-response))))
            (expect (alist-get 'ok parsed) :to-be t)
            (expect (alist-get 'total (alist-get 'messages parsed)) :to-equal 2))
        (kill-buffer buf))))

  (it "decodes non-ASCII text"
    (let ((buf (slacko-creds-tests--response-buffer
                "{\"text\":\"caf\u00e9 \u4f60\u597d\"}")))
      (unwind-protect
          (let ((parsed (with-current-buffer buf (slacko-creds--read-response))))
            (expect (alist-get 'text parsed) :to-equal "caf\u00e9 \u4f60\u597d"))
        (kill-buffer buf))))

  (it "returns nil for an empty body"
    (let ((buf (slacko-creds-tests--response-buffer "")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf))))

  (it "returns nil for a truncated body"
    (let ((buf (slacko-creds-tests--response-buffer "{\"ok\":tr")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf))))

  (it "returns nil when there are no headers at all"
    (let ((buf (generate-new-buffer " *slacko-creds-test-response*")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf)))))

(describe "slacko-creds-api-request-async"
  (before-each
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (format "fake-%s" kind))))

  (it "hands the parsed response to the callback and kills the request buffer"
    (let (result request-buffer)
      (spy-on 'url-retrieve :and-call-fake
              (lambda (_url callback &rest _)
                (setq request-buffer
                      (slacko-creds-tests--response-buffer "{\"ok\":true}"))
                (with-current-buffer request-buffer (funcall callback nil))
                request-buffer))
      (slacko-creds-api-request-async
       "team.slack.com" "search.messages" '((query "hi"))
       (lambda (data) (setq result data)))
      (expect (alist-get 'ok result) :to-be t)
      (expect (buffer-live-p request-buffer) :to-be nil)))

  (it "requests the endpoint URL with authenticated headers"
    (spy-on 'url-retrieve :and-return-value nil)
    (slacko-creds-api-request-async
     "team.slack.com" "search.messages" '((query "hi")) #'ignore)
    (let ((url (car (spy-calls-args-for 'url-retrieve 0))))
      (expect url :to-match "\\`https://slack\\.com/api/search\\.messages\\?query=hi")))

  (it "calls back with nil when the request errored"
    (let ((called 'not-called))
      (spy-on 'url-retrieve :and-call-fake
              (lambda (_url callback &rest _)
                (let ((buf (slacko-creds-tests--response-buffer "{\"ok\":true}")))
                  (with-current-buffer buf
                    (funcall callback '(:error (error connection-failed))))
                  buf)))
      (slacko-creds-api-request-async
       "team.slack.com" "search.messages" '((query "hi"))
       (lambda (data) (setq called data)))
      (expect called :to-be nil))))

(defmacro slacko-creds-tests--with-root-state (json &rest body)
  "Run BODY against a Slack data directory whose root-state.json holds JSON.
With JSON nil the directory has no such file."
  (declare (indent 1))
  `(let ((slacko-creds-slack-data-dir
          (file-name-as-directory (make-temp-file "slacko-data-" t))))
     (unwind-protect
         (progn
           (when ,json
             (make-directory (expand-file-name "storage" slacko-creds-slack-data-dir))
             (with-temp-file (expand-file-name "storage/root-state.json"
                                               slacko-creds-slack-data-dir)
               (insert ,json)))
           ,@body)
       (delete-directory slacko-creds-slack-data-dir t))))

(describe "slacko-creds--root-state-hosts"
  (it "takes each workspace's url, where an Enterprise Grid org lives"
    (slacko-creds-tests--with-root-state
        (concat "{\"workspaces\":{"
                "\"T1\":{\"domain\":\"team\",\"url\":\"https://team.slack.com/\"},"
                "\"E1\":{\"domain\":\"grid-acme\","
                "\"url\":\"https://grid-acme.enterprise.slack.com/\"}}}")
      (expect (slacko-creds--root-state-hosts)
              :to-have-same-items-as
              '("team.slack.com" "grid-acme.enterprise.slack.com"))))

  (it "falls back to the domain when a workspace has no url"
    (slacko-creds-tests--with-root-state
        "{\"workspaces\":{\"T1\":{\"domain\":\"team\"}}}"
      (expect (slacko-creds--root-state-hosts) :to-equal '("team.slack.com"))))

  (it "returns nil without the file"
    (slacko-creds-tests--with-root-state nil
      (expect (slacko-creds--root-state-hosts) :to-be nil)))

  (it "returns nil when the file does not parse"
    (slacko-creds-tests--with-root-state "{\"workspaces\":"
      (expect (slacko-creds--root-state-hosts) :to-be nil))))

(describe "slacko-creds--discover-workspaces"
  (it "takes what root-state.json lists, without scanning IndexedDB"
    (spy-on 'slacko-creds--root-state-hosts :and-return-value '("team.slack.com"))
    (spy-on 'slacko-creds--indexeddb-hosts)
    (expect (slacko-creds--discover-workspaces) :to-equal '("team.slack.com"))
    (expect 'slacko-creds--indexeddb-hosts :not :to-have-been-called))

  (it "scans IndexedDB when root-state.json lists nothing"
    (spy-on 'slacko-creds--root-state-hosts :and-return-value nil)
    (spy-on 'slacko-creds--indexeddb-hosts :and-return-value '("team.slack.com"))
    (expect (slacko-creds--discover-workspaces) :to-equal '("team.slack.com"))))

(describe "slacko-creds--cached-hosts"
  (it "lists every host with a token in the credentials file"
    (spy-on 'slacko-creds--read-gpg-file :and-return-value
            (concat "machine a.slack.com login token password xoxc-a\n"
                    "machine a.slack.com login cookie password xoxd-a\n"
                    "machine b.enterprise.slack.com login token password xoxc-b\n"))
    (expect (slacko-creds--cached-hosts)
            :to-equal '("a.slack.com" "b.enterprise.slack.com")))

  (it "returns nil without a credentials file"
    (spy-on 'slacko-creds--read-gpg-file :and-return-value nil)
    (expect (slacko-creds--cached-hosts) :to-be nil)))

(describe "slacko-creds--extract-token-from-html"
  (it "reads the token off the workspace page"
    (spy-on 'url-retrieve-synchronously :and-call-fake
            (lambda (&rest _)
              (slacko-creds-tests--response-buffer
               "<script>{\"api_token\":\"xoxc-123\",\"x\":1}</script>")))
    (expect (slacko-creds--extract-token-from-html
             "grid-acme.enterprise.slack.com" "xoxd-1")
            :to-equal "xoxc-123"))

  (it "never sends the cookie to a host outside slack.com"
    (spy-on 'url-retrieve-synchronously)
    (dolist (host '("evil.com?.slack.com" "evil.com/.slack.com" "slack.com.evil.com"))
      (expect (slacko-creds--extract-token-from-html host "xoxd-1") :to-be nil))
    (expect 'url-retrieve-synchronously :not :to-have-been-called)))

(describe "slacko-creds--refresh-due-p"
  (it "is due when no refresh ever ran"
    (let ((slacko-creds--last-refresh-time nil))
      (expect (slacko-creds--refresh-due-p) :to-be-truthy)))

  (it "waits a minute after the last refresh"
    (let ((slacko-creds--last-refresh-time (time-subtract nil 30)))
      (expect (slacko-creds--refresh-due-p) :to-be nil))
    (let ((slacko-creds--last-refresh-time (time-subtract nil 61)))
      (expect (slacko-creds--refresh-due-p) :to-be-truthy))))

(describe "slacko-creds-refresh"
  (before-each
    (spy-on 'slacko-creds--decrypt-cookie :and-return-value "xoxd-1")
    (spy-on 'slacko-creds--extract-token-from-html :and-call-fake
            (lambda (host _cookie) (concat "xoxc-" host)))
    (spy-on 'slacko-creds--save-to-gpg)
    (spy-on 'slacko-creds--clear-cache))

  (it "also covers cached hosts the app no longer lists, and the host asked for"
    (spy-on 'slacko-creds--discover-workspaces :and-return-value
            '("team.slack.com" "grid-acme.enterprise.slack.com"))
    (spy-on 'slacko-creds--cached-hosts :and-return-value
            '("team.slack.com" "acme.slack.com"))
    (let ((slacko-creds--last-refresh-time nil))
      (slacko-creds-refresh "other.slack.com")
      (expect (mapcar #'car (car (spy-calls-args-for 'slacko-creds--save-to-gpg 0)))
              :to-have-same-items-as
              '("team.slack.com" "grid-acme.enterprise.slack.com"
                "acme.slack.com" "other.slack.com"))))

  (it "counts as a refresh even when it fails"
    (spy-on 'slacko-creds--discover-workspaces :and-return-value nil)
    (spy-on 'slacko-creds--cached-hosts :and-return-value nil)
    (let ((slacko-creds--last-refresh-time nil))
      (expect (slacko-creds-refresh) :to-throw 'user-error)
      (expect (slacko-creds--refresh-due-p) :to-be nil))))

(describe "slacko-creds-get"
  (it "refreshes for a host it has nothing for, and asks for that host"
    (let ((slacko-creds--last-refresh-time nil)
          (lookups 0))
      (spy-on 'slacko-creds--auth-source-get :and-call-fake
              (lambda (&rest _)
                (setq lookups (1+ lookups))
                (when (< 1 lookups) "xoxc-new")))
      (spy-on 'slacko-creds-refresh)
      (expect (slacko-creds-get "member.slack.com" "token") :to-equal "xoxc-new")
      (expect 'slacko-creds-refresh :to-have-been-called-with "member.slack.com")))

  (it "does not refresh twice within a minute"
    (let ((slacko-creds--last-refresh-time (current-time)))
      (spy-on 'slacko-creds--auth-source-get)
      (spy-on 'slacko-creds-refresh)
      (expect (slacko-creds-get "member.slack.com" "token") :to-be nil)
      (expect 'slacko-creds-refresh :not :to-have-been-called))))

(describe "slacko-creds-api-request with rejected credentials"
  :var (token responses sent on-send)
  (before-each
    (setq token "xoxc-dead" sent nil on-send nil)
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (if (equal kind "token") token "xoxd-1")))
    (spy-on 'url-retrieve-synchronously :and-call-fake
            (lambda (&rest _)
              (push (alist-get "Authorization" url-request-extra-headers nil nil #'equal)
                    sent)
              (when on-send (funcall on-send))
              (slacko-creds-tests--response-buffer (pop responses)))))

  (it "refreshes them and sends the request once more"
    (setq responses '("{\"ok\":false,\"error\":\"invalid_auth\"}" "{\"ok\":true}"))
    (spy-on 'slacko-creds-refresh :and-call-fake
            (lambda (&rest _) (setq token "xoxc-fresh")))
    (let ((slacko-creds--last-refresh-time nil))
      (expect (alist-get 'ok (slacko-creds-api-request "team.slack.com" "auth.test" nil))
              :to-be t)
      (expect 'slacko-creds-refresh :to-have-been-called-with "team.slack.com")
      (expect (reverse sent) :to-equal '("Bearer xoxc-dead" "Bearer xoxc-fresh"))))

  (it "gives up when the refresh leaves the same credentials"
    (setq responses '("{\"ok\":false,\"error\":\"invalid_auth\"}"))
    (spy-on 'slacko-creds-refresh)
    (let ((slacko-creds--last-refresh-time nil))
      (expect (alist-get 'error (slacko-creds-api-request "team.slack.com" "auth.test" nil))
              :to-equal "invalid_auth")
      (expect (length sent) :to-equal 1)))

  (it "resends without a refresh when another request renewed them already"
    (setq responses '("{\"ok\":false,\"error\":\"invalid_auth\"}" "{\"ok\":true}")
          on-send (lambda () (setq token "xoxc-fresh")))
    (spy-on 'slacko-creds-refresh)
    (let ((slacko-creds--last-refresh-time (current-time)))
      (expect (alist-get 'ok (slacko-creds-api-request "team.slack.com" "auth.test" nil))
              :to-be t)
      (expect 'slacko-creds-refresh :not :to-have-been-called)
      (expect (reverse sent) :to-equal '("Bearer xoxc-dead" "Bearer xoxc-fresh"))))

  (it "passes other errors through"
    (setq responses '("{\"ok\":false,\"error\":\"channel_not_found\"}"))
    (spy-on 'slacko-creds-refresh)
    (let ((slacko-creds--last-refresh-time nil))
      (expect (alist-get 'error (slacko-creds-api-request
                                 "team.slack.com" "conversations.replies" nil))
              :to-equal "channel_not_found")
      (expect 'slacko-creds-refresh :not :to-have-been-called)
      (expect (length sent) :to-equal 1))))

(describe "slacko-creds-api-request-async with rejected credentials"
  :var (token responses sent)
  (before-each
    (setq token "xoxc-dead" sent nil)
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (if (equal kind "token") token "xoxd-1")))
    (spy-on 'url-retrieve :and-call-fake
            (lambda (_url callback &rest _)
              (push (alist-get "Authorization" url-request-extra-headers nil nil #'equal)
                    sent)
              (let ((buf (slacko-creds-tests--response-buffer (pop responses))))
                (with-current-buffer buf (funcall callback nil))
                buf))))

  (it "refreshes them and sends the request once more"
    (setq responses '("{\"ok\":false,\"error\":\"invalid_auth\"}" "{\"ok\":true}"))
    (spy-on 'slacko-creds-refresh :and-call-fake
            (lambda (&rest _) (setq token "xoxc-fresh")))
    (let ((slacko-creds--last-refresh-time nil)
          result)
      (slacko-creds-api-request-async "team.slack.com" "auth.test" nil
                                      (lambda (data) (setq result data)))
      (expect (alist-get 'ok result) :to-be t)
      (expect (reverse sent) :to-equal '("Bearer xoxc-dead" "Bearer xoxc-fresh"))))

  (it "still calls back when the refresh fails"
    (setq responses '("{\"ok\":false,\"error\":\"invalid_auth\"}"))
    (spy-on 'slacko-creds-refresh :and-call-fake
            (lambda (&rest _) (error "No valid credentials found")))
    (let ((slacko-creds--last-refresh-time nil)
          (result 'not-called))
      (slacko-creds-api-request-async "team.slack.com" "auth.test" nil
                                      (lambda (data) (setq result data)))
      (expect (alist-get 'error result) :to-equal "invalid_auth")
      (expect (length sent) :to-equal 1))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-creds-tests.el ends here
