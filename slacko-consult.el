;;; slacko-consult.el --- Slack search in a Consult session -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: September 22, 2026
;; Keywords: comm tools
;; Homepage: https://github.com/agzam/slacko.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Slack search as a Consult session: results arrive while the query is
;; being typed, the candidate under point is rendered in a preview
;; window, and RET opens its thread.
;;
;; `slacko-search' routes here on its own when Consult is installed.
;; Consult is not a dependency of the package: without it nothing loads
;; this file and searching works as it always has.
;;
;; Embark is optional too.  Starting a session registers the result
;; category with Embark where Embark is installed, and does nothing
;; where it is not; loading this file on its own registers nothing.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'slacko)
(require 'slacko-creds)
(require 'slacko-mrkdwn)
(require 'slacko-reactions)
(require 'slacko-render)
(require 'slacko-thread)
(require 'consult nil t)

;; Consult is optional, so it cannot be required outright
(declare-function consult--async-min-input "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--read "consult")

;; Vertico is not a dependency either
(defvar vertico-count)

;;; Customizable Variables

(defgroup slacko-consult nil
  "Slack search in a Consult session."
  :group 'slacko
  :prefix "slacko-consult-")

(defcustom slacko-consult-page-size 100
  "Messages asked for per request.  Slack caps this at 100.
A big page costs the same round trip as a small one, so asking for the
maximum is what keeps a broad query down to a few requests."
  :type 'integer
  :group 'slacko-consult)

(defcustom slacko-consult-max-pages 3
  "Pages fetched for one query before the chain stops.
Slack rate-limits search per minute, and the tail of a broad query is
candidates nobody scrolls to."
  :type 'integer
  :group 'slacko-consult)

(defcustom slacko-consult-text-lines 2
  "Message lines shown under a candidate, 0 for none.
Completion UIs size their display by candidate count and are blind to
the screen lines an annotation adds, so an uncapped message balloons the
minibuffer past its usual height."
  :type 'integer
  :group 'slacko-consult)

(defcustom slacko-consult-text-width 110
  "Width the shown message lines are filled to."
  :type 'integer
  :group 'slacko-consult)

(defcustom slacko-consult-preview-buffer-name "*Slack Preview*"
  "Name of the buffer the candidate under point is rendered in."
  :type 'string
  :group 'slacko-consult)

;;; Internal Variables

(defvar slacko-consult--history nil
  "History of queries searched for in a Consult session.")

(defvar slacko-consult--host nil
  "Workspace the live session searches.")

(defvar slacko-consult--generation 0
  "Counter identifying the search a request belongs to.
Bumped whenever the source restarts or is torn down, so a response that
outlives its request can tell that nobody wants it.")

(defvar slacko-consult--seen (make-hash-table :test 'equal)
  "Permalinks already delivered for the current search.")

;;; Message text

(defun slacko-consult--user-mentions (text host)
  "Replace user mentions in TEXT with names HOST already resolved."
  (replace-regexp-in-string
   "<@\\([UW][A-Z0-9]+\\)\\(?:|\\([^>]*\\)\\)?>"
   (lambda (match)
     (let ((id (match-string 1 match))
           (name (match-string 2 match)))
       (concat "@" (or (slacko-render--non-empty name)
                       (slacko-render-cached-user host id)
                       id))))
   text))

(defun slacko-consult--channel-mentions (text)
  "Replace channel mentions in TEXT with their names."
  (replace-regexp-in-string
   "<#\\([CGD][A-Z0-9]+\\)\\(?:|\\([^>]*\\)\\)?>"
   (lambda (match)
     (let ((id (match-string 1 match))
           (name (match-string 2 match)))
       (concat "#" (or (slacko-render--non-empty name) id))))
   text))

(defun slacko-consult--broadcasts (text)
  "Replace Slack's broadcast and group mentions in TEXT with plain names."
  (replace-regexp-in-string
   "<!\\([^|>]+\\)\\(?:|\\([^>]*\\)\\)?>"
   (lambda (match)
     (let ((keyword (match-string 1 match))
           (label (match-string 2 match)))
       (or (slacko-render--non-empty label)
           (concat "@" (car (split-string keyword "\\^"))))))
   text))

(defun slacko-consult--links (text)
  "Replace links in TEXT with their label, or the URL when unlabelled."
  (replace-regexp-in-string
   "<\\([^|>]+\\)\\(?:|\\([^>]*\\)\\)?>"
   (lambda (match)
     (or (slacko-render--non-empty (match-string 2 match))
         (match-string 1 match)))
   text))

(defun slacko-consult--plain-text (text host)
  "TEXT of a message on HOST, reduced to one plain line.
Mentions resolve from what `slacko-render' cached already and nothing
here asks Slack: one keystroke builds a whole page of candidates, and a
request per name would put the network in front of every one of them."
  (if (not (stringp text))
      ""
    (let ((result text))
      (setq result (slacko-consult--user-mentions result host))
      (setq result (slacko-consult--channel-mentions result))
      (setq result (slacko-consult--broadcasts result))
      (setq result (slacko-consult--links result))
      (setq result (slacko-mrkdwn--decode-entities result))
      (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " result)))))

(defun slacko-consult--fill (text width)
  "TEXT wrapped to WIDTH."
  (let ((fill-column width)
        (use-hard-newlines t))
    (with-temp-buffer
      (insert text)
      (fill-region-as-paragraph (point-min) (point-max) 'left)
      (buffer-string))))

(defun slacko-consult--annotation (text)
  "TEXT as an indented block of at most `slacko-consult-text-lines' lines.
Only as much text as can possibly be shown is wrapped, so the cost does
not grow with the length of the message."
  (let ((cap slacko-consult-text-lines))
    (unless (or (< cap 1) (string-blank-p text))
      (let* ((width slacko-consult-text-width)
             (bounded (truncate-string-to-width text (* (1+ cap) width)))
             (lines (split-string (slacko-consult--fill bounded width) "\n" t))
             (clipped (or (< cap (length lines))
                          (< (length bounded) (length text))))
             (kept (take cap lines))
             (kept (if clipped
                       (append (butlast kept)
                               (list (concat (string-trim-right (car (last kept)))
                                             "…")))
                     kept)))
        (mapconcat (lambda (line) (concat "  " line)) kept "\n")))))

;;; Candidates

(defun slacko-consult--channel-label (host channel)
  "Where a message from CHANNEL on HOST was posted, as a candidate reads it.
Nothing is looked up: one keystroke builds a page of candidates, and
what a name costs is paid before the page is delivered, by
`slacko-consult--with-names'."
  (let ((slacko-render-resolve-mentions nil))
    (or (slacko-render-conversation-label host channel) "")))

(defun slacko-consult--candidate (match)
  "Candidate string for search result MATCH.
The message rides along on the text properties, so every action and the
preview work off what the search already returned."
  (let* ((msg (slacko--parse-result match))
         (host (plist-get msg :host))
         (text (slacko-consult--plain-text (plist-get msg :text) host))
         (author (propertize
                  (truncate-string-to-width
                   (or (slacko-render--non-empty (plist-get msg :author))
                       "Unknown")
                   18 nil ?\s "…")
                  'face 'bold))
         (channel (propertize
                   (truncate-string-to-width
                    (slacko-consult--channel-label
                     host (alist-get 'channel match))
                    20 nil ?\s "…")
                   'face 'shadow))
         (stamp (propertize
                 (or (slacko-render-format-timestamp (plist-get msg :ts)) "")
                 'face 'shadow))
         (headline (truncate-string-to-width
                    text
                    (if (< 0 slacko-consult-text-lines)
                        80
                      slacko-consult-text-width)
                    nil nil "…"))
         (annotation (slacko-consult--annotation text)))
    (propertize
     ;; the whole text goes on the candidate, hidden, so that narrowing
     ;; with the `#query#filter' syntax matches a message by what it
     ;; says and not merely by the part of it that fits on the line
     (concat (format "%s  %s  %s  %s" author channel stamp headline)
             (propertize text 'invisible t))
     'slacko-consult--match match
     'slacko-consult--message msg
     'slacko-consult--annotation (and annotation (concat "\n" annotation)))))

(defun slacko-consult--annotate (candidate)
  "Annotation for CANDIDATE, built when the candidate was."
  (or (get-text-property 0 'slacko-consult--annotation candidate) ""))

(defun slacko-consult--message (candidate)
  "The Slack message behind CANDIDATE, or nil."
  (when (and (stringp candidate) (not (string-empty-p candidate)))
    (get-text-property 0 'slacko-consult--message candidate)))

(defun slacko-consult--match (candidate)
  "The raw search result behind CANDIDATE, or nil."
  (when (and (stringp candidate) (not (string-empty-p candidate)))
    (get-text-property 0 'slacko-consult--match candidate)))

(defun slacko-consult--url (msg)
  "The https permalink of MSG, which is what Slack links look like."
  (when-let* ((permalink (slacko-render--non-empty (plist-get msg :permalink))))
    (replace-regexp-in-string "\\`slack:" "https:" permalink)))

;;; Fetching

(defun slacko-consult--dedup (rows)
  "ROWS this search has not delivered already.
Slack pages over a moving index, so a message can sit on two pages when
newer ones arrive between the requests."
  (seq-filter
   (lambda (row)
     (let ((key (plist-get (slacko-consult--message row) :permalink)))
       (cond ((or (null key) (string-empty-p key)) t)
             ((gethash key slacko-consult--seen) nil)
             (t (puthash key t slacko-consult--seen) t))))
   rows))

(defun slacko-consult--next-page (paging)
  "Page to ask for after PAGING, or nil when the chain stops."
  (let ((page (alist-get 'page paging))
        (pages (alist-get 'pages paging)))
    (when (and (numberp page) (numberp pages)
               (< page pages)
               (< page slacko-consult-max-pages))
      (1+ page))))

(defun slacko-consult--receive (data async)
  "Send the candidates in DATA downstream to ASYNC.
Returns the page to ask for next, or nil when there is none."
  (cond
   ((null data)
    (message "Slack search request failed")
    nil)
   ((not (eq (alist-get 'ok data) t))
    (message "Slack search failed: %s" (alist-get 'error data))
    nil)
   (t
    (let* ((messages (alist-get 'messages data))
           (rows (slacko-consult--dedup
                  (mapcar #'slacko-consult--candidate
                          (alist-get 'matches messages)))))
      (when rows (funcall async rows))
      (slacko-consult--next-page (alist-get 'paging messages))))))

(defun slacko-consult--dm-partner (match)
  "User id of the other party in MATCH, when it comes from a direct message."
  (let ((channel (alist-get 'channel match)))
    (when (eq (alist-get 'is_im channel) t)
      (alist-get 'user channel))))

(defun slacko-consult--learn-names (matches host)
  "Cache what MATCHES give away about workspace HOST at no cost.
Every result describes the conversation it came from, which is what
naming a thread would otherwise cost a request.  Only two people write
in a one-to-one conversation, so a message the other party wrote also
names them: their name and their id sit in the same result."
  (dolist (match matches)
    (slacko-render-cache-channel host (alist-get 'channel match))
    (when-let* ((partner (slacko-consult--dm-partner match))
                ((equal partner (alist-get 'user match))))
      (slacko-render-cache-user host partner (alist-get 'username match)))))

(defun slacko-consult--unknown-partners (matches host)
  "Ids on HOST that MATCHES are conversations with, whose name is unknown."
  (delete-dups
   (delq nil
         (mapcar (lambda (match)
                   (when-let* ((partner (slacko-consult--dm-partner match))
                               ((not (slacko-render-cached-user host partner))))
                     partner))
                 matches))))

(defun slacko-consult--with-names (matches host callback)
  "Name the conversations MATCHES come from on HOST, then call CALLBACK.
What the results already say is free.  What is left costs one
`users.info' request per person, once per session, and the candidates
wait for it rather than showing a user id nobody can read."
  (slacko-consult--learn-names matches host)
  (let ((unknown (slacko-consult--unknown-partners matches host)))
    (if (null unknown)
        (funcall callback)
      (let ((pending (length unknown)))
        (dolist (id unknown)
          (slacko-creds-api-request-async
           host "users.info" `((user ,id))
           (lambda (data)
             (when (eq (alist-get 'ok data) t)
               (let ((profile (alist-get 'profile (alist-get 'user data))))
                 (slacko-render-cache-user
                  host id (or (slacko-render--non-empty
                               (alist-get 'display_name profile))
                              (slacko-render--non-empty
                               (alist-get 'real_name profile))
                              (alist-get 'name (alist-get 'user data))))))
             (setq pending (1- pending))
             (when (= 0 pending)
               (funcall callback)))))))))

(defun slacko-consult--fetch (query page async generation next)
  "Ask Slack for PAGE of QUERY and send what comes back to ASYNC.
GENERATION is the search this request belongs to; a response from an
older one is dropped.  NEXT is called with the page after this one.
Returns the request buffer."
  (let ((host slacko-consult--host))
    (slacko-creds-api-request-async
     host "search.messages"
     `((query ,query)
       (count ,(number-to-string slacko-consult-page-size))
       (page ,(number-to-string page)))
     (lambda (data)
       (when (eql generation slacko-consult--generation)
         (slacko-consult--with-names
          (alist-get 'matches (alist-get 'messages data)) host
          (lambda ()
            ;; checked again: naming the conversations takes a request of
            ;; its own, and the search can be restarted while it is out
            (when (eql generation slacko-consult--generation)
              (when-let* ((page-after (slacko-consult--receive data async)))
                (funcall next page-after))))))))))

(defun slacko-consult--source (async)
  "Async source function feeding ASYNC with Slack search results."
  (let ((request-buffers nil)
        (input ""))
    (cl-labels
        ((cancel ()
           ;; a newer generation retires whatever is still in the air;
           ;; killing the buffers stops the rest from arriving
           (setq slacko-consult--generation (1+ slacko-consult--generation))
           (clrhash slacko-consult--seen)
           (dolist (buffer request-buffers)
             (when (buffer-live-p buffer)
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer buffer))))
           (setq request-buffers nil))
         (fetch (page)
           (when-let* ((buffer (slacko-consult--fetch
                                input page async
                                slacko-consult--generation #'fetch)))
             (push buffer request-buffers)))
         (restart ()
           (cancel)
           ;; the previous result set goes at once, rather than the new
           ;; one arriving mixed into it
           (funcall async 'flush)
           (unless (string-blank-p input)
             (fetch 1))))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (setq input action)
           (restart))

          ('cancel
           (cancel)
           (funcall async action))

          ('destroy
           (cancel)
           ;; the stages downstream tear down here too: the indicator
           ;; deletes its overlay and the refresh stage its timer
           (funcall async action))

          (_ (funcall async action)))))))

;;; Rendering a result set

(defun slacko-consult--render-buffer (name matches host &optional reactions)
  "Render MATCHES of workspace HOST into a `slacko-search-mode' buffer NAME.
With REACTIONS, the reactions Slack leaves out of a search result are
fetched once the buffer is on screen."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (slacko-reactions-reset)
      (let ((inhibit-read-only t)
            (pending nil))
        (erase-buffer)
        (dolist (match matches)
          (push (slacko--render-match match) pending))
        (setq slacko-render-host host)
        (unless (derived-mode-p 'slacko-search-mode)
          (slacko-search-mode))
        (goto-char (point-min))
        ;; after the major mode, which wipes buffer-local state
        (when reactions
          (dolist (entry (delq nil (nreverse pending)))
            (apply #'slacko-reactions-register entry))
          (slacko-reactions-setup))))
    buffer))

(defun slacko-consult--preview-buffer (candidate)
  "Buffer showing CANDIDATE on its own.
Nothing here goes to the network: a preview follows the cursor, and a
request per candidate would make it crawl."
  (let ((slacko-render-resolve-mentions nil)
        (slacko-render-inline-images nil)
        (msg (slacko-consult--message candidate)))
    (slacko-consult--render-buffer slacko-consult-preview-buffer-name
                                   (list (slacko-consult--match candidate))
                                   (plist-get msg :host))))

;;; Session

(defun slacko-consult--scale-vertico-count ()
  "Shrink the session's `vertico-count' to the usual height budget.
Vertico sizes its display by candidate count, blind to the screen lines
an annotation adds, and every candidate here occupies its own line plus
up to `slacko-consult-text-lines' of message.  A count already
buffer-local, e.g. set through vertico-multiform, is respected."
  (when (and (boundp 'vertico-count)
             (< 0 slacko-consult-text-lines)
             (not (local-variable-p 'vertico-count)))
    (setq-local vertico-count
                (max 4 (floor vertico-count (1+ slacko-consult-text-lines))))))

(defun slacko-consult--state ()
  "Preview the candidate under point, open the thread behind the chosen one."
  (let (window original preview)
    (lambda (action candidate)
      (pcase action
        ('setup
         (setq window (selected-window)
               original (window-buffer)))
        ('preview
         (when (window-live-p window)
           (if-let* ((msg (slacko-consult--message candidate)))
               (progn
                 (setq preview (slacko-consult--preview-buffer candidate))
                 (with-selected-window window
                   (switch-to-buffer preview 'norecord)))
             (when (buffer-live-p original)
               (with-selected-window window
                 (switch-to-buffer original 'norecord))))))
        ('exit
         (when (and (window-live-p window) (buffer-live-p original))
           (with-selected-window window
             (switch-to-buffer original 'norecord)))
         (when (buffer-live-p preview)
           (kill-buffer preview)))
        ('return
         (slacko-consult-open-thread candidate))))))

;;; Actions

(defun slacko-consult-open-thread (candidate)
  "Open the thread CANDIDATE belongs to."
  (when-let* ((msg (slacko-consult--message candidate))
              (url (slacko-consult--url msg)))
    (slacko-thread-capture url)))

(defun slacko-consult-open-message (candidate)
  "Show CANDIDATE on its own in a `slacko-search-mode' buffer."
  (when-let* ((msg (slacko-consult--message candidate)))
    (pop-to-buffer
     (slacko-consult--render-buffer slacko-search-buffer-name
                                    (list (slacko-consult--match candidate))
                                    (plist-get msg :host)
                                    t))))

(defun slacko-consult-open-in-slack (candidate)
  "Open CANDIDATE in the Slack desktop app."
  (when-let* ((msg (slacko-consult--message candidate))
              (url (slacko-consult--url msg)))
    (slacko--open-in-slack url)))

(defun slacko-consult-copy-url (candidate)
  "Copy CANDIDATE's Slack link."
  (when-let* ((msg (slacko-consult--message candidate))
              (url (slacko-consult--url msg)))
    (kill-new url)
    (message "Copied %s" url)))

(defun slacko-consult-copy-text (candidate)
  "Copy the text of CANDIDATE's message."
  (when-let* ((msg (slacko-consult--message candidate))
              (text (plist-get msg :text)))
    (kill-new text)
    (message "Copied message text")))

;;; Embark

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-exporters-alist)
(defvar embark-default-action-overrides)

(defvar-keymap slacko-consult-embark-map
  :doc "Embark actions for a Slack search result."
  "RET" #'slacko-consult-open-thread
  "o" #'slacko-consult-open-message
  "s" #'slacko-consult-open-in-slack
  "w" #'slacko-consult-copy-url
  "W" #'slacko-consult-copy-text)

(defun slacko-consult-embark-export (candidates)
  "Render CANDIDATES into a `slacko-search-mode' buffer."
  (let* ((matches (delq nil (mapcar #'slacko-consult--match candidates)))
         (host (plist-get (slacko-consult--message (car candidates)) :host)))
    (slacko-consult--render-buffer slacko-search-buffer-name matches host t)))

(defun slacko-consult--embark-setup ()
  "Give the `slacko-consult-result' category to Embark, where it is installed.
Embark is loaded here rather than waited for: a session is often what
pulls this file in, by which time anything left for Embark to run on
load has already run."
  (when (require 'embark nil t)
    (set-keymap-parent slacko-consult-embark-map embark-general-map)
    (setf (alist-get 'slacko-consult-result embark-keymap-alist)
          'slacko-consult-embark-map)
    (setf (alist-get 'slacko-consult-result embark-exporters-alist)
          #'slacko-consult-embark-export)
    (setf (alist-get 'slacko-consult-result embark-default-action-overrides)
          #'slacko-consult-open-thread)))

;;; Entry point

(defun slacko-consult--search (&optional query host)
  "Search Slack messages in a Consult session.
QUERY is what the session starts with.  HOST is the workspace to search,
defaulting to `slacko-default-host' or the first one available.

Not a command: `slacko-search' is the way in, and it comes here on its
own wherever Consult is installed.

Results arrive as the query is typed.  The candidate under point is
rendered in a preview window, and RET opens its thread."
  (unless (featurep 'consult)
    (user-error "A Consult session needs Consult.  Use `slacko-search'"))
  (slacko-consult--embark-setup)
  (let* ((slacko-consult--host (or host (slacko--default-host)))
         (slacko-consult--seen (make-hash-table :test 'equal))
         (workspace (car (split-string slacko-consult--host "\\."))))
    (minibuffer-with-setup-hook #'slacko-consult--scale-vertico-count
      (consult--read
       (consult--async-pipeline
        (consult--async-min-input)
        (consult--async-throttle)
        #'slacko-consult--source)
       :prompt (format "Slack %s: " workspace)
       :lookup #'consult--lookup-member
       :state (slacko-consult--state)
       :annotate #'slacko-consult--annotate
       :category 'slacko-consult-result
       :history '(:input slacko-consult--history)
       :initial query
       :require-match t
       :sort nil))))

(provide 'slacko-consult)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-consult.el ends here
