;;; slacko-jump.el --- Jump to recent Slack conversations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: November 11, 2026
;; Keywords: comm tools
;; Homepage: https://github.com/agzam/slacko.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `slacko-jump' lists the channels and DMs viewed lately and the threads
;; followed, newest first, a group for each org the Slack app shows.  The
;; row under point is previewed; RET opens a thread in Emacs, a channel or
;; DM in the Slack app.  Needs Consult, which the package does not require.
;;
;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'slacko)
(require 'slacko-consult)
(require 'slacko-creds)
(require 'slacko-render)
(require 'slacko-thread)
(require 'consult nil t)

;; Consult is optional, so it cannot be required outright
(declare-function consult--async-indicator "consult")
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-refresh "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--read "consult")

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

;;; Orgs and workspaces

(defun slacko-jump--orgs ()
  "Orgs to list: those the Slack app shows, or else every credentials host."
  (or (slacko-creds--root-state-orgs)
      (mapcar (lambda (host)
                (list :name (car (split-string host "\\.")) :host host))
              (slacko--available-hosts))))

(defvar slacko-jump--workspaces nil
  "What `auth.test' says of each credentials host, as plists.
An Enterprise Grid org answers for its history and its threads at a
host of its own, which Slack refuses some methods, chat.getPermalink
among them, so links go to the host of a workspace inside the org.")

(defun slacko-jump--workspace (host auth)
  "HOST as AUTH, its `auth.test' response, describes it, or nil."
  (when (eq (alist-get 'ok auth) t)
    (list :host host
          :team (alist-get 'team_id auth)
          :enterprise (alist-get 'enterprise_id auth)
          :org-wide (eq (alist-get 'is_enterprise_install auth) t))))

(defun slacko-jump--learn-workspaces ()
  "Fill `slacko-jump--workspaces' in the background, once a session."
  (unless slacko-jump--workspaces
    (let ((hosts (slacko--available-hosts)))
      (slacko-creds-async-all
       (mapcar (lambda (host)
                 (lambda (done)
                   ;; no renewal: a host left behind in the credentials
                   ;; file must not set off a refresh of them all
                   (condition-case nil
                       (slacko-creds--get-async
                        "auth.test" nil (slacko-creds--auth-headers host) done)
                     (error (funcall done nil)))))
               hosts)
       (lambda (responses)
         (setq slacko-jump--workspaces
               (delq nil (seq-mapn #'slacko-jump--workspace
                                   hosts responses))))))))

(defun slacko-jump--team (item)
  "Id of the workspace ITEM lives in, or nil when it is not known."
  (or (plist-get item :team)
      (alist-get 'context_team_id
                 (slacko-render-cached-channel (plist-get item :host)
                                               (plist-get item :channel)))))

(defun slacko-jump--link-host (item)
  "Host for links to ITEM: its workspace's, else one in its org, else the org's."
  (let ((workspaces (seq-remove (lambda (workspace)
                                  (plist-get workspace :org-wide))
                                slacko-jump--workspaces))
        (team (slacko-jump--team item))
        (org-id (plist-get item :org-id)))
    (or (and team
             (plist-get (seq-find (lambda (workspace)
                                    (equal team (plist-get workspace :team)))
                                  workspaces)
                        :host))
        (and org-id
             (plist-get (seq-find (lambda (workspace)
                                    (equal org-id (plist-get workspace :enterprise)))
                                  workspaces)
                        :host))
        (plist-get item :host))))

(defun slacko-jump--url (item)
  "The Slack link of ITEM, a thread or a conversation."
  (concat "https://" (slacko-jump--link-host item)
          "/archives/" (plist-get item :channel)
          (when-let* ((ts (plist-get item :ts)))
            (concat "/p" (string-replace "." "" ts)))))

;;; What Slack says

(defun slacko-jump--parse (json)
  "JSON, the string a pref holds, parsed into alists, or nil."
  (when-let* (((stringp json))
              ((not (string-empty-p json)))
              (parsed (condition-case nil
                          (let ((json-object-type 'alist)
                                (json-array-type 'list)
                                (json-key-type 'symbol))
                            (json-read-from-string json))
                        (json-error nil)))
              ((consp parsed)))
    parsed))

(defun slacko-jump--conversation-entry-p (entry)
  "Non-nil when navigation ENTRY is a channel or a DM, not a page."
  (let ((id (alist-get 'id entry))
        (case-fold-search nil))
    (and (stringp id)
         (string-match-p "\\`[CDG][A-Z0-9]+\\'" id)
         (not (equal (alist-get 'object_type entry) "PAGE"))
         (numberp (alist-get 'timestamp entry)))))

(defun slacko-jump--viewed (response)
  "Conversations the `recents' pref in RESPONSE lists, as (ID . TIME).
TIME is in seconds, the newest first.  Pages such as the Threads view
are left out, and a conversation viewed twice shows once."
  (seq-uniq (thread-last response
              (alist-get 'prefs)
              (alist-get 'recents)
              (slacko-jump--parse)
              (alist-get 'navigation)
              (seq-filter #'slacko-jump--conversation-entry-p)
              (mapcar (lambda (entry)
                        (cons (alist-get 'id entry)
                              (/ (alist-get 'timestamp entry) 1000.0))))
              (seq-sort (lambda (a b) (< (cdr b) (cdr a)))))
            (lambda (a b) (equal (car a) (car b)))))

(defun slacko-jump--profile-name (profile)
  "The name PROFILE, the `user_profile' of a message, gives its author."
  (or (slacko-render--non-empty (alist-get 'display_name profile))
      (slacko-render--non-empty (alist-get 'real_name profile))))

(defun slacko-jump--threads (host response)
  "The threads in RESPONSE, the Threads view on HOST.
Each message there carries its author's profile, so their names are
cached on the way, for nothing."
  (let ((threads (seq-filter (lambda (thread)
                               (let ((root (alist-get 'root_msg thread)))
                                 (and (stringp (alist-get 'channel root))
                                      (stringp (alist-get 'ts root)))))
                             (alist-get 'threads response))))
    (dolist (thread threads)
      (dolist (msg (cons (alist-get 'root_msg thread)
                         (alist-get 'latest_replies thread)))
        (slacko-render-cache-user
         host (alist-get 'user msg)
         (slacko-jump--profile-name (alist-get 'user_profile msg)))))
    threads))

(defun slacko-jump--report (org responses)
  "Say which of RESPONSES ORG answered with an error."
  (dolist (response responses)
    (unless (eq (alist-get 'ok response) t)
      (message "Slack %s: %s" (plist-get org :name)
               (or (alist-get 'error response) "no answer")))))

;;; Items

(defun slacko-jump--make-item (org &rest fields)
  "A list item of ORG, made of FIELDS."
  (append (list :org (plist-get org :name)
                :org-id (plist-get org :id)
                :host (plist-get org :host))
          fields))

(defun slacko-jump--thread-item (org thread)
  "Item for THREAD, out of ORG's Threads view.
It is unread when its latest reply is newer than what was read of it."
  (let* ((root (alist-get 'root_msg thread))
         (latest (alist-get 'latest_reply root))
         (last-read (alist-get 'last_read root)))
    (slacko-jump--make-item
     org
     :channel (alist-get 'channel root)
     :ts (alist-get 'ts root)
     :team (alist-get 'team root)
     :time (string-to-number (or latest (alist-get 'ts root)))
     :text (alist-get 'text root)
     :replies (alist-get 'reply_count root)
     :unread (and (stringp latest)
                  (or (not (stringp last-read))
                      (string< last-read latest))))))

(defun slacko-jump--items (org viewed threads)
  "Items of ORG for the VIEWED conversations and followed THREADS, newest first."
  (seq-sort (lambda (a b) (< (plist-get b :time) (plist-get a :time)))
            (append (mapcar (pcase-lambda (`(,id . ,time))
                              (slacko-jump--make-item org :channel id :time time))
                            viewed)
                    (mapcar (apply-partially #'slacko-jump--thread-item org)
                            threads))))

;;; Candidates

(defun slacko-jump--label (host channel-id)
  "How the conversation CHANNEL-ID on HOST reads, from what is cached."
  (let ((slacko-render-resolve-mentions nil))
    (or (slacko-render-conversation-label
         host (slacko-render-cached-channel host channel-id))
        channel-id)))

(defun slacko-jump--age (time &optional now)
  "How long before NOW, both in seconds since the epoch, TIME was."
  (let ((seconds (- (or now (float-time)) time)))
    (cond ((< seconds 60) "now")
          ((< seconds 3600) (format "%dm" (floor seconds 60)))
          ((< seconds 86400) (format "%dh" (floor seconds 3600)))
          (t (format "%dd" (floor seconds 86400))))))

(defun slacko-jump--annotation (item &optional now)
  "What ITEM is and how long before NOW it last moved."
  (let* ((replies (or (plist-get item :replies) 0))
         (what (cond ((not (plist-get item :ts)) "viewed")
                     ((= replies 1) "1 reply")
                     (t (format "%d replies" replies)))))
    (concat (propertize (format "  %-11s %4s" what
                                (slacko-jump--age (plist-get item :time) now))
                        'face 'completions-annotations)
            (when (plist-get item :unread)
              (propertize "  new" 'face 'warning)))))

(defun slacko-jump--candidate (item)
  "Candidate string for ITEM, which rides along on its text properties."
  (let* ((host (plist-get item :host))
         (text (slacko-consult--plain-text (plist-get item :text) host))
         (shown (truncate-string-to-width text 70 nil ?\s "…")))
    (propertize
     (concat (truncate-string-to-width
              (slacko-jump--label host (plist-get item :channel)) 24 nil ?\s "…")
             "  "
             (if (plist-get item :unread) (propertize shown 'face 'bold) shown)
             ;; hidden, so that filtering reaches the whole message and
             ;; the org, and one channel name in two orgs stays two
             (propertize (concat " " text " " (plist-get item :org))
                         'invisible t))
     'slacko-jump--item item
     'slacko-jump--annotation (slacko-jump--annotation item))))

(defun slacko-jump--item (candidate)
  "The item behind CANDIDATE, or nil."
  (when (and (stringp candidate) (not (string-empty-p candidate)))
    (get-text-property 0 'slacko-jump--item candidate)))

(defun slacko-jump--annotate (candidate)
  "Annotation for CANDIDATE, built along with it."
  (or (and (stringp candidate) (not (string-empty-p candidate))
           (get-text-property 0 'slacko-jump--annotation candidate))
      ""))

(defun slacko-jump--group (candidate transform)
  "Name of the org CANDIDATE is in, or CANDIDATE itself when TRANSFORM."
  (if transform
      candidate
    (plist-get (slacko-jump--item candidate) :org)))

;;; Fetching

(defun slacko-jump--unknown-partners (host channel-ids)
  "Ids of the people DMs among CHANNEL-IDS on HOST are with, names unknown."
  (delete-dups
   (seq-keep (lambda (id)
               (when-let* ((channel (slacko-render-cached-channel host id))
                           ((eq (alist-get 'is_im channel) t))
                           (user (alist-get 'user channel))
                           ((not (slacko-render-cached-user host user))))
                 user))
             channel-ids)))

(defun slacko-jump--with-names (host channel-ids callback)
  "Name the conversations CHANNEL-IDS on HOST, then call CALLBACK.
What is cached costs nothing.  The rest costs a `conversations.info' per
conversation, then a `users.info' per person a DM is with."
  (slacko-render-fetch-channels
   host (seq-remove (apply-partially #'slacko-render-cached-channel host)
                    channel-ids)
   (lambda ()
     (slacko-render-fetch-users
      host (slacko-jump--unknown-partners host channel-ids) callback))))

(defun slacko-jump--fetch-org (org callback)
  "Ask ORG what was viewed and which threads are followed, and name them.
CALLBACK receives ORG's candidates, nil when nothing could be asked."
  (let ((host (plist-get org :host)))
    (condition-case err
        (slacko-creds-async-all
         (list (lambda (done)
                 (slacko-creds-api-request-async
                  host "users.prefs.get" '((prefs "recents")) done))
               (lambda (done)
                 (slacko-creds-api-request-async
                  host "subscriptions.thread.getView" nil done)))
         (pcase-lambda (`(,prefs ,view))
           (slacko-jump--report org (list prefs view))
           (let ((viewed (slacko-jump--viewed prefs))
                 (threads (slacko-jump--threads host view)))
             (slacko-jump--with-names
              host
              (delete-dups
               (append (mapcar #'car viewed)
                       (mapcar (lambda (thread)
                                 (alist-get 'channel (alist-get 'root_msg thread)))
                               threads)))
              (lambda ()
                (funcall callback
                         (mapcar #'slacko-jump--candidate
                                 (slacko-jump--items org viewed threads))))))))
      (error
       (message "Slack %s: %s" (plist-get org :name) (error-message-string err))
       (funcall callback nil)))))

(defun slacko-jump--source (orgs)
  "Async source listing ORGS, each org's candidates arriving as it answers.
They are shown in the order of ORGS, whichever org answers first."
  (lambda (sink)
    (let ((results (make-vector (length orgs) nil))
          (pending (length orgs))
          (live t))
      (lambda (action)
        (pcase action
          ('setup
           (funcall sink 'setup)
           (funcall sink [indicator running])
           (slacko-jump--learn-workspaces)
           (seq-do-indexed
            (lambda (org index)
              (slacko-jump--fetch-org
               org
               (lambda (candidates)
                 (when live
                   ;; orgs answer one at a time, in any order
                   (aset results index candidates)
                   (setq pending (1- pending))
                   (funcall sink 'flush)
                   (when-let* ((all (mapcan #'copy-sequence results)))
                     (funcall sink all))
                   (when (= 0 pending)
                     (funcall sink [indicator finished]))))))
            orgs)
           nil)
          ('destroy
           (setq live nil)
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun slacko-jump--async-wrap (async)
  "Wrap ASYNC in Consult's indicator and refresh, leaving the input whole.
Nothing here is searched for: the completion style filters what came."
  (consult--async-pipeline async
                           (consult--async-indicator)
                           (consult--async-refresh)))

;;; Preview

(defun slacko-jump--mentions (text)
  "Ids of the people TEXT mentions."
  (when (stringp text)
    (let ((case-fold-search nil))
      (seq-keep (lambda (piece)
                  (when (string-match "\\`\\([UW][A-Z0-9]+\\)[|>]" piece)
                    (match-string 1 piece)))
                (cdr (split-string text "<@"))))))

(defun slacko-jump--unknown-people (host messages)
  "Ids of the authors of MESSAGES on HOST, and of whom they mention, unnamed."
  (thread-last messages
    (mapcan (lambda (msg)
              (cons (alist-get 'user msg)
                    (slacko-jump--mentions (alist-get 'text msg)))))
    (seq-filter (lambda (id)
                  (and (stringp id) (not (slacko-render-cached-user host id)))))
    (delete-dups)))

(defun slacko-jump--fetch-preview (item callback)
  "Ask Slack what ITEM shows, name the people in it, then call CALLBACK.
CALLBACK receives the response: the whole of a thread, or the latest 30
messages of a conversation."
  (let ((host (plist-get item :host))
        (channel (plist-get item :channel))
        (ts (plist-get item :ts)))
    (condition-case err
        (slacko-creds-api-request-async
         host
         (if ts "conversations.replies" "conversations.history")
         (if ts
             `((channel ,channel) (ts ,ts) (limit "200"))
           `((channel ,channel) (limit "30")))
         (lambda (data)
           (let ((messages (alist-get 'messages data)))
             (dolist (msg messages)
               (slacko-render-cache-user
                host (alist-get 'user msg)
                (slacko-jump--profile-name (alist-get 'user_profile msg))))
             (slacko-render-fetch-users
              host (slacko-jump--unknown-people host messages)
              (lambda ()
                (funcall callback
                         (or data '((ok . :json-false) (error . "no answer")))))))))
      (error
       (funcall callback `((ok . :json-false)
                           (error . ,(error-message-string err))))))))

(defun slacko-jump--preview-buffer (host)
  "The preview buffer, set up to show what HOST says."
  (let ((buffer (get-buffer-create slacko-consult-preview-buffer-name)))
    (with-current-buffer buffer
      ;; before the major mode, so `slacko-emoji-mode' knows whose
      ;; custom emojis to fetch when it turns itself on
      (setq slacko-render-host host)
      (unless (derived-mode-p 'slacko-search-mode)
        (slacko-search-mode)))
    buffer))

(defun slacko-jump--render-preview (buffer item &optional data)
  "Render into BUFFER what ITEM shows, out of DATA, Slack's response.
Without DATA the response is on its way.  Point goes to the end, where
the latest message is.  No name is asked for here."
  (let ((host (plist-get item :host))
        (channel (plist-get item :channel))
        (thread (plist-get item :ts))
        (slacko-render-resolve-mentions nil)
        (slacko-render-inline-images nil))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n\n" (slacko-jump--label host channel)))
        (cond
         ((null data) (insert "Loading…\n"))
         ((not (eq (alist-get 'ok data) t))
          (insert (format "Slack: %s\n" (or (alist-get 'error data) "no answer"))))
         (t
          (seq-do-indexed
           (lambda (msg index)
             (slacko-render-message
              (slacko-thread--normalize-message
               msg host channel (if (and thread (< 0 index)) 2 1))))
           (let ((messages (alist-get 'messages data)))
             (if thread messages (reverse messages)))))))
      (goto-char (point-max))
      (dolist (window (get-buffer-window-list buffer nil t))
        (with-selected-window window
          (goto-char (point-max))
          ;; mid-window, where Consult's own previews put point: a
          ;; minibuffer in a child frame often hides the bottom
          (recenter (/ (window-body-height) 2)))))))

(defun slacko-jump--state ()
  "Preview the row under point in the window the session started from.
A row costs one request the first time it is previewed; the answer is
kept for the rest of the session."
  (let ((answers (make-hash-table :test 'equal))
        window original current live)
    (lambda (action candidate)
      (pcase action
        ('setup
         (setq window (selected-window)
               original (window-buffer window)
               live t))
        ('preview
         (setq current (slacko-jump--item candidate))
         (cond
          ((not (window-live-p window)))
          (current
           (let ((item current)
                 (buffer (slacko-jump--preview-buffer (plist-get current :host))))
             (with-selected-window window
               (switch-to-buffer buffer 'norecord))
             (if-let* ((data (gethash item answers)))
                 (slacko-jump--render-preview buffer item data)
               (slacko-jump--render-preview buffer item)
               (slacko-jump--fetch-preview
                item
                (lambda (data)
                  (puthash item data answers)
                  (when (and live (equal item current) (buffer-live-p buffer))
                    (slacko-jump--render-preview buffer item data)))))))
          ((buffer-live-p original)
           (with-selected-window window
             (switch-to-buffer original 'norecord)))))
        ('exit
         (setq live nil)
         (when (and (window-live-p window) (buffer-live-p original))
           (with-selected-window window
             (switch-to-buffer original 'norecord)))
         (when-let* ((buffer (get-buffer slacko-consult-preview-buffer-name)))
           (kill-buffer buffer)))))))

;;; Actions

(defun slacko-jump-open (candidate)
  "Open CANDIDATE: a thread in Emacs, a channel or DM in the Slack app.
Emacs shows threads, not whole conversations."
  (when-let* ((item (slacko-jump--item candidate)))
    (if (plist-get item :ts)
        (slacko-thread-capture (slacko-jump--url item))
      (slacko--open-in-slack (slacko-jump--url item)))))

(defun slacko-jump-open-in-slack (candidate)
  "Open CANDIDATE in the Slack desktop app."
  (when-let* ((item (slacko-jump--item candidate)))
    (slacko--open-in-slack (slacko-jump--url item))))

(defun slacko-jump-copy-url (candidate)
  "Copy CANDIDATE's Slack link."
  (when-let* ((item (slacko-jump--item candidate))
              (url (slacko-jump--url item)))
    (kill-new url)
    (message "Copied %s" url)))

;;; Embark

(defvar-keymap slacko-jump-embark-map
  :doc "Embark actions for a `slacko-jump' candidate."
  "RET" #'slacko-jump-open
  "s" #'slacko-jump-open-in-slack
  "w" #'slacko-jump-copy-url)

(defun slacko-jump--embark-setup ()
  "Give the `slacko-jump' category to Embark, where it is installed."
  (when (require 'embark nil t)
    (set-keymap-parent slacko-jump-embark-map embark-general-map)
    (setf (alist-get 'slacko-jump embark-keymap-alist)
          'slacko-jump-embark-map)
    (setf (alist-get 'slacko-jump embark-default-action-overrides)
          #'slacko-jump-open)))

;;; Entry point

;;;###autoload
(defun slacko-jump ()
  "Jump to a Slack conversation viewed lately, or to a thread followed.
Each org the Slack app shows is a group, newest first, and the row under
point is previewed.  RET opens a thread in Emacs, and a channel or DM in
the Slack app."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "`slacko-jump' needs Consult"))
  (let ((orgs (or (slacko-jump--orgs)
                  (user-error "No Slack workspaces found.  Run `slacko-creds-refresh'"))))
    (slacko-jump--embark-setup)
    (slacko-jump-open
     (consult--read (slacko-jump--source orgs)
                    :prompt "Slack: "
                    :async-wrap #'slacko-jump--async-wrap
                    :state (slacko-jump--state)
                    ;; every row previewed costs a request: wait for the
                    ;; cursor to rest rather than fetch every row it crosses
                    :preview-key '(:debounce 0.2 any)
                    :lookup #'consult--lookup-member
                    :group #'slacko-jump--group
                    :annotate #'slacko-jump--annotate
                    :category 'slacko-jump
                    :require-match t
                    :sort nil))))

(provide 'slacko-jump)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-jump.el ends here
