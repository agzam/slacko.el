;;; slacko-thread.el --- Capture Slack threads -*- lexical-binding: t; -*-
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
;; Fetch and display Slack messages and threads given a Slack URL.
;; Works with `slacko-creds' for workspace-aware authentication.
;;
;;; Code:

(require 'org)
(require 'rx)
(require 'seq)
(require 'thingatpt)
(require 'slacko-creds)
(require 'slacko-render)

;; `slacko' requires this file, so it can only be declared here
(declare-function slacko-open-in-slack "slacko")

;;; Customizable Variables

(defgroup slacko-thread nil
  "Fetch and display Slack threads."
  :group 'slacko
  :prefix "slacko-thread-")

(defcustom slacko-thread-buffer-name "*Slack Thread*"
  "Name of the buffer to display captured threads."
  :type 'string
  :group 'slacko-thread)

;;; URL Parsing

(defconst slacko-thread--url-regexp
  (rx "https://" (group (+ (not "/"))) ".slack.com"
      "/archives/" (group (+ (not "/")))
      "/p" (group (+ digit))
      (* anything))
  "Regexp matching a Slack message URL.
Groups: 1=workspace, 2=channel-id, 3=raw-timestamp.")

(defun slacko-thread--parse-url (url)
  "Parse a Slack message URL into its components.
Returns a plist (:workspace :channel-id :ts :thread-ts) or nil."
  (when (and url (string-match slacko-thread--url-regexp url))
    (let* ((workspace (match-string 1 url))
           (channel-id (match-string 2 url))
           (raw-ts (match-string 3 url))
           ;; Convert p1771363006380029 -> 1771363006.380029
           (ts (concat (substring raw-ts 0 -6) "." (substring raw-ts -6)))
           ;; Check for thread_ts query param
           (thread-ts (when (string-match "thread_ts=\\([0-9.]+\\)" url)
                        (match-string 1 url))))
      (list :workspace workspace
            :channel-id channel-id
            :ts ts
            :thread-ts thread-ts))))

(defun slacko-thread--detect-url ()
  "Detect a Slack URL from context.
Checks `thing-at-point' first, then the latest `kill-ring' entry."
  (or (when-let* ((url (thing-at-point 'url t)))
        (when (string-match-p slacko-thread--url-regexp url)
          url))
      (when-let* ((clip (ignore-errors (current-kill 0 t))))
        (when (string-match-p slacko-thread--url-regexp clip)
          clip))))

;;; API Requests

(defun slacko-thread--fetch-thread (host channel-id ts)
  "Fetch the thread TS started in CHANNEL-ID on workspace HOST.
Uses conversations.replies.  Returns the list of messages or nil."
  (let* ((resp (slacko-creds-api-request
                host "conversations.replies"
                `((channel ,channel-id)
                  (ts ,ts)
                  (limit "200")))))
    (if (eq (alist-get 'ok resp) t)
        (alist-get 'messages resp)
      (message "Slack API error: %s" (alist-get 'error resp))
      nil)))

(defun slacko-thread--fetch-single-message (host channel-id ts)
  "Fetch the message TS in CHANNEL-ID on workspace HOST.
Uses conversations.history.  Returns a one-element list or nil."
  (let* ((resp (slacko-creds-api-request
                host "conversations.history"
                `((channel ,channel-id)
                  (latest ,ts)
                  (inclusive "true")
                  (limit "1")))))
    (if (eq (alist-get 'ok resp) t)
        (alist-get 'messages resp)
      (message "Slack API error: %s" (alist-get 'error resp))
      nil)))

;;; Display

(defun slacko-thread--normalize-message (msg host channel-id level)
  "Normalize a Slack API message MSG into a plist for rendering.
HOST is the workspace domain.  CHANNEL-ID is the channel.
LEVEL is the org heading level (1 or 2)."
  (let* ((user-id (alist-get 'user msg))
         (author (or (when user-id
                       (let ((name (slacko-render-resolve-user host user-id)))
                         (when (and (stringp name) (not (string-empty-p name)))
                           name)))
                     "Unknown"))
         (ts (alist-get 'ts msg))
         (text (alist-get 'text msg))
         (files (alist-get 'files msg))
         (reactions (alist-get 'reactions msg))
         ;; Build permalink from components
         (msg-ts (replace-regexp-in-string "\\." "" ts))
         (permalink (format "slack://%s/archives/%s/p%s"
                            host channel-id msg-ts)))
    (list :author author
          :author-id user-id
          :text (or text "")
          :ts ts
          :permalink permalink
          :level level
          :files files
          :reactions reactions
          :host host)))

(defvar-local slacko-thread--id nil
  "Which thread this buffer holds: host, conversation and parent timestamp.")

(defun slacko-thread--buffer-name (label)
  "Name of a thread buffer showing LABEL.
Keeps the shape of `slacko-thread-buffer-name', which is normally
wrapped in asterisks."
  (cond ((null label) slacko-thread-buffer-name)
        ((string-suffix-p "*" slacko-thread-buffer-name)
         (concat (substring slacko-thread-buffer-name 0 -1) ": " label "*"))
        (t (concat slacko-thread-buffer-name ": " label))))

(defun slacko-thread--find-buffer (id)
  "The buffer already holding the thread ID, or nil."
  (seq-find (lambda (buffer)
              (equal id (buffer-local-value 'slacko-thread--id buffer)))
            (buffer-list)))

(defun slacko-thread--buffer (label id)
  "Buffer for the thread ID, named after LABEL.
A thread that is open already is refreshed in place; a different one
gets a buffer of its own, so picking several threads out of one set of
search results leaves them all open."
  (or (slacko-thread--find-buffer id)
      (generate-new-buffer (slacko-thread--buffer-name label))))

(defun slacko-thread--display (messages host channel-id url)
  "Display MESSAGES in an org buffer.
HOST is the workspace domain.  CHANNEL-ID and URL are for context."
  (let* ((label (or (slacko-render-conversation-label
                     host (slacko-render-channel host channel-id))
                    channel-id))
         (id (list host channel-id (alist-get 'ts (car messages))))
         (buf (slacko-thread--buffer label id))
         (parent (car messages))
         (replies (cdr messages)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n" label))
        (insert (format "#+SOURCE: %s\n" url))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%F %T")))
        ;; Parent message
        (slacko-render-message
         (slacko-thread--normalize-message parent host channel-id 1))
        ;; Replies
        (dolist (reply replies)
          (slacko-render-message
           (slacko-thread--normalize-message reply host channel-id 2))))
      ;; before the major mode, so `slacko-emoji-mode' knows whose
      ;; custom emojis to fetch when it turns itself on
      (setq slacko-render-host host)
      (unless (derived-mode-p 'slacko-thread-mode)
        (slacko-thread-mode))
      ;; after the major mode, which wipes buffer-local state
      (setq slacko-thread--id id)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;; Major Mode

(defvar slacko-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    ;; the buffer is read-only, so RET is free to take over the job
    ;; `C-c C-o' does in org, which this mode needs the key for
    (define-key map (kbd "RET") #'org-open-at-point)
    (define-key map (kbd "C-c C-o") #'slacko-open-in-slack)
    map)
  "Keymap for `slacko-thread-mode'.")

(define-derived-mode slacko-thread-mode org-mode "Slack-Thread"
  "Major mode for displaying a captured Slack thread.
Derived from `org-mode'.
\\{slacko-thread-mode-map}"
  (setq buffer-read-only t)
  (slacko-render-setup-font-lock)
  (slacko-render-setup-emoji))

;;; Interactive Commands

;;;###autoload
(defun slacko-thread-capture (&optional url)
  "Capture a Slack message or thread and display it in an org buffer.

URL resolution order:
1. Explicit URL argument
2. URL at point
3. Latest `kill-ring' entry matching a Slack URL
4. Error

If the message is a thread parent, the full thread is fetched.
If it's a standalone message, just that message is shown."
  (interactive)
  (let* ((url (or url
                  (slacko-thread--detect-url)
                  (user-error "No Slack URL found at point or in kill-ring")))
         (parsed (slacko-thread--parse-url url))
         (_ (unless parsed (user-error "Could not parse Slack URL: %s" url)))
         (workspace (plist-get parsed :workspace))
         (channel-id (plist-get parsed :channel-id))
         (ts (plist-get parsed :ts))
         (thread-ts (plist-get parsed :thread-ts))
         (host (format "%s.slack.com" workspace))
         ;; If URL has thread_ts, use that as the parent ts
         (parent-ts (or thread-ts ts)))
    (message "Fetching thread from %s..." host)
    (let ((messages (slacko-thread--fetch-thread host channel-id parent-ts)))
      (if messages
          (slacko-thread--display messages host channel-id url)
        ;; Fallback: try as single message
        (let ((single (slacko-thread--fetch-single-message host channel-id ts)))
          (if single
              (slacko-thread--display single host channel-id url)
            (user-error "Could not fetch message from %s" url)))))))

(provide 'slacko-thread)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-thread.el ends here
