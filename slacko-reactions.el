;;; slacko-reactions.el --- Fill in reactions while you read -*- lexical-binding: t; -*-
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
;; Slack's `search.messages' leaves reactions out, and the only way to
;; get them is one `conversations.history' request per message.  Asking
;; for all of them up front holds a search hostage for as long as the
;; page is big.
;;
;; A buffer here registers each rendered message with a marker, and the
;; messages on screen plus a few screenfuls below get their reactions
;; fetched asynchronously, so the reading stays ahead of the fetching.
;;
;;; Code:

(require 'seq)
(require 'slacko-creds)
(require 'slacko-render)

;;; Customizable Variables

(defgroup slacko-reactions nil
  "Reactions fetched behind the reading."
  :group 'slacko
  :prefix "slacko-reactions-")

(defcustom slacko-reactions-lookahead 2
  "Screenfuls below the window fetched ahead of the scrolling."
  :type 'integer
  :group 'slacko-reactions)

(defcustom slacko-reactions-max-requests 6
  "Reaction requests allowed in flight at once.
Slack rate-limits per method, so a whole page asking at once earns HTTP
429 for the tail of it."
  :type 'integer
  :group 'slacko-reactions)

(defcustom slacko-reactions-delay 0.05
  "Seconds between a scroll and the requests it triggers.
Fetching straight from `window-scroll-functions' would open network
connections inside redisplay.  A timer moves that to a clean context and
folds a burst of scrolling into one pass."
  :type 'number
  :group 'slacko-reactions)

;;; Internal Variables

(defvar-local slacko-reactions--entries nil
  "Messages in this buffer that may still have reactions to show.
Each entry is a plist with `:marker', `:host', `:channel', `:ts' and
`:state', where state is `pending', `active' or `done'.")

(defvar-local slacko-reactions--requests 0
  "Reaction requests currently in flight for this buffer.")

(defvar-local slacko-reactions--timer nil
  "Timer that will fetch what the windows of this buffer show.")

;;; Registration

(defun slacko-reactions-register (host channel ts position)
  "Remember the message at POSITION as one whose reactions are missing.
HOST, CHANNEL and TS identify the message to Slack.  POSITION is where
its reactions line belongs, as returned by `slacko-render-message'."
  (when (and host channel ts position)
    (let ((entry (list :marker (copy-marker position)
                       :host host
                       :channel channel
                       :ts ts
                       :state 'pending)))
      (push entry slacko-reactions--entries)
      entry)))

(defun slacko-reactions-reset ()
  "Forget every message registered in the current buffer."
  (when slacko-reactions--timer
    (cancel-timer slacko-reactions--timer)
    (setq slacko-reactions--timer nil))
  (dolist (entry slacko-reactions--entries)
    (set-marker (plist-get entry :marker) nil))
  (setq slacko-reactions--entries nil
        slacko-reactions--requests 0))

(defun slacko-reactions-setup ()
  "Start filling in reactions in the current buffer."
  (add-hook 'window-scroll-functions #'slacko-reactions--on-scroll nil t)
  (slacko-reactions-refresh))

;;; Scheduling

(defun slacko-reactions--span (window)
  "Buffer span WINDOW wants reactions for: what it shows, plus lookahead.
Measured in lines rather than from `window-end', which forces a
redisplay the scroll hook cannot afford."
  (let ((start (window-start window))
        (lines (* (1+ (max 0 slacko-reactions-lookahead))
                  (max 1 (window-body-height window)))))
    (cons start
          (save-excursion
            (goto-char start)
            (forward-line lines)
            (point)))))

(defun slacko-reactions--due (entries start end limit)
  "At most LIMIT of ENTRIES still pending between START and END.
Ordered by position, so the top of the span is asked for first."
  (let ((in-span
         (seq-filter
          (lambda (entry)
            (and (eq (plist-get entry :state) 'pending)
                 (let ((pos (marker-position (plist-get entry :marker))))
                   (and pos (<= start pos) (< pos end)))))
          entries)))
    (take (max 0 limit)
          (sort in-span
                (lambda (a b)
                  (< (marker-position (plist-get a :marker))
                     (marker-position (plist-get b :marker))))))))

(defun slacko-reactions--on-scroll (window _start)
  "Fetch what WINDOW now shows, once redisplay is over."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (slacko-reactions-refresh)))

(defun slacko-reactions-refresh ()
  "Fetch reactions for what the windows of this buffer show."
  (unless slacko-reactions--timer
    (setq slacko-reactions--timer
          (run-at-time slacko-reactions-delay nil
                       #'slacko-reactions--run (current-buffer)))))

(defun slacko-reactions--run (buffer)
  "Ask for the reactions the windows of BUFFER are waiting for."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq slacko-reactions--timer nil)
      (dolist (window (get-buffer-window-list buffer nil t))
        (let* ((span (slacko-reactions--span window))
               (limit (- slacko-reactions-max-requests
                         slacko-reactions--requests)))
          (dolist (entry (slacko-reactions--due
                          slacko-reactions--entries
                          (car span) (cdr span) limit))
            (slacko-reactions--fetch entry)))))))

;;; Fetching

(defun slacko-reactions--fetch (entry)
  "Ask Slack for ENTRY's reactions."
  (let ((buffer (current-buffer)))
    (plist-put entry :state 'active)
    (setq slacko-reactions--requests (1+ slacko-reactions--requests))
    (slacko-creds-api-request-async
     (plist-get entry :host) "conversations.history"
     `((channel ,(plist-get entry :channel))
       (latest ,(plist-get entry :ts))
       (inclusive "true")
       (limit "1"))
     (lambda (data)
       (slacko-reactions--receive buffer entry data)))))

(defun slacko-reactions--response-reactions (data)
  "Reactions carried by a `conversations.history' response DATA."
  (when (eq (alist-get 'ok data) t)
    (alist-get 'reactions (car (alist-get 'messages data)))))

(defun slacko-reactions--receive (buffer entry data)
  "Show ENTRY's reactions from DATA in BUFFER, then take the next message.
A request that came back empty is not retried: an error there is a rate
limit or a channel this session cannot read, and asking again produces
the same answer at the same cost."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq slacko-reactions--requests (max 0 (1- slacko-reactions--requests)))
      (plist-put entry :state 'done)
      (when-let* ((reactions (slacko-reactions--response-reactions data)))
        (slacko-reactions--insert entry reactions))
      (slacko-reactions-refresh))))

(defun slacko-reactions--insert (entry reactions)
  "Insert REACTIONS at the place ENTRY's message left for them.
Point and the window start are markers, so an insert above either of
them keeps its text where the reader last saw it."
  (let* ((marker (plist-get entry :marker))
         (buffer (marker-buffer marker)))
    (when (and (buffer-live-p buffer) (marker-position marker))
      (with-current-buffer buffer
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char marker)
            (slacko-render--insert-reactions reactions)))))))

(provide 'slacko-reactions)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-reactions.el ends here
