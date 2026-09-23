;;; slacko-render.el --- Unified message rendering for Slacko -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 19, 2026
;; Keywords: comm tools
;; Homepage: https://github.com/agzam/slacko.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Shared message rendering for all Slacko views (search, thread, etc.).
;; Provides a single `slacko-render-message' entry point that both
;; `slacko-search-mode' and `slacko-thread-mode' use.  Also houses
;; user/channel mention resolution, inline image display, and
;; timestamp formatting.
;;
;;; Code:

(require 'url)
(require 'slacko-mrkdwn)
(require 'slacko-creds)

;; emojify is optional, and `slacko-emoji' hard-requires it
(declare-function slacko-emoji--maybe-enable "slacko-emoji")

;;; Buffer State

(defvar-local slacko-render-host nil
  "Slack workspace the current buffer was rendered from.")

;; the display functions set the host before the major-mode call, and
;; `kill-all-local-variables' would otherwise wipe it before the mode
;; body runs
(put 'slacko-render-host 'permanent-local t)

;;; Faces

(defface slacko-render-reaction-count
  '((t :height 0.7 :inherit default))
  "Face for the count superscript next to a reaction emoji."
  :group 'slacko)

;;; Customizable Variables

(defcustom slacko-render-inline-images t
  "Whether to display images inline in message buffers.
When non-nil, image files are downloaded and shown inline.
When nil, images are shown as regular file links.
Only works when workspace credentials are available."
  :type 'boolean
  :group 'slacko)

(defcustom slacko-render-image-max-width 480
  "Maximum width in pixels for inline images.
Images wider than this will be scaled down."
  :type 'integer
  :group 'slacko)

(defcustom slacko-render-timestamp-format "%Y-%m-%d %H:%M"
  "Format string for message timestamps.
See `format-time-string' for available format specifiers."
  :type 'string
  :group 'slacko)

;;; User Resolution

(defvar slacko-render--user-cache (make-hash-table :test 'equal)
  "Cache of user-id -> display-name, keyed as \"host:user-id\".")

(defvar slacko-render-resolve-mentions t
  "Whether an unknown mention may be looked up over the network.
Bound to nil where a render must not block, such as a preview: names
already cached still resolve, the rest stay as their raw ids.")

(defun slacko-render--non-empty (s)
  "Return S if it is a non-empty string, otherwise nil."
  (when (and (stringp s) (not (string-empty-p s))) s))

(defun slacko-render-cached-user (host user-id)
  "Display name already known for USER-ID on HOST, or nil.
For callers that turn many messages into text at once and cannot
afford a request per name."
  (slacko-render--non-empty
   (gethash (format "%s:%s" host user-id) slacko-render--user-cache)))

(defun slacko-render-cache-user (host user-id name)
  "Remember NAME as the display name of USER-ID on HOST.
For names that arrive as part of something else, so that nobody spends
a `users.info' request on a name Slack already sent.  An entry that is
there already stands: it came from the profile itself and says what the
person calls themselves."
  (when (and host user-id (slacko-render--non-empty name))
    (let ((key (format "%s:%s" host user-id)))
      (unless (gethash key slacko-render--user-cache)
        (puthash key name slacko-render--user-cache))
      (gethash key slacko-render--user-cache))))

(defun slacko-render-resolve-user (host user-id)
  "Resolve USER-ID to a display name for workspace HOST.
Results are cached.  Returns USER-ID if resolution fails."
  (let ((cache-key (format "%s:%s" host user-id)))
    (or (slacko-render--non-empty
         (gethash cache-key slacko-render--user-cache))
        (and (not slacko-render-resolve-mentions) user-id)
        (condition-case nil
            (let* ((resp (slacko-creds-api-request
                          host "users.info"
                          `((user ,user-id))))
                   (user (alist-get 'user resp))
                   (profile (alist-get 'profile user))
                   (name (or (slacko-render--non-empty
                              (alist-get 'display_name profile))
                             (slacko-render--non-empty
                              (alist-get 'real_name user))
                             (slacko-render--non-empty
                              (alist-get 'name user))
                             user-id)))
              (puthash cache-key name slacko-render--user-cache)
              name)
          (error user-id)))))

(defun slacko-render-resolve-user-mentions (host text)
  "Replace <@USER_ID> mentions in TEXT with display names for HOST.
If HOST is nil or credentials unavailable, return TEXT unchanged."
  (if (and host text (string-match-p "<@U[A-Z0-9]+>" text))
      (condition-case nil
          (let ((result text))
            (while (string-match "<@\\(U[A-Z0-9]+\\)>" result)
              (let* ((uid (match-string 1 result))
                     (name (save-match-data
                             (slacko-render-resolve-user host uid)))
                     (replacement (format "@%s" name)))
                (setq result (replace-match replacement t t result))))
            result)
        (error text))
    text))

;;; Channel Resolution

(defvar slacko-render--channel-cache (make-hash-table :test 'equal)
  "Cache of channel-id -> the alist Slack describes it with.
Keyed as \"host:channel-id\".")

(defun slacko-render-cache-channel (host channel)
  "Remember CHANNEL, as Slack describes it, for workspace HOST.
A search result describes the conversation each message came from well
enough to name it, which spares whoever opens one a
`conversations.info' request."
  (when-let* ((id (alist-get 'id channel))
              (key (format "%s:%s" host id)))
    (unless (gethash key slacko-render--channel-cache)
      (puthash key channel slacko-render--channel-cache))
    (gethash key slacko-render--channel-cache)))

(defun slacko-render-channel (host channel-id)
  "The conversation CHANNEL-ID on HOST, as Slack describes it, or nil.
Results are cached, one `conversations.info' per conversation."
  (let ((cache-key (format "%s:%s" host channel-id)))
    (or (gethash cache-key slacko-render--channel-cache)
        (and slacko-render-resolve-mentions
             (condition-case nil
                 (when-let* ((resp (slacko-creds-api-request
                                    host "conversations.info"
                                    `((channel ,channel-id))))
                             (channel (alist-get 'channel resp)))
                   (puthash cache-key channel slacko-render--channel-cache)
                   channel)
               (error nil))))))

(defun slacko-render-resolve-channel (host channel-id)
  "Resolve CHANNEL-ID to a channel name for workspace HOST.
Results are cached.  Returns CHANNEL-ID if resolution fails."
  (or (slacko-render--non-empty
       (alist-get 'name (slacko-render-channel host channel-id)))
      channel-id))

(defun slacko-render--group-members (name)
  "Members of a group conversation NAME, as Slack spells it.
Slack names one `mpdm-alice--bob--carol-1' and leaves the reading of it
to whoever displays it."
  (when (and (stringp name)
             (string-match "\\`mpdm-\\(.+\\)-[0-9]+\\'" name))
    (string-join (split-string (match-string 1 name) "--" t) ", ")))

(defun slacko-render-conversation-label (host channel)
  "How the conversation CHANNEL on HOST reads, or nil when it cannot be named.
CHANNEL is the alist Slack describes it with, from a search result or
from `conversations.info'; the two spell it the same way.  Slack names
a one-to-one conversation after the other party's user id alone, so
that name is resolved like any other mention."
  (cond
   ((null channel) nil)
   ((eq (alist-get 'is_im channel) t)
    (when-let* ((user (or (alist-get 'user channel)
                          (alist-get 'name channel))))
      (concat "@" (slacko-render-resolve-user host user))))
   ((eq (alist-get 'is_mpim channel) t)
    (when-let* ((members (slacko-render--group-members
                          (alist-get 'name channel))))
      (concat "@" members)))
   ((slacko-render--non-empty (alist-get 'name channel))
    (concat "#" (alist-get 'name channel)))))

(defun slacko-render-resolve-channel-mentions (host text)
  "Replace <#CHANNEL_ID|name> mentions in TEXT with org links.
When name is provided after the pipe, use it directly.
Otherwise, resolve via API (cached).
If HOST is nil or credentials unavailable, return TEXT unchanged."
  (if (and host text (string-match-p "<#C[A-Z0-9]+|" text))
      (condition-case nil
          (let ((result text))
            (while (string-match "<#\\(C[A-Z0-9]+\\)|\\([^>]*\\)>" result)
              (let* ((cid (match-string 1 result))
                     (inline-name (match-string 2 result))
                     (name (if (and inline-name
                                    (not (string-empty-p inline-name)))
                               inline-name
                             (save-match-data
                               (slacko-render-resolve-channel host cid))))
                     (replacement (format "[[slack://%s/archives/%s][#%s]]"
                                          host cid name)))
                (setq result (replace-match replacement t t result))))
            result)
        (error text))
    text))

;;; Images

(defun slacko-render--image-url-for-file (file)
  "Return the best thumbnail URL for FILE, or nil if not an image.
Prefers the 480px thumbnail, then 360, then 720, then the private URL."
  (let ((mimetype (alist-get 'mimetype file)))
    (when (and mimetype (string-prefix-p "image/" mimetype))
      (or (alist-get 'thumb_480 file)
          (alist-get 'thumb_360 file)
          (alist-get 'thumb_720 file)
          (alist-get 'url_private file)))))

(defun slacko-render--download-image (url host)
  "Download image at URL using credentials for HOST.
Returns image data as a string, or nil on failure."
  (condition-case nil
      (let* ((token (slacko-creds-get host "token"))
             (cookie (slacko-creds-get host "cookie"))
             (url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(format "Bearer %s" token))
                ("Cookie" . ,(format "d=%s;" cookie))))
             (url-cookie-storage nil)
             (url-cookie-secure-storage nil)
             (buf (url-retrieve-synchronously url t nil 15)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer buf))))
    (error nil)))

(defun slacko-render--insert-image (file host)
  "Insert an inline image for FILE using credentials for HOST.
Returns non-nil if the image was successfully inserted."
  (when-let* ((img-url (slacko-render--image-url-for-file file))
              (data (slacko-render--download-image img-url host))
              (img (create-image data nil t
                                 :max-width slacko-render-image-max-width
                                 :scale 1.0)))
    (insert-image img (format "[%s]" (or (alist-get 'name file) "image")))
    (insert "\n")
    t))

;;; Timestamp

(defun slacko-render-format-timestamp (ts)
  "Format a Slack timestamp TS into a human-readable string."
  (when (and ts (stringp ts))
    (ignore-errors
      (format-time-string slacko-render-timestamp-format
                          (seconds-to-time (string-to-number ts))))))

;;; Rendering Helpers

(defun slacko-render--build-author-link (host author author-id)
  "Build an org link for AUTHOR with AUTHOR-ID on HOST.
Returns a plain AUTHOR string if linking is not possible.
Never produces an empty link description."
  (let ((name (if (slacko-render--non-empty author)
                  author
                (or author-id "Unknown"))))
    (if (and host author-id)
        (format "[[slack://%s/team/%s][%s]]" host author-id name)
      name)))

(defun slacko-render--build-channel-link (host channel-name channel-id
                                               conversation-type)
  "Build an org link for a channel.
Uses CHANNEL-NAME, CHANNEL-ID, HOST, and CONVERSATION-TYPE.
Returns nil when channel info is not available."
  (when (and host channel-id)
    (let ((link-text (if (and conversation-type
                              (not (string= conversation-type "Channel")))
                         conversation-type
                       (format "#%s" (or channel-name "unknown")))))
      (format "[[slack://%s/archives/%s][%s]]" host channel-id link-text))))

(defun slacko-render--format-file-size (size)
  "Format file SIZE in bytes to human-readable string."
  (cond
   ((not size) "")
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%.1f MB" (/ size 1024.0 1024.0)))))

(defun slacko-render--insert-files (files host)
  "Insert file attachments for FILES using HOST for image auth."
  (dolist (file files)
    (let* ((name (or (alist-get 'name file) "file"))
           (url (or (alist-get 'url_private file)
                    (alist-get 'permalink file)
                    ""))
           (ptype (alist-get 'pretty_type file))
           (mimetype (alist-get 'mimetype file))
           (size (alist-get 'size file)))
      ;; Try inline image first for image files
      (if (and slacko-render-inline-images
               host mimetype
               (string-prefix-p "image/" mimetype)
               (slacko-render--insert-image file host))
          ;; Link below the image for reference
          (insert (format "  /[[%s][%s]]/\n" url name))
        ;; Fallback: regular link with size info
        (let ((size-str (slacko-render--format-file-size size)))
          (insert (format "- [[%s][%s]]%s\n"
                          url name
                          (cond
                           ((and (not (string-empty-p size-str)) ptype)
                            (format " (%s, %s)" size-str ptype))
                           ((not (string-empty-p size-str))
                            (format " (%s)" size-str))
                           (ptype (format " (%s)" ptype))
                           (t "")))))))))

(defun slacko-render--insert-reactions (reactions)
  "Insert REACTIONS as a formatted line."
  (when reactions
    (insert (mapconcat
             (lambda (r)
               (concat
                (format ":%s:" (alist-get 'name r))
                ;; zero-width space so the raised digits do not glue
                ;; themselves onto the shortcode
                (propertize (concat "\u200B"
                                    (number-to-string (alist-get 'count r)))
                            'display '(raise 0.3)
                            'face 'slacko-render-reaction-count)))
             reactions "  ")
            "\n")))

(defun slacko-render--insert-share-info (share-info host)
  "Insert shared message sub-heading from SHARE-INFO plist.
HOST is used for building links."
  (when share-info
    (let* ((orig-author (plist-get share-info :author-name))
           (orig-author-id (plist-get share-info :author-id))
           (orig-channel-id (plist-get share-info :channel-id))
           (orig-url (plist-get share-info :from-url)))
      (when (and orig-url
                 (string-match "/archives/\\([^/]+\\)/p\\([0-9]+\\)" orig-url))
        (let* ((orig-ts (match-string 2 orig-url))
               (orig-timestamp
                (when orig-ts
                  (ignore-errors
                    (slacko-render-format-timestamp
                     (concat (substring orig-ts 0 -6) "."
                             (substring orig-ts -6))))))
               (orig-author-link
                (slacko-render--build-author-link
                 host (or (slacko-render--non-empty orig-author) "Unknown")
                 orig-author-id))
               (orig-channel-link
                (if (and host orig-channel-id)
                    (format "[[slack://%s/archives/%s][#%s]]"
                            host orig-channel-id orig-channel-id)
                  "#unknown"))
               (orig-permalink
                (replace-regexp-in-string "^https:" "slack:" orig-url)))
          (insert (format "** /Shared from %s | Posted in %s | [[%s][%s]]/\n"
                          orig-author-link
                          orig-channel-link
                          orig-permalink
                          (or orig-timestamp "unknown date"))))))))

;;; Main Rendering Entry Point

(defun slacko-render-message (msg)
  "Render a normalized message MSG into the current buffer.

MSG is a plist with these keys:

  :author       - display name string
  :author-id    - user ID (for building profile link)
  :text         - raw mrkdwn text (will be converted to org)
  :ts           - raw Slack timestamp string
  :permalink    - slack:// URL string
  :level        - org heading level (integer, default 1)
  :files        - list of file alists from API (optional)
  :reactions    - list of reaction alists from API (optional)
  :host         - workspace host for API calls (may be nil)
  :channel-name - channel name (optional)
  :channel-id   - channel ID (optional)
  :conversation-type - \"Channel\", \"DM\", etc. (optional)
  :share-info   - shared message metadata plist (optional)

Returns a marker where the reactions line of this message belongs, for
callers that fetch reactions after the message is on screen."
  (let* ((author (or (slacko-render--non-empty (plist-get msg :author))
                     "Unknown"))
         (author-id (plist-get msg :author-id))
         (raw-text (plist-get msg :text))
         (ts (plist-get msg :ts))
         (permalink (or (plist-get msg :permalink) ""))
         (level (or (plist-get msg :level) 1))
         (files (plist-get msg :files))
         (reactions (plist-get msg :reactions))
         (host (plist-get msg :host))
         (channel-name (plist-get msg :channel-name))
         (channel-id (plist-get msg :channel-id))
         (conv-type (plist-get msg :conversation-type))
         (share-info (plist-get msg :share-info))
         ;; Process text through the full pipeline
         (text (slacko-render-resolve-user-mentions host raw-text))
         (text (slacko-render-resolve-channel-mentions host text))
         (text (if text (slacko-mrkdwn-to-org text) ""))
         ;; Build display elements
         (stars (make-string level ?*))
         (timestamp (or (slacko-render-format-timestamp ts) "unknown date"))
         (author-link (slacko-render--build-author-link
                       host author author-id))
         (channel-link (slacko-render--build-channel-link
                        host channel-name channel-id conv-type)))
    ;; Heading
    (if channel-link
        (insert (format "%s %s | %s | [[%s][%s]]\n"
                        stars author-link channel-link permalink timestamp))
      (insert (format "%s %s | [[%s][%s]]\n"
                      stars author-link permalink timestamp)))
    ;; Share info sub-heading
    (slacko-render--insert-share-info share-info host)
    ;; Text body
    (unless (string-empty-p text)
      (insert text "\n"))
    ;; Files
    (when files
      (slacko-render--insert-files files host))
    ;; Reactions, and where a later arriving set of them goes
    (prog1 (point-marker)
      (slacko-render--insert-reactions reactions)
      ;; Trailing newline
      (insert "\n"))))

(defun slacko-render-setup-font-lock ()
  "Set up font-lock keywords for a Slacko buffer.
Call this from mode definitions."
  (font-lock-add-keywords nil slacko-mrkdwn-font-lock-keywords))

(defun slacko-render-setup-emoji ()
  "Turn on emoji rendering in this buffer, where emojify is installed.
Call this from mode definitions."
  ;; emojify checked first: `slacko-emoji' hard-requires it, and a bare
  ;; NOERROR require would still signal from that inner require
  (when (and (require 'emojify nil t)
             (require 'slacko-emoji nil t))
    (slacko-emoji--maybe-enable)))

(provide 'slacko-render)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-render.el ends here
