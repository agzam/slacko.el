;;; slacko-mrkdwn.el --- Convert Slack mrkdwn to org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 19, 2025
;; Keywords: comm tools
;; Homepage: https://github.com/agzam/slacko.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Convert Slack's mrkdwn format to `org-mode' markup.
;;
;; Slack uses a variant of markdown called "mrkdwn" which has some
;; non-standard syntax like <url|text> for links and <@USER_ID> for
;; mentions.
;;
;;; Code:

(require 'subr-x)
(require 'org-element)

;;; Faces

(defface slacko-mrkdwn-blockquote-face
  '((((class color) (background light))
     :foreground "gray50" :slant italic :extend t)
    (((class color) (background dark))
     :foreground "gray60" :slant italic :extend t))
  "Face for blockquoted text in Slack messages."
  :group 'slacko)

(defface slacko-mrkdwn-blockquote-bar-face
  '((((class color) (background light))
     :foreground "gray70")
    (((class color) (background dark))
     :foreground "gray40"))
  "Face for the │ bar character in blockquotes."
  :group 'slacko)

;;; Font-lock

(defvar slacko-mrkdwn-font-lock-keywords
  `((,(rx bol (zero-or-more blank) (group "│") (group " " (zero-or-more nonl)))
     (1 'slacko-mrkdwn-blockquote-bar-face t)
     (2 'slacko-mrkdwn-blockquote-face t)))
  "Font-lock keywords for Slack mrkdwn elements.")

;;; Conversion Functions

(defun slacko-mrkdwn--convert-code-blocks (text)
  "Convert Slack code blocks to org src blocks in TEXT.
Handles blocks with or without language identifiers."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match ``` followed by optional lang (only if followed by newline), then code, then ```
    ;; Two patterns: ```lang\ncode``` or ```code```
    (while (re-search-forward "```\\(?:\\([a-zA-Z0-9]+\\)\n\\)?\\(\\(?:.\\|\n\\)*?\\)```" nil t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (lang (match-string-no-properties 1))  ; Only captured if followed by newline
             (code (match-string-no-properties 2))
             ;; Trim leading/trailing whitespace from code
             (trimmed-code (string-trim code))
             ;; Escape lines starting with * or #+  (org's comma-escape)
             (escaped-code
              (mapconcat
               (lambda (line)
                 (if (string-match-p "\\`\\(?:\\*\\|#\\+\\)" line)
                     (concat "," line)
                   line))
               (split-string trimmed-code "\n")
               "\n"))
             (replacement (format "#+begin_src %s\n%s\n#+end_src"
                                 (if (and lang (not (string-empty-p lang))) lang "text")
                                 escaped-code)))
        (delete-region match-start match-end)
        (goto-char match-start)
        (insert replacement)))
    (buffer-string)))

(defun slacko-mrkdwn--convert-inline-code (text)
  "Convert inline code (`code`) to org verbatim (~code~) in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match `code` but not inside code blocks (those should already be converted)
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (replace-match "~\\1~" t))
    (buffer-string)))

(defun slacko-mrkdwn--convert-links (text)
  "Convert Slack links to org links in TEXT.
Converts both <url|text> and bare <url> formats."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Convert <url|text> to [[url][text]]
    (while (re-search-forward "<\\([^|>]+\\)|\\([^>]+\\)>" nil t)
      (replace-match "[[\\1][\\2]]" t))

    (goto-char (point-min))
    ;; Convert bare <url> to [[url]]
    (while (re-search-forward "<\\(https?://[^>]+\\)>" nil t)
      (replace-match "[[\\1]]" t))
    (buffer-string)))

(defun slacko-mrkdwn--convert-strikethrough (text)
  "Convert Slack strikethrough (~text~) to org strikethrough (+text+) in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "~\\([^~\n]+\\)~" nil t)
      (replace-match "+\\1+" t))
    (buffer-string)))

(defun slacko-mrkdwn--convert-blockquotes (text)
  "Convert Slack blockquotes to `org-mode' quote lines in TEXT.
Slack uses `&gt;' (HTML-encoded `>') at the start of a line for quotes.
Converts to `│' prefix for a lightweight visual quote indicator."
  (replace-regexp-in-string
   "^&gt; ?" "│ " text))

(defun slacko-mrkdwn--decode-entities (text)
  "Decode Slack's HTML entities in TEXT.
Slack escapes only `&', `<' and `>'.  `&amp;' must be decoded last,
otherwise double-encoded text like `&amp;gt;' would collapse to `>'."
  (replace-regexp-in-string
   "&amp;" "&"
   (replace-regexp-in-string
    "&gt;" ">"
    (replace-regexp-in-string "&lt;" "<" text))))

(defun slacko-mrkdwn-to-org (text)
  "Convert Slack mrkdwn TEXT to `org-mode' format.

This handles:
- Code blocks: ```lang\\ncode\\n``` → #+begin_src lang...#+end_src
- Inline code: `code` → ~code~
- Links: <url|text> → [[url][text]]
- Bare URLs: <url> → [[url]]
- Blockquotes: &gt; text → │ text
- Strikethrough: ~text~ → +text+

Not yet implemented:
- User mentions: <@USER_ID>
- Channel mentions: <#CHANNEL_ID>
- Special formatting (these mostly work as-is in `org-mode')
  - Bold: *text*
  - Italic: _text_"
  (when (stringp text)
    (let* ((protected-blocks '())
           (counter 0)
           (text-with-placeholders text))

      ;; First, extract and protect code blocks with placeholders
      (setq text-with-placeholders
            (replace-regexp-in-string
             "```\\(?:[a-zA-Z0-9]+\n\\)?\\(?:.\\|\n\\)*?```"
             (lambda (match)
               (let ((placeholder (format "<<<CODE-BLOCK-%d>>>" counter)))
                 (push (cons placeholder match) protected-blocks)
                 (setq counter (1+ counter))
                 placeholder))
             text-with-placeholders))

      ;; Process links (now code blocks are protected)
      (setq text-with-placeholders (slacko-mrkdwn--convert-links text-with-placeholders))

      ;; Convert blockquotes (before restoring code blocks)
      (setq text-with-placeholders (slacko-mrkdwn--convert-blockquotes text-with-placeholders))

      ;; Convert strikethrough ~text~ -> +text+ (before inline code, which also uses ~)
      (setq text-with-placeholders (slacko-mrkdwn--convert-strikethrough text-with-placeholders))

      ;; Restore and convert code blocks
      (dolist (pair protected-blocks)
        (setq text-with-placeholders
              (replace-regexp-in-string
               (regexp-quote (car pair))
               (slacko-mrkdwn--convert-code-blocks (cdr pair))
               text-with-placeholders
               t t)))

      ;; Convert inline code, then decode HTML entities last: they are
      ;; transport encoding, so they must decode even inside code blocks,
      ;; and blockquote conversion above consumed the literal "&gt; " lines.
      (slacko-mrkdwn--decode-entities
       (slacko-mrkdwn--convert-inline-code text-with-placeholders)))))

;;; Org to mrkdwn

(defun slacko-mrkdwn--escape (text)
  "TEXT with the three characters Slack reads as markup escaped."
  (replace-regexp-in-string
   "[&<>]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (_ "&gt;")))
   text t t))

(defun slacko-mrkdwn--link-url (link)
  "URL Slack opens for the org LINK, or nil when it opens none.
A thread buffer writes Slack's own links as slack: links."
  (let ((raw (org-element-property :raw-link link)))
    (cond ((string-match-p "\\`\\(?:https?\\|mailto\\):" raw) raw)
          ((string-prefix-p "slack:" raw) (concat "https:" (substring raw 6))))))

(defun slacko-mrkdwn--from-org-node (node)
  "Mrkdwn of NODE, an `org-element' node or string of the current buffer.
What Slack has markup for is translated, the rest stays as typed."
  (if (stringp node)
      (slacko-mrkdwn--escape node)
    (let* ((type (org-element-type node))
           (inner (mapconcat #'slacko-mrkdwn--from-org-node
                             (org-element-contents node) ""))
           (blank (or (org-element-property :post-blank node) 0))
           (spaces (make-string blank ?\s))
           (lines (make-string blank ?\n))
           (url (and (eq type 'link) (slacko-mrkdwn--link-url node))))
      (pcase type
        ('org-data inner)
        ('bold (concat "*" inner "*" spaces))
        ((or 'italic 'underline) (concat "_" inner "_" spaces))
        ('strike-through (concat "~" inner "~" spaces))
        ((or 'code 'verbatim)
         (concat "`" (slacko-mrkdwn--escape (org-element-property :value node))
                 "`" spaces))
        ('line-break "\n")
        ((guard url)
         (concat "<" (slacko-mrkdwn--escape url)
                 (unless (string-empty-p inner) (concat "|" inner))
                 ">" spaces))
        ((or 'src-block 'example-block)
         ;; Slack shows a language name as the block's first line
         (concat "```\n"
                 (slacko-mrkdwn--escape
                  (string-trim-right (org-element-property :value node) "\n"))
                 "\n```\n" lines))
        ('quote-block
         (concat (replace-regexp-in-string "^" "> " (string-trim-right inner "\n"))
                 "\n" lines))
        (_ (let ((begin (org-element-property :begin node))
                 (end (org-element-property :end node))
                 (from (org-element-property :contents-begin node))
                 (to (org-element-property :contents-end node)))
             (if (and from to)
                 (concat (slacko-mrkdwn--escape (buffer-substring-no-properties begin from))
                         inner
                         (slacko-mrkdwn--escape (buffer-substring-no-properties to end)))
               (slacko-mrkdwn--escape (buffer-substring-no-properties begin end)))))))))

(defun slacko-mrkdwn-from-org (text)
  "Convert org TEXT to Slack mrkdwn, the reverse of `slacko-mrkdwn-to-org'.
Org's own parser decides what is markup, so what org shows as code goes
as code.  &, < and > are escaped, as Slack's API requires."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert text)
    (substring-no-properties
     (string-trim-right
      (slacko-mrkdwn--from-org-node (org-element-parse-buffer))))))

(provide 'slacko-mrkdwn)

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:

;;; slacko-mrkdwn.el ends here
