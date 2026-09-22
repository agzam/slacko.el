;;; slacko-thread-tests.el --- tests for slacko-thread -*- lexical-binding: t; -*-
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
;;  Tests for captured threads.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-thread)

(describe "slacko-thread--url-parse"
  (it "breaks a message URL into its parts"
    (let ((parsed (slacko-thread--parse-url
                   "https://team.slack.com/archives/C456/p1738226435123456")))
      (expect (plist-get parsed :workspace) :to-equal "team")
      (expect (plist-get parsed :channel-id) :to-equal "C456")
      (expect (plist-get parsed :ts) :to-equal "1738226435.123456")))

  (it "takes the thread parent from the query when there is one"
    (expect (plist-get (slacko-thread--parse-url
                        "https://team.slack.com/archives/C456/p1738226435123456?thread_ts=1738000000.000100")
                       :thread-ts)
            :to-equal "1738000000.000100"))

  (it "returns nil for anything else"
    (expect (slacko-thread--parse-url "https://example.com/x") :to-be nil)))

(describe "slacko-thread--buffer-name"
  (it "puts the conversation inside the asterisks"
    (let ((slacko-thread-buffer-name "*Slack Thread*"))
      (expect (slacko-thread--buffer-name "#general")
              :to-equal "*Slack Thread: #general*")))

  (it "appends to a name that has no asterisks"
    (let ((slacko-thread-buffer-name "slack-thread"))
      (expect (slacko-thread--buffer-name "#general")
              :to-equal "slack-thread: #general")))

  (it "leaves the name alone when the conversation has none"
    (let ((slacko-thread-buffer-name "*Slack Thread*"))
      (expect (slacko-thread--buffer-name nil) :to-equal "*Slack Thread*"))))

(describe "slacko-thread--buffer"
  (it "gives every thread a buffer of its own"
    (let (buffers)
      (unwind-protect
          (let ((one (slacko-thread--buffer "#general" '("host" "C1" "1")))
                (two (slacko-thread--buffer "@alice" '("host" "D1" "2"))))
            (setq buffers (list one two))
            (expect one :not :to-be two)
            (expect (buffer-name one) :to-equal "*Slack Thread: #general*")
            (expect (buffer-name two) :to-equal "*Slack Thread: @alice*"))
        (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers)))))

  (it "reuses the buffer a thread is already open in"
    (let (buffer)
      (unwind-protect
          (progn
            (setq buffer (slacko-thread--buffer "#general" '("host" "C1" "1")))
            (with-current-buffer buffer
              (setq slacko-thread--id '("host" "C1" "1")))
            (expect (slacko-thread--buffer "#general" '("host" "C1" "1"))
                    :to-be buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  (it "keeps two threads of one conversation apart"
    (let (buffers)
      (unwind-protect
          (let ((one (slacko-thread--buffer "#general" '("host" "C1" "1"))))
            (with-current-buffer one
              (setq slacko-thread--id '("host" "C1" "1")))
            (let ((two (slacko-thread--buffer "#general" '("host" "C1" "2"))))
              (setq buffers (list one two))
              (expect two :not :to-be one)
              (expect (buffer-name two) :to-equal "*Slack Thread: #general*<2>")))
        (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(describe "slacko-thread--display"
  (before-each
    (spy-on 'switch-to-buffer)
    (spy-on 'slacko-render-resolve-user :and-return-value "alice")
    (spy-on 'slacko-render-resolve-user-mentions :and-call-fake
            (lambda (_host text) text))
    (spy-on 'slacko-render-resolve-channel-mentions :and-call-fake
            (lambda (_host text) text)))

  (it "names the buffer and the document after the conversation"
    (spy-on 'slacko-render-channel :and-return-value
            '((id . "C456") (name . "general") (is_channel . t)))
    (let ((buffer nil))
      (unwind-protect
          (progn
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738226435.123456") (text . "hi")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738226435123456")
            (setq buffer (car (spy-calls-args-for 'switch-to-buffer 0)))
            (with-current-buffer buffer
              (expect (buffer-name) :to-equal "*Slack Thread: #general*")
              (expect (buffer-string) :to-match "#\\+TITLE: #general")
              (expect (buffer-string) :to-match "hi")
              (expect slacko-thread--id
                      :to-equal '("team.slack.com" "C456" "1738226435.123456"))))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  (it "falls back to the channel id when Slack will not name it"
    (spy-on 'slacko-render-channel :and-return-value nil)
    (let ((buffer nil))
      (unwind-protect
          (progn
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738226435.123456") (text . "hi")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738226435123456")
            (setq buffer (car (spy-calls-args-for 'switch-to-buffer 0)))
            (expect (buffer-name buffer) :to-equal "*Slack Thread: C456*"))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  (it "leaves the thread already open where it is and refreshes it"
    (spy-on 'slacko-render-channel :and-return-value
            '((id . "C456") (name . "general") (is_channel . t)))
    (let ((buffer nil))
      (unwind-protect
          (progn
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738226435.123456") (text . "hi")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738226435123456")
            (setq buffer (car (spy-calls-args-for 'switch-to-buffer 0)))
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738226435.123456") (text . "hi again")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738226435123456")
            (expect (car (spy-calls-args-for 'switch-to-buffer 1)) :to-be buffer)
            (with-current-buffer buffer
              (expect (buffer-string) :to-match "hi again")))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  (it "opens a second thread of the same conversation beside the first"
    (spy-on 'slacko-render-channel :and-return-value
            '((id . "C456") (name . "general") (is_channel . t)))
    (let ((buffers nil))
      (unwind-protect
          (progn
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738226435.123456") (text . "one")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738226435123456")
            (slacko-thread--display
             (list '((user . "U1") (ts . "1738300000.000100") (text . "two")))
             "team.slack.com" "C456"
             "https://team.slack.com/archives/C456/p1738300000000100")
            (setq buffers (list (car (spy-calls-args-for 'switch-to-buffer 0))
                                (car (spy-calls-args-for 'switch-to-buffer 1))))
            (expect (nth 0 buffers) :not :to-be (nth 1 buffers))
            (expect (with-current-buffer (nth 0 buffers) (buffer-string))
                    :to-match "one")
            (expect (with-current-buffer (nth 1 buffers) (buffer-string))
                    :to-match "two"))
        (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(describe "slacko-thread-mode-map"
  (it "follows the link at point with RET"
    (expect (lookup-key slacko-thread-mode-map (kbd "RET"))
            :to-be #'org-open-at-point))

  (it "forces the Slack app with C-c C-o"
    (expect (lookup-key slacko-thread-mode-map (kbd "C-c C-o"))
            :to-be #'slacko-open-in-slack))

  (it "binds no C-c LETTER, which is reserved for users"
    (expect (lookup-key slacko-thread-mode-map (kbd "C-c o")) :to-be nil)))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-thread-tests.el ends here
