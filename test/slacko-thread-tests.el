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

(defconst slacko-thread-tests--messages
  '(((user . "U1") (ts . "1738226435.123456") (text . "question"))
    ((user . "U2") (ts . "1738226500.000100") (text . "answer")))
  "A thread of a question and its answer, as conversations.replies has it.")

(defmacro slacko-thread-tests--in-thread (&rest body)
  "Run BODY in the buffer showing `slacko-thread-tests--messages'.
The specs around it spy on `switch-to-buffer' and the name lookups."
  (declare (indent 0))
  `(let ((buffer nil))
     (unwind-protect
         (progn
           (slacko-thread--display
            slacko-thread-tests--messages "team.slack.com" "C456"
            "https://team.slack.com/archives/C456/p1738226435123456")
           (setq buffer (car (spy-calls-args-for 'switch-to-buffer 0)))
           (with-current-buffer buffer ,@body))
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(describe "replying in a thread buffer"
  (before-each
    (spy-on 'switch-to-buffer)
    (spy-on 'slacko-render-resolve-user :and-return-value "alice")
    (spy-on 'slacko-render-resolve-user-mentions :and-call-fake
            (lambda (_host text) text))
    (spy-on 'slacko-render-resolve-channel-mentions :and-call-fake
            (lambda (_host text) text))
    (spy-on 'slacko-render-channel :and-return-value
            '((id . "C456") (name . "general") (is_channel . t))))

  (it "drafts a heading after the last message and leaves point below it"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (expect (buffer-substring-no-properties (line-beginning-position 0) (point-max))
              :to-equal "** New reply\n")
      (expect (point) :to-equal (point-max))
      (expect slacko-thread-reply-mode :to-be t)
      (expect (key-binding (kbd "C-c C-c")) :to-be #'slacko-thread-reply-submit)
      (expect (key-binding (kbd "C-c C-k")) :to-be #'slacko-thread-reply-abort)))

  (it "takes text in the draft and keeps the messages read-only"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "my answer")
      (expect (slacko-thread--draft-text) :to-equal "my answer")
      (goto-char (point-min))
      (expect (insert "x") :to-throw 'text-read-only)
      (goto-char (/ (point-max) 2))
      (expect (insert "x") :to-throw 'text-read-only)
      (goto-char (slacko-thread--draft-body))
      (expect (delete-char -1) :to-throw 'text-read-only)))

  (it "undoes the typing and nothing before it"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "abc")
      (undo-boundary)
      (expect (primitive-undo 100 buffer-undo-list) :not :to-throw)
      (expect (slacko-thread--draft-text) :to-equal "")
      (expect (save-excursion (goto-char slacko-thread--draft)
                              (looking-at-p "\\*\\* New reply\n\\'"))
              :to-be t)))

  (it "names the conversation and the keys in the header line"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      ;; `format-mode-line' gives nothing in batch, so the parts are joined
      (let ((line (apply #'concat header-line-format)))
        (expect line :to-match "Reply in #general")
        (expect line :to-match "C-c C-c submit")
        (expect line :to-match "C-c C-k abort"))))

  (it "goes to the draft rather than starting a second one"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (goto-char (point-min))
      (slacko-thread-reply)
      (expect (point) :to-equal (point-max))
      (expect (how-many "^\\*\\* New reply$" (point-min) (point-max)) :to-equal 1)))

  (it "refuses to draft outside a thread buffer"
    (with-temp-buffer
      (expect (slacko-thread-reply) :to-throw 'user-error)))

  (it "posts the draft to the thread as mrkdwn and shows the thread again"
    (spy-on 'slacko-creds-api-request :and-return-value
            '((ok . t) (ts . "1738226600.000200")))
    (spy-on 'slacko-thread--fetch-thread :and-return-value
            (append slacko-thread-tests--messages
                    '(((user . "U3") (ts . "1738226600.000200") (text . "*mine* `x`")))))
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "*mine* ~x~")
      (slacko-thread-reply-submit)
      (expect 'slacko-creds-api-request :to-have-been-called-with
              "team.slack.com" "chat.postMessage"
              '((channel "C456") (thread_ts "1738226435.123456") (text "*mine* `x`"))
              t)
      (expect 'slacko-thread--fetch-thread :to-have-been-called-with
              "team.slack.com" "C456" "1738226435.123456")
      (expect (buffer-string) :not :to-match "New reply")
      (expect (buffer-string) :to-match "\\*mine\\* ~x~")
      (expect buffer-read-only :to-be t)
      (expect slacko-thread-reply-mode :to-be nil)
      (expect header-line-format :to-be nil)
      (expect (buffer-substring-no-properties (point) (line-end-position))
              :to-match "/p1738226600000200\\]")))

  (it "refuses an empty draft"
    (spy-on 'slacko-creds-api-request)
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "  \n ")
      (expect (slacko-thread-reply-submit)
              :to-throw 'user-error '("The reply is empty"))
      (expect 'slacko-creds-api-request :not :to-have-been-called)))

  (it "keeps the draft when Slack refuses it"
    (spy-on 'slacko-creds-api-request :and-return-value
            '((ok . :json-false) (error . "not_in_channel")))
    (spy-on 'slacko-thread-capture)
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "hello")
      (expect (slacko-thread-reply-submit)
              :to-throw 'user-error '("Slack refused the reply: not_in_channel"))
      (expect (slacko-thread--draft-text) :to-equal "hello")
      (expect slacko-thread-reply-mode :to-be t)
      (expect 'slacko-thread-capture :not :to-have-been-called)))

  (it "says when Slack did not answer, and keeps the draft"
    (spy-on 'slacko-creds-api-request :and-return-value nil)
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "hello")
      (expect (slacko-thread-reply-submit)
              :to-throw 'user-error
              '("No answer from Slack, check the thread before posting again"))
      (expect (slacko-thread--draft-text) :to-equal "hello")))

  (it "drops an empty draft unasked and leaves the thread as it was"
    (spy-on 'yes-or-no-p)
    (slacko-thread-tests--in-thread
      (let ((before (buffer-string)))
        (slacko-thread-reply)
        (slacko-thread-reply-abort)
        (expect 'yes-or-no-p :not :to-have-been-called)
        (expect (buffer-string) :to-equal before)
        (expect (text-property-not-all (point-min) (point-max) 'read-only nil)
                :to-be nil)
        (expect buffer-read-only :to-be t)
        (expect header-line-format :to-be nil)
        (expect slacko-thread-reply-mode :to-be nil))))

  (it "asks before dropping text, and keeps it on no"
    (spy-on 'yes-or-no-p :and-return-value nil)
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "half a thought")
      (slacko-thread-reply-abort)
      (expect 'yes-or-no-p :to-have-been-called)
      (expect (slacko-thread--draft-text) :to-equal "half a thought")))

  (it "will not render the thread over a draft"
    (slacko-thread-tests--in-thread
      (slacko-thread-reply)
      (insert "keep me")
      (expect (slacko-thread--display
               slacko-thread-tests--messages "team.slack.com" "C456"
               "https://team.slack.com/archives/C456/p1738226435123456")
              :to-throw 'user-error)
      (expect (slacko-thread--draft-text) :to-equal "keep me"))))

(describe "slacko-thread-mode-map"
  (it "follows the link at point with RET"
    (expect (lookup-key slacko-thread-mode-map (kbd "RET"))
            :to-be #'org-open-at-point))

  (it "forces the Slack app with C-c C-o"
    (expect (lookup-key slacko-thread-mode-map (kbd "C-c C-o"))
            :to-be #'slacko-open-in-slack))

  (it "binds no C-c LETTER, which is reserved for users"
    (expect (lookup-key slacko-thread-mode-map (kbd "C-c o")) :to-be nil))

  (it "drafts a reply with C-c C-r"
    (expect (lookup-key slacko-thread-mode-map (kbd "C-c C-r"))
            :to-be #'slacko-thread-reply)))

(describe "slacko-thread-reply-mode-map"
  (it "posts with C-c C-c and drops with C-c C-k"
    (expect (lookup-key slacko-thread-reply-mode-map (kbd "C-c C-c"))
            :to-be #'slacko-thread-reply-submit)
    (expect (lookup-key slacko-thread-reply-mode-map (kbd "C-c C-k"))
            :to-be #'slacko-thread-reply-abort)))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-thread-tests.el ends here
