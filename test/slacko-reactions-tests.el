;;; slacko-reactions-tests.el --- tests for slacko-reactions -*- lexical-binding: t; -*-
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
;;  Tests for reactions fetched behind the reading.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-reactions)

(defmacro slacko-reactions-tests--with-buffer (lines &rest body)
  "Run BODY in a buffer holding LINES numbered lines, shown in a window."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer " *slacko-reactions-test*"))
         (original (window-buffer (selected-window))))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (dotimes (i ,lines)
               (insert (format "line %d\n" i))))
           (set-window-buffer (selected-window) buffer)
           (with-current-buffer buffer ,@body))
       (set-window-buffer (selected-window) original)
       (kill-buffer buffer))))

(defun slacko-reactions-tests--entry (position &optional state)
  "A registered entry at POSITION, pending unless STATE says otherwise."
  (let ((entry (slacko-reactions-register
                "team.slack.com" "C1" "1738226435.123456" position)))
    (when state (plist-put entry :state state))
    entry))

(describe "slacko-reactions-register"
  (it "keeps the message and where its reactions go"
    (slacko-reactions-tests--with-buffer 3
      (let ((entry (slacko-reactions-tests--entry 5)))
        (expect (plist-get entry :host) :to-equal "team.slack.com")
        (expect (plist-get entry :channel) :to-equal "C1")
        (expect (plist-get entry :state) :to-be 'pending)
        (expect (marker-position (plist-get entry :marker)) :to-equal 5)
        (expect (length slacko-reactions--entries) :to-equal 1))))

  (it "ignores a message Slack cannot be asked about"
    (slacko-reactions-tests--with-buffer 3
      (expect (slacko-reactions-register "team.slack.com" nil "123" 1) :to-be nil)
      (expect slacko-reactions--entries :to-be nil)))

  (it "follows the text it marked when the buffer grows above it"
    (slacko-reactions-tests--with-buffer 3
      (let ((entry (slacko-reactions-tests--entry 8)))
        (save-excursion
          (goto-char (point-min))
          (insert "prefix\n"))
        (expect (marker-position (plist-get entry :marker)) :to-equal 15)))))

(describe "slacko-reactions-reset"
  (it "drops every entry and its marker"
    (slacko-reactions-tests--with-buffer 5
      (let ((entry (slacko-reactions-tests--entry 3)))
        (slacko-reactions-reset)
        (expect slacko-reactions--entries :to-be nil)
        (expect slacko-reactions--requests :to-equal 0)
        (expect (marker-position (plist-get entry :marker)) :to-be nil)))))

(describe "slacko-reactions--due"
  (it "takes the pending entries inside the span"
    (slacko-reactions-tests--with-buffer 20
      (let ((inside (slacko-reactions-tests--entry 20))
            (outside (slacko-reactions-tests--entry 90)))
        (expect (slacko-reactions--due (list inside outside) 1 50 10)
                :to-equal (list inside)))))

  (it "leaves out what was asked for already"
    (slacko-reactions-tests--with-buffer 20
      (let ((active (slacko-reactions-tests--entry 20 'active))
            (done (slacko-reactions-tests--entry 25 'done))
            (pending (slacko-reactions-tests--entry 30)))
        (expect (slacko-reactions--due (list active done pending) 1 50 10)
                :to-equal (list pending)))))

  (it "asks for the top of the span first"
    (slacko-reactions-tests--with-buffer 20
      (let ((lower (slacko-reactions-tests--entry 40))
            (upper (slacko-reactions-tests--entry 10)))
        (expect (slacko-reactions--due (list lower upper) 1 50 10)
                :to-equal (list upper lower)))))

  (it "stops at the limit"
    (slacko-reactions-tests--with-buffer 20
      (let ((first (slacko-reactions-tests--entry 10))
            (second (slacko-reactions-tests--entry 20)))
        (ignore second)
        (expect (slacko-reactions--due (list first second) 1 50 1)
                :to-equal (list first)))))

  (it "takes nothing when no request slot is free"
    (slacko-reactions-tests--with-buffer 20
      (let ((entry (slacko-reactions-tests--entry 10)))
        (expect (slacko-reactions--due (list entry) 1 50 0) :to-be nil)
        (expect (slacko-reactions--due (list entry) 1 50 -3) :to-be nil)))))

(describe "slacko-reactions--span"
  (it "starts where the window starts and reaches past what it shows"
    (slacko-reactions-tests--with-buffer 200
      (let* ((slacko-reactions-lookahead 1)
             (window (selected-window))
             (span (slacko-reactions--span window))
             (height (window-body-height window)))
        (expect (car span) :to-equal (window-start window))
        (expect (cdr span)
                :to-equal
                (save-excursion
                  (goto-char (window-start window))
                  (forward-line (* 2 height))
                  (point)))))))

(describe "slacko-reactions--insert"
  (it "writes the reactions line where the message left room for it"
    (slacko-reactions-tests--with-buffer 5
      (let ((entry (slacko-reactions-tests--entry (point-min)))
            (buffer-read-only t))
        (slacko-reactions--insert entry '(((name . "thumbsup") (count . 3))))
        (expect (buffer-substring-no-properties (point-min) (line-end-position))
                :to-match ":thumbsup:"))))

  (it "leaves point where the reader left it"
    (slacko-reactions-tests--with-buffer 20
      (let ((entry (slacko-reactions-tests--entry (point-min))))
        (goto-char (point-max))
        (let ((line (line-number-at-pos)))
          (slacko-reactions--insert entry '(((name . "tada") (count . 1))))
          (expect (line-number-at-pos) :to-equal (1+ line))
          (expect (point) :to-equal (point-max))))))

  (it "keeps the window showing the same text when the line lands above it"
    (slacko-reactions-tests--with-buffer 200
      (let* ((window (selected-window))
             (entry (slacko-reactions-tests--entry (point-min)))
             (top (save-excursion (goto-char (point-min)) (forward-line 50) (point))))
        (set-window-start window top)
        (let ((shown (save-excursion
                       (goto-char (window-start window))
                       (buffer-substring-no-properties
                        (point) (line-end-position)))))
          (slacko-reactions--insert entry '(((name . "eyes") (count . 2))))
          (expect (save-excursion
                    (goto-char (window-start window))
                    (buffer-substring-no-properties (point) (line-end-position)))
                  :to-equal shown)))))

  (it "does nothing once the buffer is gone"
    (let ((entry nil))
      (slacko-reactions-tests--with-buffer 3
        (setq entry (slacko-reactions-tests--entry (point-min))))
      (expect (slacko-reactions--insert entry '(((name . "x") (count . 1))))
              :to-be nil))))

(describe "slacko-reactions--response-reactions"
  (it "reads the reactions of the single returned message"
    (expect (slacko-reactions--response-reactions
             '((ok . t)
               (messages . (((ts . "1") (reactions . (((name . "wave") (count . 2)))))))))
            :to-equal '(((name . "wave") (count . 2)))))

  (it "returns nil for a message without reactions"
    (expect (slacko-reactions--response-reactions
             '((ok . t) (messages . (((ts . "1"))))))
            :to-be nil))

  (it "returns nil for a failed request"
    (expect (slacko-reactions--response-reactions nil) :to-be nil)
    (expect (slacko-reactions--response-reactions '((ok . :json-false)
                                                    (error . "ratelimited")))
            :to-be nil)))

(describe "slacko-reactions--fetch"
  (it "asks conversations.history for that one message"
    (slacko-reactions-tests--with-buffer 5
      (spy-on 'slacko-creds-api-request-async)
      (let ((entry (slacko-reactions-tests--entry (point-min))))
        (slacko-reactions--fetch entry)
        (expect (plist-get entry :state) :to-be 'active)
        (expect slacko-reactions--requests :to-equal 1)
        (let ((args (spy-calls-args-for 'slacko-creds-api-request-async 0)))
          (expect (nth 0 args) :to-equal "team.slack.com")
          (expect (nth 1 args) :to-equal "conversations.history")
          (expect (nth 2 args)
                  :to-equal '((channel "C1")
                              (latest "1738226435.123456")
                              (inclusive "true")
                              (limit "1"))))))))

(describe "slacko-reactions--receive"
  (it "shows what came back and frees the request slot"
    (slacko-reactions-tests--with-buffer 5
      (spy-on 'slacko-reactions-refresh)
      (let ((entry (slacko-reactions-tests--entry (point-min))))
        (setq slacko-reactions--requests 1)
        (slacko-reactions--receive
         (current-buffer) entry
         '((ok . t)
           (messages . (((reactions . (((name . "rocket") (count . 4)))))))))
        (expect (plist-get entry :state) :to-be 'done)
        (expect slacko-reactions--requests :to-equal 0)
        (expect (buffer-substring-no-properties (point-min) (line-end-position))
                :to-match ":rocket:"))))

  (it "moves on without retrying a request that failed"
    (slacko-reactions-tests--with-buffer 5
      (spy-on 'slacko-reactions-refresh)
      (let ((entry (slacko-reactions-tests--entry (point-min)))
            (before (buffer-string)))
        (setq slacko-reactions--requests 1)
        (slacko-reactions--receive (current-buffer) entry nil)
        (expect (plist-get entry :state) :to-be 'done)
        (expect slacko-reactions--requests :to-equal 0)
        (expect (buffer-string) :to-equal before)
        (expect 'slacko-reactions-refresh :to-have-been-called)))))

(describe "slacko-reactions--run"
  (it "fetches what the window shows, up to the request cap"
    (slacko-reactions-tests--with-buffer 200
      (spy-on 'slacko-reactions--fetch)
      (let ((slacko-reactions-max-requests 2))
        (dotimes (i 5)
          (slacko-reactions-tests--entry (+ (point-min) i)))
        (slacko-reactions--run (current-buffer))
        (expect (spy-calls-count 'slacko-reactions--fetch) :to-equal 2))))

  (it "leaves a buffer no window shows alone"
    (let ((buffer (generate-new-buffer " *slacko-reactions-hidden*")))
      (unwind-protect
          (progn
            (spy-on 'slacko-reactions--fetch)
            (with-current-buffer buffer
              (insert "text\n")
              (slacko-reactions-tests--entry (point-min))
              (slacko-reactions--run buffer))
            (expect 'slacko-reactions--fetch :not :to-have-been-called))
        (kill-buffer buffer)))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-reactions-tests.el ends here
