;;; slacko-jump-tests.el --- tests for slacko-jump -*- lexical-binding: t; -*-
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
;;  Tests for the list of recent conversations and followed threads.
;;
;;; Code:

(require 'buttercup)
(require 'json)
(require 'slacko-jump)

(defconst slacko-jump-tests--org
  '(:id "E1" :name "Acme" :host "grid-acme.enterprise.slack.com")
  "An Enterprise Grid org, as `slacko-creds--root-state-orgs' lists it.")

(defun slacko-jump-tests--entry (id type ms)
  "A navigation entry of the `recents' pref: ID of TYPE, viewed at MS."
  `((id . ,id) (object_type . ,type) (timestamp . ,ms)
    (client_platform . "desktop")))

(defun slacko-jump-tests--prefs (&rest entries)
  "A `users.prefs.get' response whose `recents' pref lists ENTRIES."
  `((ok . t)
    (prefs . ((recents . ,(json-encode
                           `((navigation . ,(vconcat entries))
                             (search . []))))))))

(defun slacko-jump-tests--thread (channel ts &rest root)
  "A Threads view thread started at TS in CHANNEL, with ROOT fields over it."
  (let ((msg (list (cons 'channel channel) (cons 'ts ts)
                   (cons 'text "the question") (cons 'team "T1")
                   (cons 'reply_count 3))))
    (dolist (field root)
      (setf (alist-get (car field) msg) (cdr field)))
    (list (cons 'root_msg msg) (cons 'latest_replies nil))))

(defmacro slacko-jump-tests--with-caches (&rest body)
  "Run BODY with empty user and channel caches."
  (declare (indent 0))
  `(let ((slacko-render--user-cache (make-hash-table :test 'equal))
         (slacko-render--channel-cache (make-hash-table :test 'equal))
         (slacko-jump--workspaces nil))
     ,@body))

(describe "slacko-jump--viewed"
  (it "lists channels and DMs, newest first, in seconds"
    (expect (slacko-jump--viewed
             (slacko-jump-tests--prefs
              (slacko-jump-tests--entry "C1" "CHANNEL" 1000000)
              (slacko-jump-tests--entry "D1" "DM" 3000000)))
            :to-equal '(("D1" . 3000.0) ("C1" . 1000.0))))

  (it "leaves out pages and what is not a conversation"
    (expect (slacko-jump--viewed
             (slacko-jump-tests--prefs
              (slacko-jump-tests--entry "Vall_threads" "PAGE" 2000000)
              (slacko-jump-tests--entry "Ddms" "PAGE" 2000000)
              (slacko-jump-tests--entry "F1" "FILE" 2000000)
              (slacko-jump-tests--entry "C1" "CHANNEL" 1000000)))
            :to-equal '(("C1" . 1000.0))))

  (it "shows a conversation viewed twice once, at its latest view"
    (expect (slacko-jump--viewed
             (slacko-jump-tests--prefs
              (slacko-jump-tests--entry "C1" "CHANNEL" 1000000)
              (slacko-jump-tests--entry "C1" "CHANNEL" 5000000)))
            :to-equal '(("C1" . 5000.0))))

  (it "returns nil when the pref is missing or does not parse"
    (expect (slacko-jump--viewed '((ok . t) (prefs . nil))) :to-be nil)
    (expect (slacko-jump--viewed '((ok . t) (prefs . ((recents . "{\"navig")))))
            :to-be nil)
    (expect (slacko-jump--viewed '((ok . t) (prefs . ((recents . "\"text\"")))))
            :to-be nil)
    (expect (slacko-jump--viewed nil) :to-be nil)))

(describe "slacko-jump--threads"
  (it "keeps the threads that say where they are"
    (expect (length (slacko-jump--threads
                     "host"
                     `((threads . (,(slacko-jump-tests--thread "C1" "1.000001")
                                   ((root_msg . ((text . "nowhere")))))))))
            :to-equal 1))

  (it "caches the names the profiles carry"
    (slacko-jump-tests--with-caches
      (slacko-jump--threads
       "host"
       `((threads . (((root_msg . ((channel . "C1") (ts . "1.000001") (user . "U1")
                                   (user_profile . ((display_name . "")
                                                    (real_name . "Al Kay")))))
                      (latest_replies . (((user . "U2")
                                          (user_profile . ((display_name . "bo")))))))))))
      (expect (slacko-render-cached-user "host" "U1") :to-equal "Al Kay")
      (expect (slacko-render-cached-user "host" "U2") :to-equal "bo"))))

(describe "slacko-jump--thread-item"
  (it "is unread when its latest reply is newer than what was read"
    (expect (plist-get (slacko-jump--thread-item
                        slacko-jump-tests--org
                        (slacko-jump-tests--thread
                         "C1" "1.000001"
                         '(latest_reply . "9.000001") '(last_read . "5.000001")))
                       :unread)
            :to-be t))

  (it "is read when what was read is the latest reply"
    (expect (plist-get (slacko-jump--thread-item
                        slacko-jump-tests--org
                        (slacko-jump-tests--thread
                         "C1" "1.000001"
                         '(latest_reply . "9.000001") '(last_read . "9.000001")))
                       :unread)
            :to-be nil))

  (it "is unread when it was never read"
    (expect (plist-get (slacko-jump--thread-item
                        slacko-jump-tests--org
                        (slacko-jump-tests--thread
                         "C1" "1.000001" '(latest_reply . "9.000001")))
                       :unread)
            :to-be t))

  (it "is timed by its latest reply, or by itself without one"
    (expect (plist-get (slacko-jump--thread-item
                        slacko-jump-tests--org
                        (slacko-jump-tests--thread
                         "C1" "1.5" '(latest_reply . "9.25")))
                       :time)
            :to-equal 9.25)
    (expect (plist-get (slacko-jump--thread-item
                        slacko-jump-tests--org
                        (slacko-jump-tests--thread "C1" "1.5"))
                       :time)
            :to-equal 1.5))

  (it "knows its org, its conversation and the workspace it was posted in"
    (let ((item (slacko-jump--thread-item
                 slacko-jump-tests--org
                 (slacko-jump-tests--thread "C1" "1.000001"))))
      (expect (plist-get item :org) :to-equal "Acme")
      (expect (plist-get item :org-id) :to-equal "E1")
      (expect (plist-get item :host) :to-equal "grid-acme.enterprise.slack.com")
      (expect (plist-get item :channel) :to-equal "C1")
      (expect (plist-get item :ts) :to-equal "1.000001")
      (expect (plist-get item :team) :to-equal "T1"))))

(describe "slacko-jump--items"
  (it "mixes viewed conversations and threads, newest first"
    (expect (mapcar (lambda (item) (plist-get item :channel))
                    (slacko-jump--items
                     slacko-jump-tests--org
                     '(("C1" . 30.0) ("C2" . 10.0))
                     (list (slacko-jump-tests--thread
                            "C3" "1.0" '(latest_reply . "20.0")))))
            :to-equal '("C1" "C3" "C2"))))

(describe "slacko-jump--age"
  (it "reads like Slack does"
    (expect (slacko-jump--age 1000.0 1030.0) :to-equal "now")
    (expect (slacko-jump--age 1000.0 (+ 1000.0 (* 5 60) 10)) :to-equal "5m")
    (expect (slacko-jump--age 1000.0 (+ 1000.0 (* 3 3600) 10)) :to-equal "3h")
    (expect (slacko-jump--age 1000.0 (+ 1000.0 (* 2 86400) 10)) :to-equal "2d")))

(describe "slacko-jump--annotation"
  (it "says a conversation was viewed, and when"
    (expect (substring-no-properties
             (slacko-jump--annotation '(:channel "C1" :time 1000.0) 1300.0))
            :to-equal "  viewed        5m"))

  (it "counts a thread's replies and says when it has new ones"
    (let ((annotation (slacko-jump--annotation
                       '(:channel "C1" :ts "1.0" :time 1000.0 :replies 4 :unread t)
                       1300.0)))
      (expect (substring-no-properties annotation)
              :to-equal "  4 replies     5m  new")
      (expect (get-text-property (1- (length annotation)) 'face annotation)
              :to-be 'warning)))

  (it "counts one reply as one"
    (expect (substring-no-properties
             (slacko-jump--annotation '(:ts "1.0" :time 1000.0 :replies 1) 1000.0))
            :to-match "1 reply ")))

(describe "slacko-jump--candidate"
  (it "names a channel from the cache"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-channel "host" '((id . "C1") (name . "general")))
      (expect (slacko-jump--candidate
               '(:org "Acme" :host "host" :channel "C1" :time 0.0))
              :to-match "\\`#general ")))

  (it "names a DM after the person it is with"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-channel "host" '((id . "D1") (is_im . t) (user . "U1")))
      (slacko-render-cache-user "host" "U1" "natalie.see")
      (expect (slacko-jump--candidate
               '(:org "Acme" :host "host" :channel "D1" :time 0.0))
              :to-match "\\`@natalie\\.see ")))

  (it "shows the id of a conversation nobody named, asking nobody"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request)
      (expect (slacko-jump--candidate
               '(:org "Acme" :host "host" :channel "C9" :time 0.0))
              :to-match "\\`C9 ")
      (expect 'slacko-creds-api-request :not :to-have-been-called)))

  (it "shows a thread's message, in bold while it has new replies"
    (slacko-jump-tests--with-caches
      (let ((candidate (slacko-jump--candidate
                        '(:org "Acme" :host "host" :channel "C1" :ts "1.0"
                          :time 0.0 :unread t
                          :text "can someone look at <https://x|this>"))))
        (expect candidate :to-match "can someone look at this")
        (expect (get-text-property (string-match "can someone" candidate)
                                   'face candidate)
                :to-be 'bold))))

  (it "hides the whole message and the org's name for filtering to reach"
    (slacko-jump-tests--with-caches
      (let* ((long (string-join (make-list 30 "needle") " "))
             (candidate (slacko-jump--candidate
                         `(:org "Acme" :host "host" :channel "C1" :ts "1.0"
                           :time 0.0 :text ,long))))
        (expect (string-match-p (regexp-quote long) candidate) :to-be-truthy)
        (expect (string-match-p "Acme\\'" candidate) :to-be-truthy)
        (expect (get-text-property (1- (length candidate)) 'invisible candidate)
                :to-be t))))

  (it "keeps one channel name in two orgs as two candidates"
    (slacko-jump-tests--with-caches
      (expect (equal (slacko-jump--candidate
                      '(:org "Acme" :host "a" :channel "C1" :time 0.0))
                     (slacko-jump--candidate
                      '(:org "Other" :host "b" :channel "C1" :time 0.0)))
              :to-be nil)))

  (it "carries its item and its annotation"
    (slacko-jump-tests--with-caches
      (let* ((item '(:org "Acme" :host "host" :channel "C1" :time 0.0))
             (candidate (slacko-jump--candidate item)))
        (expect (slacko-jump--item candidate) :to-equal item)
        (expect (slacko-jump--annotate candidate) :to-match "viewed")
        (expect (slacko-jump--group candidate nil) :to-equal "Acme")
        (expect (slacko-jump--group candidate t) :to-be candidate))))

  (it "returns nothing for a string that carries no item"
    (expect (slacko-jump--item "") :to-be nil)
    (expect (slacko-jump--item nil) :to-be nil)
    (expect (slacko-jump--annotate "plain") :to-equal "")))

(describe "slacko-jump--link-host"
  :var (workspaces)
  (before-each
    (setq workspaces
          '((:host "grid-acme.enterprise.slack.com" :team "E1" :enterprise "E1"
             :org-wide t)
            (:host "acme.slack.com" :team "T1" :enterprise "E1" :org-wide nil)
            (:host "acme-global.slack.com" :team "T2" :enterprise "E1" :org-wide nil)
            (:host "other.slack.com" :team "T9" :enterprise nil :org-wide nil))))

  (it "takes the host of the workspace a thread was posted in"
    (slacko-jump-tests--with-caches
      (setq slacko-jump--workspaces workspaces)
      (expect (slacko-jump--link-host
               '(:org-id "E1" :host "grid-acme.enterprise.slack.com"
                 :channel "C1" :ts "1.0" :team "T2"))
              :to-equal "acme-global.slack.com")))

  (it "takes the workspace a channel lives in, from its description"
    (slacko-jump-tests--with-caches
      (setq slacko-jump--workspaces workspaces)
      (slacko-render-cache-channel "grid-acme.enterprise.slack.com"
                                   '((id . "C1") (context_team_id . "T1")))
      (expect (slacko-jump--link-host
               '(:org-id "E1" :host "grid-acme.enterprise.slack.com" :channel "C1"))
              :to-equal "acme.slack.com")))

  (it "takes a workspace of the org for a DM, which belongs to the org itself"
    (slacko-jump-tests--with-caches
      (setq slacko-jump--workspaces workspaces)
      (slacko-render-cache-channel "grid-acme.enterprise.slack.com"
                                   '((id . "D1") (context_team_id . "E1")))
      (expect (slacko-jump--link-host
               '(:org-id "E1" :host "grid-acme.enterprise.slack.com" :channel "D1"))
              :to-equal "acme.slack.com")))

  (it "falls back to the org's own host when no workspace is known"
    (slacko-jump-tests--with-caches
      (expect (slacko-jump--link-host
               '(:org-id "E1" :host "grid-acme.enterprise.slack.com"
                 :channel "C1" :team "T1"))
              :to-equal "grid-acme.enterprise.slack.com")))

  (it "takes no workspace of another org for an org without an id"
    (slacko-jump-tests--with-caches
      (setq slacko-jump--workspaces workspaces)
      (expect (slacko-jump--link-host '(:host "solo.slack.com" :channel "C1"))
              :to-equal "solo.slack.com"))))

(describe "slacko-jump--url"
  (it "links a thread to its first message"
    (slacko-jump-tests--with-caches
      (expect (slacko-jump--url '(:host "acme.slack.com" :channel "C1"
                                  :ts "1794408710.234009"))
              :to-equal "https://acme.slack.com/archives/C1/p1794408710234009")))

  (it "links a conversation to itself"
    (slacko-jump-tests--with-caches
      (expect (slacko-jump--url '(:host "acme.slack.com" :channel "D1"))
              :to-equal "https://acme.slack.com/archives/D1"))))

(describe "slacko-jump actions"
  (it "opens a thread in Emacs"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-thread-capture)
      (spy-on 'slacko--open-in-slack)
      (slacko-jump-open (slacko-jump--candidate
                         '(:org "Acme" :host "acme.slack.com" :channel "C1"
                           :ts "1.000001" :time 0.0)))
      (expect 'slacko-thread-capture :to-have-been-called-with
              "https://acme.slack.com/archives/C1/p1000001")
      (expect 'slacko--open-in-slack :not :to-have-been-called)))

  (it "opens a channel or DM in the Slack app"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-thread-capture)
      (spy-on 'slacko--open-in-slack)
      (slacko-jump-open (slacko-jump--candidate
                         '(:org "Acme" :host "acme.slack.com" :channel "C1" :time 0.0)))
      (expect 'slacko--open-in-slack :to-have-been-called-with
              "https://acme.slack.com/archives/C1")
      (expect 'slacko-thread-capture :not :to-have-been-called)))

  (it "opens a thread in the Slack app when asked to"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko--open-in-slack)
      (slacko-jump-open-in-slack (slacko-jump--candidate
                                  '(:org "Acme" :host "acme.slack.com" :channel "C1"
                                    :ts "1.000001" :time 0.0)))
      (expect 'slacko--open-in-slack :to-have-been-called-with
              "https://acme.slack.com/archives/C1/p1000001")))

  (it "copies the link"
    (slacko-jump-tests--with-caches
      (spy-on 'message)
      (slacko-jump-copy-url (slacko-jump--candidate
                             '(:org "Acme" :host "acme.slack.com" :channel "C1"
                               :time 0.0)))
      (expect (current-kill 0) :to-equal "https://acme.slack.com/archives/C1")))

  (it "does nothing for a candidate with no item behind it"
    (spy-on 'slacko-thread-capture)
    (spy-on 'slacko--open-in-slack)
    (slacko-jump-open "plain")
    (expect 'slacko-thread-capture :not :to-have-been-called)
    (expect 'slacko--open-in-slack :not :to-have-been-called)))

(defun slacko-jump-tests--answer (answers)
  "Spy `slacko-creds-api-request-async' to answer from ANSWERS at once.
ANSWERS maps an endpoint, or an endpoint and an id, to the response."
  (spy-on 'slacko-creds-api-request-async :and-call-fake
          (lambda (_host endpoint params callback)
            (funcall callback
                     (cdr (or (assoc (list endpoint (cadr (car params))) answers)
                              (assoc endpoint answers)))))))

(describe "slacko-jump--with-names"
  (it "asks only for what is not cached, conversations first, then people"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-channel "host" '((id . "C1") (name . "known")))
      (slacko-jump-tests--answer
       '((("conversations.info" "D1")
          . ((ok . t) (channel . ((id . "D1") (is_im . t) (user . "U1")))))
         (("users.info" "U1")
          . ((ok . t) (user . ((name . "al") (profile . ((display_name . "Al")))))))))
      (let ((continued nil))
        (slacko-jump--with-names "host" '("C1" "D1") (lambda () (setq continued t)))
        (expect continued :to-be t)
        ;; the person's id comes out of the conversation's answer, which is
        ;; what puts the `users.info' request after the other one
        (expect (mapcar (lambda (call) (list (nth 1 call) (nth 2 call)))
                        (mapcar #'spy-context-args
                                (spy-calls-all 'slacko-creds-api-request-async)))
                :to-have-same-items-as '(("conversations.info" ((channel "D1")))
                                         ("users.info" ((user "U1")))))
        (expect (slacko-jump--label "host" "D1") :to-equal "@Al")))))

(describe "slacko-jump--fetch-org"
  (it "asks the org's host for its recents and its Threads view"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request-async)
      (slacko-jump--fetch-org slacko-jump-tests--org #'ignore)
      (expect (mapcar (lambda (call) (seq-take call 3))
                      (mapcar #'spy-context-args
                              (spy-calls-all 'slacko-creds-api-request-async)))
              :to-equal '(("grid-acme.enterprise.slack.com" "users.prefs.get"
                           ((prefs "recents")))
                          ("grid-acme.enterprise.slack.com"
                           "subscriptions.thread.getView" nil)))))

  (it "hands back named candidates, newest first, once everything has answered"
    (slacko-jump-tests--with-caches
      (slacko-jump-tests--answer
       `(("users.prefs.get"
          . ,(slacko-jump-tests--prefs
              (slacko-jump-tests--entry "C1" "CHANNEL" 1794400000000)))
         ("subscriptions.thread.getView"
          . ((ok . t)
             (threads . (,(slacko-jump-tests--thread
                           "C2" "1794300000.000001"
                           '(latest_reply . "1794410000.000001")
                           '(text . "is the build green?"))))))
         (("conversations.info" "C1")
          . ((ok . t) (channel . ((id . "C1") (name . "general")))))
         (("conversations.info" "C2")
          . ((ok . t) (channel . ((id . "C2") (name . "builds")))))))
      (let ((candidates 'not-called))
        (slacko-jump--fetch-org slacko-jump-tests--org
                                (lambda (result) (setq candidates result)))
        (expect (length candidates) :to-equal 2)
        (expect (nth 0 candidates) :to-match "\\`#builds +is the build green\\?")
        (expect (nth 1 candidates) :to-match "\\`#general ")
        (expect (plist-get (slacko-jump--item (nth 0 candidates)) :org)
                :to-equal "Acme"))))

  (it "says what Slack refused and still lists what it gave"
    (slacko-jump-tests--with-caches
      (spy-on 'message)
      (slacko-render-cache-channel "grid-acme.enterprise.slack.com"
                                   '((id . "C1") (name . "general")))
      (slacko-jump-tests--answer
       `(("users.prefs.get"
          . ,(slacko-jump-tests--prefs
              (slacko-jump-tests--entry "C1" "CHANNEL" 1794400000000)))
         ("subscriptions.thread.getView"
          . ((ok . :json-false) (error . "team_is_restricted")))))
      (let ((candidates nil))
        (slacko-jump--fetch-org slacko-jump-tests--org
                                (lambda (result) (setq candidates result)))
        (expect (length candidates) :to-equal 1)
        (expect 'message :to-have-been-called-with
                "Slack %s: %s" "Acme" "team_is_restricted"))))

  (it "calls back with nil when Slack cannot even be asked"
    (slacko-jump-tests--with-caches
      (spy-on 'message)
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (&rest _)
                (error "No credentials for grid-acme.enterprise.slack.com")))
      (let ((candidates 'not-called))
        (slacko-jump--fetch-org slacko-jump-tests--org
                                (lambda (result) (setq candidates result)))
        (expect candidates :to-be nil)
        (expect 'message :to-have-been-called-with
                "Slack %s: %s" "Acme"
                "No credentials for grid-acme.enterprise.slack.com")))))

(describe "slacko-jump--source"
  :var (actions callbacks source)
  (before-each
    (setq actions nil
          callbacks nil)
    (spy-on 'slacko-jump--learn-workspaces)
    (spy-on 'slacko-jump--fetch-org :and-call-fake
            (lambda (org callback)
              (push (cons (plist-get org :name) callback) callbacks)))
    (setq source (funcall (slacko-jump--source '((:name "first") (:name "second")))
                          (lambda (action) (push action actions)))))

  (it "asks every org once the session is set up"
    (funcall source 'setup)
    (expect (mapcar #'car callbacks) :to-have-same-items-as '("first" "second"))
    (expect 'slacko-jump--learn-workspaces :to-have-been-called)
    (expect (reverse actions) :to-equal '(setup [indicator running])))

  (it "shows the orgs in their own order, whichever answers first"
    (funcall source 'setup)
    (setq actions nil)
    (funcall (alist-get "second" callbacks nil nil #'equal) '("s1"))
    (expect (reverse actions) :to-equal '(flush ("s1")))
    (setq actions nil)
    (funcall (alist-get "first" callbacks nil nil #'equal) '("f1" "f2"))
    (expect (reverse actions)
            :to-equal '(flush ("f1" "f2" "s1") [indicator finished])))

  (it "pushes nothing but the flush while no org has anything to show"
    (funcall source 'setup)
    (setq actions nil)
    (funcall (alist-get "first" callbacks nil nil #'equal) nil)
    (expect (reverse actions) :to-equal '(flush)))

  (it "drops what arrives after the session is gone"
    (funcall source 'setup)
    (funcall source 'destroy)
    (setq actions nil)
    (funcall (alist-get "first" callbacks nil nil #'equal) '("f1"))
    (expect actions :to-be nil))

  (it "passes the input and every other action straight through"
    (funcall source "anything")
    (funcall source nil)
    (expect (reverse actions) :to-equal '("anything" nil))))

(describe "slacko-jump--mentions"
  (it "lists the people a text mentions"
    (expect (slacko-jump--mentions "hi <@U1> and <@W2|bo>, see <#C3|x>")
            :to-equal '("U1" "W2")))

  (it "finds nobody where nobody is mentioned"
    (expect (slacko-jump--mentions nil) :to-be nil)
    (expect (slacko-jump--mentions "mail <@u1> is no id") :to-be nil)))

(describe "slacko-jump--unknown-people"
  (it "lists authors and the people they mention once, leaving out the named"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-user "host" "U1" "Al")
      (expect (slacko-jump--unknown-people
               "host" '(((user . "U1") (text . "hi <@U2>"))
                        ((user . "U2") (text . "hey <@U1> <@U3>"))
                        ((bot_id . "B1") (text . "beep"))))
              :to-equal '("U2" "U3")))))

(describe "slacko-jump--fetch-preview"
  (it "asks for the whole of a thread"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request-async)
      (slacko-jump--fetch-preview '(:host "host" :channel "C1" :ts "1.5") #'ignore)
      (expect (seq-take (spy-calls-args-for 'slacko-creds-api-request-async 0) 3)
              :to-equal '("host" "conversations.replies"
                          ((channel "C1") (ts "1.5") (limit "200"))))))

  (it "asks for the latest messages of a conversation"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request-async)
      (slacko-jump--fetch-preview '(:host "host" :channel "D1") #'ignore)
      (expect (seq-take (spy-calls-args-for 'slacko-creds-api-request-async 0) 3)
              :to-equal '("host" "conversations.history"
                          ((channel "D1") (limit "30"))))))

  (it "names everyone in the answer before calling back"
    (slacko-jump-tests--with-caches
      (slacko-jump-tests--answer
       '(("conversations.history"
          . ((ok . t)
             (messages . (((user . "U1") (text . "ping <@U2>")
                           (user_profile . ((display_name . "Al"))))))))
         (("users.info" "U2")
          . ((ok . t) (user . ((name . "bo") (profile . ((display_name . "Bo")))))))))
      (let ((answer nil))
        (slacko-jump--fetch-preview '(:host "host" :channel "D1")
                                    (lambda (data) (setq answer data)))
        (expect (alist-get 'ok answer) :to-be t)
        (expect (slacko-render-cached-user "host" "U1") :to-equal "Al")
        (expect (slacko-render-cached-user "host" "U2") :to-equal "Bo")
        (expect (spy-calls-count 'slacko-creds-api-request-async) :to-equal 2))))

  (it "turns a request that never arrived into a refusal"
    (slacko-jump-tests--with-caches
      (slacko-jump-tests--answer nil)
      (let ((answer nil))
        (slacko-jump--fetch-preview '(:host "host" :channel "D1")
                                    (lambda (data) (setq answer data)))
        (expect answer :to-equal '((ok . :json-false) (error . "no answer"))))))

  (it "calls back with a refusal when Slack cannot even be asked"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (&rest _) (error "No credentials for host")))
      (let ((answer nil))
        (slacko-jump--fetch-preview '(:host "host" :channel "D1")
                                    (lambda (data) (setq answer data)))
        (expect (alist-get 'ok answer) :to-be :json-false)
        (expect (alist-get 'error answer) :to-equal "No credentials for host")))))

(defmacro slacko-jump-tests--with-preview-buffer (var &rest body)
  "Run BODY with VAR bound to a throwaway buffer, killed afterwards."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *slacko-jump-test-preview*")))
     (unwind-protect (progn ,@body)
       (kill-buffer ,var))))

(describe "slacko-jump--render-preview"
  (it "says the answer is on its way"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-channel "host" '((id . "C1") (name . "builds")))
      (slacko-jump-tests--with-preview-buffer buffer
        (slacko-jump--render-preview buffer '(:host "host" :channel "C1" :ts "1.0"))
        (expect (with-current-buffer buffer (buffer-string))
                :to-equal "#+TITLE: #builds\n\nLoading…\n"))))

  (it "shows a thread down to its latest reply, the replies under the first message"
    (slacko-jump-tests--with-caches
      (slacko-render-cache-user "host" "U1" "Al")
      (slacko-jump-tests--with-preview-buffer buffer
        (slacko-jump--render-preview
         buffer '(:host "host" :channel "C1" :ts "1.000001")
         '((ok . t) (messages . (((user . "U1") (ts . "1.000001") (text . "the question"))
                                 ((user . "U1") (ts . "2.000001") (text . "the answer"))))))
        (with-current-buffer buffer
          (expect (buffer-string) :to-match "^\\* \\[\\[slack://host/team/U1\\]\\[Al\\]\\]")
          (expect (buffer-string) :to-match "^\\*\\* .*\n\\(.*\n\\)*?the answer")
          (expect (< (string-match "the question" (buffer-string))
                     (string-match "the answer" (buffer-string)))
                  :to-be t)
          (expect (point) :to-equal (point-max))))))

  (it "shows a conversation oldest first, down to its latest message"
    (slacko-jump-tests--with-caches
      (slacko-jump-tests--with-preview-buffer buffer
        (slacko-jump--render-preview
         buffer '(:host "host" :channel "D1")
         '((ok . t) (messages . (((user . "U1") (ts . "2.000001") (text . "newer"))
                                 ((user . "U1") (ts . "1.000001") (text . "older"))))))
        (with-current-buffer buffer
          (expect (< (string-match "older" (buffer-string))
                     (string-match "newer" (buffer-string)))
                  :to-be t)
          (expect (buffer-string) :to-match "\\`#\\+TITLE: D1\n")
          (expect (point) :to-equal (point-max))))))

  (it "shows what Slack refused"
    (slacko-jump-tests--with-caches
      (slacko-jump-tests--with-preview-buffer buffer
        (slacko-jump--render-preview buffer '(:host "host" :channel "C1")
                                     '((ok . :json-false) (error . "not_in_channel")))
        (expect (with-current-buffer buffer (buffer-string))
                :to-match "^Slack: not_in_channel$"))))

  (it "asks nobody for a name, showing the id of whoever is not named"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko-creds-api-request)
      (slacko-jump-tests--with-preview-buffer buffer
        (slacko-jump--render-preview
         buffer '(:host "host" :channel "C1")
         '((ok . t) (messages . (((user . "U9") (ts . "1.000001") (text . "hi <@U8>"))))))
        (expect (with-current-buffer buffer (buffer-string)) :to-match "\\[U9\\]")
        (expect (with-current-buffer buffer (buffer-string)) :to-match "@U8")
        (expect 'slacko-creds-api-request :not :to-have-been-called)))))

(describe "slacko-jump--state"
  :var (state original preview fetches)
  (before-each
    (setq fetches nil
          original (get-buffer-create " *slacko-jump-test-original*")
          preview (get-buffer-create " *slacko-jump-test-preview*"))
    (spy-on 'slacko-jump--preview-buffer :and-return-value preview)
    (spy-on 'slacko-jump--fetch-preview :and-call-fake
            (lambda (item callback) (push (cons item callback) fetches)))
    (switch-to-buffer original)
    (setq state (slacko-jump--state))
    (funcall state 'setup nil))

  (after-each
    (dolist (buffer (list original preview))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  (it "shows the row in the window, loading until the answer comes"
    (slacko-jump-tests--with-caches
      (funcall state 'preview (slacko-jump--candidate
                               '(:org "Acme" :host "host" :channel "C1" :ts "1.0"
                                 :time 0.0)))
      (expect (window-buffer (selected-window)) :to-be preview)
      (expect (with-current-buffer preview (buffer-string)) :to-match "Loading")
      (funcall (cdr (car fetches))
               '((ok . t) (messages . (((ts . "1.0") (text . "the question"))))))
      (expect (with-current-buffer preview (buffer-string)) :to-match "the question")
      (expect (window-point (selected-window))
              :to-equal (with-current-buffer preview (point-max)))))

  (it "asks Slack about a row once a session"
    (slacko-jump-tests--with-caches
      (let ((first (slacko-jump--candidate
                    '(:org "Acme" :host "host" :channel "C1" :ts "1.0" :time 0.0)))
            (second (slacko-jump--candidate
                     '(:org "Acme" :host "host" :channel "C2" :time 0.0))))
        (funcall state 'preview first)
        (funcall (cdr (car fetches))
                 '((ok . t) (messages . (((ts . "1.0") (text . "the question"))))))
        (funcall state 'preview second)
        (funcall state 'preview first)
        (expect (length fetches) :to-equal 2)
        (expect (with-current-buffer preview (buffer-string))
                :to-match "the question"))))

  (it "drops an answer that arrives after the cursor moved on"
    (slacko-jump-tests--with-caches
      (funcall state 'preview (slacko-jump--candidate
                               '(:org "Acme" :host "host" :channel "C1" :ts "1.0"
                                 :time 0.0)))
      (funcall state 'preview (slacko-jump--candidate
                               '(:org "Acme" :host "host" :channel "C2" :time 0.0)))
      (funcall (cdr (cadr fetches))
               '((ok . t) (messages . (((ts . "1.0") (text . "the question"))))))
      (expect (with-current-buffer preview (buffer-string)) :to-match "Loading")))

  (it "puts the original buffer back without a row, and on exit"
    (slacko-jump-tests--with-caches
      (let ((candidate (slacko-jump--candidate
                        '(:org "Acme" :host "host" :channel "C1" :time 0.0))))
        (funcall state 'preview candidate)
        (funcall state 'preview nil)
        (expect (window-buffer (selected-window)) :to-be original)
        (funcall state 'preview candidate)
        (let ((slacko-consult-preview-buffer-name (buffer-name preview)))
          (funcall state 'exit nil))
        (expect (window-buffer (selected-window)) :to-be original)
        (expect (buffer-live-p preview) :to-be nil))))

  (it "renders nothing once the session is over"
    (slacko-jump-tests--with-caches
      (funcall state 'preview (slacko-jump--candidate
                               '(:org "Acme" :host "host" :channel "C1" :time 0.0)))
      (let ((slacko-consult-preview-buffer-name "no such buffer"))
        (funcall state 'exit nil))
      (with-current-buffer preview
        (let ((inhibit-read-only t)) (erase-buffer)))
      (funcall (cdr (car fetches)) '((ok . t) (messages . (((ts . "1.0") (text . "late"))))))
      (expect (with-current-buffer preview (buffer-string)) :to-equal ""))))

(describe "slacko-jump--learn-workspaces"
  (it "asks every credentials host once, past the credentials renewal"
    (slacko-jump-tests--with-caches
      (spy-on 'slacko--available-hosts :and-return-value
              '("acme.slack.com" "grid-acme.enterprise.slack.com" "gone.slack.com"))
      (spy-on 'slacko-creds--auth-headers :and-call-fake
              (lambda (host) `(("Test-Host" . ,host))))
      (spy-on 'slacko-creds-api-request-async)
      (spy-on 'slacko-creds--get-async :and-call-fake
              (lambda (_endpoint _params headers callback)
                (funcall callback
                         (pcase (alist-get "Test-Host" headers nil nil #'equal)
                           ("acme.slack.com"
                            '((ok . t) (team_id . "T1") (enterprise_id . "E1")))
                           ("grid-acme.enterprise.slack.com"
                            '((ok . t) (team_id . "E1") (enterprise_id . "E1")
                              (is_enterprise_install . t)))
                           (_ '((ok . :json-false) (error . "invalid_auth")))))))
      (slacko-jump--learn-workspaces)
      (expect 'slacko-creds-api-request-async :not :to-have-been-called)
      (expect slacko-jump--workspaces
              :to-equal
              '((:host "acme.slack.com" :team "T1" :enterprise "E1" :org-wide nil)
                (:host "grid-acme.enterprise.slack.com" :team "E1" :enterprise "E1"
                 :org-wide t)))))

  (it "asks nobody once the session knows the workspaces"
    (slacko-jump-tests--with-caches
      (setq slacko-jump--workspaces '((:host "acme.slack.com" :team "T1")))
      (spy-on 'slacko-creds--get-async)
      (slacko-jump--learn-workspaces)
      (expect 'slacko-creds--get-async :not :to-have-been-called))))

(describe "slacko-jump--orgs"
  (it "lists the orgs the Slack app shows"
    (spy-on 'slacko-creds--root-state-orgs :and-return-value
            (list slacko-jump-tests--org))
    (spy-on 'slacko--available-hosts)
    (expect (slacko-jump--orgs) :to-equal (list slacko-jump-tests--org))
    (expect 'slacko--available-hosts :not :to-have-been-called))

  (it "takes every credentials host for an org where the app lists none"
    (spy-on 'slacko-creds--root-state-orgs :and-return-value nil)
    (spy-on 'slacko--available-hosts :and-return-value '("team.slack.com"))
    (expect (slacko-jump--orgs)
            :to-equal '((:name "team" :host "team.slack.com")))))

(describe "slacko-jump--async-wrap"
  (it "leaves the input whole, for the completion style to filter with"
    (assume (require 'consult nil t) "Consult is not installed")
    (spy-on 'consult--async-split :and-call-through)
    (slacko-jump--async-wrap (lambda (sink) sink))
    (expect 'consult--async-split :not :to-have-been-called)))

(describe "the slacko-jump command"
  (it "opens what was picked"
    (assume (require 'consult nil t) "Consult is not installed")
    (spy-on 'slacko-jump--orgs :and-return-value (list slacko-jump-tests--org))
    (spy-on 'slacko-jump--embark-setup)
    (spy-on 'consult--read :and-return-value "picked")
    (spy-on 'slacko-jump-open)
    (slacko-jump)
    (let ((options (cdr (spy-calls-args-for 'consult--read 0))))
      (expect (plist-get options :async-wrap) :to-be #'slacko-jump--async-wrap)
      (expect (plist-get options :category) :to-be 'slacko-jump)
      (expect (plist-get options :group) :to-be #'slacko-jump--group)
      (expect (functionp (plist-get options :state)) :to-be t)
      (expect (plist-get options :preview-key) :to-equal '(:debounce 0.2 any)))
    (expect 'slacko-jump--embark-setup :to-have-been-called)
    (expect 'slacko-jump-open :to-have-been-called-with "picked"))

  (it "says so when no workspace is known"
    (assume (require 'consult nil t) "Consult is not installed")
    (spy-on 'slacko-jump--orgs)
    (spy-on 'consult--read)
    (expect (slacko-jump) :to-throw 'user-error)
    (expect 'consult--read :not :to-have-been-called)))

(describe "slacko-jump--embark-setup"
  (it "registers the category and its actions"
    (assume (require 'embark nil t) "Embark is not installed")
    (let ((embark-keymap-alist nil)
          (embark-default-action-overrides nil))
      (slacko-jump--embark-setup)
      (expect (alist-get 'slacko-jump embark-keymap-alist)
              :to-be 'slacko-jump-embark-map)
      (expect (alist-get 'slacko-jump embark-default-action-overrides)
              :to-be #'slacko-jump-open))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-jump-tests.el ends here
