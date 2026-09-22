;;; slacko-consult-tests.el --- tests for slacko-consult -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for Slack search in a Consult session.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-consult)

(defun slacko-consult-tests--match (&rest overrides)
  "A search result, with OVERRIDES layered over the usual fields.
Built rather than quoted: a quoted literal would be the same object on
every call, and one test overriding a field would change every other
test's message."
  (let ((match (list (cons 'username "john_doe")
                     (cons 'user "U123")
                     (cons 'text "hello there")
                     (cons 'channel (list (cons 'id "C456")
                                          (cons 'name "general")
                                          (cons 'is_channel t)))
                     (cons 'ts "1738226435.123456")
                     (cons 'permalink
                           "https://team.slack.com/archives/C456/p1738226435123456"))))
    (dolist (override overrides)
      (setf (alist-get (car override) match) (cdr override)))
    match))

(defun slacko-consult-tests--response (matches &optional page pages)
  "A search response carrying MATCHES at PAGE of PAGES."
  `((ok . t)
    (messages . ((matches . ,matches)
                 (total . ,(length matches))
                 (paging . ((page . ,(or page 1))
                            (pages . ,(or pages 1))))))))

(describe "the Consult session"
  (it "is not a second command beside `slacko-search'"
    (expect (commandp 'slacko-consult--search) :to-be nil)
    (expect (commandp 'slacko-search) :to-be-truthy)))

(describe "slacko-consult--plain-text"
  (it "resolves a mention from the name Slack sent with it"
    (expect (slacko-consult--plain-text "hi <@U123|alice> there" "team.slack.com")
            :to-equal "hi @alice there"))

  (it "resolves a mention from what the renderer cached"
    (spy-on 'slacko-render-cached-user :and-return-value "bob")
    (expect (slacko-consult--plain-text "hi <@U123>" "team.slack.com")
            :to-equal "hi @bob"))

  (it "leaves an unknown mention as its id rather than asking Slack"
    (spy-on 'slacko-render-cached-user :and-return-value nil)
    (spy-on 'slacko-creds-api-request)
    (expect (slacko-consult--plain-text "hi <@U123>" "team.slack.com")
            :to-equal "hi @U123")
    (expect 'slacko-creds-api-request :not :to-have-been-called))

  (it "names channels"
    (expect (slacko-consult--plain-text "see <#C1|dev> and <#C2>" "team.slack.com")
            :to-equal "see #dev and #C2"))

  (it "plainens broadcasts"
    (expect (slacko-consult--plain-text "<!here> ship it" "team.slack.com")
            :to-equal "@here ship it")
    (expect (slacko-consult--plain-text "<!subteam^S1|@platform> ping" "team.slack.com")
            :to-equal "@platform ping"))

  (it "keeps the label of a link, or the URL when it has none"
    (expect (slacko-consult--plain-text "<https://x.com|the docs>" "team.slack.com")
            :to-equal "the docs")
    (expect (slacko-consult--plain-text "<https://x.com>" "team.slack.com")
            :to-equal "https://x.com"))

  (it "decodes the entities Slack escapes"
    (expect (slacko-consult--plain-text "a &lt;b&gt; &amp; c" "team.slack.com")
            :to-equal "a <b> & c"))

  (it "folds a multi-line message onto one line"
    (expect (slacko-consult--plain-text "one\n\ntwo   three\t four" "team.slack.com")
            :to-equal "one two three four"))

  (it "survives a message without text"
    (expect (slacko-consult--plain-text nil "team.slack.com") :to-equal "")))

(describe "slacko-consult--annotation"
  (it "indents the message under the candidate"
    (let ((slacko-consult-text-lines 2)
          (slacko-consult-text-width 40))
      (expect (slacko-consult--annotation "short message")
              :to-equal "  short message")))

  (it "stops at the line cap and says so"
    (let ((slacko-consult-text-lines 1)
          (slacko-consult-text-width 20))
      (let ((annotation (slacko-consult--annotation
                         (string-join (make-list 40 "word") " "))))
        (expect (length (split-string annotation "\n")) :to-equal 1)
        (expect annotation :to-match "…\\'"))))

  (it "returns nil when annotations are turned off"
    (let ((slacko-consult-text-lines 0))
      (expect (slacko-consult--annotation "anything") :to-be nil)))

  (it "returns nil for a message with no text"
    (let ((slacko-consult-text-lines 2))
      (expect (slacko-consult--annotation "   ") :to-be nil))))

(defun slacko-consult-tests--dm-match (partner author &optional username)
  "A direct message result from a conversation with PARTNER, written by AUTHOR.
USERNAME is what Slack calls the author."
  (let ((match (slacko-consult-tests--match)))
    (setf (alist-get 'user match) author)
    (setf (alist-get 'username match) (or username "the-author"))
    (setf (alist-get 'channel match)
          (list (cons 'id "D123")
                (cons 'name partner)
                (cons 'user partner)
                (cons 'is_im t)
                (cons 'is_channel :json-false)))
    match))

(describe "slacko-consult--channel-label"
  (it "names a channel"
    (expect (slacko-consult--channel-label
             "team.slack.com" '((id . "C1") (name . "general") (is_channel . t)))
            :to-equal "#general"))

  (it "names the person a direct message is with"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-render-cache-user "team.slack.com" "U090" "natalie.see")
      (expect (slacko-consult--channel-label
               "team.slack.com" '((id . "D1") (is_im . t) (user . "U090")))
              :to-equal "@natalie.see")))

  (it "shows the id rather than asking Slack for the name"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (spy-on 'slacko-creds-api-request)
      (expect (slacko-consult--channel-label
               "team.slack.com" '((id . "D1") (is_im . t) (user . "U090")))
              :to-equal "@U090")
      (expect 'slacko-creds-api-request :not :to-have-been-called)))

  (it "names the members of a group conversation"
    (expect (slacko-consult--channel-label
             "team.slack.com" '((id . "G1") (is_mpim . t)
                                (name . "mpdm-alice--bob--carol-1")))
            :to-equal "@alice, bob, carol"))

  (it "says nothing when Slack said nothing"
    (expect (slacko-consult--channel-label "team.slack.com" nil)
            :to-equal "")))

(describe "slacko-consult--learn-names"
  (it "takes the partner's name from a message the partner wrote"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-consult--learn-names
       (list (slacko-consult-tests--dm-match "U090" "U090" "natalie.see"))
       "team.slack.com")
      (expect (slacko-render-cached-user "team.slack.com" "U090")
              :to-equal "natalie.see")))

  (it "learns nothing from a message written by somebody else"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-consult--learn-names
       (list (slacko-consult-tests--dm-match "U090" "UME" "me"))
       "team.slack.com")
      (expect (slacko-render-cached-user "team.slack.com" "U090") :to-be nil)))

  (it "leaves a name that came from the profile alone"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-render-cache-user "team.slack.com" "U090" "Natalie See")
      (slacko-consult--learn-names
       (list (slacko-consult-tests--dm-match "U090" "U090" "natalie.see"))
       "team.slack.com")
      (expect (slacko-render-cached-user "team.slack.com" "U090")
              :to-equal "Natalie See")))

  (it "learns nothing from a channel message"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-consult--learn-names (list (slacko-consult-tests--match))
                                   "team.slack.com")
      (expect (hash-table-count slacko-render--user-cache) :to-equal 0))))

(describe "slacko-consult--unknown-partners"
  (it "lists each unknown person once"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (expect (slacko-consult--unknown-partners
               (list (slacko-consult-tests--dm-match "U090" "UME")
                     (slacko-consult-tests--dm-match "U090" "UME")
                     (slacko-consult-tests--dm-match "U091" "UME")
                     (slacko-consult-tests--match))
               "team.slack.com")
              :to-equal '("U090" "U091"))))

  (it "leaves out the people already known"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal)))
      (slacko-render-cache-user "team.slack.com" "U090" "natalie.see")
      (expect (slacko-consult--unknown-partners
               (list (slacko-consult-tests--dm-match "U090" "UME"))
               "team.slack.com")
              :to-be nil))))

(describe "slacko-consult--with-names"
  (it "continues at once when every conversation has a name"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal))
          (continued nil))
      (spy-on 'slacko-creds-api-request-async)
      (slacko-consult--with-names (list (slacko-consult-tests--match))
                                  "team.slack.com"
                                  (lambda () (setq continued t)))
      (expect continued :to-be t)
      (expect 'slacko-creds-api-request-async :not :to-have-been-called)))

  (it "asks for each unknown person once and continues when all have answered"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal))
          (continued 0)
          (callbacks nil))
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (_host _endpoint _params cb) (push cb callbacks) nil))
      (slacko-consult--with-names
       (list (slacko-consult-tests--dm-match "U090" "UME")
             (slacko-consult-tests--dm-match "U091" "UME"))
       "team.slack.com"
       (lambda () (setq continued (1+ continued))))
      (expect (length callbacks) :to-equal 2)
      (expect continued :to-equal 0)
      (funcall (nth 0 callbacks)
               '((ok . t) (user . ((name . "bob")
                                   (profile . ((display_name . "bobby")))))))
      (expect continued :to-equal 0)
      (funcall (nth 1 callbacks)
               '((ok . t) (user . ((name . "alice") (profile . nil)))))
      (expect continued :to-equal 1)
      (expect (slacko-render-cached-user "team.slack.com" "U091")
              :to-equal "bobby")
      (expect (slacko-render-cached-user "team.slack.com" "U090")
              :to-equal "alice")))

  (it "continues even when Slack refused to name the person"
    (let ((slacko-render--user-cache (make-hash-table :test 'equal))
          (continued nil)
          (callback nil))
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (_host _endpoint _params cb) (setq callback cb) nil))
      (slacko-consult--with-names
       (list (slacko-consult-tests--dm-match "U090" "UME"))
       "team.slack.com"
       (lambda () (setq continued t)))
      (funcall callback nil)
      (expect continued :to-be t)
      (expect (slacko-render-cached-user "team.slack.com" "U090") :to-be nil))))

(describe "slacko-consult--candidate"
  (it "shows who said what, where and when"
    (let ((candidate (slacko-consult--candidate (slacko-consult-tests--match))))
      (expect candidate :to-match "john_doe")
      (expect candidate :to-match "#general")
      (expect candidate :to-match "hello there")))

  (it "carries the message and the raw result"
    (let* ((match (slacko-consult-tests--match))
           (candidate (slacko-consult--candidate match)))
      (expect (slacko-consult--match candidate) :to-equal match)
      (expect (plist-get (slacko-consult--message candidate) :channel-id)
              :to-equal "C456")))

  (it "hides the whole message on the candidate so filtering can reach it"
    (let* ((long (string-join (make-list 60 "needle") " "))
           (candidate (slacko-consult--candidate
                       (slacko-consult-tests--match (cons 'text long)))))
      (expect (string-match-p "needle needle" candidate) :to-be-truthy)
      (expect (text-property-any 0 (length candidate) 'invisible t candidate)
              :to-be-truthy)))

  (it "builds the annotation once, when the candidate is built"
    (let ((slacko-consult-text-lines 2))
      (expect (slacko-consult--annotate
               (slacko-consult--candidate (slacko-consult-tests--match)))
              :to-match "hello there")))

  (it "returns nothing for a candidate that carries no message"
    (expect (slacko-consult--message "") :to-be nil)
    (expect (slacko-consult--message nil) :to-be nil)
    (expect (slacko-consult--annotate "plain") :to-equal "")))

(describe "slacko-consult--url"
  (it "turns the stored link back into the one Slack publishes"
    (expect (slacko-consult--url
             (slacko--parse-result (slacko-consult-tests--match)))
            :to-equal
            "https://team.slack.com/archives/C456/p1738226435123456"))

  (it "returns nil without a permalink"
    (expect (slacko-consult--url '(:permalink "")) :to-be nil)))

(describe "slacko-consult--dedup"
  (it "drops a message this search already showed"
    (let* ((slacko-consult--seen (make-hash-table :test 'equal))
           (rows (mapcar #'slacko-consult--candidate
                         (list (slacko-consult-tests--match)
                               (slacko-consult-tests--match)))))
      (expect (length (slacko-consult--dedup rows)) :to-equal 1)))

  (it "keeps messages that differ"
    (let* ((slacko-consult--seen (make-hash-table :test 'equal))
           (rows (mapcar #'slacko-consult--candidate
                         (list (slacko-consult-tests--match)
                               (slacko-consult-tests--match
                                '(permalink . "https://team.slack.com/archives/C456/p9"))))))
      (expect (length (slacko-consult--dedup rows)) :to-equal 2))))

(describe "slacko-consult--next-page"
  (it "asks for the page after this one"
    (let ((slacko-consult-max-pages 3))
      (expect (slacko-consult--next-page '((page . 1) (pages . 5))) :to-equal 2)))

  (it "stops at the last page Slack has"
    (let ((slacko-consult-max-pages 10))
      (expect (slacko-consult--next-page '((page . 2) (pages . 2))) :to-be nil)))

  (it "stops at the page cap"
    (let ((slacko-consult-max-pages 2))
      (expect (slacko-consult--next-page '((page . 2) (pages . 9))) :to-be nil)))

  (it "stops when Slack sent no paging at all"
    (expect (slacko-consult--next-page nil) :to-be nil)))

(describe "slacko-consult--receive"
  (it "sends the candidates downstream and asks for the next page"
    (let ((slacko-consult--seen (make-hash-table :test 'equal))
          (slacko-consult-max-pages 5)
          (delivered nil))
      (let ((next (slacko-consult--receive
                   (slacko-consult-tests--response
                    (list (slacko-consult-tests--match)) 1 3)
                   (lambda (rows) (setq delivered rows)))))
        (expect (length delivered) :to-equal 1)
        (expect next :to-equal 2))))

  (it "delivers nothing when every message was seen already"
    (let ((slacko-consult--seen (make-hash-table :test 'equal))
          (calls 0))
      (let ((async (lambda (_rows) (setq calls (1+ calls)))))
        (slacko-consult--receive
         (slacko-consult-tests--response (list (slacko-consult-tests--match)))
         async)
        (slacko-consult--receive
         (slacko-consult-tests--response (list (slacko-consult-tests--match)))
         async))
      (expect calls :to-equal 1)))

  (it "reports what Slack refused and stops paging"
    (spy-on 'message)
    (expect (slacko-consult--receive '((ok . :json-false) (error . "ratelimited"))
                                     #'ignore)
            :to-be nil)
    (expect 'message :to-have-been-called-with
            "Slack search failed: %s" "ratelimited"))

  (it "reports a request that never arrived"
    (spy-on 'message)
    (expect (slacko-consult--receive nil #'ignore) :to-be nil)
    (expect 'message :to-have-been-called)))

(describe "slacko-consult--fetch"
  (it "asks search.messages for that query and page"
    (let ((slacko-consult--host "team.slack.com")
          (slacko-consult-page-size 100))
      (spy-on 'slacko-creds-api-request-async)
      (slacko-consult--fetch "budget" 2 #'ignore 0 #'ignore)
      (let ((args (spy-calls-args-for 'slacko-creds-api-request-async 0)))
        (expect (nth 0 args) :to-equal "team.slack.com")
        (expect (nth 1 args) :to-equal "search.messages")
        (expect (nth 2 args) :to-equal '((query "budget")
                                         (count "100")
                                         (page "2"))))))

  (it "chains to the page after the one that arrived"
    (let ((slacko-consult--host "team.slack.com")
          (slacko-consult--seen (make-hash-table :test 'equal))
          (slacko-consult--generation 7)
          (slacko-consult-max-pages 5)
          (asked nil)
          (callback nil))
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (_host _endpoint _params cb) (setq callback cb) nil))
      (slacko-consult--fetch "q" 1 #'ignore 7 (lambda (page) (setq asked page)))
      (funcall callback (slacko-consult-tests--response
                         (list (slacko-consult-tests--match)) 1 4))
      (expect asked :to-equal 2)))

  (it "drops a response that belongs to an older search"
    (let ((slacko-consult--host "team.slack.com")
          (slacko-consult--generation 9)
          (delivered nil)
          (callback nil))
      (spy-on 'slacko-creds-api-request-async :and-call-fake
              (lambda (_host _endpoint _params cb) (setq callback cb) nil))
      (slacko-consult--fetch "q" 1 (lambda (rows) (setq delivered rows)) 8 #'ignore)
      (funcall callback (slacko-consult-tests--response
                         (list (slacko-consult-tests--match))))
      (expect delivered :to-be nil))))

(describe "slacko-consult--source"
  (it "starts a search when input arrives and flushes what was shown"
    (let ((slacko-consult--seen (make-hash-table :test 'equal))
          (actions nil))
      (spy-on 'slacko-consult--fetch)
      (let ((source (slacko-consult--source
                     (lambda (action) (push action actions)))))
        (funcall source "budget")
        (expect 'slacko-consult--fetch :to-have-been-called)
        (expect (nth 1 (spy-calls-args-for 'slacko-consult--fetch 0)) :to-equal 1)
        (expect actions :to-equal '(flush)))))

  (it "asks for nothing when the input is blank"
    (let ((slacko-consult--seen (make-hash-table :test 'equal)))
      (spy-on 'slacko-consult--fetch)
      (funcall (slacko-consult--source #'ignore) "   ")
      (expect 'slacko-consult--fetch :not :to-have-been-called)))

  (it "retires the old search before starting the next"
    (let ((slacko-consult--seen (make-hash-table :test 'equal))
          (slacko-consult--generation 0))
      (spy-on 'slacko-consult--fetch)
      (let ((source (slacko-consult--source #'ignore)))
        (funcall source "one")
        (funcall source "two")
        (expect slacko-consult--generation :to-equal 2))))

  (it "kills the request buffers it is still holding when torn down"
    (let ((slacko-consult--seen (make-hash-table :test 'equal))
          (buffer (generate-new-buffer " *slacko-consult-test-request*"))
          (passed nil))
      (spy-on 'slacko-consult--fetch :and-return-value buffer)
      (let ((source (slacko-consult--source
                     (lambda (action) (setq passed action)))))
        (funcall source "one")
        (funcall source 'destroy)
        (expect (buffer-live-p buffer) :to-be nil)
        (expect passed :to-be 'destroy))))

  (it "passes actions it does not handle straight through"
    (let ((seen nil))
      (funcall (slacko-consult--source (lambda (action) (setq seen action)))
               'setup)
      (expect seen :to-be 'setup))))

(describe "slacko-consult--render-buffer"
  (it "renders the messages into a slacko-search-mode buffer"
    (let ((slacko-render-resolve-mentions nil)
          (name " *slacko-consult-test-render*"))
      (unwind-protect
          (with-current-buffer (slacko-consult--render-buffer
                                name (list (slacko-consult-tests--match))
                                "team.slack.com")
            (expect major-mode :to-be 'slacko-search-mode)
            (expect (buffer-string) :to-match "hello there")
            (expect (buffer-string) :to-match "john_doe")
            (expect slacko-reactions--entries :to-be nil))
        (kill-buffer name))))

  (it "registers the messages for reactions when asked to"
    (let ((slacko-render-resolve-mentions nil)
          (name " *slacko-consult-test-render*"))
      (unwind-protect
          (progn
            (spy-on 'slacko-reactions-setup)
            (with-current-buffer (slacko-consult--render-buffer
                                  name (list (slacko-consult-tests--match))
                                  "team.slack.com" t)
              (expect (length slacko-reactions--entries) :to-equal 1)
              (expect 'slacko-reactions-setup :to-have-been-called)))
        (kill-buffer name)))))

(describe "slacko-consult actions"
  (it "opens the thread the message belongs to"
    (spy-on 'slacko-thread-capture)
    (slacko-consult-open-thread
     (slacko-consult--candidate (slacko-consult-tests--match)))
    (expect 'slacko-thread-capture :to-have-been-called-with
            "https://team.slack.com/archives/C456/p1738226435123456"))

  (it "opens the message in the Slack app"
    (spy-on 'slacko--open-in-slack)
    (slacko-consult-open-in-slack
     (slacko-consult--candidate (slacko-consult-tests--match)))
    (expect 'slacko--open-in-slack :to-have-been-called-with
            "https://team.slack.com/archives/C456/p1738226435123456"))

  (it "copies the link"
    (spy-on 'message)
    (slacko-consult-copy-url
     (slacko-consult--candidate (slacko-consult-tests--match)))
    (expect (current-kill 0)
            :to-equal "https://team.slack.com/archives/C456/p1738226435123456"))

  (it "copies the text"
    (spy-on 'message)
    (slacko-consult-copy-text
     (slacko-consult--candidate (slacko-consult-tests--match)))
    (expect (current-kill 0) :to-equal "hello there"))

  (it "shows one message in a slacko-search-mode buffer"
    (let ((slacko-render-resolve-mentions nil))
      (spy-on 'pop-to-buffer)
      (spy-on 'slacko-reactions-setup)
      (slacko-consult-open-message
       (slacko-consult--candidate (slacko-consult-tests--match)))
      (let ((buffer (car (spy-calls-args-for 'pop-to-buffer 0))))
        (expect (buffer-name buffer) :to-equal slacko-search-buffer-name)
        (with-current-buffer buffer
          (expect (buffer-string) :to-match "hello there")))))

  (it "does nothing for a candidate with no message behind it"
    (spy-on 'slacko-thread-capture)
    (slacko-consult-open-thread "plain")
    (expect 'slacko-thread-capture :not :to-have-been-called)))

(describe "slacko-consult--embark-setup"
  (it "registers the category, its actions and its exporter"
    (assume (require 'embark nil t) "Embark is not installed")
    (let ((embark-keymap-alist nil)
          (embark-exporters-alist nil)
          (embark-default-action-overrides nil))
      (slacko-consult--embark-setup)
      (expect (alist-get 'slacko-consult-result embark-keymap-alist)
              :to-be 'slacko-consult-embark-map)
      (expect (alist-get 'slacko-consult-result embark-exporters-alist)
              :to-be #'slacko-consult-embark-export)
      (expect (alist-get 'slacko-consult-result embark-default-action-overrides)
              :to-be #'slacko-consult-open-thread))))

(describe "slacko-consult-embark-export"
  (it "renders every candidate into the search buffer"
    (let ((slacko-render-resolve-mentions nil)
          (candidates (mapcar #'slacko-consult--candidate
                              (list (slacko-consult-tests--match)
                                    (slacko-consult-tests--match
                                     '(text . "second message")
                                     '(permalink . "https://team.slack.com/archives/C456/p9"))))))
      (spy-on 'slacko-reactions-setup)
      (with-current-buffer (slacko-consult-embark-export candidates)
        (expect major-mode :to-be 'slacko-search-mode)
        (expect (buffer-string) :to-match "hello there")
        (expect (buffer-string) :to-match "second message")
        (expect (length slacko-reactions--entries) :to-equal 2)))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-consult-tests.el ends here
