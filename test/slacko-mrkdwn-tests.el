;;; slacko-mrkdwn-tests.el --- tests for slacko-mrkdwn.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 19, 2025
;; Keywords: tools tests
;; Homepage: https://github.com/agzam/slacko.el
;; Package-Requires: ((emacs "29.4"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for Slack mrkdwn to org-mode conversion
;;
;;; Code:

(require 'buttercup)
(require 'slacko-mrkdwn)

(describe "slacko-mrkdwn--convert-code-blocks"
  (it "converts multiline code block with language"
    (let ((input "```python\nprint('hello')\nprint('world')\n```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src python\nprint('hello')\nprint('world')\n#+end_src")))

  (it "converts multiline code block without language"
    (let ((input "```\nsome code\nmore code\n```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nsome code\nmore code\n#+end_src")))

  (it "converts single-line code block without newlines"
    (let ((input "```export OHI_TOKEN='secret'```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nexport OHI_TOKEN='secret'\n#+end_src")))

  (it "converts code block starting on same line without language"
    (let ((input "```stream | project_types\ndiscovered_schema | data\n```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\nstream | project_types\ndiscovered_schema | data\n#+end_src")))

  (it "does not treat first word as language without newline"
    (let ((input "```server: https://example.com:6448```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :not :to-match "#\\+begin_src server")))

  (it "handles multiple code blocks"
    (let* ((input "Text ```code1``` and ```python\ncode2\n``` end")
           (result (slacko-mrkdwn--convert-code-blocks input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "code1")
      (expect result :to-match "code2")))

  (it "handles empty code blocks"
    (let ((input "``````"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\n\n#+end_src")))

  (it "trims whitespace from code content"
    (let ((input "```\n  code with spaces  \n```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-equal "#+begin_src text\ncode with spaces\n#+end_src")))

  (it "preserves internal whitespace and newlines"
    (let ((input "```\nline1\n\nline2\n  indented\n```"))
      (expect (slacko-mrkdwn--convert-code-blocks input)
              :to-match "line1\n\nline2\n  indented"))))

(describe "slacko-mrkdwn--convert-inline-code"
  (it "converts single backtick to org verbatim"
    (expect (slacko-mrkdwn--convert-inline-code "`code`")
            :to-equal "~code~"))

  (it "converts multiple inline code segments"
    (expect (slacko-mrkdwn--convert-inline-code "Use `foo` and `bar` here")
            :to-equal "Use ~foo~ and ~bar~ here"))

  (it "handles inline code with special characters"
    (expect (slacko-mrkdwn--convert-inline-code "`$variable` and `function()`")
            :to-equal "~$variable~ and ~function()~"))

  (it "does not convert backticks spanning multiple lines"
    (expect (slacko-mrkdwn--convert-inline-code "`code\nwith\nnewlines`")
            :to-equal "`code\nwith\nnewlines`"))

  (it "handles text without inline code"
    (expect (slacko-mrkdwn--convert-inline-code "regular text")
            :to-equal "regular text"))

  (it "handles empty string"
    (expect (slacko-mrkdwn--convert-inline-code "")
            :to-equal "")))

(describe "slacko-mrkdwn--convert-links"
  (it "converts Slack link with text"
    (expect (slacko-mrkdwn--convert-links "<https://example.com|Example>")
            :to-equal "[[https://example.com][Example]]"))

  (it "converts bare URL"
    (expect (slacko-mrkdwn--convert-links "<https://example.com>")
            :to-equal "[[https://example.com]]"))

  (it "converts multiple links"
    (let ((input "See <https://one.com|One> and <https://two.com>"))
      (expect (slacko-mrkdwn--convert-links input)
              :to-equal "See [[https://one.com][One]] and [[https://two.com]]")))

  (it "handles links with complex URLs"
    (expect (slacko-mrkdwn--convert-links "<https://example.com/path?query=1&foo=bar|Link>")
            :to-equal "[[https://example.com/path?query=1&foo=bar][Link]]"))

  (it "handles text without links"
    (expect (slacko-mrkdwn--convert-links "regular text")
            :to-equal "regular text"))

  (it "handles empty string"
    (expect (slacko-mrkdwn--convert-links "")
            :to-equal "")))

(describe "slacko-mrkdwn--convert-strikethrough"
  (it "converts strikethrough to org format"
    (expect (slacko-mrkdwn--convert-strikethrough "~deleted text~")
            :to-equal "+deleted text+"))

  (it "converts multiple strikethrough segments"
    (expect (slacko-mrkdwn--convert-strikethrough "~one~ and ~two~")
            :to-equal "+one+ and +two+"))

  (it "does not convert tildes spanning multiple lines"
    (expect (slacko-mrkdwn--convert-strikethrough "~multi\nline~")
            :to-equal "~multi\nline~"))

  (it "handles text without strikethrough"
    (expect (slacko-mrkdwn--convert-strikethrough "regular text")
            :to-equal "regular text")))

(describe "slacko-mrkdwn--convert-blockquotes"
  (it "converts single blockquote line"
    (expect (slacko-mrkdwn--convert-blockquotes "&gt; quoted text")
            :to-equal "│ quoted text"))

  (it "converts blockquote without space after marker"
    (expect (slacko-mrkdwn--convert-blockquotes "&gt;quoted text")
            :to-equal "│ quoted text"))

  (it "converts multiline blockquote"
    (expect (slacko-mrkdwn--convert-blockquotes "&gt; line one\n&gt; line two")
            :to-equal "│ line one\n│ line two"))

  (it "does not convert &gt; in the middle of a line"
    (expect (slacko-mrkdwn--convert-blockquotes "foo &gt; bar")
            :to-equal "foo &gt; bar"))

  (it "handles mixed quoted and unquoted lines"
    (expect (slacko-mrkdwn--convert-blockquotes "normal\n&gt; quoted\nnormal again")
            :to-equal "normal\n│ quoted\nnormal again")))

(describe "slacko-mrkdwn--decode-entities"
  (it "decodes &amp;"
    (expect (slacko-mrkdwn--decode-entities "fish &amp; chips")
            :to-equal "fish & chips"))

  (it "decodes &lt; and &gt;"
    (expect (slacko-mrkdwn--decode-entities "1 &lt; 2 &gt; 3")
            :to-equal "1 < 2 > 3"))

  (it "decodes &amp; last so double-encoded entities stay literal"
    (expect (slacko-mrkdwn--decode-entities "&amp;gt; &amp;lt; &amp;amp;")
            :to-equal "&gt; &lt; &amp;")))

(describe "slacko-mrkdwn-to-org"
  (it "handles nil input"
    (expect (slacko-mrkdwn-to-org nil)
            :to-be nil))

  (it "handles empty string"
    (expect (slacko-mrkdwn-to-org "")
            :to-equal ""))

  (it "converts code blocks before links"
    (let* ((input "```server: <https://example.com:6448>```")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "<https://example.com:6448>")
      (expect result :not :to-match "\\[\\[https://example.com")))

  (it "converts links outside code blocks"
    (let* ((input "See <https://example.com|link> and ```code```")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "\\[\\[https://example.com\\]\\[link\\]\\]")
      (expect result :to-match "#\\+begin_src")))

  (it "handles complex real-world example"
    (let* ((input "If you're using OHI:\n1. <https://devops.com|Console>\n2. Download config\n```server: <https://kubectl.example.com:6448>```\n3. Use `tunnel.sh` script")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "\\[\\[https://devops.com\\]\\[Console\\]\\]")
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "server: <https://kubectl.example.com:6448>")
      (expect result :to-match "~tunnel.sh~")))

  (it "preserves formatting markers (bold, italic)"
    (let ((input "*bold* and _italic_ text"))
      (expect (slacko-mrkdwn-to-org input)
              :to-equal "*bold* and _italic_ text")))

  (it "handles mixed inline code and links"
    (let* ((input "Use `command` from <https://example.com|here>")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "~command~")
      (expect result :to-match "\\[\\[https://example.com\\]\\[here\\]\\]")))

  (it "handles multiple code blocks with different content types"
    (let* ((input "First ```python\ncode1\n``` then ```code2``` and <https://link.com|link>")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src python")
      (expect result :to-match "code1")
      (expect result :to-match "code2")
      (expect result :to-match "\\[\\[https://link.com\\]\\[link\\]\\]")))

  (it "handles code blocks with URLs that should not be converted"
    (let* ((input "Config:\n```\nserver: <https://example.com:8080>\n```\nSee also <https://docs.com|docs>")
           (result (slacko-mrkdwn-to-org input)))
      ;; URL inside code block should NOT be converted to org link
      (expect result :to-match "server: <https://example.com:8080>")
      (expect result :not :to-match "server: \\[\\[https://example.com:8080\\]\\]")
      ;; URL outside code block SHOULD be converted
      (expect result :to-match "\\[\\[https://docs.com\\]\\[docs\\]\\]")))

  (it "handles edge case with adjacent code and links"
    (let* ((input "```code```<https://link.com>")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "\\[\\[https://link.com\\]\\]")))

  (it "handles nested formatting correctly"
    (let* ((input "Text with `inline code` and *bold `code`* and links <https://example.com>")
           (result (slacko-mrkdwn-to-org input)))
      (expect result :to-match "~inline code~")
      (expect result :to-match "~code~")
      (expect result :to-match "\\[\\[https://example.com\\]\\]")))

  (it "decodes &amp; in prose (user-reported example)"
    (let ((input "\"You can also, in all directives, specify keys after &amp; which will not be bound, for documentation or checking purposes.\""))
      (expect (slacko-mrkdwn-to-org input)
              :to-equal "\"You can also, in all directives, specify keys after & which will not be bound, for documentation or checking purposes.\"")))

  (it "decodes mid-line &gt;"
    (expect (slacko-mrkdwn-to-org "foo &gt; bar")
            :to-match "foo > bar"))

  (it "converts line-start &gt; to a quote bar before decoding inner entities"
    (expect (slacko-mrkdwn-to-org "&gt; quoted &amp; text")
            :to-equal "│ quoted & text"))

  (it "decodes entities inside inline code"
    (expect (slacko-mrkdwn-to-org "`a &amp;&amp; b`")
            :to-match "~a && b~"))

  (it "decodes entities inside code blocks"
    (let ((result (slacko-mrkdwn-to-org "```\nif (a &lt; b &amp;&amp; b &gt; c) {}\n```")))
      (expect result :to-match "#\\+begin_src")
      (expect result :to-match "if (a < b && b > c) {}"))))

(describe "slacko-mrkdwn-from-org"
  (it "turns org emphasis into Slack's"
    (expect (slacko-mrkdwn-from-org "*b* /i/ _u_ +s+")
            :to-equal "*b* _i_ _u_ ~s~"))

  (it "turns code and verbatim into backticks"
    (expect (slacko-mrkdwn-from-org "run ~make test~ or =make lint=")
            :to-equal "run `make test` or `make lint`"))

  (it "leaves alone what org does not read as markup"
    (expect (slacko-mrkdwn-from-org "a*b*c snake_case 1+1+2 C++ ~/path x+y+")
            :to-equal "a*b*c snake_case 1+1+2 C++ ~/path x+y+"))

  (it "turns links into Slack's, with and without a description"
    (expect (slacko-mrkdwn-from-org
             "[[https://a.com][A]] [[https://b.com]] https://c.com [[mailto:x@y.z][mail]]")
            :to-equal "<https://a.com|A> <https://b.com> <https://c.com> <mailto:x@y.z|mail>"))

  (it "sends a slack: link as the https link it stands for"
    (expect (slacko-mrkdwn-from-org "[[slack://team.slack.com/archives/C1/p1][here]]")
            :to-equal "<https://team.slack.com/archives/C1/p1|here>"))

  (it "keeps a link Slack cannot open as typed"
    (expect (slacko-mrkdwn-from-org "see [[file:notes.org][notes]]")
            :to-equal "see [[file:notes.org][notes]]"))

  (it "turns a src block into a code block without its language"
    (expect (slacko-mrkdwn-from-org "#+begin_src clojure\n(def a 1)\n,* x\n#+end_src\nafter")
            :to-equal "```\n(def a 1)\n* x\n```\nafter"))

  (it "turns a quote block into quoted lines"
    (expect (slacko-mrkdwn-from-org "#+begin_quote\nfirst *b*\nsecond\n#+end_quote\ntail")
            :to-equal "> first *b*\n> second\ntail"))

  (it "turns a line break into a newline"
    (expect (slacko-mrkdwn-from-org "one\\\\\ntwo")
            :to-equal "one\ntwo"))

  (it "escapes &, < and > in text and in code"
    (expect (slacko-mrkdwn-from-org "a < b & c > d =x<y= &amp;")
            :to-equal "a &lt; b &amp; c &gt; d `x&lt;y` &amp;amp;"))

  (it "keeps lists, headings and paragraphs as typed"
    (expect (slacko-mrkdwn-from-org "- one\n- two /x/\n\n* top\nlast")
            :to-equal "- one\n- two _x_\n\n* top\nlast"))

  (it "returns what `slacko-mrkdwn-to-org' reads back"
    (let ((org "*b* ~c~ +s+ [[https://a.com][A]] & <x>"))
      (expect (slacko-mrkdwn-to-org (slacko-mrkdwn-from-org org))
              :to-equal org)))

  (it "returns plain text without properties"
    (expect (text-properties-at 0 (slacko-mrkdwn-from-org "plain words"))
            :to-be nil)))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-mrkdwn-tests.el ends here
