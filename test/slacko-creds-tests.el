;;; slacko-creds-tests.el --- tests for slacko-creds -*- lexical-binding: t; -*-
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
;;  Tests for request building and response parsing.
;;
;;; Code:

(require 'buttercup)
(require 'slacko-creds)

(defun slacko-creds-tests--response-buffer (body)
  "Buffer holding an HTTP response with BODY, as `url' delivers it.
Undecoded bytes in a unibyte buffer, headers separated by CRLF."
  (let ((buf (generate-new-buffer " *slacko-creds-test-response*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
      (insert (encode-coding-string body 'utf-8)))
    buf))

(describe "slacko-creds--api-url"
  (it "builds the endpoint URL with query parameters"
    (expect (slacko-creds--api-url "search.messages" '((query "hello") (page "1")))
            :to-equal "https://slack.com/api/search.messages?query=hello&page=1"))

  (it "escapes parameter values"
    ;; which characters `url-hexify-string' spares differs between Emacs
    ;; versions, so the escaping is checked by what it means, not by how
    ;; it is spelled
    (let* ((url (slacko-creds--api-url "search.messages" '((query "in:#dev a b"))))
           (query (cadr (split-string url "[?]"))))
      (expect (string-match-p "[ #]" query) :to-be nil)
      (expect (url-unhex-string (cadr (split-string query "=")))
              :to-equal "in:#dev a b"))))

(describe "slacko-creds--auth-headers"
  (it "carries the token and the session cookie"
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (format "fake-%s" kind)))
    (let ((headers (slacko-creds--auth-headers "team.slack.com")))
      (expect (alist-get "Authorization" headers nil nil #'equal)
              :to-equal "Bearer fake-token")
      (expect (alist-get "Cookie" headers nil nil #'equal)
              :to-equal "d=fake-cookie;")))

  (it "errors when the workspace has no token"
    (spy-on 'slacko-creds-get :and-return-value nil)
    (expect (slacko-creds--auth-headers "team.slack.com") :to-throw 'error)))

(describe "slacko-creds--read-response"
  (it "parses the body as an alist"
    (let ((buf (slacko-creds-tests--response-buffer
                "{\"ok\":true,\"messages\":{\"total\":2}}")))
      (unwind-protect
          (let ((parsed (with-current-buffer buf (slacko-creds--read-response))))
            (expect (alist-get 'ok parsed) :to-be t)
            (expect (alist-get 'total (alist-get 'messages parsed)) :to-equal 2))
        (kill-buffer buf))))

  (it "decodes non-ASCII text"
    (let ((buf (slacko-creds-tests--response-buffer
                "{\"text\":\"caf\u00e9 \u4f60\u597d\"}")))
      (unwind-protect
          (let ((parsed (with-current-buffer buf (slacko-creds--read-response))))
            (expect (alist-get 'text parsed) :to-equal "caf\u00e9 \u4f60\u597d"))
        (kill-buffer buf))))

  (it "returns nil for an empty body"
    (let ((buf (slacko-creds-tests--response-buffer "")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf))))

  (it "returns nil for a truncated body"
    (let ((buf (slacko-creds-tests--response-buffer "{\"ok\":tr")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf))))

  (it "returns nil when there are no headers at all"
    (let ((buf (generate-new-buffer " *slacko-creds-test-response*")))
      (unwind-protect
          (expect (with-current-buffer buf (slacko-creds--read-response))
                  :to-be nil)
        (kill-buffer buf)))))

(describe "slacko-creds-api-request-async"
  (before-each
    (spy-on 'slacko-creds-get :and-call-fake
            (lambda (_host kind) (format "fake-%s" kind))))

  (it "hands the parsed response to the callback and kills the request buffer"
    (let (result request-buffer)
      (spy-on 'url-retrieve :and-call-fake
              (lambda (_url callback &rest _)
                (setq request-buffer
                      (slacko-creds-tests--response-buffer "{\"ok\":true}"))
                (with-current-buffer request-buffer (funcall callback nil))
                request-buffer))
      (slacko-creds-api-request-async
       "team.slack.com" "search.messages" '((query "hi"))
       (lambda (data) (setq result data)))
      (expect (alist-get 'ok result) :to-be t)
      (expect (buffer-live-p request-buffer) :to-be nil)))

  (it "requests the endpoint URL with authenticated headers"
    (spy-on 'url-retrieve :and-return-value nil)
    (slacko-creds-api-request-async
     "team.slack.com" "search.messages" '((query "hi")) #'ignore)
    (let ((url (car (spy-calls-args-for 'url-retrieve 0))))
      (expect url :to-match "\\`https://slack\\.com/api/search\\.messages\\?query=hi")))

  (it "calls back with nil when the request errored"
    (let ((called 'not-called))
      (spy-on 'url-retrieve :and-call-fake
              (lambda (_url callback &rest _)
                (let ((buf (slacko-creds-tests--response-buffer "{\"ok\":true}")))
                  (with-current-buffer buf
                    (funcall callback '(:error (error connection-failed))))
                  buf)))
      (slacko-creds-api-request-async
       "team.slack.com" "search.messages" '((query "hi"))
       (lambda (data) (setq called data)))
      (expect called :to-be nil))))

;; Local Variables:
;; package-lint-main-file: "slacko.el"
;; End:
;;; slacko-creds-tests.el ends here
