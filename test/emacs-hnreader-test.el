;;; emacs-hnreader-test.el --- Tests for emacs-hnreader
(ert-deftest-async test/promise-dom (done)
                   (promise-done
                    (promise-chain
                        (hnreader--promise-dom "https://news.ycombinator.com/news")
                      (then (lambda (result)
                              (should (listp result))
                              ;; (message "%s" result)
                              (funcall done)))
                      (promise-catch done))))

(defun hnreader-test--dom (file)
  "Parse fixture FILE the way `hnreader--promise-dom' parses a response."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file root-test-path))
    (goto-char (point-min))
    (while (re-search-forward ">\\*" nil t)
      (replace-match ">-"))
    (libxml-parse-html-region (point-min) (point-max))))

(defmacro hnreader-test--with-rendered (file &rest body)
  "Render fixture FILE into a throwaway comment buffer and run BODY there."
  (declare (indent 1))
  `(let ((hnreader--comment-buffer " *hnreader-test*"))
     (unwind-protect
         (progn
           (hnreader--print-comments (hnreader-test--dom ,file)
                                     "https://news.ycombinator.com/item?id=1")
           (with-current-buffer (hnreader--get-hn-comment-buffer)
             ,@body))
       (when (get-buffer " *hnreader-test*")
         (kill-buffer " *hnreader-test*")))))

;; A job post carries neither a score nor navs, so `hnreader--get-post-info'
;; returns nil for it; the front page and a user profile have no fatitem at
;; all.  `insert' signals on that nil rather than skipping it.
(ert-deftest test/renders-job-item-without-byline ()
  (hnreader-test--with-rendered "job-item.html"
    (should (string-match-p "^#\\+TITLE: ATG (YC F25) Is Hiring" (buffer-string)))))

(ert-deftest test/renders-user-page-without-fatitem ()
  (hnreader-test--with-rendered "user-page.html"
    (should (string-match-p "^#\\+TITLE: Profile: erdaltoprak" (buffer-string)))))

(ert-deftest test/renders-frontpage-without-fatitem ()
  (hnreader-test--with-rendered "frontpage.html"
    (should (string-prefix-p "#+STARTUP:" (buffer-string)))))

(ert-deftest test/post-info-is-nil-without-byline ()
  (should-not (hnreader--get-post-info (hnreader-test--dom "user-page.html"))))

(ert-deftest test/post-info-is-nil-for-a-job-item ()
  (should-not (hnreader--get-post-info (hnreader-test--dom "job-item.html"))))

;; A page with nothing to point at must not advertise item?id=nil
(ert-deftest test/title-has-no-link-without-an-item ()
  (let ((title (hnreader--get-title (hnreader-test--dom "user-page.html"))))
    (should (car title))
    (should-not (cdr title))))

;; Hacker News answers concurrent item requests with HTTP 429 and keeps
;; throttling afterwards, so a second page must cancel the first rather
;; than race it.
(ert-deftest test/second-fetch-aborts-the-first ()
  (let ((aborted '())
        (issued '())
        (hnreader--request nil))
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest _) (push url issued) (list 'response url)))
              ((symbol-function 'request-abort)
               (lambda (r) (push r aborted))))
      (hnreader--promise-dom "https://news.ycombinator.com/item?id=1")
      (hnreader--promise-dom "https://news.ycombinator.com/item?id=2")
      (should (equal (nreverse issued)
                     '("https://news.ycombinator.com/item?id=1"
                       "https://news.ycombinator.com/item?id=2")))
      (should (equal aborted '((response "https://news.ycombinator.com/item?id=1"))))
      (should (equal hnreader--request '(response "https://news.ycombinator.com/item?id=2"))))))

(ert-deftest test/identifies-itself-when-fetching ()
  (let (headers (hnreader--request nil))
    (cl-letf (((symbol-function 'request)
               (lambda (_url &rest args) (setq headers (plist-get args :headers)) 'response))
              ((symbol-function 'request-abort) #'ignore))
      (let ((hnreader-user-agent "probe/1"))
        (hnreader--promise-dom "https://news.ycombinator.com/item?id=1"))
      (should (equal headers '(("User-Agent" . "probe/1"))))
      (let ((hnreader-user-agent nil))
        (hnreader--promise-dom "https://news.ycombinator.com/item?id=1"))
      (should-not headers))))

(ert-deftest test/first-fetch-has-nothing-to-abort ()
  (let ((aborted 0)
        (hnreader--request nil))
    (cl-letf (((symbol-function 'request) (lambda (&rest _) 'response))
              ((symbol-function 'request-abort) (lambda (_) (cl-incf aborted))))
      (hnreader--promise-dom "https://news.ycombinator.com/item?id=1")
      (should (= aborted 0)))))

;; Rejecting an aborted fetch would paint the error page over the page the
;; user actually asked for.
(ert-deftest test/aborting-a-fetch-reports-no-failure ()
  (let ((rejected '())
        (error-cb nil)
        (hnreader--request nil))
    (cl-letf (((symbol-function 'request)
               (lambda (_url &rest args) (setq error-cb (plist-get args :error)) 'response))
              ((symbol-function 'request-abort) #'ignore))
      (hnreader--promise-dom "https://news.ycombinator.com/item?id=1")
      (promise-catch (hnreader--promise-dom "https://news.ycombinator.com/item?id=2")
                     (lambda (r) (push r rejected)))
      (funcall error-cb :error-thrown '(error http 429) :symbol-status 'abort)
      (should-not rejected))))

(ert-deftest test/browser-fetch-command ()
  (let ((hnreader-browser-name "Test Browser"))
    (let ((cmd (hnreader--browser-fetch-command
                "https://news.ycombinator.com/item?id=1")))
      (should (equal (car cmd) "osascript"))
      (should (member "-l" cmd))
      (should (member "JavaScript" cmd))
      (should (equal (car (last cmd)) "Test Browser"))
      (should (equal (nth (- (length cmd) 2) cmd)
                     "https://news.ycombinator.com/item?id=1"))
      ;; the in-tab request has to be same-origin to carry the session
      (should (string-match-p "news\\.ycombinator\\.com"
                              hnreader--browser-fetch-jxa)))))

(ert-deftest test/promise-dom-goes-through-the-fetch-function ()
  (let (seen)
    (let ((hnreader-fetch-function (lambda (url) (setq seen url) :promise)))
      (should (eq (hnreader--promise-dom "https://news.ycombinator.com/item?id=7")
                  :promise))
      (should (equal seen "https://news.ycombinator.com/item?id=7")))))

(ert-deftest test/explains-a-rate-limit ()
  (should (string-match-p "rate limiting this address"
                          (hnreader--explain-reason '(error http 429)))))

(ert-deftest test/prints-an-unrecognised-reason-verbatim ()
  (should (string-match-p "exited abnormally with code 3"
                          (hnreader--explain-reason
                           '(error . "exited abnormally with code 3")))))

(ert-deftest test/print-error-replaces-the-loading-placeholder ()
  (let ((buf (get-buffer-create " *hnreader-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "Loading..."))
          (hnreader--print-error buf "https://news.ycombinator.com/item?id=1"
                                 '(error http 429) 'hnreader-comment)
          (with-current-buffer buf
            (should-not (string-match-p "Loading\\.\\.\\." (buffer-string)))
            (should (string-match-p "rate limiting this address" (buffer-string)))
            (should (string-match-p "item\\?id=1" (buffer-string)))
            (should (string-match-p "\\[Retry\\]\\]" (buffer-string)))))
      (kill-buffer buf))))

;;; emacs-hnreader-test.el ends here
