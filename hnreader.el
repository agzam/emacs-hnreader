;;; hnreader.el --- A hackernews reader -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-hnreader/
;; Package-Requires: ((emacs "25.1") (promise "1.1") (request "0.3.0") (org "9.2"))
;; Version: 0.2.9

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package renders hackernews website at https://news.ycombinator.com/ in
;; an org buffer. Almost everything works. Features that are not supported are
;; account related features. You cannot add comment, downvote or upvote.

;;; Dependencies
;; `promise' and `request' are required.
;; user must have `org-mode' 9.2 or later installed also.

;;; Commands
;; hnreader-news: Load news page.
;; hnreader-past: Load past page.
;; hnreader-ask: Load ask page.
;; hnreader-show: Load show page.
;; hnreader-newest: Load new link page.
;; hnreader-best: Load page with best articles.
;; hnreader-more: Load more.
;; hnreader-back: Go back to previous page.
;; hnreader-comment: read an HN item url such as https://news.ycombinator.com/item?id=1

;;; Customization
;; hnreader-history-max: max number history items to remember.
;; hnreader-view-comments-in-same-window: if nil then will not create new window
;; when viewing comments

;;; Changelog
;; 0.2.9 2026-08-14 Render pages without a byline, supersede an in-flight
;;                  fetch instead of racing it, optionally fetch through a
;;                  logged-in browser tab, report load failures
;; 0.2.8 2025-07-02 Show title in all pages
;; 0.2.7 2025-07-01 update title capture for page and item
;; 0.2.6 2024-11-09 update css class capture
;; 0.2.5 2022-11-16 handle all kinds of items
;; 0.2.4 2022-11-16 add reply link
;; 0.2.3 2022-11-14 add reply link
;; 0.2.2 2022-09-27 update css class grab for entry title
;; 0.2.1 2021-10-18 update css class grab for entry title

;;; Code:
(require 'promise)
(require 'request)
(require 'shr)
(require 'dom)
(require 'cl-lib)
(require 'org)

;; public variables
(defgroup hnreader nil
  "Search and read stackoverflow and sisters's sites."
  :group 'extensions
  :group 'convenience
  :version "25.1"
  :link '(emacs-commentary-link "hnreader.el"))

(defcustom hnreader-history-max 100
  "Max history to remember."
  :type 'integer
  :group 'hnreader)

(defcustom hnreader-view-comments-in-same-window t
  "Max history to remember."
  :type 'boolean
  :group 'hnreader)

(defcustom hnreader-user-agent
  (format "hnreader/%s (Emacs %s; +https://github.com/thanhvg/emacs-hnreader)"
          "0.2.9" emacs-version)
  "What to identify as when fetching a page.
Hacker News hands out a much smaller request budget to the User-Agent
curl sends by default, so leaving this unset costs pages."
  :type '(choice (const :tag "Whatever curl sends" nil) string)
  :group 'hnreader)

(defcustom hnreader-fetch-function #'hnreader--fetch-via-request
  "How a page is retrieved.
`hnreader--fetch-via-request' fetches directly.  Hacker News gives an
anonymous client a small request budget and answers HTTP 429 once it is
spent, so reading more than a few items in a sitting fails.
`hnreader--fetch-via-browser' runs the request inside a running browser
tab instead, carrying that browser's logged-in session."
  :type '(choice (const :tag "Directly" hnreader--fetch-via-request)
                 (const :tag "Through a browser tab" hnreader--fetch-via-browser)
                 function)
  :group 'hnreader)

(defcustom hnreader-browser-name "Brave Browser"
  "MacOS browser application used by `hnreader--fetch-via-browser'."
  :type 'string
  :group 'hnreader)

;; internal stuff
(defvar hnreader--buffer "*HN*"
  "Buffer for HN pages.")

(defvar hnreader--comment-buffer "*HNComments*"
  "Comment buffer.")

(defvar hnreader--indent 40
  "Tab value which is the width of an indent.
top level commnet is 0 indent
second one is 40
third one is 80.")

(defvar hnreader--history '()
  "History list.")

(defvar hnreader--more-link nil
  "Load more link.")

(define-derived-mode hnreader-mode
  org-mode "HN Reader"
  "Major mode for browsing Hackernews.")

(defvar hnreader-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `hnreader-mode'")

;;;###autoload
(defun hnreader-back ()
  "Go back to previous location in history."
  (interactive)
  (let ((link (nth 1 hnreader--history)))
    (if link
        (progn
          (setq hnreader--history (cdr hnreader--history))
          (when (> (length hnreader--history) hnreader-history-max)
            (setq hnreader--history (seq-take hnreader--history hnreader-history-max)))
          (hnreader-read-page-back link))
      (message "Nothing to go back."))))

(defun hnreader--get-hn-buffer ()
  "Get hn buffer."
  (get-buffer-create hnreader--buffer))

(defun hnreader--get-hn-comment-buffer ()
  "Get hn commnet buffer."
  (get-buffer-create hnreader--comment-buffer))

(defvar hnreader--request nil
  "The fetch in flight, if any.")

(defun hnreader--parse-html ()
  "Parse the current buffer as a Hacker News page."
  (goto-char (point-min))
  (while (re-search-forward ">\\*" nil t)
    (replace-match ">-"))
  (libxml-parse-html-region (point-min) (point-max)))

(defun hnreader--fetch-via-request (url)
  "Promise the dom of URL, fetched directly."
  ;; Hacker News answers concurrent requests with HTTP 429 and keeps
  ;; throttling for a while after, and every page lands in the same buffer,
  ;; so an earlier fetch is only ever in the way of a later one.
  (when hnreader--request
    (request-abort hnreader--request))
  (promise-new
   (lambda (resolve reject)
     (setq hnreader--request
           (request url
             :headers (when hnreader-user-agent
                        `(("User-Agent" . ,hnreader-user-agent)))
             :parser #'hnreader--parse-html
             :error (cl-function (lambda (&rest args &key error-thrown symbol-status &allow-other-keys)
                                   ;; a superseded fetch is not a failure to report
                                   (unless (eq symbol-status 'abort)
                                     (funcall reject error-thrown))))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (funcall resolve data))))))))

(defconst hnreader--browser-fetch-jxa
  "function run(argv) {
  var url = argv[0];
  var appName = argv[1];
  var app = Application(appName);
  var origin = 'https://news.ycombinator.com';
  var wins = app.windows();
  if (wins.length === 0) { throw new Error('no ' + appName + ' windows open'); }
  var tab = null, win = null, created = false;
  for (var i = 0; i < wins.length && !tab; i++) {
    var tabs = wins[i].tabs();
    for (var j = 0; j < tabs.length; j++) {
      var u = tabs[j].url();
      if (u && u.lastIndexOf(origin, 0) === 0) { tab = tabs[j]; win = wins[i]; break; }
    }
  }
  if (!tab) {
    win = wins[0];
    win.tabs.push(app.Tab({url: origin + '/robots.txt'}));
    created = true;
    var ts = win.tabs();
    tab = ts[ts.length - 1];
    for (var k = 0; k < 100; k++) {
      delay(0.1);
      try { if (tab.loading() === false) { break; } } catch (e) { break; }
    }
  }
  var js = \"(function(){try{var x=new XMLHttpRequest();x.open('GET',\" + JSON.stringify(url) + \",false);x.send(null);return 'HTTP '+x.status+String.fromCharCode(10)+x.responseText}catch(e){return 'HTTP 0'+String.fromCharCode(10)+String(e)}})()\";
  try {
    return tab.execute({javascript: js});
  } finally {
    if (created) { tab.close(); }
  }
}"
  "JXA program running a same-origin Hacker News request inside the browser.
Takes URL and browser name.  Reuses an existing news.ycombinator.com tab
when present, otherwise opens a throwaway one and closes it after.
Prints \"HTTP <status>\" on the first line and the page after it.")

(defun hnreader--browser-fetch-command (url)
  "Build the osascript command list requesting URL via the browser."
  (list "osascript" "-l" "JavaScript" "-e" hnreader--browser-fetch-jxa
        (url-encode-url url)
        hnreader-browser-name))

(defun hnreader--fetch-via-browser (url)
  "Promise the dom of URL, fetched inside a running browser tab.
The request carries the browser's Hacker News session, which is given a
far larger request budget than an anonymous one; anonymous fetches draw
HTTP 429 after a handful of pages."
  (promise-new
   (lambda (resolve reject)
     (let ((stdout (generate-new-buffer " *hnreader-browser-fetch*"))
           (stderr (generate-new-buffer " *hnreader-browser-fetch-err*")))
       (make-process
        :name "hnreader-browser-fetch"
        :buffer stdout
        :stderr stderr
        :noquery t
        :command (hnreader--browser-fetch-command url)
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (/= (process-exit-status proc) 0)
                    (funcall reject
                             (format "browser fetch failed: %s"
                                     (string-trim
                                      (with-current-buffer stderr (buffer-string)))))
                  (with-current-buffer stdout
                    (goto-char (point-min))
                    (let ((status (buffer-substring-no-properties
                                   (point-min) (line-end-position))))
                      (if (not (string= status "HTTP 200"))
                          (funcall reject (list 'error 'http
                                                (string-to-number
                                                 (or (cadr (split-string status)) "0"))))
                        (delete-region (point-min) (min (1+ (line-end-position)) (point-max)))
                        (funcall resolve (hnreader--parse-html))))))
              (when-let* ((p (get-buffer-process stderr)))
                (delete-process p))
              (kill-buffer stdout)
              (kill-buffer stderr)))))))))

(defun hnreader--promise-dom (url)
  "Promise the dom of URL via `hnreader-fetch-function'."
  (funcall hnreader-fetch-function url))

(defun hnreader--prepare-buffer (buf &optional msg)
  "Print MSG message and prepare window for BUF buffer."
  (when (not (equal (window-buffer) buf))
    (if hnreader-view-comments-in-same-window
        ;; (switch-to-buffer buf)
        (select-window (display-buffer buf '(display-buffer-use-some-window)))
      (switch-to-buffer-other-window buf)))
  ;; (display-buffer buf '(display-buffer-use-some-window (inhibit-same-window . t))))
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert (if msg
                msg
              "Loading...")))
  buf)

(defun hnreader--print-header ()
  "Print header links to current buffer."
  (insert "[[elisp:(hnreader-news)][News]] | ")
  (insert "[[elisp:(hnreader-newest)][New]] | ")
  (insert "[[elisp:(hnreader-past)][Past]] | ")
  (insert "[[elisp:(hnreader-ask)][Ask]] | ")
  (insert "[[elisp:(hnreader-show)][Show]] | ")
  (insert "[[elisp:(hnreader-jobs)][Jobs]] | ")
  (insert "[[elisp:(hnreader-best)][Best]]"))

(defun hnreader--print-frontpage-item (thing subtext)
  "Print THING dom and SUBTEXT dom."
  (let* ((url (format "https://news.ycombinator.com/item?id=%s" (dom-attr thing 'id)))
         (a-node (dom-child-by-tag (dom-by-class thing "^titleline$") 'a))
         (title-link (dom-attr a-node 'href)))
    (insert (format "\n* %s %s (%s) [%s]\n"
                    ;; rank
                    (dom-text (dom-by-class thing "^rank$"))
                    ;; title
                    (dom-text a-node)
                    ;; points
                    (dom-text (dom-by-class subtext "^score$"))
                    ;; comments
                    (dom-text (last (dom-by-tag subtext 'a)))))
    ;; (setq thanh subtext)
    ;; link
    (insert (format "%s\n[[eww:%s][view story in eww]]\n" title-link title-link))
    ;; comment link
    (insert (format "[[elisp:(hnreader-comment \"%s\")][%s]]"
                    url
                    url))))

(defun hnreader--get-morelink (dom)
  "Get more link from DOM."
  (let* ((more-link (dom-by-class dom "morelink"))
         (full-more-link (concat "https://news.ycombinator.com/"
                                 (dom-attr more-link 'href))))
    (setq hnreader--more-link full-more-link)
    (format "[[elisp:(hnreader-read-page \"%s\")][More]]"
            full-more-link)))

(defun hnreader--get-time-top-link (node)
  "Get date link in route /past from NODE."
  (let ((a-link (dom-attr (dom-by-tag node 'a) 'href))
        (text (dom-texts node)))
    (format "[[elisp:(hnreader-read-page \"https://news.ycombinator.com/%s\")][%s]]"
            a-link
            text)))

(defun hnreader--past-time-top-links (node-list)
  "Get date, month and year links from NODE-LIST."
  (if (= 3 (length node-list))
      (concat (seq-reduce (lambda (acc it)
                            (concat acc (hnreader--get-time-top-link it) " "))
                          node-list
                          "- Go back to a "))
    (concat (seq-reduce (lambda (acc it)
                          (concat acc (hnreader--get-time-top-link it) " "))
                        (seq-take node-list 3)
                        "- Go back to a ")
            (seq-reduce (lambda (acc it)
                          (concat acc (hnreader--get-time-top-link it) " "))
                        (seq-drop node-list 3)
                        "- Go forward to a "))))

(defun hnreader--get-route-top-info (dom)
  "Get top info of route like title, date of hn routes such as front, past from DOM."
  (if-let* ((hn-more (dom-by-class dom "^hnmore$")))
      (format "\n%s"
              (hnreader--past-time-top-links hn-more))
    "\n"))

(defun hnreader--print-frontpage (dom buf url)
  "Print raw DOM and URL on BUF."
  (let ((things (dom-by-class dom "^athing"))
        (subtexts (dom-by-class dom "^subtext$")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n")
      (insert "#+TITLE: " (dom-text (dom-by-tag dom 'title)) "\n")
      (hnreader--print-header)
      (insert (hnreader--get-route-top-info dom))
      (cl-mapcar #'hnreader--print-frontpage-item things subtexts)
      ;; (setq-local org-confirm-elisp-link-function nil)
      (if hnreader--history
          (insert "\n* "(format "[[elisp:(hnreader-back)][< Back]]" ) " | ")
        (insert "\n* "))
      (insert (hnreader--get-morelink dom) " | ")
      (insert (format "[[elisp:(hnreader-read-page-back \"%s\")][Reload]]" url) )
      (hnreader-mode)
      (goto-char (point-min))
      (forward-line 2))))

(defun hnreader--get-title (dom)
  "Get title and link from DOM comment page."
  ;; conventional case
  (or (let ((a-link (dom-child-by-tag (dom-by-class dom "^titleline$") 'a)))
        (if a-link
            (cons (dom-text a-link) (dom-attr a-link 'href))
          nil))
      ;; item case
      (let ((title (dom-text (dom-by-tag dom 'title)))
            (id (dom-attr (car (dom-by-class dom "athing")) 'id)))
        (if title
            (cons title
                  ;; a user profile and other pages that list no item have
                  ;; nothing to link to
                  (when id
                    (format "https://news.ycombinator.com/item?id=%s" id)))
          nil))))

(defun hnreader--get-post-info (dom)
  "Get top info about the DOM comment page."
  (let* ((fat-item (dom-by-class dom "^fatitem$"))
         (user (dom-by-class fat-item "^hnuser$"))
         (score (dom-by-class fat-item "^score$"))
         (tr-tag (dom-by-tag fat-item 'tr))
         (comment-count (last (dom-by-tag (dom-by-class fat-item "^subtext$") 'a)))
         (intro (if (= (length tr-tag) 6)
                    (nth 3 tr-tag)
                  (hnreader--get-comment tr-tag))))
    ;; (setq thanh tr-tag)
    ;; (setq thanh-fat fat-item)
    ;; conventional case
    (or (and score
             (cons
              (format "%s | by %s | %s\n"
                      (dom-text score)
                      (dom-text user)
                      (dom-text comment-count))
              intro))
        ;; item case
        (let* ((a-links (dom-by-tag (dom-by-class dom "navs") 'a))
               (parent-id (dom-attr (car a-links) 'href))
               (context-id (dom-attr (nth 1 a-links) 'href)))
          (and parent-id
               (cons
                (format "by %s | [[elisp:(hnreader-comment \"https://news.ycombinator.com/%s\")][parent]] | [[elisp:(hnreader-comment \"https://news.ycombinator.com/%s\")][context]]\n"
                        (dom-text user)
                        parent-id
                        context-id)
                intro))))))

(defun hnreader--print-node (node)
  "Print the NODE with extra options."
  (let ((shr-width 80)
        (shr-use-fonts nil))
    (shr-insert-document node)))

(defun hnreader--explain-reason (reason)
  "Say what REASON means, falling back to printing it as it came."
  (pcase reason
    (`(error http 429)
     "Hacker News is rate limiting this address (HTTP 429).
The limit is per address and covers every client on it, so a browser
on the same machine can still work. It lifts on its own; retry in a
few minutes.")
    (_ (format "%s" reason))))

(defun hnreader--print-error (buf url reason retry)
  "Report REASON for a failed load of URL in BUF, offering RETRY as a link.
The buffer otherwise sits on the \"Loading...\" placeholder forever,
so a rate limit or a malformed url looks like nothing happened."
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+STARTUP: overview indent\n")
    (insert "#+TITLE: Hacker News\n\n")
    (insert (format "Could not load %s\n\n%s\n\n" url (hnreader--explain-reason reason)))
    (insert (format "[[elisp:(%s \"%s\")][Retry]]" retry url))
    (hnreader-mode)
    (goto-char (point-min))))

(defun hnreader--print-comments (dom url)
  "Print DOM comment and URL to buffer."
  (let ((comments (dom-by-class dom "^athing comtr$"))
        (title (hnreader--get-title dom))
        (info (hnreader--get-post-info dom))
        (more-link (dom-by-class dom "morelink")))
    (with-current-buffer (hnreader--get-hn-comment-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+STARTUP: overview indent\n")
      ;; both getters below are documented to return nil, and `insert'
      ;; signals on nil rather than skipping it
      (insert "#+TITLE: " (or (car title) "Hacker News") "\n")
      (when (cdr title)
        (insert (format "%s\n[[eww:%s][view story in eww]]\n" (cdr title) (cdr title))))
      (when (car info)
        (insert (car info)))
      (when (cdr info)
        (insert "\n")
        (hnreader--print-node (cdr info)))
      (dolist (comment comments)
        ;; (setq thanh comment)
        (insert (format "%s %s\n"
                        (hnreader--get-indent
                         (hnreader--get-img-tag-width comment))
                        (hnreader--get-comment-owner comment)))
        ;; append an empty p node and let shr deal with new line consistency
        (hnreader--print-node (dom-append-child (hnreader--get-comment comment) '(p)))
        (when-let* ((reply (hnreader--get-reply comment)))
          (insert (format "[[https://news.ycombinator.com/%s][reply]]\n"
                          reply))))
      (when more-link
        (insert "\n* " (format "[[elisp:(hnreader-comment \"%s\")][More]]" (concat "https://news.ycombinator.com/"
                                                                                   (dom-attr more-link 'href)))))
      (insert "\n* " (format  "[[elisp:(hnreader-comment \"%s\")][Reload]]" url))
      (hnreader-mode)
      ;; (org-shifttab 3)
      (goto-char (point-min))
      (forward-line 2))))

(defun hnreader--get-img-tag-width (comment-dom)
  "Get width attribute of img tag in COMMENT-DOM."
  (string-to-number
   (dom-attr (dom-by-tag (dom-by-class comment-dom "^ind$") 'img)
             'width)))

(defun hnreader--get-indent (width)
  "Return headline star string from WIDTH of img tag."
  (let ((stars "\n*")) (dotimes (_ (round (/ width hnreader--indent)) stars)
                         (setq stars (concat stars "*")))))

(defun hnreader--get-comment-owner (comment-dom)
  "Return user who wrote this COMMENT-DOM."
  (dom-text (dom-by-class comment-dom "^hnuser$")))

(defun hnreader--it-to-it (it)
  "Map node to node.
IT is an element in the DOM tree. Map to different IT when it is
a, img or pre. Otherwise copy"
  (if (and (listp it)
           (listp (cdr it))) ;; check for list but not cons
      (if (and (equal (car it) 'a)
               (not (dom-by-tag it 'img))) ;; bail out if img
          ;; (dom-attr it 'href)
          `(span nil ,(dom-attr it 'href))
        (mapcar #'hnreader--it-to-it it))
    it))

(defun hnreader--get-comment (comment-dom)
  "Get comment dom from COMMENT-DOM."
  (hnreader--it-to-it (dom-by-class comment-dom "^commtext")))

(defun hnreader--get-reply (comment-dom)
  (dom-attr (dom-by-tag (dom-by-class comment-dom "^reply$") 'a) 'href))

(defun hnreader-readpage-promise (url)
  "Promise HN URL."
  (hnreader--prepare-buffer (hnreader--get-hn-buffer))
  (promise-chain (hnreader--promise-dom url)
    (then (lambda (result)
            (hnreader--print-frontpage result (hnreader--get-hn-buffer) url)))
    (promise-catch (lambda (reason)
                     (hnreader--print-error (hnreader--get-hn-buffer)
                                            url reason 'hnreader-read-page-back)))))

(defun hnreader-read-page-back (url)
  "Print HN URL page and won't change the history."
  (interactive "sLink: ")
  (hnreader-readpage-promise url)
  nil)

(defun hnreader-read-page (url)
  "Print HN URL page.
Also upate `hnreader--history'."
  (interactive "sLink: ")
  (setq hnreader--history (cons url hnreader--history))
  (hnreader-readpage-promise url)
  nil)

;;;###autoload
(defun hnreader-news()
  "Read front page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/news"))

;;;###autoload
(defun hnreader-past()
  "Read past page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/front"))

;;;###autoload
(defun hnreader-newest()
  "Read past page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/newest"))

;;;###autoload
(defun hnreader-ask ()
  "Read ask page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/ask"))

;;;###autoload
(defun hnreader-show ()
  "Read show page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/show"))

;;;###autoload
(defun hnreader-jobs ()
  "Read jobs page."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/jobs"))

;;;###autoload
(defun hnreader-best ()
  "Read page with best/top items."
  (interactive)
  (hnreader-read-page "https://news.ycombinator.com/best"))

;;;###autoload
(defun hnreader-more()
  "Load more."
  (interactive)
  (if hnreader--more-link
      (hnreader-read-page hnreader--more-link)
    (message "no more link.")))

(defun hnreader-promise-comment (url)
  "Promise to print hn URL page to buffer."
  (hnreader--prepare-buffer (hnreader--get-hn-comment-buffer))
  (promise-chain (hnreader--promise-dom url)
    (then (lambda (dom)
            (hnreader--print-comments dom url)))
    (promise-catch (lambda (reason)
                     (hnreader--print-error (hnreader--get-hn-comment-buffer)
                                            url reason 'hnreader-comment)))))

;;;###autoload
(defun hnreader-comment (url)
  "Print hn URL page to buffer."
  (interactive "sLink: ")
  (hnreader-promise-comment url)
  nil)

;;;###autoload
(defun hnreader-org-insert-hn-link (url)
  "Insert link in org buffer to open a hn item link"
  (interactive "sUrl: ")
  (with-current-buffer (current-buffer)
    (insert (format "[[elisp:(hnreader-comment \"%s\")][%s]]"
                    url
                    url))))

(provide 'hnreader)
;;; hnreader.el ends here
