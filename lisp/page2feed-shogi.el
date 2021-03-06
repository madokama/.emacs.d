;;; page2feed-shogi --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'url-parse)
(require 'dom)
(require 'seq)

(defun page2feed-shogi-watch-link (url)
  (format "<a href=\"%s\">棋譜を見る</a>" url))

(defun page2feed-shogi-json-game (title date game)
  (let-hash game
    (let* ((body (aref .gamebodies 0))
           (url (gethash "url" body)))
      (list 'title (format "%s%s%s―%s%s %s%s%s"
                           (if-let ((status (gethash "status" body)))
                               (format "*%s " status)
                             "")
                           .player1.name
                           .player1.grade
                           .player2.name
                           .player2.grade
                           .kaiki
                           title
                           .division.name)
            'link url
            'updated (format "%s %s" date .division.starttime)
            'content (page2feed-shogi-watch-link url)))))

(defun page2feed-shogi-json-games/date (title games)
  (let-hash games
    (mapcar (apply-partially #'page2feed-shogi-json-game title .matchdate)
            .gameheads)))

(defun page2feed-shogi-json-games (title json)
  (mapcan (apply-partially #'page2feed-shogi-json-games/date title)
          (gethash "games" json)))

(defun page2feed-shogi-dom-find (dom stop)
  (when-let ((tag (dom-tag dom)))
    (cond ((eq tag stop) dom)
          ((stringp tag)
           (page2feed-shogi-dom-find (cdr dom) stop))
          (t
           (page2feed-shogi-dom-find (dom-children dom) stop)))))

(defun page2feed-shogi-kif-date (prefix url)
  (when (string-match
         (format
          "/%s\\([[:digit:]]\\{4\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)"
          prefix)
         url)
    (mapcar #'string-to-number
            (list (match-string 3 url)
                  (match-string 2 url)
                  (match-string 1 url)))))

(defun page2feed-shogi-match-title (plst)
  (format "%s-%s %s"
          (plist-get plst :player1)
          (plist-get plst :player2)
          (plist-get plst :match)))

(declare-function url-curl-sync "url-curl")

;; Mynavi

(defun page2feed-shogi-games/mynavi (plst)
  (save-match-data
    (url-curl-sync
     (plist-get plst :url)
     (lambda ()
       (when (re-search-forward
              (rx "<div class=\"link_area" (+? anything) "</div>")
              nil t)
         (let* ((dom
                 (page2feed-shogi-dom-find
                  (libxml-parse-html-region (match-beginning 0)
                                            (match-end 0)
                                            nil)
                  'ul))
                (kif (thread-first dom
                       (dom-by-tag 'a)
                       car
                       (dom-attr 'href)))
                (time (apply #'encode-time
                             0 0 10
                             (page2feed-shogi-kif-date "mynavi" kif))))
           (list
            (list 'title (page2feed-shogi-match-title plst)
                  'link kif
                  'updated time
                  'content (page2feed-shogi-watch-link kif)))))))))

;; JT Cup

(require 'url-expand)

(defun page2feed-shogi-jt-kif (dom)
  (thread-first dom
    (dom-by-tag 'a)
    car
    (dom-attr 'href)))

(defun page2feed-shogi-jt-title (dom)
  (concat (string-join (mapcar (lambda (dom)
                                 (string-join (dom-strings dom)))
                               (dom-by-class dom "relayname"))
                       "-")
          " 将棋日本シリーズ"
          (thread-first (dom-by-tag dom 'dt)
            dom-strings
            string-join)))

(defun page2feed-shogi-jt-time (dom)
  (let ((date
         (thread-first (dom-by-class dom "relaydate")
           dom-strings
           car)))
    (when (string-match (rx (group (= 2 digit))
                            ":"
                            (group (= 2 digit)))
                        date)
      (mapcar #'string-to-number
              (list (match-string 2 date)
                    (match-string 1 date))))))

(defun page2feed-shogi-jt-dom (url)
  (let ((redir
         (url-curl-sync
          url
          (lambda ()
            (seq-some (lambda (meta)
                        (when-let ((attr (dom-attr meta 'http-equiv))
                                   (content (dom-attr meta 'content)))
                          (when (string-match "url=\\(.+\\)$" content)
                            (url-expand-file-name (match-string 1 content) url))))
                      (dom-by-tag (libxml-parse-html-region (point-min) (point-max) nil)
                                  'meta))))))
    (url-curl-sync
     redir
     (lambda ()
       (thread-first (libxml-parse-html-region (point-min) (point-max) nil)
         (dom-by-id "jtweb-main-content")
         (dom-by-class (rx bos "pcBlock" eos))
         (dom-by-class (rx bos "relay" eos))
         car)))))

(defun page2feed-shogi-games/jt (plst)
  (save-match-data
    (let ((dom
           (page2feed-shogi-jt-dom
            (url-expand-file-name "../professional/live/index.html"
                                  (plist-get plst :url)))))
      ;; Time may be nil after the match.
      (when-let ((time (page2feed-shogi-jt-time dom))
                 (kif (page2feed-shogi-jt-kif dom)))
        (list
         (list 'title (page2feed-shogi-jt-title dom)
               'link kif
               'updated
               (apply #'encode-time
                      0
                      (append time
                              (page2feed-shogi-kif-date "jt" kif)))
               'content (page2feed-shogi-watch-link kif)))))))

(defun page2feed-shogi-games/mainichi (plst)
  (when-let ((num
              (and (string-match (rx (group digit)) (plist-get plst :match))
                   (string-to-number (match-string 1 (plist-get plst :match))))))
    (url-curl-sync
     (plist-get plst :url)
     (lambda ()
       (when-let* ((dom
                    (nth (1- num)
                         (thread-first (libxml-parse-html-region
                                        (point-min) (point-max)
                                        (plist-get plst :url))
                           (dom-by-class "table-typeA")
                           car
                           (dom-by-tag 'tbody)
                           (dom-by-tag 'tr))))
                   (kif (url-expand-file-name (dom-attr (dom-by-tag dom 'a) 'href)
                                              (plist-get plst :url)))
                   (date (and (string-match (rx "/"
                                                (group (= 2 digit))
                                                (group (= 2 digit))
                                                (group (= 2 digit)))
                                            kif)
                              (mapcar #'string-to-number
                                      (list (match-string 3 kif)
                                            (match-string 2 kif)
                                            (concat "20" (match-string 1 kif)))))))
         (list
          (list 'title (page2feed-shogi-match-title plst)
                'link kif
                'updated (apply #'encode-time 0 0 9 date)
                'content (page2feed-shogi-watch-link kif))))))))

;; 
(defun page2feed-shogi-games (plst)
  (let* ((url (plist-get plst :url))
         (urlobj (url-generic-parse-url url))
         (host (url-host urlobj)))
    (pcase host
      ("live.shogi.or.jp"
       (page2feed-shogi-json-games
        ;; Strip the match number, which we obtain from JSON later.
        (replace-regexp-in-string (rx (1+ blank) "第" (1+ anything) "局" eos)
                                  ""
                                  (plist-get plst :match))
        (url-curl-sync (concat url "index.json") #'json-parse-buffer)))
      ("mynavi-open.jp"
       (page2feed-shogi-games/mynavi plst))
      ("www.jti.co.jp"
       (page2feed-shogi-games/jt plst))
      ("mainichi.jp"
       (page2feed-shogi-games/mainichi plst))
      (_
       (message "[p2f]Not supported yet: %S" plst)
       nil))))

(defun page2feed-shogi-parse-past (dom)
  (seq-let (match _ p1 p2 _ url) (dom-children dom)
    (list match p1 p2 url)))

(defun page2feed-shogi-parse-future (dom)
  (seq-let (match p1 p2 _ _ url) (dom-children dom)
    (list match p1 p2 url)))

(defun page2feed-shogi--parser (dom)
  (if (seq-find (lambda (node)
                  (string= (car (dom-strings node))
                           "先手"))
                (dom-by-tag (dom-child-by-tag (page2feed-shogi-dom-find dom 'table)
                                              'thead)
                            'th))
      #'page2feed-shogi-parse-past
    #'page2feed-shogi-parse-future))

(defun page2feed-shogi-extract (dom)
  (cl-flet* ((find-kif (dom)
               (seq-find (lambda (node)
                           (and (consp node)
                                (string-match-p
                                 (rx bos (or "中継" "毎日新聞ニュースサイト"))
                                 (car (dom-strings node)))))
                         dom))
             (kifp (dom)
               (find-kif (dom-children (last (dom-children dom)))))
             (str (node)
               (car (dom-strings node)))
             (plistify (parser dom)
               (seq-let (match p1 p2 url)
                   (funcall parser dom)
                 (list :match (str match)
                       :player1 (str p1)
                       :player2 (str p2)
                       :url (dom-attr (find-kif url) 'href)))))
    (mapcar (apply-partially #'plistify (page2feed-shogi--parser dom))
            (cl-delete-if-not #'kifp
                              (dom-children
                               (page2feed-shogi-dom-find dom 'table))))))

(defun page2feed-shogi-author ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "<p class=\"logo\"><a.+?>\\(.+?\\)</a>" nil t)
      (match-string 1))))

;;;###autoload
(defun page2feed-shogi-scrape ()
  ;; "https://www.shogi.or.jp/game/"
  (when (re-search-forward
         "<link rel=\"alternate\".+?href=\"\\(.+?shogi\\.or\\.jp/.+?\\)\""
         nil t)
    (let ((link (match-string 1))
          (sched
           (cl-loop
             for m = (re-search-forward (rx "<table" (+? anything) "</table>")
                                        nil t)
             while m
             nconc (page2feed-shogi-extract
                    (libxml-parse-html-region (match-beginning 0)
                                              (match-end 0)
                                              nil)))))
      (list 'link link
            'author (page2feed-shogi-author)
            'entries (mapcan #'page2feed-shogi-games
                             (cl-delete-duplicates
                              sched
                              :test (lambda (a b)
                                      (string= (plist-get a :url)
                                               (plist-get b :url)))))))))

;;;###autoload(add-hook 'page2feed-scrapers #'page2feed-shogi-scrape)

(provide 'page2feed-shogi)
;;; page2feed-shogi.el ends here
