;;; page2feed-shogi --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'url-parse)
(require 'dom)
(require 'json)

(defun page2feed-shogi-json-game (title date game)
  (let-alist game
    (let* ((body (aref .gamebodies 0))
           (url (alist-get 'url body)))
      (list 'title (format "%s%s%s―%s%s %s%s%s"
                           (if-let* ((status (alist-get 'status body)))
                               (format "*%s " status)
                             "")
                           (alist-get 'name .player1)
                           (alist-get 'grade .player1)
                           (alist-get 'name .player2)
                           (alist-get 'grade .player2)
                           .kaiki
                           title
                           (alist-get 'name .division))
            'link url
            'updated (format "%s %s" date (alist-get 'starttime .division))
            'content (format "<a href=\"%s\">棋譜を見る</a>" url)))))

(defun page2feed-shogi-json-games/date (title games)
  (let-alist games
    (mapcar (apply-partially #'page2feed-shogi-json-game title .matchdate)
            .gameheads)))

(defun page2feed-shogi-json-games (title json)
  (mapcan (apply-partially #'page2feed-shogi-json-games/date title)
          (alist-get 'games json)))

(defun page2feed-shogi-dom-find (dom stop)
  (when-let* ((tag (dom-tag dom)))
    (cond ((eq tag stop) dom)
          ((stringp tag)
           (page2feed-shogi-dom-find (cdr dom) stop))
          (t
           (page2feed-shogi-dom-find (dom-children dom) stop)))))

(declare-function url-curl-sync "url-curl")

(defun page2feed-shogi-games/mynavi (plst)
  (url-curl-sync (plist-get plst :url)
                 (lambda ()
                   (when (re-search-forward
                          (rx "<div class=\"link_area" (+? anything) "</div>")
                          nil t)
                     (page2feed-shogi-dom-find
                      (libxml-parse-html-region (match-beginning 0)
                                                (match-end 0))
                      'ul)))))

(defun page2feed-shogi-games (plst)
  (let* ((url (plist-get plst :url))
         (urlobj (url-generic-parse-url url))
         (host (url-host urlobj)))
    (cond ((string= host "live.shogi.or.jp")
           (if (string= (car (url-path-and-query urlobj)) "/denou/")
               (progn
                 (message "[p2f]Not supported yet: %s" url)
                 nil)
             (page2feed-shogi-json-games
              ;; Strip the match number, which we obtain from JSON later.
              (replace-regexp-in-string (rx (1+ blank) "第" (1+ anything) "局" eos)
                                        ""
                                        (plist-get plst :match))
              (url-curl-sync (concat url "index.json") #'json-read))))
          ((string= host "mynavi-open.jp")
           ;; (page2feed-shogi-games/mynavi plst)
           nil)
          (t
           (message "[p2f]Not supported yet: %s" url)
           nil))))

(defun page2feed-shogi-extract (dom)
  (cl-flet* ((kifp (dom)
               (when-let* (tag-a
                           (dom-child-by-tag (last (dom-children dom))
                                             'a))
                 (string= (car (dom-strings tag-a))
                          "中継")))
             (str (node)
               (car (dom-strings node)))
             (plistify (dom)
               (seq-let (match p1 p2 url)
                   (cl-reduce (lambda (node acc)
                                (if-let* (tag-a
                                          (and (consp node)
                                               (dom-child-by-tag node 'a)))
                                    (cons tag-a acc)
                                  acc))
                              (dom-children dom)
                              :initial-value nil :from-end t)
                 (list :match (str match)
                       :player1 (str p1)
                       :player2 (str p2)
                       :url (dom-attr url 'href)))))
    (mapcar #'plistify
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
                                              (match-end 0))))))
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
