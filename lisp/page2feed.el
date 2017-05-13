;;; page2feed --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; RIP to page2rss.com

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'shr)
(require 'url-expand)

(defvar page2feed-scrapers nil)

(defun page2feed-string-match-all (re str)
  (save-match-data
    (let ((matches nil)
          (start 0))
      (while (string-match re str start)
        (push (page2feed-string-match-subgroups (match-data) str)
              matches)
        (setq start (match-end 0)))
      (nreverse matches))))

(defun page2feed-string-match-subgroups (data str)
  (let (matches)
    ;; Extract matched subgroups
    (when (> (length data) 2)
      (setq data (cddr data)))
    (while data
      (push (substring-no-properties str (car data) (cadr data))
            matches)
      (setq data (cddr data)))
    (nreverse matches)))

(defmacro page2feed-test (url &rest body)
  `(elfeed-with-fetch ,url
       (set-buffer-multibyte 'to)
     (page2feed-recode-buffer (point-min) (point-max))
     (goto-char (point-min))
     ,@body))

(defun page2feed-recode-buffer (beg end)
  (when (search-forward-regexp "<meta[^>]*?charset=\"?\\([-_[:alnum:]]+\\)" nil t)
    (when-let* (cs
                (ignore-errors
                  (check-coding-system (intern-soft (downcase (match-string 1))))))
      (recode-region beg end cs 'raw-text))))

(defun page2feed (r)
  (save-excursion
    (save-match-data
      (let ((beg (car r))
            (end (cadr r))
            (case-fold-search t))
        (when (search-forward "<html" nil t)
          (page2feed-recode-buffer beg end)
          (goto-char beg)
          (cl-dolist (s page2feed-scrapers)
            (when-let* ((parsed (save-excursion (funcall s)))
                        (title (page2feed-title)))
              ;; Region could change after reencode.
              (delete-region beg (point-max))
              (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
              (shr-dom-print
               (page2feed-sxmlize (if (plist-get parsed 'title)
                                      parsed
                                    (plist-put parsed 'title title))))
              (setq r (cl-list* beg (point-max) (cddr r)))
              (cl-return)))))))
  r)

(defun page2feed-title ()
  (when (search-forward-regexp "<title>\\(.+?\\)</title>" nil t)
    (match-string 1)))

(defun aggressive-encode-time (time)
  (let ((parts (seq-partition time 6)))
    (apply #'encode-time
           (append (cl-nsubstitute 0 nil (car parts))
                   (cadr parts)))))

;; <feed xmlns="http://www.w3.org/2005/Atom">
;;   <title>Example Feed</title>
;;   <link href="http://example.org/"/>
;;   <updated>2003-12-13T18:30:02Z</updated>
;;   <author>
;;     <name>John Doe</name>
;;   </author>
;;   <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
;;   <entry>
;;     <title>Atom-Powered Robots Run Amok</title>
;;     <link href="http://example.org/2003/12/13/atom03"/>
;;     <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
;;     <updated>2003-12-13T18:30:02Z</updated>
;;     <summary>Some text.</summary>
;;   </entry>
;; </feed>

(defun page2feed-entries (parsed)
  (seq-sort-by (lambda (entry)
                 (plist-get entry :updated))
               (lambda (a b)
                 (time-less-p b a))     ;oldest last
               (mapcar (lambda (entry)
                         (let ((time
                                (plist-get entry 'updated)))
                           (when (stringp time)
                             (setq time
                                   (aggressive-encode-time
                                    (parse-time-string time))))
                           (append
                            (plist-put entry
                                       'updated
                                       (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                           time))
                            (list :updated time))))
                       (plist-get parsed 'entries))))

(defun page2feed-sxmlize (parsed)
  (let ((entries (page2feed-entries parsed)))
    `(feed ((xmlns . "http://www.w3.org/2005/Atom"))
           (title nil ,(plist-get parsed 'title))
           (link ((href . ,(plist-get parsed 'link))))
           (updated nil ,(plist-get (car entries) 'updated))
           (author nil (name nil ,(plist-get parsed 'author)))
           (id nil ,(plist-get parsed 'link))
           ,@(mapcar (lambda (entry)
                       (let ((elink
                              (url-expand-file-name (plist-get entry 'link)
                                                    (plist-get parsed 'link))))
                         `(entry nil
                                 (title nil ,(plist-get entry 'title))
                                 (link ((href . ,elink)
                                        ,@(when-let* (type
                                                      (plist-get entry 'enclosure))
                                            `((rel . "enclosure")
                                              (type . ,type)))))
                                 (id nil ,elink)
                                 (updated nil ,(plist-get entry 'updated))
                                 ,@(when-let* (summary (plist-get entry 'summary))
                                     `((summary nil ,summary)))
                                 ,@(when-let* (content (plist-get entry 'content))
                                     `((content ((type . "xhtml")) ,content))))))
                     entries))))

(defun page2feed-match-date/paoweb (str)
  (when (string-match "\\([[:alpha:]]\\{3\\}\\)[[:alpha:].]*\\s +\\([[:digit:]]+\\)[, ]+\\([[:digit:]]+\\)" str)
    (string-join (mapcar (lambda (n)
                           (match-string n str))
                         (number-sequence 1 3))
                 " ")))

(defun page2feed-scrape-paoweb ()
  (when (search-forward-regexp "\\<snupdate\\>" nil t)
    (let ((url "http://paoweb.com/updates.htm")
          (entries nil))
      (while (search-forward-regexp
              "<a href[^\"]+\"\\(sn[0-9]+\\..+?\\)\">\\(.+?\\)</a>"
              nil t)
        (push (list 'link (match-string 1)
                    'title (match-string 2)
                    'updated (page2feed-match-date/paoweb (match-string 2)))
              entries))
      (list 'link url
            'author "Sheldan Nidle"
            'entries entries))))

(add-hook 'page2feed-scrapers #'page2feed-scrape-paoweb)

(defun page2feed-scrape-loverin-month (table)
  (let ((month (and (string-match "<th>\\(.+?\\)</th>" table)
                    (match-string-no-properties 1 table)))
        (entries nil))
    (with-temp-buffer
      (insert table)
      (goto-char (point-min))
      (while (search-forward-regexp "<a href=\"\\(.+?\\)\">\\(.+?\\)</a>" nil t)
        (let ((path (match-string-no-properties 1))
              (date (concat month (match-string-no-properties 2))))
          (push (list 'link path
                      'title date
                      'updated (apply #'format "%d-%02d-%02d 24:30"
                                      (mapcar #'string-to-number
                                              (split-string date
                                                            "[^[:digit:]]" t))))
                entries))))
    entries))

(defun page2feed-scrape-loverin ()
  (when (and (search-forward "<title>CBCラジオ" nil t)
             (search-forward "backnumber-box" nil t))
    (let ((url "http://hicbc.com/radio/marian-loverin-desu/backnumber/"))
      (let (entries)
        (while (search-forward-regexp "<table \\(?:.*\n?\\)*?.*</table>" nil t)
          (setq entries
                (append entries
                        (page2feed-scrape-loverin-month (match-string 0)))))
        (list 'link url
              'author "CBC Radio"
              'entries entries)))))

(add-hook 'page2feed-scrapers #'page2feed-scrape-loverin)

;; (defun page2feed-scrape--helloup ())

(defun page2feed-scrape-morcoff-link (str base)
  (when (string-match "<a href=\"\\(.+?\\)\"" str)
    (with-temp-buffer
      (call-process "curl" nil t nil "-s" "-I" "-L"
                    (url-expand-file-name (match-string 1 str) base))
      `((type . ,(when (search-backward-regexp "^Content-Type: \\(.+\\)$" nil t)
                   (match-string 1)))
        (link . ,(when (search-backward-regexp "^Location: \\(.+\\)$" nil t)
                   (match-string 1)))))))

(defun page2feed-encode-time (re keys str)
  ;; (encode-time sec min hour day mon year &optional tz)
  (let ((time
         (cl-mapcar (lambda (num key)
                      (cons key (string-to-number num)))
                    (page2feed-string-match-subgroups
                     (and (string-match re str) (match-data))
                     str)
                    keys)))
    (apply #'encode-time
           (mapcar (lambda (key)
                     (or (cdr (assq key time)) 0))
                   '(sec min hour day mon year)))))

(defvar page2feed-scrape-morcoff-filter)

(defun page2feed-scrape-morcoff-entry (str base)
  (seq-let (_ link title size date orig)
      (mapcar #'car
              (page2feed-string-match-all "<td[^>]*>\\(.*?\\)</td>" str))
    (when (and title
               (or (not (bound-and-true-p page2feed-scrape-morcoff-filter))
                   (string-match-p page2feed-scrape-morcoff-filter title)))
      (let ((parsed (page2feed-scrape-morcoff-link link base)))
        (list 'title title
              'link (cdr (assq 'link parsed))
              'enclosure (cdr (assq 'type parsed))
              'updated (page2feed-encode-time
                        "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)(.+?)\\([0-9]+\\):\\([0-9]+\\)"
                        '(year mon day hour min)
                        date)
              'summary (format "%s [%s]" orig size
                               ;; (cons (and (string-match "<tr><!--\\(.+?\\)-->" str)
                               ;;            (match-string 1 str)))
                               ))))))

(defun page2feed-scrape-morcoff ()
  (when (search-forward "<title>もおこー（ラジオ）" nil t)
    (let ((url "http://www.morcoff.uu.dnsdojo.net/morcoff/ldr2/"))
      (let (entries)
        (while (search-forward-regexp "^<tr>.*?</tr>" nil t)
          (push (page2feed-scrape-morcoff-entry (match-string 0) url)
                entries))
        (list 'link url
              'entries (delq nil entries))))))

(add-hook 'page2feed-scrapers #'page2feed-scrape-morcoff)

;; (with-current-buffer (get-buffer "radio.html")
;;   (save-excursion
;;     (goto-char (point-min))
;;     (page2feed-sxmlize (page2feed-scrape-morcoff))))

(defun page2feed-scrape-linelive-entry (entry)
  (let-alist entry
    (list 'title .title
          'link .shareURL
          'updated .createdAt
          'content (format "<a href=%S><img src=%S /></a>"
                           .shareURL
                           (cdr (assq 'swipe .thumbnailURLs))))))

(declare-function json-read-from-string "json")

(defun page2feed-scrape-linelive ()
  (when (and (re-search-forward "<meta .+?og:site_name.+?content=\"LINE LIVE\"" nil t)
             (search-forward "data-channel=" nil t))
    (require 'json)
    (let-alist (json-read-from-string
                (replace-regexp-in-string "&quot;" "\"" (read (current-buffer))))
      (list 'link .shareURL
            'author (and (string-match "\?id=\\(.+\\)$" .lineScheme)
                         (match-string 1 .lineScheme))
            'entries
            (thread-last .archivedBroadcasts
              (assq 'rows)
              cdr
              (mapcar #'page2feed-scrape-linelive-entry))))))

(add-hook 'page2feed-scrapers #'page2feed-scrape-linelive)

(advice-add 'elfeed-xml-parse-region :filter-args #'page2feed)

(provide 'page2feed)
;;; page2feed.el ends here
