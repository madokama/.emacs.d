;;; elshogi-kif --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-game)
(require 'elshogi-move)
(require 'elshogi-replay)
(require 'ffap)

;;; Kif notation parsers

(defvar elshogi-kif-file-alist
  (eval-when-compile
    (cl-loop for i from ?1 to ?9
             for k from ?１ to ?９
             collect (cons k i))))

(defvar elshogi-kif-rank-alist
  (eval-when-compile
    (cl-loop for i from ?a to ?i
             for j from ?1 to ?9
             for k across "一二三四五六七八九"
             collect (cons k i)
             and collect (cons j i))))

(defun elshogi-kif-char->file (char)
  (elshogi-char->file
   (or (assoc-default char elshogi-kif-file-alist #'char-equal)
       char)))

(defun elshogi-kif-char->rank (char)
  (elshogi-char->rank (assoc-default char elshogi-kif-rank-alist #'char-equal)))

(defun elshogi-kif-coord->index (coord)
  (elshogi-calc-index (elshogi-kif-char->file (aref coord 0))
                      (elshogi-kif-char->rank (aref coord 1))))

(defvar elshogi-kif-piece-alist
  '(("歩" :name p)
    ("香" :name l)
    ("桂" :name n)
    ("銀" :name s)
    ("金" :name g)
    ("角" :name b)
    ("飛" :name r)
    ("玉" :name k)
    ("と" :name p :promoted t)
    ("馬" :name b :promoted t)
    ("龍" :name r :promoted t)))

(defsubst elshogi-kif-assoc (k al)
  (assoc-default k al #'string-equal))

(defsubst elshogi-kif-piece (piece)
  (elshogi-kif-assoc piece elshogi-kif-piece-alist))

(defun elshogi-kif-parse-piece (piece)
  (save-match-data
    (cond ((string-match (rx bos (group word) "成") piece)
           (append (elshogi-kif-piece (match-string 1 piece)) '(:promote t)))
          ((string-match (rx bos (group word) "不成") piece)
           (elshogi-kif-piece (match-string 1 piece)))
          ((string-match (rx bos "成" (group word)) piece)
           (append (elshogi-kif-piece (match-string 1 piece)) '(:promoted t)))
          (t (elshogi-kif-piece piece)))))

(defun elshogi-kif-analyze-move (move)
  (save-match-data
   (cond ( ;; ７六歩(77)
          (string-match (rx bos
                            (group (= 2 word)) (group (1+ word))
                            "(" (group (= 2 digit)))
                        move)
          (append (elshogi-kif-parse-piece (match-string 2 move))
                  (list :origin (elshogi-kif-coord->index (match-string 3 move))
                        :target (elshogi-kif-coord->index (match-string 1 move)))))
         ( ;; 同　飛(62) /  同　銀不成(76)
          (string-match (rx bos "同" blank (group (1+ word))
                            "(" (group (= 2 digit)))
                        move)
          (append (elshogi-kif-parse-piece (match-string 1 move))
                  (list :target 'ditto
                        :origin
                        (elshogi-kif-coord->index (match-string 2 move)))))
         ( ;; ６六角打
          (string-match (rx bos (group (= 2 word)) (group word) "打") move)
          (append (elshogi-kif-piece (match-string 2 move))
                  (list :drop t
                        :target
                        (elshogi-kif-coord->index (match-string 1 move))))))))

;; (mapcar #'elshogi-kif-analyze-move
;;         '("７六歩(77)"
;;           "４七角打"
;;           "同 銀成(76)"
;;           "同 成銀(76)" "同 銀不成(76)" "５七馬(47)" "６三歩成(64)" "同　飛(62)"))
;; '((:name p :origin 56 :target 47)
;;   (:name b :drop t :target 59)
;;   (:name s :promote t :origin 47)
;;   (:name s :promoted t :origin 47)
;;   (:name s :origin 47)
;;   (:name b :promoted t :origin 59 :target 58)
;;   (:name p :promote t :origin 30 :target 21)
;;   (:name r :origin 12))



;;; Buffer utilities

(defsubst elshogi-kif-trim-line (line)
  (replace-regexp-in-string (rx (or (and bos (1+ blank))
                                    (and (1+ blank) eos)))
                            "" line))

(defsubst elshogi-kif-read-line ()
  (elshogi-kif-trim-line
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position))))

(defsubst elshogi-kif-read-next-line ()
  (goto-char (line-end-position))
  (skip-syntax-forward " ")
  (unless (eobp)
    (elshogi-kif-read-line)))

(defun elshogi-kif-parse-lines (matcher)
  (save-excursion
    (save-match-data
      (cl-loop for line = (elshogi-kif-read-next-line)
               for matched = (and line (funcall matcher line))
               while matched collect matched))))

(defun elshogi-kif-search-line (re)
  (when (re-search-forward re nil t)
    (elshogi-kif-read-line)))



(defun elshogi-kif-p ()
  (goto-char (point-min))
  (elshogi-kif-search-line (rx bol "#" blank (1+ "-") blank (or "柿木" "Kifu"))))

(defun elshogi-kif-live-p (header)
  (not (elshogi-kif-assoc "終了日時" header)))

(defun elshogi-kif-coding-system (url)
  (if (string-match-p (rx "_utf8" eow) url)
      'utf-8
    (save-excursion
      (goto-char (point-min))
      (or (when-let* (line (elshogi-kif-search-line (rx bol "#KIF")))
            (when (string-match (rx "encoding=" (group (1+ (not blank)))) line)
              (ignore-errors
                (thread-last line
                  (match-string 1) downcase intern-soft check-coding-system))))
          'shift_jis-dos))))

(declare-function mail-narrow-to-head "mail-parse")

(defun elshogi-kif-open (kif callback)
  (if (ffap-url-p kif)
      (url-retrieve
       kif
       (lambda (status)
         (message "%S" status)
         (if (plist-get status :error)
             (error "Failed to open kif: %S" status)
           (let ((buf (current-buffer)))
             (unwind-protect
                  (save-match-data
                    (save-restriction
                      (mail-narrow-to-head)
                      (delete-region (point-min) (point-max)))
                    (set-buffer-multibyte 'to)
                    (recode-region (point) (point-max)
                                   (elshogi-kif-coding-system kif) 'raw-text)
                    (funcall callback))
               (kill-buffer buf)))))
       nil t)
    (with-current-buffer (find-file-noselect kif)
      (funcall callback))))



(defvar elshogi-kif-url/jsa
  (rx bow "live.shogi.or.jp/" (1+ (not (any ?/))) "/kifu/"))
(defvar elshogi-kif-url/mainichi
  (rx bow "mainichi.jp/oshosen-kifu/"))
(defvar elshogi-kif-url/mynavi
  (rx bow "book.mynavi.jp/shogi/mynavi-open/result/"))

(declare-function url-expand-file-name "url-expand")

(defun elshogi-kif-gunzip-url (url)
  (replace-regexp-in-string (rx ".Z" eos) "" url))

(defun elshogi-kif-parse-url/mainichi (url callback)
  (url-retrieve
   url
   (lambda (status)
     (let ((buf (current-buffer)))
       (unwind-protect
            (if (plist-get status :error)
                (error "Couldn't obtain kif: %S" status)
              (goto-char (point-min))
              (when (re-search-forward
                     (rx "params.FlashVars" (0+ blank) "=" (0+ blank))
                     nil t)
                (let ((params
                       (url-parse-query-string (read (current-buffer)))))
                  (cl-flet ((expand (k)
                              (url-expand-file-name
                               (car (elshogi-kif-assoc k params))
                               "http://mainichi.jp")))
                    (funcall callback
                             (list :url url
                                   :kif (expand "kifu")
                                   :black (expand "right_image")
                                   :white (expand "left_image")))))))
         (kill-buffer buf))))))

(defun elshogi-kif-parse-url/jsa (url callback)
  (url-retrieve
   url
   (lambda (status)
     (if (plist-get status :error)
         (error "Couldn't obtain kif: %s %S" url status)
       (let ((buf (current-buffer)))
         (unwind-protect
              (let ((params
                     (progn
                       (elshogi-kif-search-line (rx bol "// ***"))
                       (elshogi-kif-parse-lines
                        (lambda (line)
                          (when (string-match (rx bol "so.addVariable("
                                                  (group (1+ (not (any ?\))))))
                                              line)
                            (split-string-and-unquote (match-string 1 line)
                                                      ", ")))))))
                (cl-flet ((expand (k)
                            (when-let* (val (elshogi-kif-assoc k params))
                              (url-expand-file-name (car val) url))))
                  (if-let* (kif (expand "kifu"))
                      (funcall callback
                               (list :url url
                                     :kif (elshogi-kif-gunzip-url kif)
                                     :black (expand "right_image")
                                     :white (expand "left_image")))
                    (error "Parse failed: %s" url))))
           (kill-buffer buf)))))))

(defun elshogi-kif-parse-url-query (url)
  (when-let* ((query
               (cdr (url-path-and-query (url-generic-parse-url url))))
              (count
               (elshogi-kif-assoc "te" (url-parse-query-string query))))
    (list :count (string-to-number (car count)))))

(defun elshogi-kif-url-p (url)
  (or (string-match-p elshogi-kif-url/jsa url)
      (string-match-p elshogi-kif-url/mainichi url)
      (string-match-p elshogi-kif-url/mynavi url)
      (string-match-p (rx ".kif" eos) url)))

(defun elshogi-kif-parse-url (url callback)
  (if (string-match-p (rx ".htm" (opt "l") (or eos "?")) url)
      (cond ((string-match-p elshogi-kif-url/mainichi url)
             (elshogi-kif-parse-url/mainichi url callback))
            (t
             (elshogi-kif-parse-url/jsa url callback)))
    (funcall callback (list :kif url))))

(defun elshogi-kif-parse (url callback)
  (let ((orig-buf (current-buffer)))
    (elshogi-kif-parse-url
     url
     (lambda (params)
       (elshogi-kif-open
        (plist-get params :kif)
        (lambda ()
          (when (elshogi-kif-p)
            (let ((game
                   (elshogi-kif-make-game (append (elshogi-kif-parse-url-query url)
                                                  params)
                                          (elshogi-kif-parse-header))))
              (with-current-buffer orig-buf
                (funcall callback game))))))))))

(defun elshogi-kif-parse-header ()
  ;; Assuming the cursor is on the initial comment line
  (elshogi-kif-parse-lines (lambda (line)
                             (when (string-match "^\\(.+?\\)：\\(.+\\)$" line)
                               (cons (match-string 1 line)
                                     (match-string 2 line))))))

(defun elshogi-kif-parse-comment ()
  (when-let* (comment
              (elshogi-kif-parse-lines
               (lambda (line)
                 (cond ((string-match-p (rx bos "*") line)
                        (substring line 1))
                       ((string-match-p (rx bos "まで") line)
                        line)))))
    (string-join comment "\n")))

(defun elshogi-kif-parse-move ()
  (when-let* (move (elshogi-kif-search-line (rx bol (1+ blank) digit)))
    (when (string-match (rx bos
                            (group (1+ digit)) (1+ blank)
                            (group (minimal-match (1+ print))) (1+ blank)
                            "(" (0+ blank)
                            (group (1+ (any digit ":"))))
                        move)
      (append (elshogi-kif-analyze-move (match-string 2 move))
              (list :count (string-to-number (match-string 1 move))
                    :time (elshogi-kif-parse-time (match-string 3 move)))))))

(defun elshogi-kif-parse-time (time)
  (cl-loop for i from 0
           for n in (nreverse (split-string time ":"))
           sum (* (string-to-number n) (expt 60 i))))

(defsubst elshogi-kif-parse-side (move)
  (if (cl-oddp (plist-get move :count))
      'b 'w))



(defun elshogi-kif-make-move (game move)
  (when-let* ((target (plist-get move :target))
              (mrec
               (if (plist-get move :drop)
                   (elshogi-move-make-drop game
                                           target
                                           (elshogi-make-piece
                                            :name (plist-get move :name)
                                            :side (elshogi-kif-parse-side move)))
                 (elshogi-move-make-move game
                                         (plist-get move :origin)
                                         (if (eq target 'ditto)
                                             (elshogi-mrec/target
                                              (elshogi-game-final-move game))
                                           target)
                                         (plist-get move :promote)))))
    (setf (elshogi-mrec/count mrec) (plist-get move :count)
          (elshogi-mrec/time mrec) (plist-get move :time)
          (elshogi-mrec/note mrec) (elshogi-kif-parse-comment))
    (elshogi-toggle-side game)))

(defun elshogi-kif-make-game (params header)
  (save-match-data
    (let* ((black (elshogi-kif-assoc "先手" header))
           (white (elshogi-kif-assoc "後手" header))
           (game
            (elshogi-game-initialize
             :position (elshogi-new-position)
             :black (elshogi-make-player :name black
                                         :side 'b
                                         :image (plist-get params :black))
             :white (elshogi-make-player :name white
                                         :side 'w
                                         :image (plist-get params :white))
             :record (elshogi-make-grec
                      :startpos 'startpos
                      :moves
                      (list->dlist
                       (list (elshogi-make-mrec
                              :note
                              (progn
                                (elshogi-kif-search-line (rx bol "手数"))
                                (elshogi-kif-parse-comment))))))
             :display (elshogi-make-display)
             :watch-p t
             :live-p (elshogi-kif-live-p header)
             :url (plist-get params :url)
             :kif (plist-get params :kif)
             :title (format "▲%s △%s | %s"
                            black white
                            (elshogi-kif-assoc "棋戦" header)))))
      (cl-loop for move = (elshogi-kif-parse-move)
               while move
               do (elshogi-kif-make-move game move))
      (elshogi-replay-seek game (plist-get params :count)))))

(provide 'elshogi-kif)

;;; elshogi-kif.el ends here
