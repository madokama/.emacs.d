;;; elshogi-sfen --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'dash)
(require 'subr-x)
(require 'elshogi-core)

;; Since s-match-strings-all of s.el is not what I want here
(defun elshogi-string-match-all (re str &optional icase)
  (save-match-data
    (let ((case-fold-search icase)
          (matches nil)
          (start 0))
      (while (string-match re str start)
        (push (match-string 0 str) matches)
        (setq start (match-end 0)))
      (nreverse matches))))

(defun elshogi-sfen-parse-piece-on-square (sfen callback)
  (declare (indent 1))
  (when (string-match "^\\(\\+?\\)\\([[:alpha:]]\\)$" sfen)
    (let ((promoted (match-string 1 sfen))
          (name (match-string 2 sfen)))
      (funcall callback
               (intern (downcase name))
               (not (string-empty-p promoted))
               (if (let ((case-fold-search nil))
                     (string-match-p "[[:upper:]]" name))
                   elshogi-black
                 elshogi-white)))))

(defun elshogi-sfen-piece-on-square (sfen)
  (if (string-match-p "[1-9]" sfen)
      (make-list (string-to-number sfen) nil)
    (elshogi-sfen-parse-piece-on-square sfen
      (lambda (name promoted side)
        (list
         (elshogi-make-piece :name name
                             :promoted promoted
                             :side side))))))

(defun elshogi-sfen-pieces-on-rank (rank)
  (apply #'vector
         (cl-mapcan #'elshogi-sfen-piece-on-square
                    (elshogi-string-match-all "\\(?:[1-9]\\|\\+?[a-z]\\)"
                                              rank t))))

(defun elshogi-sfen-pieces-on-board (sfen)
  (apply #'vconcat
         (mapcar #'elshogi-sfen-pieces-on-rank
                 (split-string sfen "/"))))

(defun elshogi-sfen-parse-piece-on-stand (sfen)
  (when (string-match "\\([0-9]*\\)\\(.\\)" sfen)
    (cons (max 1 (string-to-number (match-string 1 sfen)))
          (intern (downcase (match-string 2 sfen))))))

(defun elshogi-sfen-pieces-on-stand (sfen side)
  (unless (string= sfen "-")
    (let ((pieces
           (mapcar #'elshogi-sfen-parse-piece-on-stand
                   (elshogi-string-match-all (format "[1-9]*[[:%s:]]"
                                                     (if (elshogi-black-p side)
                                                         "upper"
                                                       "lower"))
                                             sfen))))
      (cl-mapcan (lambda (piece)
                   (make-list (car piece)
                              (elshogi-make-piece :name (cdr piece)
                                                  :side side)))
                 pieces))))

(defun elshogi-sfen->position (sfen)
  (if (eq sfen 'startpos)
      (elshogi-new-position)
    (seq-let (board side stand count)
        (cdr (split-string sfen " "))
      (elshogi-make-position
       :on-board (elshogi-sfen-pieces-on-board board)
       :on-black-stand (elshogi-sfen-pieces-on-stand stand elshogi-black)
       :on-white-stand (elshogi-sfen-pieces-on-stand stand elshogi-white)
       :side (intern side)
       :count (string-to-number count)))))

(defun elshogi-rank->sfen (rank)
  (mapconcat (lambda (pieces)
               (if (null (car pieces))
                   (number-to-string (length pieces))
                 (mapconcat #'elshogi-piece-text pieces "")))
             (-partition-by #'null (append rank nil))
             ""))

(defun elshogi-pieces-on-stand->sfen (pieces)
  (mapconcat (lambda (group)
               (let ((num (length (cdr group))))
                 (format "%s%s"
                         (if (> num 1) num "")
                         (elshogi-piece-text (cadr group)))))
             (seq-group-by #'elshogi-piece/name pieces)
             ""))

(defun elshogi-pieces-on-stands->sfen (position)
  (let ((black (elshogi-position/on-black-stand position))
        (white (elshogi-position/on-white-stand position)))
    (if (or black white)
        (format "%s%s"
                (elshogi-pieces-on-stand->sfen black)
                (elshogi-pieces-on-stand->sfen white))
      "-")))

(defun elshogi-position->sfen (position)
  (format "sfen %s %s %s %d"
          (string-join
           (mapcar #'elshogi-rank->sfen
                   (seq-partition (elshogi-position/on-board position) 9))
           "/")
          (elshogi-position/side position)
          (elshogi-pieces-on-stands->sfen position)
          (elshogi-position/count position)))

(defun elshogi-mrec->sfen (mrec)
  (if (elshogi-mrec-drop-p mrec)
      (elshogi-drop->sfen (elshogi-mrec/target mrec)
                          (elshogi-mrec/piece mrec))
    (elshogi-move->sfen (elshogi-mrec/origin mrec)
                        (elshogi-mrec/target mrec)
                        (elshogi-mrec/promote mrec))))

(defun elshogi-drop->sfen (index piece)
  (format "%s*%s"
          (upcase (symbol-name (elshogi-piece/name piece)))
          (elshogi-index->coord index)))

(defun elshogi-move->sfen (orig target promote)
  (format "%s%s%s"
          (elshogi-index->coord orig)
          (elshogi-index->coord target)
          (if promote "+" "")))

(provide 'elshogi-sfen)
;;; elshogi-sfen.el ends here
