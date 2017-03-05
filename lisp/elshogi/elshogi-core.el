;;; elshogi-core --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'elshogi-struct)
(require 'dlist)

;; http://ja.wikipedia.org/wiki/%E5%B0%86%E6%A3%8B#.E9.A7.92.E3.81.AE.E4.BE.A1.E5.80.A4
(defconst elshogi-piece-values
  `((k . ,most-positive-fixnum)
    (r . 10)
    (b . 8)
    (g . 6)
    (s . 5)
    (n . 4)
    (l . 3)
    (p . 1)))

(defvar-local elshogi-current-game nil)
;; (defvar-local elshogi-game-player nil)
(defvar-local elshogi-game-engine nil)

(defun elshogi-make-piece-or-space (name side)
  (when name
    (elshogi-make-piece :name name :side side)))

(defmacro elshogi-make-piece-set (side &rest pieces)
  (declare (indent 1))
  `(vector
    ,@(cl-mapcan (lambda (piece)
                   (if (consp piece)
                       (make-list (car piece)
                                  `(elshogi-make-piece-or-space ',(cadr piece)
                                                                ',side))
                     (list
                      `(elshogi-make-piece-or-space ',piece ',side))))
                 pieces)))

(defun elshogi-new-position (&optional handicap)
  (elshogi-make-position :on-board
                         (vconcat (elshogi-make-piece-set w
                                    l n s g k g s n l
                                    nil r (5 nil) b nil
                                    (9 p))
                                  (make-vector (* 3 9) nil)
                                  (elshogi-make-piece-set b
                                    (9 p)
                                    nil b (5 nil) r nil
                                    l n s g k g s n l))))

;; Defining as macros so as to be `seetf'able.
(defmacro elshogi-current-position (game)
  `(elshogi-game/position ,game))

(defmacro elshogi-current-side (game)
  `(elshogi-position/side (elshogi-current-position ,game)))

(defsubst elshogi-char->file (char)
  (- 8 (- char ?1)))

(defsubst elshogi-char->rank (char)
  (- char ?a))

(defsubst elshogi-calc-index (file rank)
  (+ (* 9 rank) file))

(defun elshogi-coord->index (coord)
  ;; `coord' is a string like "7f"
  (elshogi-calc-index (elshogi-char->file (aref coord 0))
                      (elshogi-char->rank (aref coord 1))))

(defsubst elshogi-valid-file-p (file)
  (<= 0 file 8))

(defalias 'elshogi-valid-rank-p 'elshogi-valid-file-p)

(defsubst elshogi-valid-index-p (index)
  (<= 0 index 80))

(defsubst elshogi-index-file (index)
  (mod index 9))

(defsubst elshogi-index-rank (index)
  ;; Zero-based
  (/ index 9))

(defsubst elshogi-file->char (file)
  (+ ?1 (- 8 file)))

(defsubst elshogi-rank->char (rank)
  (+ rank ?a))

(defun elshogi-index->coord (index)
  (string (elshogi-file->char (elshogi-index-file index))
          (elshogi-rank->char (elshogi-index-rank index))))

(defsubst elshogi-current-position-on-board (game)
  (elshogi-position/on-board (elshogi-current-position game)))

(defsubst elshogi-current-position-on-display ()
  (elshogi-display/board (elshogi-game/display elshogi-current-game)))

(defun elshogi-index->point (index)
  (aref (elshogi-current-position-on-display) index))

(defun elshogi-piece-at (game index)
  (aref (elshogi-current-position-on-board game) index))

(defsubst elshogi-negate-side (side)
  (if (eq side elshogi-black) elshogi-white elshogi-black))

(defun elshogi-toggle-side (game)
  (setf (elshogi-current-side game)
        (elshogi-negate-side (elshogi-current-side game))))

(defsubst elshogi-players-side (game)
  (elshogi-display/pov (elshogi-game/display game)))

(defsubst elshogi-players-side-p (game side)
  (and (not (elshogi-game/watch-p game))
       (eq side (elshogi-players-side game))))

(defsubst elshogi-players-turn-p (game)
  (elshogi-players-side-p game (elshogi-current-side game)))

(defsubst elshogi-black-p (side)
  (eq side elshogi-black))

(defun elshogi-pov-coord (pov)
  (if (elshogi-black-p pov)
      #'identity
    (apply-partially #'- 8)))

(defsubst elshogi-piece-of-side-p (piece side)
  (and (elshogi-piece-p piece)
       (eq (elshogi-piece/side piece) side)))

(defun elshogi-pieces-same-side-p (piece x)
  (elshogi-piece-of-side-p x (elshogi-piece/side piece)))

(defun elshogi-flip-piece (piece)
  (elshogi-make-piece :name (elshogi-piece/name piece)
                      :side (elshogi-piece/side piece)
                      :promoted (not (elshogi-piece/promoted piece))))

(defmacro elshogi-pieces-on-stand (game side)
  `(let ((position (elshogi-current-position ,game)))
     (if (eq ,side elshogi-black)
         (elshogi-position/on-black-stand position)
       (elshogi-position/on-white-stand position))))

(defun elshogi-piece-black-p (piece)
  (elshogi-piece-of-side-p piece elshogi-black))

(defun elshogi-collect-indices (pred)
  (cl-loop for index below 81
           when (funcall pred index)
             collect index))

(defun elshogi-find-index (pred)
  (cl-loop for index below 81
           when (funcall pred index)
             return index))

(defun elshogi-indices-on-board (game side)
  (elshogi-collect-indices (lambda (index)
                             (elshogi-piece-of-side-p
                              (elshogi-piece-at game index)
                              side))))

(defun elshogi-piece-index (game index)
  (append (list :game game)
          (if (elshogi-piece-p index)
              (list :piece index)
            (list :index index))))

(defmacro elshogi-define-plist-getter (&rest syms)
  `(progn
     ,@(mapcar (lambda (sym)
                 `(defsubst ,(intern (format "elshogi-plst:%s" sym)) (plst)
                    (plist-get plst ,(intern (format ":%s" sym)))))
                syms)))

(elshogi-define-plist-getter game index piece)

(defsubst elshogi-piece= (p1 p2)
  (and (eq (elshogi-piece/side p1) (elshogi-piece/side p2))
       (eq (elshogi-piece/name p1) (elshogi-piece/name p2))))

(defun elshogi-plst= (p1 p2)
  (if-let* ((p1 (elshogi-plst:piece p1))
            (p2 (elshogi-plst:piece p2)))
      (elshogi-piece= p1 p2)
    (eq (elshogi-plst:index p1) (elshogi-plst:index p2))))

(defun elshogi-piece-text (piece)
  (format "%s%s"
          (if (elshogi-piece/promoted piece)
              "+" "")
          (funcall (if (elshogi-piece-black-p piece)
                       #'upcase #'identity)
                   (symbol-name (elshogi-piece/name piece)))))

(defsubst elshogi-mrec-drop-p (mrec)
  (eq (elshogi-mrec/origin mrec) '*))

(defsubst elshogi-dlist-head (dlist)
  (dlink/content (dlist/head dlist)))

(defsubst elshogi-dlist-tail (dlist)
  (dlink/content (dlist/tail dlist)))

(provide 'elshogi-core)
;;; elshogi-core.el ends here
