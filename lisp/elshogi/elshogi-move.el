;;; elshogi-move --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'elshogi-struct))
(require 'elshogi-game)
(require 'elshogi-sfen)
(require 'dlist)

(defun elshogi-move-equals-sfen (mrec sfen)
  (string= (elshogi-mrec->sfen mrec) sfen))

(defun elshogi-move-capture-internal (game piece side)
  (push (elshogi-make-piece :name (elshogi-piece/name piece)
                            :side side)
        (elshogi-pieces-on-stand game side)))

(defun elshogi-move-drop-internal (game piece side)
  (let ((name (elshogi-piece/name piece)))
    (setf (elshogi-pieces-on-stand game side)
          (-remove-first (lambda (p)
                           (eq (elshogi-piece/name p) name))
                         (elshogi-pieces-on-stand game side)))))

(defsubst elshogi-move-set-index (game index piece)
  (aset (elshogi-current-position-on-board game) index piece))

(defun elshogi-move-add (game mrec)
  ;; (setf (elshogi-mrec/time mrec) (current-time))
  (setf (elshogi-game-cursor game)
        (dlist-insert-tail (elshogi-game-moves game) mrec))
  (cl-incf (elshogi-position/count (elshogi-game/position game))))

(defun elshogi-move-make-move (game origin target promote)
  (let ((side (elshogi-current-side game))
        (origin-piece
         (funcall (if promote
                      #'elshogi-flip-piece
                    #'identity)
                  (elshogi-piece-at game origin))))
    (cl-assert (eq (elshogi-piece/side origin-piece) side))
    (let* ((target-piece (elshogi-piece-at game target))
           (mrec
            (elshogi-make-mrec :piece origin-piece
                               :side side
                               :origin origin
                               :target target
                               :promote promote
                               :capture target-piece)))
      (when target-piece
        (elshogi-move-capture-internal game target-piece side))
      (elshogi-move-set-index game origin nil)
      (elshogi-move-set-index game target origin-piece)
      (elshogi-move-add game mrec)
      mrec)))

(defun elshogi-move-make-drop (game index piece)
  (let ((side (elshogi-current-side game)))
    (cl-assert (eq (elshogi-piece/side piece) side))
    (let ((mrec
           (elshogi-make-mrec :piece piece
                              :side side
                              :origin '*
                              :target index)))
      (elshogi-move-drop-internal game piece side)
      (elshogi-move-set-index game index piece)
      (elshogi-move-add game mrec)
      mrec)))

(provide 'elshogi-move)
;;; elshogi-move.el ends here
