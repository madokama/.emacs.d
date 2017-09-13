;;; elshogi-replay --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'elshogi-game)
(require 'elshogi-move)
(require 'elshogi-sfen)

(defun elshogi-replay-rewind (game)
  (setf (elshogi-current-position game)
        (thread-first game
          elshogi-game/record
          elshogi-grec/startpos
          elshogi-sfen->position))
  (setf (elshogi-game-cursor game) (dlist/head (elshogi-game-moves game)))
  game)

(defun elshogi-replay-prev-internal (game)
  (let* ((cursor (elshogi-game-cursor game))
         (mrec (dlink/content cursor)))
    (when-let* ((piece (elshogi-mrec/piece mrec))
                (origin (elshogi-mrec/origin mrec))
                (target (elshogi-mrec/target mrec))
                (side (elshogi-mrec/side mrec)))
      (cond ((elshogi-mrec-drop-p mrec)
             (elshogi-move-capture-internal game piece side)
             (elshogi-move-set-index game target nil))
            (t
             (elshogi-move-set-index game
                                     target
                                     (when-let* ((capture
                                                  (elshogi-mrec/capture mrec)))
                                       (elshogi-move-drop-internal
                                        game capture side)
                                       capture))
             (elshogi-move-set-index game
                                     origin
                                     (if (elshogi-mrec/promote mrec)
                                         (elshogi-flip-piece piece)
                                       piece))))
      (when-let* ((prev (dlink/prev cursor)))
        (setf (elshogi-current-side game) side)
        (setf (elshogi-game-cursor game) prev)
        (cl-decf (elshogi-position/count (elshogi-game/position game))))))
  game)

(defun elshogi-replay-next-internal (game)
  (when-let* ((cursor (dlink/next (elshogi-game-cursor game)))
              (mrec (dlink/content cursor)))
    (let ((piece (elshogi-mrec/piece mrec))
          (origin (elshogi-mrec/origin mrec))
          (target (elshogi-mrec/target mrec))
          (side (elshogi-mrec/side mrec)))
      (cond ((elshogi-mrec-drop-p mrec)
             (elshogi-move-drop-internal game piece side)
             (elshogi-move-set-index game target piece))
            (t
             (when-let* ((capture (elshogi-mrec/capture mrec)))
               (elshogi-move-capture-internal game capture side))
             (elshogi-move-set-index game origin nil)
             (elshogi-move-set-index game target piece)))
      (setf (elshogi-current-side game) (elshogi-negate-side side))
      (setf (elshogi-game-cursor game) cursor)
      (cl-incf (elshogi-position/count (elshogi-game/position game)))))
  game)

(defun elshogi-replay-seek (game count)
  (elshogi-replay-rewind game)
  (when count
    (dotimes (_ count)
      (elshogi-replay-next-internal game)))
  game)

(provide 'elshogi-replay)
;;; elshogi-replay.el ends here
