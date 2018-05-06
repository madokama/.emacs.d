;;; elshogi-game --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'elshogi-core)
(require 'elshogi-struct)

(defmacro elshogi-game-cursor (game)
  `(elshogi-grec/cursor (elshogi-game/record ,game)))

(defun elshogi-game-buffer (game)
  (get-buffer-create (format "*elshogi:%s*" (elshogi-game/title game))))

(defun elshogi-game-focus (game)
  (thread-first game
    elshogi-game-buffer get-buffer-window select-window))

(defun elshogi-game-frame (game)
  (when-let ((win
              (get-buffer-window (elshogi-game-buffer game) t)))
    (window-frame win)))

(defsubst elshogi-game-moves (game)
  (elshogi-grec/moves (elshogi-game/record game)))

(defalias 'elshogi-game-pov #'elshogi-players-side)

(defun elshogi-game-player (game)
  (if (elshogi-black-p (elshogi-game-pov game))
      (elshogi-game/black game)
    (elshogi-game/white game)))

(defun elshogi-game-engine (game)
  (if (elshogi-black-p (elshogi-game-pov game))
      (elshogi-game/white game)
    (elshogi-game/black game)))

(defmacro elshogi-game-initialize (&rest params)
  (unless (plist-get params :position)
    (plist-put params :position '(elshogi-new-position)))
  (unless (plist-get params :display)
    (plist-put params :display '(elshogi-make-display)))
  (let ((gamevar (make-symbol "game")))
    `(let ((,gamevar (elshogi-make-game ,@params)))
       (unless (elshogi-game/title ,gamevar)
         (setf (elshogi-game/title ,gamevar)
               (format "▲%s △%s  %s"
                       (elshogi-player/name (elshogi-game/black ,gamevar))
                       (elshogi-player/name (elshogi-game/white ,gamevar))
                       (format-time-string "%c" (current-time)))))
       (setf (elshogi-game-cursor ,gamevar)
             (dlist/head (elshogi-game-moves ,gamevar)))
       ,gamevar)))

(defsubst elshogi-game-latest-move (game)
  (dlink/content (elshogi-game-cursor game)))

(defsubst elshogi-game-final-move (game)
  (elshogi-dlist-tail (elshogi-game-moves game)))

(provide 'elshogi-game)
;;; elshogi-game.el ends here
