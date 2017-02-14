;;; elshogi-game --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-core)
(require 'elshogi-struct)

(defmacro elshogi-game-cursor (game)
  `(elshogi-grec/cursor (elshogi-game/record ,game)))

(defun elshogi-game-buffer (game)
  (get-buffer-create (format "*elshogi:%s*" (elshogi-game/title game))))

(defun elshogi-game-frame (game)
  (when-let* (win
              (get-buffer-window (elshogi-game-buffer game) t))
    (window-frame win)))

(defsubst elshogi-game-moves (game)
  (elshogi-grec/moves (elshogi-game/record game)))

(defsubst elshogi-game-pov (game)
  (elshogi-display/pov (elshogi-game/display game)))

(defmacro elshogi-game-initialize (&rest params)
  (unless (plist-get params :position)
    (plist-put params :position '(elshogi-new-position)))
  (unless (plist-get params :display)
    (plist-put params :display '(elshogi-make-display)))
  (let ((gamevar (make-symbol "game")))
    `(let ((,gamevar (elshogi-make-game ,@params)))
       (setf (elshogi-game-cursor ,gamevar)
             (dlist/head (elshogi-game-moves ,gamevar)))
       ,gamevar)))

(defsubst elshogi-game-latest-move (game)
  (dlink/content (elshogi-game-cursor game)))

(defsubst elshogi-game-final-move (game)
  (elshogi-dlist-tail (elshogi-game-moves game)))

(provide 'elshogi-game)
;;; elshogi-game.el ends here
