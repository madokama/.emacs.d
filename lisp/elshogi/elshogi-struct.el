;;; elshogi-struct --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dlist)

(defconst elshogi-black 'b)
(defconst elshogi-white 'w)

(defmacro elshogi-defstruct (name &rest slots)
  (declare (indent 1))
  (let ((struct-name (intern (format "elshogi-%s" name)))
        (conc-name (intern (format "elshogi-%s/" name)))
        (constructor (intern (format "elshogi-make-%s" name)))
        (copier (intern (format "elshogi-copy-%s" name))))
    `(cl-defstruct (,struct-name (:conc-name ,conc-name)
                                 (:constructor ,constructor)
                                 (:copier ,copier))
       ,@slots)))

(elshogi-defstruct piece
  name promoted side)

(elshogi-defstruct position
  on-board
  on-black-stand
  on-white-stand
  (side elshogi-black)
  (count 1))

(elshogi-defstruct player
  name side image engine)

;; move record
(elshogi-defstruct mrec
  piece side origin target promote capture count time note)

;; game record
(elshogi-defstruct grec
  startpos
  (moves (list->dlist (list (elshogi-make-mrec)))) ; dummy move
  cursor)

(elshogi-defstruct game
  position black white record display result watch-p live-p url kif title)

(elshogi-defstruct display
  piece-stand/b
  piece-stand/w
  (pov elshogi-black))

(provide 'elshogi-struct)
;;; elshogi-struct.el ends here
