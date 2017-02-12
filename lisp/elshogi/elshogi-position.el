;;; elshogi-position --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-candidates)

(defun elshogi-king-index (side)
  (elshogi-find-index (lambda (index)
                        (let ((piece (elshogi-piece-at index)))
                          (and (elshogi-piece-of-side-p piece side)
                               (eq (elshogi-piece/name piece) 'k))))))

(defun elshogi-index-in-check-p (index side)
  (cl-loop for opponent in (elshogi-indices-on-board (elshogi-negate-side side))
           if (memq index (elshogi-candidates-target opponent))
             return t))

(defun elshogi-king-in-check-p (side)
  (elshogi-index-in-check-p (elshogi-king-index side) side))

(provide 'elshogi-position)
;;; elshogi-position.el ends here
