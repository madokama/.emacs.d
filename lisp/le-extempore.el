;;; le-extempore --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lispy)
(require 'extempore-mode)

(defun lispy-eval-extempore (&optional _plain)
  (let ((bnd (lispy--bounds-dwim)))
    (extempore-send-region (car bnd) (cdr bnd))))

(add-to-list 'lispy-eval-alist '(extempore-mode lispy-eval-extempore le-extempore))

(provide 'le-extempore)
;;; le-extempore.el ends here
