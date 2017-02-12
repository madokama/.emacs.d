;;; recentb-woman --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; DONE replace with `man'

;;; Code:

(require 'recentb)

(recentb-define-mode woman-mode
   :var recentb-woman
   :history recentb-woman-history
   :candidate recentb-woman-candidate)

(defvar woman-buffer-alist)

(defun recentb-woman-history ()
  (cl-delete-duplicates (append woman-buffer-alist recentb-woman)
                        :test (lambda (a b)
                                (string= (cdr a) (cdr b)))
                        :from-end t))

(defun recentb-woman-candidate (item)
  (unless (get-buffer (cdr item))
    (propertize (cdr item) 'recentb (list 'woman-find-file (car item)))))

(provide 'recentb-woman)
;;; recentb-woman.el ends here
