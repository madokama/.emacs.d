;;; recentb-man --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(recentb-define-mode Man-mode
  :var recentb-man
  :history recentb-man-history
  :candidate recentb-man-candidate)

(defsubst recentb-man-topic (item)
  (replace-regexp-in-string (rx (or (and bos "*Man ")
                                    (and "*" eos)))
                            ""
                            item))

(defun recentb-man-history ()
  (when (derived-mode-p 'Man-mode)
    (cl-delete-duplicates
     (cons (buffer-name) recentb-man)
     :test #'string= :from-end t)))

(defun recentb-man-candidate (item)
  (unless (get-buffer item)
    (propertize item 'recentb (list 'man (recentb-man-topic item)))))

(provide 'recentb-man)
;;; recentb-man.el ends here
