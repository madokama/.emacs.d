;;; recentb-elfeed --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)
(require 'elfeed)

(recentb-define-mode elfeed-show-mode
   :var recentb-elfeed
   :history recentb-elfeed-history
   :candidate recentb-elfeed-candidate)

(defun recentb-elfeed-history ()
  (cl-delete-if (lambda (item)
                  (memq 'junk (elfeed-entry-tags item)))
                (cl-delete-duplicates (cons elfeed-show-entry recentb-elfeed)
                                      :test (lambda (a b)
                                              (equal (elfeed-entry-id a)
                                                     (elfeed-entry-id b)))
                                      :from-end t)))

(defun recentb-elfeed-candidate (item)
  (propertize (format "*elfeed:%s*" (elfeed-entry-title item))
              'recentb (list 'elfeed-show-entry item)))

(provide 'recentb-elfeed)
;;; recentb-elfeed.el ends here
