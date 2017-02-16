;;; recentb-elfeed --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(recentb-define-mode elfeed-show-mode
   :var recentb-elfeed
   :history recentb-elfeed-history
   :candidate recentb-elfeed-candidate)

(defvar elfeed-show-entry)
(declare-function elfeed-entry-id "ext:elfeed-db")
;; (declare-function elfeed-entry-tags "ext:elfeed-db")
(autoload 'elfeed-entry-tags "elfeed-db")
(autoload 'elfeed-show-entry "elfeed-show")
(autoload 'elfeed-entry-title "elfeed-db")

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

(defun recentb-elfeed-register-hook ()
  (add-hook 'kill-buffer-hook #'recentb-save1 nil t))

(add-hook 'elfeed-show-mode-hook #'recentb-elfeed-register-hook)

(provide 'recentb-elfeed)
;;; recentb-elfeed.el ends here
