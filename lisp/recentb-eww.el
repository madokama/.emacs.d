;;; recentb-eww --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)
(require 'eww-savehist)

(recentb-define-mode eww-mode
  :history recentb-eww-history
  :candidate recentb-eww-candidate)

(defun recentb-eww-history ()
  eww-savehist)

(defun recentb-eww-candidate (item)
  (let ((url (plist-get item :url)))
    (propertize (format "*eww:%s: %s*" (plist-get item :title) url)
                'recentb (list 'eww url))))

(provide 'recentb-eww)
;;; recentb-eww.el ends here
