;;; recentb-firefox --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)
(require 'emoz)

(recentb-define-mode firefox-mode
  :var recentb-firefox
  :candidate recentb-firefox-candidate)

(defun recentb-firefox-fetch-tabs ()
  (emoz-tabs (lambda (tabs)
               (setq recentb-firefox tabs)))
  (run-with-timer (* 60 10) nil #'recentb-firefox-fetch-tabs))

(defun recentb-firefox-candidate (item)
  (let ((url (plist-get item :url)))
    (propertize (format "*firefox:%s: %s*" (plist-get item :title) url)
                'recentb (list 'eww url))))

(run-with-idle-timer 7 nil #'recentb-firefox-fetch-tabs)

(provide 'recentb-firefox)
;;; recentb-firefox.el ends here
