;;; recentb-firefox --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)
(require 'firefox)

(recentb-define-mode firefox-mode
  :var recentb-firefox
  :candidate recentb-firefox-candidate)

(defun recentb-firefox-fetch-tabs ()
  (firefox-tabs (lambda (tabs)
                  (setq recentb-firefox tabs)))
  (run-with-timer (* 60 30) nil #'recentb-firefox-fetch-tabs))

(defun recentb-firefox-candidate (item)
  ;; ITEM = (time url . title)
  (let ((url (cadr item)))
    (propertize (format "*firefox:%s: %s*" (cddr item) url)
                'recentb (list 'eww url))))

(run-with-idle-timer 7 nil #'recentb-firefox-fetch-tabs)

(provide 'recentb-firefox)
;;; recentb-firefox.el ends here
