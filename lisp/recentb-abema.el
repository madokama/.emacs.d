;;; recentb-abema --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(defvar recentb-abema-channels nil)

(recentb-define-mode abema
  :history recentb-abema-history
  :candidate recentb-abema-candidate)

(autoload 'abema-schedule "abema")
(autoload 'abema-watch "abema")

(defun recentb-abema-history ()
  (let ((sched (mapcan #'abema-schedule recentb-abema-channels)))
    (if (> (length recentb-abema-channels) 1)
        (seq-sort-by #'car #'time-less-p sched)
      sched)))

(defun recentb-abema-candidate (item)
  (seq-let (start _ title url casts) item
      (propertize (format "*abema:%s %s%s*"
                          (format-time-string "%d(%a)%H:%M:%S" start)
                          title casts)
                  'recentb (list 'abema-watch url))))

(provide 'recentb-abema)
;;; recentb-abema.el ends here
