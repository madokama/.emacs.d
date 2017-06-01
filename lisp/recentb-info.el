;;; recentb-info --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(recentb-define-mode Info-mode
   :var recentb-info
   :history recentb-info-history
   :candidate recentb-info-candidate)

(defvar Info-current-file)
(defvar Info-current-node)
(defvar Info-history)

(autoload 'Info-find-node "info")

(defun recentb-info-candidate (item)
  (seq-let (file node point) item
    (propertize (format "*info:%s(%s)*" (file-name-nondirectory file) node)
                'recentb `((lambda ()
                             (Info-find-node ,file ,node)
                             (goto-char ,point))))))

(defun recentb-info-history ()
  (when (derived-mode-p 'Info-mode)
    (cl-delete-duplicates (append
                           (if Info-current-file
                               (cons (list Info-current-file Info-current-node (point))
                                     Info-history)
                             Info-history)
                           recentb-info)
                          :test (lambda (a b)
                                  (and (string= (car a) (car b))
                                       (string= (cadr a) (cadr b))))
                          :from-end t)))

(provide 'recentb-info)
;;; recentb-info.el ends here
