;;; ivy-view --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'ivy)

(defun ivy-view-view ()
  (cl-labels ((ft (tr)
                (if (consp tr)
                    (if (eq (car tr) t)
                        (cons 'vert
                              (mapcar #'ft (cddr tr)))
                      (cons 'horz
                            (mapcar #'ft (cddr tr))))
                  (with-current-buffer (window-buffer tr)
                    (cond ((buffer-file-name)
                           (list 'file (buffer-file-name) (point)))
                          ((eq major-mode 'dired-mode)
                           (list 'file default-directory (point)))
                          (t
                           (list 'buffer (buffer-name) (point))))))))
    (ft (car (window-tree)))))

(defun ivy-view-update ()
  (when-let* ((name (ivy-default-view-name))
              (name
               (save-match-data
                 (and (string-match "\\`\\(.+\\) [[:digit:]]+\\'" name)
                      (match-string 1 name)))))
    (setq ivy-views
          (cl-delete-if (lambda (view)
                          (string-prefix-p name (car view)))
                        ivy-views)))
  (push (list (ivy-default-view-name)
              (ivy-view-view))
        ivy-views))

(defun ivy-view--kill-action (x)
  (if-let* ((view (assoc x ivy-views)))
      (ivy-pop-view-action view)
    (kill-buffer x)
    (ivy--reset-state ivy-last)))

(setcar (cdr (assoc "k" (plist-get ivy--actions-list 'ivy-switch-buffer)))
        'ivy-view--kill-action)

(provide 'ivy-view)
;;; ivy-view.el ends here
