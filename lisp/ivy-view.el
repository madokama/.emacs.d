;;; ivy-view --- Ivy view enhancement -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'ivy)

(defgroup ivy-view nil
  "Ivy view enhancement."
  :prefix "ivy-view-"
  :group 'ivy)

(defcustom ivy-view-no-update-modes nil
  "List of major modes not to register views."
  :type '(repeat symbol))

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

(defun ivy-view-contents (tr)
  (pcase (car tr)
    ((or 'buffer 'file) (list (cadr tr)))
    ((or 'vert 'horz)
     (mapcan #'ivy-view-contents (cdr tr)))))

(defun ivy-view-dedupe (view)
  (let ((contents (ivy-view-contents view)))
    (setq ivy-views
          (cl-delete-if (pcase-lambda (`(_ ,view))
                          (cl-subsetp (ivy-view-contents view) contents
                                      :test #'string=))
                        ivy-views))))

(defun ivy-view--no-update ()
  (cl-some (lambda (win)
             (with-selected-window win
               (memq major-mode ivy-view-no-update-modes)))
           (window-list)))

(defun ivy-view-update ()
  "Register current view."
  (unless (ivy-view--no-update)
    (let ((view (ivy-view-view)))
      ;; Prevent views from growing indefinitely.
      (ivy-view-dedupe view)
      (push (list (ivy-default-view-name) view)
            ivy-views))))

(defun ivy-view-discard ()
  "Delete views containing buffers being killed."
  (unless (ivy-view--no-update)
    (let ((contents (ivy-view-contents (ivy-view-view))))
      (when (member (buffer-name) contents)
        (setq ivy-views
              (cl-delete-if (pcase-lambda (`(_ ,view))
                              (cl-subsetp contents (ivy-view-contents view)
                                          :test #'string=))
                            ivy-views))))))

(add-hook 'kill-buffer-hook #'ivy-view-discard)

(defun ivy-view--kill-action (x)
  (if-let* ((view (assoc x ivy-views)))
      (ivy-pop-view-action view)
    (kill-buffer x)
    (ivy--reset-state ivy-last)))

(setcar (cdr (assoc "k" (plist-get ivy--actions-list 'ivy-switch-buffer)))
        'ivy-view--kill-action)

(provide 'ivy-view)
;;; ivy-view.el ends here
