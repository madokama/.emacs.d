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

(defcustom ivy-view-ignore-modes nil
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
                    (let ((file (buffer-file-name)))
                      (cond ((and file (file-exists-p file))
                             (list 'file file (point)))
                            ((eq major-mode 'dired-mode)
                             (list 'file default-directory (point)))
                            (t
                             (list 'buffer (buffer-name) (point)))))))))
    (ft (car (window-tree)))))

(defun ivy-view-iterate (fold f tr)
  (cl-labels ((iter (tr)
                (pcase (car tr)
                  ((or 'buffer 'file) (funcall f tr))
                  ((or 'vert 'horz)
                   (funcall fold #'iter (cdr tr))))))
    (iter tr)))

(defun ivy-view-contents (view)
  (ivy-view-iterate #'mapcan
                    (pcase-lambda (`(_ ,content _))
                      (list content))
                    view))

(defun ivy-view-buffers (view)
  (ivy-view-iterate #'mapcan
                    (pcase-lambda (`(,type ,content _))
                      (when (eq type 'buffer)
                        (list content)))
                    view))

(defun ivy-view-dedupe (view)
  (let ((contents (ivy-view-contents view)))
    (setq ivy-views
          (cl-delete-if (pcase-lambda (`(_ ,v))
                          (let ((c (ivy-view-contents v)))
                            (or (cl-subsetp c contents :test #'string=)
                                (cl-subsetp contents c :test #'string=))))
                        ivy-views))))

(defun ivy-view--no-update ()
  (or (string-prefix-p " *" (buffer-name))
      (cl-some (lambda (win)
                 (with-selected-window win
                   (memq major-mode ivy-view-ignore-modes)))
               (window-list))))

(defun ivy-view-cleanup ()
  (setq ivy-views
        (cl-delete-if (pcase-lambda (`(_ ,view))
                        (cl-find-if-not #'get-buffer (ivy-view-buffers view)))
                      ivy-views)))

(defun ivy-view-update ()
  "Register current view."
  (unless (ivy-view--no-update)
    (let ((view (ivy-view-view)))
      ;; Prevent views from growing indefinitely.
      (ivy-view-cleanup)
      (ivy-view-dedupe view)
      (push (list (ivy-default-view-name) view)
            ivy-views))))

(defun ivy-view-discard ()
  "Delete views containing buffers being killed."
  (unless (ivy-view--no-update)
    (let ((killed (or (buffer-file-name) (buffer-name))))
      (setq ivy-views
            (cl-delete-if (pcase-lambda (`(_ ,view))
                            (member killed (ivy-view-contents view)))
                          ivy-views)))))

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
