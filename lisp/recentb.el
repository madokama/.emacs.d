;;; recentb --- History for ephemeral buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO
;;  - ffedit
;;  - pocket

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'savehist)
(require 'ivy)

(defvar recentb-max-saved-items 100)

(defvar recentb-mode-alist nil)

(defmacro recentb-define-mode (mode &rest plst)
  (declare (indent 1))
  (let ((mandatory '()) ; :var :candidate and :history have been made optional
        (var (plist-get plst :var)))
    (unless (seq-every-p (apply-partially #'plist-get plst)
                         mandatory)
      (error "Some keys are missing: %S"
             (cl-nset-difference mandatory
                                 (cl-delete-if-not #'keywordp plst))))
    `(progn
       ,@(when var
           `((defvar ,var nil)))
       (add-to-list 'recentb-mode-alist ',(cons mode plst) t))))

(defsubst recentb-mode-plist ()
  (cl-some (pcase-lambda (`(,mode . ,plst))
             (when (derived-mode-p mode)
               plst))
           recentb-mode-alist))

(defsubst recentb-mode-ref (key &optional plst)
  (when-let* ((plst (or plst (recentb-mode-plist))))
    (plist-get plst key)))

(defun recentb-save-on-kill-buffer ()
  ;; (message "[!] %s(%s)" (buffer-name) major-mode)
  ;; Register the hook only when the mode defines a variable to save.
  (when (recentb-mode-ref :var)
    (add-hook 'kill-buffer-hook #'recentb-save1 nil t)))

(defun recentb-register-hooks ()
  (add-hook 'after-change-major-mode-hook #'recentb-save-on-kill-buffer)
  (add-hook 'kill-emacs-hook #'recentb-save))

(defun recentb-register-vars ()
  (cl-loop for (_mode . plst) in recentb-mode-alist
           do (when-let* ((var (recentb-mode-ref :var plst)))
                (add-to-list 'savehist-additional-variables var))))

(defsubst recentb-history (plst)
  (or (symbol-value (recentb-mode-ref :var plst))
      (when-let* ((hist (recentb-mode-ref :history plst))
                  (fn (or (symbol-function hist) (symbol-value hist))))
        ;; TODO handle errors
        (ignore-errors (funcall fn)))))

(defun recentb-save1 ()
  (when-let* ((plst (recentb-mode-plist))
              (var (recentb-mode-ref :var plst))
              (hist (recentb-mode-ref :history plst)))
    (set var
         (seq-take (funcall hist) recentb-max-saved-items))))

(defun recentb-save ()
  (cl-loop for buf being the buffers
           do (with-current-buffer buf
                (recentb-save1))))

;; Ivy Integration

(defun recentb-ivy-sources ()
  (mapcan (pcase-lambda (`(,_mode . ,plst))
            (let ((cand (or (symbol-function (recentb-mode-ref :candidate plst))
                            #'identity)))
              (cl-reduce (lambda (item acc)
                           (if-let* ((item (funcall cand item)))
                               (cons (propertize item 'face 'ivy-virtual)
                                     acc)
                             acc))
                         (recentb-history plst)
                         :initial-value nil :from-end t)))
          recentb-mode-alist))

(defun recentb-ivy-action (str)
  (when-let* ((action (get-text-property 0 'recentb str)))
    (apply #'funcall action)
    (current-buffer)))

(defun ad-recentb-ivy-switch-buffer-action (oldfun str)
  (or (with-ivy-window (recentb-ivy-action str))
      (funcall oldfun str)))

(defun ad-recentb-ivy-switch-buffer-ow-action (oldfun str)
  (if-let* ((buf (save-window-excursion (recentb-ivy-action str))))
      (switch-to-buffer-other-window buf)
    (funcall oldfun str)))

(defun recentb-integrate-with-ivy ()
  (ivy-set-sources 'ivy-switch-buffer
                   `(,@(plist-get ivy--sources-list 'ivy-switch-buffer)
                       (recentb-ivy-sources)))
  (advice-add 'ivy--switch-buffer-action
              :around #'ad-recentb-ivy-switch-buffer-action)
  (advice-add 'ivy--switch-buffer-other-window-action
              :around #'ad-recentb-ivy-switch-buffer-ow-action))

;; Minor Mode

(define-minor-mode recentb-mode
    "Enable `recentf' mode for ephemeral buffers."
  :global t
  (if recentb-mode
      (progn
        (recentb-register-vars)
        (recentb-register-hooks)
        (recentb-integrate-with-ivy))))

(defun recentb-enable-plugins (plugins)
  (cl-loop for plugin in plugins
           do (require (intern (format "recentb-%s" plugin))))
  (recentb-mode 1))

(provide 'recentb)
;;; recentb.el ends here
