;;; pdf-savehist --- Remember PDF positions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'savehist)
(require 'pdf-view)

(defvar pdf-savehist nil)

;;;###autoload(add-hook 'savehist-mode-hook (lambda () (add-to-list 'savehist-additional-variables 'pdf-savehist)))

(defsubst pdf-savehist-call/pdf (pdf fn)
  (funcall fn
           (lambda (regv)
             (file-equal-p pdf
                           (bookmark-prop-get (registerv-data regv) 'filename))
             ;; (string= pdf
             ;;          (expand-file-name
             ;;           (bookmark-prop-get (registerv-data regv) 'filename)))
             )
           pdf-savehist))

;;;###autoload
(defun pdf-savehist-load ()
  (when-let ((regv
              (pdf-savehist-call/pdf (pdf-view-buffer-file-name) #'cl-find-if)))
    (funcall (registerv-jump-func regv) (registerv-data regv))))

;;;###autoload(with-eval-after-load 'pdf-view (add-hook 'pdf-view-mode-hook #'pdf-savehist-load))

(defun pdf-savehist-save ()
  ;; TODO handle errors with *eww pdf*
  (ignore-errors
    (setq pdf-savehist
          (cons (pdf-view-registerv-make)
                (pdf-savehist-call/pdf (pdf-view-buffer-file-name)
                                       #'cl-delete-if)))))

(defun pdf-savehist-save-all ()
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (derived-mode-p 'pdf-view-mode)
              (pdf-savehist-save))))
        (buffer-list)))

(add-hook 'pdf-info-close-document-hook #'pdf-savehist-save)
(add-hook 'kill-emacs-hook #'pdf-savehist-save-all)

(provide 'pdf-savehist)
;;; pdf-savehist.el ends here
