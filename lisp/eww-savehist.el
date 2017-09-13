;;; eww-savehist --- save history of eww -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-and-compile
  (require 'subr-x))
(require 'seq)
(require 'savehist)
(require 'eww)

;;;###autoload
(defvar eww-savehist nil)

;;;###autoload(add-hook 'savehist-mode-hook (lambda () (add-to-list 'savehist-additional-variables 'eww-savehist) (setq-default eww-history eww-savehist)))

;;;###autoload
(defun eww-savehist-add ()
  (when (bound-and-true-p eww-data)
    (push (copy-sequence (eww-desktop-data-1 eww-data))
          eww-savehist)))

(add-hook 'eww-after-render-hook #'eww-savehist-add)

(defun eww-savehist-load ()
  (setq eww-history eww-savehist))

(add-hook 'eww-mode-hook #'eww-savehist-load)

(defun eww-savehist-prune ()
  (setq eww-savehist
        (mapcar (lambda (data)
                  (let ((data (eww-desktop-data-1 data)))
                    (if (string-empty-p (plist-get data :title))
                        (plist-put data :title (plist-get data :url))
                      data)))
                (seq-take (cl-delete-duplicates eww-savehist
                                                :test
                                                (lambda (a b)
                                                  (string= (plist-get a :url)
                                                           (plist-get b :url)))
                                                :from-end t) ;retain the newer one
                          (or eww-history-limit 0)))))

(add-hook 'savehist-save-hook #'eww-savehist-prune)

(defun eww-savehist-delete-entry ()
  "Delete the history entry at point."
  (interactive)
  ;; TODO Allow operation on multi-line selection
  (save-excursion
    (when-let* ((url
                 (plist-get (get-text-property (point-at-bol) 'eww-history)
                            :url)))
      (cl-delete-if (lambda (data)
                      (string= (plist-get data :url) url))
                    eww-savehist)
      (setq-default eww-history eww-savehist)
      (let ((inhibit-read-only t))
        (delete-region (point-at-bol) (progn (forward-line) (point)))))))

(define-key eww-history-mode-map (kbd "C-k") #'eww-savehist-delete-entry)

(provide 'eww-savehist)
;;; eww-savehist.el ends here
