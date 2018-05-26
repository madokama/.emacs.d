;;; eww-hooks --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs))

(defvar eww-data)

(defvar eww-hooks-alist
  '(("youtube\\.com/feed/subscriptions" . eww-hooks-youtube)))

(defun eww-hooks-youtube ()
  (when (re-search-forward (rx bol (or "Loading" "読み込んでいます") "...")
                           nil t)
    (delete-region (point-min) (point))))

;;;###autoload
(defun eww-hooks ()
  (let ((url (plist-get eww-data :url)))
    (cl-loop for (regex . hook) in eww-hooks-alist
             when (string-match-p regex url)
               do (let ((inhibit-read-only t))
                    (save-excursion
                      (goto-char (point-min))
                      (funcall hook)))
                  (cl-return))))

;;;###autoload(add-hook 'eww-after-render-hook #'eww-hooks)

;;;###autoload
(defun eww-register-hook (regex hook)
  (push (cons regex hook) eww-hooks-alist))

(provide 'eww-hooks)
;;; eww-hooks.el ends here
