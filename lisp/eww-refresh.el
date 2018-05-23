;;; eww-refresh --- Auto refresh eww buffers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-macs))
(require 'cl-seq)
(require 'eww)
(require 'url-cache)

(defun eww-refresh--refresh (url buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (string= (plist-get eww-data :url) url)
        (let ((shr-ignore-cache t))
          (eww-display-html nil url (plist-get eww-data :dom) (point) buf))
        (run-hooks 'eww-after-render-hook)))))

;;;###autoload
(defun eww-refresh ()
  (when-let ((refresh
              (cl-find-if (lambda (meta)
                            (when-let ((he (dom-attr meta 'http-equiv)))
                              (string= (downcase he) "refresh")))
                          (dom-by-tag (plist-get eww-data :dom) 'meta)))
             (url (plist-get eww-data :url))
             (buf (current-buffer)))
    ;; Cancel timer for the current url.
    (setq timer-list
          (cl-delete-if (lambda (timer)
                          (and (eq (timer--function timer) 'eww-refresh--refresh)
                               (equal (list url buf) (timer--args timer))))
                        timer-list))
    (run-with-timer (string-to-number (dom-attr refresh 'content))
                    nil #'eww-refresh--refresh url buf)))

;;;###autoload(add-hook 'eww-after-render-hook #'eww-refresh)

(provide 'eww-refresh)
;;; eww-refresh.el ends here
