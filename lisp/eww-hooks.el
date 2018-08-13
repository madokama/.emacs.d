;;; eww-hooks --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))

(defgroup eww-hooks nil
  "Eww hooks"
  :group 'eww
  :prefix "eww-hooks-")

(defvar eww-data)

(defvar eww-hooks-alist
  `((,(rx "youtube.com/" (or "feed/subscriptions" "channel/"))
      . eww-hooks-youtube)))

(defun eww-hooks-youtube ()
  (when (re-search-forward (rx bol (or "Loading" "読み込んでいます") "...")
                           nil t)
    (delete-region (point-min) (point))))

;;;###autoload
(defun eww-hooks ()
  (let ((url (plist-get eww-data :url)))
    (save-match-data
      (cl-loop for (host . hook) in eww-hooks-alist
               when (string-match-p host url)
                 do (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (funcall hook))
                    (cl-return)))))

;;;###autoload(add-hook 'eww-after-render-hook #'eww-hooks)

;;;###autoload
(defun eww-register-hook (regex hook)
  (push (cons regex hook) eww-hooks-alist))

;; Google declutter links

(declare-function text-property-search-forward "text-property-search")
(declare-function prop-match-beginning "text-property-search")
(declare-function prop-match-end "text-property-search")
(declare-function prop-match-value "text-property-search")
(declare-function url-generic-parse-url "url-parse")
(declare-function url-path-and-query "url-parse")

(defun eww-hooks-google-relink ()
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (cl-loop for prop = (text-property-search-forward 'shr-url)
               while prop
               do (pcase (url-path-and-query
                          (url-generic-parse-url (prop-match-value prop)))
                    (`("/url" . ,query)
                      (add-text-properties
                       (prop-match-beginning prop)
                       (prop-match-end prop)
                       (let ((url
                              (cadr (assoc "q" (url-parse-query-string query)))))
                         (list 'shr-url url
                               'help-echo url)))))))))

(eww-register-hook (rx "www.google." (+ (not (any ?/))) "/search")
                   #'eww-hooks-google-relink)


(defcustom eww-hooks-readable-alist nil
  "List of pairs of host regexp and a term to skip over."
  :group 'eww-hooks
  :type '(alist :key-type regexp :value-type regexp))

(defun eww-hooks-readable ()
  (let ((url (plist-get eww-data :url)))
    (save-match-data
      (cl-loop for (host . search) in eww-hooks-readable-alist
               when (string-match-p host url)
                 do (goto-char (point-min))
                    (when (re-search-forward search nil t)
                      (end-of-line)
                      (re-search-forward (rx nonl) nil t)
                      (when-let ((win (get-buffer-window (current-buffer) t)))
                        (with-selected-window win
                          (recenter 0))))
                    (cl-return)))))

(add-hook 'eww-after-render-hook #'eww-hooks-readable)

(provide 'eww-hooks)
;;; eww-hooks.el ends here
