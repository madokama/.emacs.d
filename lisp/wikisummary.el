;;; wikisummary --- Pop up summary on wiki terms -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'rx))
(require 'text-property-search)
(require 'posframe)

(defvar wikisummary-buffer " *wikisummary*")

(defvar wikisummary--term nil)

(defvar shr-map)

(defvar wikisummary-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'wikisummary-toggle)
    (set-keymap-parent map shr-map)
    map))

(defun wikisummary--install-map ()
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (cl-loop for echo = (text-property-search-forward 'help-echo)
               while echo
               when (string-match-p (rx ".wikipedia.org/wiki/"
                                        (+ (not (any "/" ":" " ")))
                                        " (")
                                    (prop-match-value echo))
                 do (put-text-property (prop-match-beginning echo)
                                       (prop-match-end echo)
                                       'keymap
                                       wikisummary-map)))))

;;;###autoload
(defun wikisummary-install-map ()
  (make-thread #'wikisummary--install-map))

;;;###autoload(eww-register-hook (rx ".wikipedia.org/wiki/") #'wikisummary-install-map)

(defun wikisummary-query-api (term &optional lang)
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-s"
                  (format "https://%s.wikipedia.org/api/rest_v1/page/summary/%s"
                          (or lang "en")
                          term))
    (goto-char (point-min))
    (condition-case nil
        (json-parse-buffer)
      (json-end-of-file
       (message "Summary unavailable for %s." term)
       nil))))

(defun wikisummary-toggle ()
  "Toggle wiki summary popup."
  (interactive)
  (when-let ((term (wikisummary-term)))
    (if (equal term wikisummary--term)
        (progn
          (posframe-hide wikisummary-buffer)
          (setq wikisummary--term nil))
      (setq wikisummary--term term)
      (apply #'wikisummary-popup term))))

(defun wikisummary-term ()
  "Extract wiki term and lang from the link at point."
  (when-let ((link (get-text-property (point) 'help-echo)))
    (save-match-data
      (pcase link
        ((rx "://" (group (+ (not (any "."))))
             ".wikipedia.org/wiki/"
             (group (+ (not (any "/" " "))))
             " ")
         (list (match-string 2 link)
               (match-string 1 link)))))))

(defun wikisummary-popup (term lang)
  (when-let ((json (wikisummary-query-api term lang)))
    (let-hash json
      (posframe-show wikisummary-buffer
                     :position (point)
                     :string
                     (with-temp-buffer
                       (insert .extract_html)
                       (shr-render-region (point-min) (point-max))
                       (buffer-substring-no-properties (point-min) (point-max)))
                     :background-color "#333333"
                     :foreground-color "#dcdccc"))))

(provide 'wikisummary)
;;; wikisummary.el ends here
