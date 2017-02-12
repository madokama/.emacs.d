;;; emoz-eww --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'emoz)
(require 'eww)

;;;###autoload
(defun emoz-eww ()
  "Open the current Firefox tab with `eww'."
  (interactive)
  (emoz-current-url
   (lambda (url)
     (save-excursion
       (eww url)))))

(provide 'emoz-eww)
;;; emoz-eww.el ends here
