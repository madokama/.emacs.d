;;; firefox --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'async)
(require 'json)

(defgroup firefox nil
  "Firefox utilities."
  :prefix "firefox-"
  :group 'external
  :group 'comm)

(defvar firefox-profile-directory nil)

(defun firefox-mozlz4 (file)
  (with-temp-buffer
    (insert "import sys, lz4\n"
            "out = sys.stdout\n"
            (format "f = open(%S, 'rb')\n" file)
            "magic = f.read(8)\n"
            "out.write(lz4.block.decompress(f.read()).decode('utf-8'))\n"
            "f.close()\n"
            "out.flush()\n")
    (call-process-region (point-min) (point-max) "python3" t t nil)
    (goto-char (point-min))
    (json-read)))

(defun firefox-session-tabs (json)
  (cl-flet ((normalize (url)
              (if (string-match-p "\\`moz-extension://" url)
                  (car
                   (assoc-default "url" (url-parse-query-string url)
                                  #'string=))
                url)))
    (thread-last json
      (alist-get 'windows)
      (mapcan (lambda (window)
                (mapcar (lambda (tab)
                          (let-alist tab
                            (cons .lastAccessed
                                  (let-alist (aref .entries (1- .index))
                                    (cons (normalize .url) .title)))))
                        (alist-get 'tabs window))))
      (seq-sort-by #'car #'>))))

(defun firefox-tabs (callback)
  ;; Reference: https://unix.stackexchange.com/a/389360
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'firefox)
      (firefox-session-tabs
       (firefox-mozlz4
        (expand-file-name "sessionstore-backups/recovery.jsonlz4"
                          ,firefox-profile-directory))))
   callback))

(provide 'firefox)
;;; firefox.el ends here
