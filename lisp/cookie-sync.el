;;; cookie-sync --- Synchronize curl cookies with browsers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'async)
(require 'url-curl)

(defvar csync-sqlite "sqlite3")

(defun csync--prepare-buffer ()
  (let ((buf (generate-new-buffer " *csync*")))
    (with-current-buffer buf
      (insert "# Netscape HTTP Cookie File\n# http://www.netscape.com/newsref/std/cookie_spec.html\n# This is a generated file!  Do not edit.\n\n"))
    buf))

(defun cookie-sync--internal (params)
  (let ((tmpbuf (csync--prepare-buffer)))
    (with-temp-buffer
      (apply #'call-process csync-sqlite nil t nil
             "-separator" " " params)
      (goto-char (point-min))
      (while (not (eobp))
        (seq-let (host path secure expire name value)
            (split-string (buffer-substring-no-properties (line-beginning-position)
                                                          (line-end-position))
                          " ")
          (with-current-buffer tmpbuf
            (insert
             (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
                     host
                     (if (string-prefix-p "." host) "TRUE" "FALSE")
                     path
                     (if (string= secure "1") "TRUE" "FALSE")
                     expire name value))))
        (goto-char (1+ (line-end-position)))))
    (let ((tmpfile (make-temp-file "csync"))
          (coding-system-for-write
           (if (eq system-type 'windows-nt) 'dos 'unix)))
      (with-current-buffer tmpbuf
        (write-region nil nil tmpfile))
      (kill-buffer tmpbuf)
      (rename-file tmpfile (url-curl-cookie) t))))

(defun cookie-sync (params)
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'cookie-sync)
      (cookie-sync--internal ',params))
   (lambda (_)
     (message "Cookies synced."))))

;;; Chrome

(defvar csync-profile-chrome
  ;; "~/AppData/Local/Vivaldi/User Data/Default/"
  )

(defun csync-params-chrome ()
  (list (expand-file-name "Cookies" csync-profile-chrome)
        "SELECT host_key, path, secure, expires_utc, name, value FROM cookies;"))

(defun cookie-sync-chrome ()
  "Sync cookies with Chrome."
  (interactive)
  (cookie-sync (csync-params-chrome)))

;;; Firefox interface

(require 'firefox)

(defun csync-params-firefox ()
  (unless (and firefox-profile-directory
               (file-directory-p firefox-profile-directory))
    (user-error "Set `firefox-profile-directory' to your profile directory"))
  (list (expand-file-name "cookies.sqlite" firefox-profile-directory)
        "SELECT host, path, isSecure, expiry, name, value FROM moz_cookies;"))

;;;###autoload
(defun cookie-sync-firefox ()
  "Sync cookies with Firefox."
  (interactive)
  (cookie-sync (csync-params-firefox)))

(provide 'cookie-sync)
;;; cookie-sync.el ends here
