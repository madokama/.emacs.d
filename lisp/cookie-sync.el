;;; cookie-sync --- Synchronize curl cookies with browsers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'async)

(defvar cookie-sync-sqlite "sqlite3")

(defvar cookie-sync-browsers
  '(cookie-sync-firefox-params cookie-sync-chrome-params))

(defun cookie-sync--prepare-buffer ()
  (with-current-buffer (generate-new-buffer " *csync*")
    (insert "# Netscape HTTP Cookie File\n# http://www.netscape.com/newsref/std/cookie_spec.html\n# This is a generated file!  Do not edit.\n\n")
    (current-buffer)))

(defun cookie-sync--internal (params cookie)
  (let ((tmpbuf (cookie-sync--prepare-buffer)))
    (unwind-protect
         (with-temp-buffer
           (unless (zerop (apply #'call-process cookie-sync-sqlite nil t nil
                                 "-separator" " " params))
             (error "Cookie sync failed"))
           (goto-char (point-min))
           (while (not (eobp))
             (pcase (split-string (buffer-substring (line-beginning-position)
                                                    (line-end-position))
                                  " ")
               (`(,host ,path ,secure ,expire ,name ,value)
                 (with-current-buffer tmpbuf
                   (insert
                    (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
                            host
                            (if (string-prefix-p "." host) "TRUE" "FALSE")
                            path
                            (if (string= secure "1") "TRUE" "FALSE")
                            expire name value)))))
             (goto-char (1+ (line-end-position))))
           (let ((tmpfile (make-temp-file "csync"))
                 (coding-system-for-write
                  (if (eq system-type 'windows-nt) 'dos 'unix)))
             (with-current-buffer tmpbuf
               (write-region nil nil tmpfile))
             (rename-file tmpfile cookie t))
           t)
      (kill-buffer tmpbuf))))

(defun cookie-sync-params-list (hosts)
  (delq nil
        (mapcar (lambda (f)
                  (ignore-errors (funcall f hosts)))
                cookie-sync-browsers)))

(defun cookie-sync-try (params-list cookie)
  (cl-find-if (lambda (params)
                (ignore-errors (cookie-sync--internal params cookie)))
              params-list))

(defun cookie-sync (&optional hosts cookie)
  "Synchronize curl cookies with external browsers.
If the optional first argument HOSTS is specified, only cookies
exactly matching those names will be synced.

The second argument COOKIE specifies the file to which the
cookies will be written."
  (interactive)
  (let ((cookie (or cookie (url-curl-cookie)))
        (params-list (cookie-sync-params-list hosts)))
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        (require 'cookie-sync)
        (cookie-sync-try ',params-list ,cookie))
     (lambda (_)
       (message "Cookies synced.")))))

(defun cookie-sync--match-host (key hosts)
  (if hosts
      (format " WHERE %s"
              (mapconcat (lambda (host)
                           (format "%s = %S" key host))
                         hosts " OR "))
    ""))

;;; Chrome

(defvar cookie-sync-chrome-directory
  ;; "~/AppData/Local/Vivaldi/User Data/Default/"
  )

(defun cookie-sync-chrome-params (&optional hosts)
  (list (expand-file-name "Cookies" cookie-sync-chrome-directory)
        (format "SELECT host_key, path, secure, expires_utc, name, value FROM cookies%s;"
                (cookie-sync--match-host "host_key" hosts))))

;;; Firefox interface

(require 'firefox)

(defun cookie-sync-firefox-params (&optional hosts)
  (unless (and firefox-profile-directory
               (file-directory-p firefox-profile-directory))
    (user-error "Set `firefox-profile-directory' to your profile directory"))
  (list (expand-file-name "cookies.sqlite" firefox-profile-directory)
        (format "SELECT host, path, isSecure, expiry, name, value FROM moz_cookies%s;"
                (cookie-sync--match-host "host" hosts))))

(provide 'cookie-sync)
;;; cookie-sync.el ends here
