;;; cookie-sync --- Synchronize curl cookies with browsers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'url-curl)

(defvar csync-sqlite "sqlite3")

(defvar csync--midline nil)

(defun csync--prepare-buffer ()
  (let ((buf (generate-new-buffer " *csync*")))
    (with-current-buffer buf
      (insert "# Netscape HTTP Cookie File\n# http://www.netscape.com/newsref/std/cookie_spec.html\n# This is a generated file!  Do not edit.\n\n"))
    buf))

(defun csync--filter (proc output)
  (when csync--midline
    (setq output (concat csync--midline output)
          csync--midline nil))
  (let ((start 0)
        (len (length output)))
    (cl-block nil
      (with-current-buffer (process-buffer proc)
        (while (< start len)
          (let ((cookie (and (string-match (rx (+ nonl)) output start)
                             (match-string 0 output))))
            (if (string-match "\n" output start)
                (progn
                  (setq start (match-end 0))
                  (seq-let (host path secure expire name value)
                      (split-string cookie " ")
                    (insert
                     (format "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
                             host
                             (if (string-prefix-p "." host) "TRUE" "FALSE")
                             path
                             (if (string= secure "1") "TRUE" "FALSE")
                             expire name value))))
              ;; The batch ends prematurely.
              (setq csync--midline cookie)
              (cl-return))))))))

(defvar url-cookie-file)
(declare-function url-do-setup "url")

(defun csync--sentinel (proc signal)
  (let ((buf (process-buffer proc)))
    (if (zerop (process-exit-status proc))
        (let ((tmp (make-temp-file "csync"))
              (coding-system-for-write
               (if (eq system-type 'windows-nt) 'dos 'unix)))
          (with-current-buffer buf
            (write-region nil nil tmp))
          (rename-file tmp (url-curl-cookie) t)
          (kill-buffer buf)
          (message "Cookies synced."))
      (pop-to-buffer buf)
      (error "Sync failed: %s" signal))))

(defun cookie-sync (params)
  ;; Not thread safe.
  (make-process :name "csync-sqlite"
                :buffer (csync--prepare-buffer)
                :command (cl-list* csync-sqlite "-separator" " " params)
                :filter #'csync--filter
                :sentinel #'csync--sentinel))

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

(defvar csync-profile-firefox nil)

(defun csync-params-firefox ()
  (unless (and csync-profile-firefox
               (file-directory-p csync-profile-firefox))
    (user-error "Set `csync-profile-firefox' to your profile directory"))
  (list (expand-file-name "cookies.sqlite" csync-profile-firefox)
        "SELECT host, path, isSecure, expiry, name, value FROM moz_cookies;"))

;;;###autoload
(defun cookie-sync-firefox ()
  "Sync cookies with Firefox."
  (interactive)
  (cookie-sync (csync-params-firefox)))

(provide 'cookie-sync)
;;; cookie-sync.el ends here
