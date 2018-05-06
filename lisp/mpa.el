;;; mpa --- $ mpv --no-video --shuffle -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'mpv-ipc)

(defgroup mpa ()
  "Audio player interface for mpv."
  :prefix "mpa-"
  :group 'multimedia)

(defcustom mpa-shuffle t
  "Special option to turn shuffle mode on or off."
  :type 'boolean)

(defcustom mpa-options nil
  "Command-line options to pass to mpv."
  :type '(repeat string))

(defvar mpa-ipc nil)

(defvar mpa-scrapers-alist nil)

;;;###autoload
(defun mpa-play (lst)
  (unless (process-live-p mpa-ipc)
    (setq mpa-ipc
          (mpv-ipc-watch `("--no-video" "--keep-open=yes"
                           ,@(when mpa-shuffle '("--shuffle"))
                           ,@mpa-options)
                         #'mpa--filter))
    (mpv-ipc:request-log mpa-ipc))
  (mpv-ipc:load mpa-ipc (mpa-scrape lst)))

(defun mpa--filter (tag json)
  (let-hash json
    (cond (.event
           (pcase .event
             ("log-message"
              (pcase .prefix
                ("ytdl_hook"
                 (message "[mpa] %s" (string-trim-right .text)))))))
          (.request_id
           (throw tag .data)))))

(defun mpa-interact ()
  (cl-block nil
    (while t
      (if-let* ((char (read-char "mpa>")))
          (progn
            (mpv-ipc:keypress mpa-ipc (string char))
            (when (memq char '(?q ?Q))
              (cl-return)))
        (cl-return)))))

(defun mpa-next ()
  (mpv-ipc:next mpa-ipc))

(defun mpa-pause ()
  (mpv-ipc:pause mpa-ipc))

(defun mpa-volume-down ()
  (mpv-ipc:volume mpa-ipc -2))

(defun mpa-volume-up ()
  (mpv-ipc:volume mpa-ipc 2))

(defun mpa-loop ()
  (mpv-ipc:loop mpa-ipc))

(defun mpa-stop ()
  (mpv-ipc:stop mpa-ipc))

(declare-function org-make-link-string "org")

(defun mpa-now-playing ()
  (when-let ((url (mpv-ipc:get-path mpa-ipc)))
    (let-hash (ytdl-get-json url)
      (insert "\n"
              (org-make-link-string url .title)
              "\n#+BEGIN_QUOTE\n"
              .description
              "\n#+END_QUOTE\n"))))

(defun mpa-scrape (urls)
  (mapcan (lambda (url)
            (or (seq-some (pcase-lambda (`(,pat . ,fn))
                            (when (string-match-p pat url)
                              (with-temp-buffer
                                (call-process "curl" nil t nil "-s" url)
                                (goto-char (point-min))
                                (save-match-data
                                  (funcall fn)))))
                          mpa-scrapers-alist)
                (list url)))
          urls))

(defun mpa-scrape-pianeys ()
  (cl-loop while (re-search-forward "<td class=\"movie\"><a href=\"\\(.+?\\)\""
                                    nil t)
           collect (match-string 1)))

(add-to-list 'mpa-scrapers-alist '("\\<pianeys\\.com" . mpa-scrape-pianeys))

(provide 'mpa)
;;; mpa.el ends here
