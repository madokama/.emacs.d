;;; recentb-mpv --- Mpv history interface with Ivy -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(defvar recentb-mpv nil)

(recentb-define-mode mpv
  :history recentb-mpv-history
  :candidate recentb-mpv-candidate)

(defvar recentb-mpv--history
  (expand-file-name ".history" (getenv "MPV_HOME")))

(defun recentb-mpv-save-history (history)
  (let ((tmp (make-temp-file "mpvhist")))
    (with-temp-file tmp
      (let ((buf (current-buffer)))
        (cl-loop for item in history
                 do (pp item buf))))
    (rename-file tmp recentb-mpv--history t)))

(defun recentb-mpv-truncate-history (history)
  (when (> (length history) recentb-max-saved-items)
    (recentb-mpv-save-history
     (setq history
           (seq-drop (cl-delete-duplicates history
                                           :test (lambda (a b)
                                                   (string= (plist-get a :url)
                                                            (plist-get b :url))))
                     (- (length history)
                        recentb-max-saved-items)))))
  history)

(defun recentb-mpv-history ()
  (prog1 (or recentb-mpv
             (recentb-mpv-read-history))
    (recentb-mpv-watch-history)))

(defun recentb-mpv-candidate (item)
  (let ((url (plist-get item :url))
        (title (plist-get item :title)))
    (propertize (format "*mpv:%s*" (if (string= title "nil") url title))
                'recentb (list 'youtube-dl/play
                               (if (ffap-url-p url)
                                   url
                                 (format "ytdl://%s" url))))))



(require 'filenotify)

(defvar recentb-mpv-watch-desc nil)

(defun recentb-mpv-history-watcher (event)
  (pcase event
    (`(,_ changed ,_) (recentb-mpv-read-history))
    (`(,_ stopped ,_) (recentb-mpv-watch-history))))

(defun recentb-mpv-watch-history ()
  (unless (file-notify-valid-p recentb-mpv-watch-desc)
    (setq recentb-mpv-watch-desc
          (file-notify-add-watch recentb-mpv--history '(change)
                                 #'recentb-mpv-history-watcher))))

(defun recentb-mpv-read-history ()
  (when (file-exists-p recentb-mpv--history)
    (let ((visitedp (get-file-buffer recentb-mpv--history)))
      (with-current-buffer (find-file-noselect recentb-mpv--history)
        (when visitedp
          (revert-buffer t t t))
        (goto-char (point-min))
        (prog1 (setq recentb-mpv
                     (nreverse
                      (recentb-mpv-truncate-history
                       (cl-loop for item = (ignore-errors (read (current-buffer)))
                                while item collect item))))
          (unless visitedp
            (kill-buffer (current-buffer))))))))

(provide 'recentb-mpv)
;;; recentb-mpv.el ends here
