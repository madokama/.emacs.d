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



;;; Major mode

(defvar mpa-buffer "*mpa*")

(defvar-local mpa-frame nil)

(defvar mpa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<SPC>") #'mpa-pause)
    (define-key map (kbd "+") #'mpa-volume-up)
    (define-key map (kbd "-") #'mpa-volume-down)
    (define-key map (kbd "l") #'mpa-loop)
    (define-key map (kbd ".") #'mpa-stop)
    (define-key map (kbd "q") #'mpa-quit-frame)
    map))

(define-derived-mode mpa-mode special-mode "Mpv audio"
  "Major mode to control mpv status."
  (buffer-disable-undo)
  (setq cursor-type nil
        mode-line-format nil
        header-line-format nil))

(defun mpa-popup-frame ()
  (let ((current-frame (window-frame)))
    (with-current-buffer (get-buffer-create mpa-buffer)
      (if (frame-live-p mpa-frame)
          (raise-frame mpa-frame)
        (let ((after-make-frame-functions nil))
          (unless (derived-mode-p 'mpa-mode)
            (mpa-mode))
          (setq mpa-frame
                (make-frame
                 `((fullscreen . nil)
                   (background-color . "#dcdccc")
                   (foreground-color . "#333333")
                   (height . 1)
                   (min-width . 0)
                   (min-height . 0)
                   (border-width . 0)
                   (internal-border-width . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (line-spacing . 0)
                   (unsplittable . t)
                   (no-other-frame . t)
                   (undecorated . t)
                   (minibuffer . nil)))))))
    (select-frame-set-input-focus current-frame)))

(defvar mpa-now-playing-function #'mpa-now-playing-default)

(defun mpa-now-playing-default (title)
  (message "[mpa] %s" title))

(defun mpa-now-playing-frame (title)
  (mpa-popup-frame)
  (with-current-buffer mpa-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title)
      (set-frame-width mpa-frame
                       (min (* (frame-char-width mpa-frame)
                               (string-width title))
                            (display-pixel-width))
                       nil t)
      (set-frame-position mpa-frame -1 -1))))

(defun mpa-quit-frame ()
  "Quit mpa frame."
  (interactive)
  (mpa-stop)
  (when mpa-frame
    (delete-frame mpa-frame)))


;;;

(declare-function ffap-url-p "ffap")

(defun mpa--suggested-uris ()
  (delete-dups
   (cl-delete-if-not (lambda (x)
                       (and (stringp x)
                            (or (ffap-url-p x) (file-exists-p x))))
                     (list (gui-get-selection 'CLIPBOARD)
                           (car kill-ring)))))

;;;###autoload
(defun mpa-play (lst)
  "Play list of audio specified by LST."
  (interactive
   (list (list (completing-read "URI: " (mpa--suggested-uris)))))
  (unless (process-live-p mpa-ipc)
    (setq mpa-ipc
          (mpv-ipc-watch `("--no-video" "--keep-open=yes"
                                        ,@(when mpa-shuffle '("--shuffle"))
                                        ,@mpa-options)
                         #'mpa--filter))
    (mpv-ipc:request-log mpa-ipc))
  (mpv-ipc:load mpa-ipc (mpa-scrape lst)))

(defun mpa--filter (json)
  (let-hash json
    (pcase .event
      ("log-message"
       (pcase .prefix
         ("ytdl_hook"
          (save-match-data
            (let ((text (string-trim-right .text)))
              ;; NOTE you need to hack ytdl_hook.lua to output this line.
              (when (string-match (rx bos "Now playing " (group (+ print)))
                                  text)
                (funcall mpa-now-playing-function (match-string 1 text)))))))))))

(defun mpa-interact ()
  (mpv-ipc-interact mpa-ipc))

(defun mpa-next ()
  "Play next song."
  (interactive)
  (mpv-ipc:next mpa-ipc))

(defun mpa-pause ()
  "Pause playback."
  (interactive)
  (mpv-ipc:pause mpa-ipc))

(defun mpa-volume-down ()
  "Lower volume."
  (interactive)
  (mpv-ipc:volume mpa-ipc -2))

(defun mpa-volume-up ()
  "Raise volume."
  (interactive)
  (mpv-ipc:volume mpa-ipc 2))

(defun mpa-loop ()
  "Loop current song."
  (interactive)
  (mpv-ipc:loop mpa-ipc))

(defun mpa-stop ()
  "Stop playback."
  (interactive)
  (mpv-ipc:stop mpa-ipc))

(declare-function org-make-link-string "org")

(defun mpa-org-quote ()
  (when-let ((url (mpv-ipc:get-path mpa-ipc)))
    (let-hash (ytdl-get-json url)
      (insert "\n"
              (org-make-link-string url .title)
              "\n#+BEGIN_QUOTE\n"
              .description
              "\n#+END_QUOTE\n"))))


;;; Web scrapers

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
