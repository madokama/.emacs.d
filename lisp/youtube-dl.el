;;; youtube-dl --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (require 'cl-lib)
                                        ; for cl-caddr
(require 'seq)
(require 'dash)
(require 'dash-functional)
(require 'ffap)
(require 'counsel)
(require 'progchar)
(require 'ytdl-lib)

(defvar youtube-dl-command/dl
  "youtube-dl %s"
  "Command line template for downloading videos.")

(defvar youtube-dl-command/play
  "mpv %s"
  "Command line template for playing videos.")

(defvar youtube-dl-format
  "best[height<=?720]"
  "Preferred download format.")

(defvar youtube-dl-directory
  "~/Downloads/vids/"
  "Download directory.")

;;;

(defun youtube-dl--raw-formats (url)
  (with-temp-buffer
    (call-process-shell-command
     (format "youtube-dl -F %s" (shell-quote-argument url)) nil t)
    (search-backward "format code" nil t)
    (forward-line 1)
    (delq nil
          (mapcar (lambda (line)
                    (when (string-match
                           "\\`\\(\\sw+\\) +\\(\\sw+\\) +\\(.+\\)\\'"
                           line)
                      (list (match-string 1 line)
                            (match-string 2 line)
                            (match-string 3 line))))
                  (split-string (buffer-substring (point) (point-max)) "\n" t)))))

(defun youtube-dl--formats (url)
  (youtube-dl--analyze-formats (youtube-dl--raw-formats url)))

(defun youtube-dl--prefer-mp4 (formats)
  (seq-let (other mp4)
      (-separate (lambda (fmt)
                   (string-match-p "\\`\\(?:flv\\|webm\\|3gp\\)\\'" (cadr fmt)))
                 formats)
    (or mp4 other)))

(defun youtube-dl--analyze-formats (formats)
  (seq-let (partial complete)
      (-separate (lambda (fmt)
                   (string-match-p "\\(?:audio\\|video\\) only" (caddr fmt)))
                 (youtube-dl--prefer-mp4 formats))
    (let ((complete-height
           (mapcar #'youtube-dl--video-height complete)))
      (append complete
              ;; Remove duplicates
              (seq-remove (lambda (fmt)
                            (member (youtube-dl--video-height fmt)
                                    complete-height))
                          (youtube-dl--merge partial))))))

(defun youtube-dl--video-height (fmt)
  (let ((desc (caddr fmt)))
    (and (or (string-match "\\`\\([0-9]+\\)p" desc)
             (string-match "\\`[0-9]+x\\([0-9]+\\)" desc))
         (string-to-number (match-string 1 desc)))))

(defun youtube-dl--merge (partial)
  (seq-let (audio video)
      (-separate (lambda (fmt)
                   (string-match-p "audio" (caddr fmt)))
                 partial)
    (cl-flet ((quality-audio (bitrate)
                (-filter (lambda (fmt)
                           (when (string-match "@\\([0-9]+\\)k" (caddr fmt))
                             (>= (string-to-number (match-string 1 (caddr fmt)))
                                 bitrate)))
                         audio))
              (merge (vfmt afmt)
                (cons (concat (car vfmt) "+" (car afmt)) (cdr vfmt))))
      (let* ((fallback (car audio))
             (audio-normal (or (car (quality-audio 128)) fallback))
             (audio-best (or (-last-item (quality-audio 256)) audio-normal)))
        (mapcar (lambda (vfmt)
                  (merge vfmt
                         (if (< (youtube-dl--video-height vfmt) 1080)
                             audio-normal
                           audio-best)))
                video)))))

(defun youtube-dl-query-format (url)
  (completing-read
   "Video formats: "
   (mapcar (lambda (fmt)
             (cons (and (string-match "\\`\\sw+" (caddr fmt))
                        (match-string 0 (caddr fmt)))
                   (car fmt)))
           (youtube-dl--formats url))
   nil t))

(defun youtube-dl--qualify-command (template fmt)
  (or (and fmt
           (cond ((string-match "\\_<youtube-dl\\_>" template)
                  (replace-match (concat "\\& -f "
                                         fmt
                                         (if (string-match-p "\\+" fmt)
                                             " --merge-output-format mp4"
                                           ""))
                                 t nil template))
                 ((string-match "\\_<mpv\\_>" template)
                  (replace-match (format "\\& --ytdl-format %s" fmt)
                                 t nil template))))
      template))

(defun youtube-dl--command (template url query-fmt)
  (format (youtube-dl--qualify-command template
                                       (if query-fmt
                                           (youtube-dl-query-format url)
                                         youtube-dl-format))
          url))

(defun youtube-dl--command-list (template url query-fmt)
  (split-string (youtube-dl--command template url query-fmt) " "))

(defun youtube-dl--execute (template url &optional query-fmt)
  (let ((url (or url (youtube-dl-url))))
    (process-put
     (make-process :name "youtube-dl"
                   :buffer (generate-new-buffer "*ytdl-exec*")
                   :command
                   (ytdl-detach-process
                    (youtube-dl--command-list template url query-fmt))
                   :noquery t
                   :sentinel
                   (lambda (proc signal)
                     (let ((stop (process-get proc :spinner)))
                       (if (string-match-p "finished" signal)
                           (progn
                             (kill-buffer (process-buffer proc))
                             (funcall stop))
                         (message "ERROR [youtube-dl] %s" signal)
                         (pop-to-buffer (process-buffer proc))
                         (funcall stop 'failed)))))
     :spinner (progchar url))))

(declare-function org-ytdl-link-at-point "org-ytdl")

(defvar youtube-dl--cleanup-string #'identity)
(with-eval-after-load 'ivy
  (setq youtube-dl--cleanup-string #'ivy-cleanup-string))

(defun youtube-dl--normalize-url (url)
  ;; This is to handle cases where `shr-url' and its text
  ;; representation point to the same url with different schemes.
  (let ((urlobj (url-generic-parse-url url)))
    (if (string= "https" (url-type urlobj))
        (progn
          (setf (url-type urlobj) "http")
          (url-recreate-url urlobj))
      url)))

(defvar eww-data)
(defvar elfeed-show-entry)
(defvar ffedit-video-path)
(declare-function elfeed-entry-link "ext:elfeed-db")
(declare-function eww-current-url "eww")

(defun youtube-dl--visiting-url ()
  (cond ((derived-mode-p 'org-mode)
         (org-ytdl-link-at-point))
        ((derived-mode-p 'eww-mode)
         (eww-current-url))
        ((derived-mode-p 'elfeed-show-mode)
         (elfeed-entry-link elfeed-show-entry))
        ((derived-mode-p 'ffedit-mode)
         ffedit-video-path)))

;;;###autoload
(defun youtube-dl-url (&rest candidates)
  (completing-read "Video URL: "
                   (delete-dups
                    (mapcar (-compose youtube-dl--cleanup-string
                                      #'youtube-dl--normalize-url)
                            (let ((ffap-url-regexp (concat "^" ffap-url-regexp)))
                              (cl-remove-if-not
                               #'ffap-url-p
                               (append candidates
                                       (list (get-text-property (point) 'shr-url)
                                             (thing-at-point 'url)
                                             (gui-get-selection 'CLIPBOARD)
                                             (car kill-ring)
                                             (youtube-dl--visiting-url)))))))))

(defun youtube-dl--dir ()
  (unless (file-directory-p youtube-dl-directory)
    (make-directory youtube-dl-directory t))
  youtube-dl-directory)

;;; Entry points

;;;###autoload
(defun youtube-dl/dl (url &optional query-fmt)
  (interactive (list nil current-prefix-arg))
  (let ((default-directory (youtube-dl--dir)))
    (youtube-dl--execute youtube-dl-command/dl url query-fmt)))

;;;###autoload
(defun youtube-dl/play (url &optional query-fmt)
  (interactive (list nil current-prefix-arg))
  (youtube-dl--execute youtube-dl-command/play url query-fmt))

(provide 'youtube-dl)
;;; youtube-dl.el ends here
