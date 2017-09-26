;;; ytdl-lib --- Youtub-dl utilities  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'json)
(require 'spinner)

(defvar ytdl--debug nil)

(defvar ytdl-command "youtube-dl")

(defvar ytdl-format/lq
  (string-join '("worstvideo[ext=webm]"
                 "worstvideo[ext=mp4]"
                 "worst[ext=webm]"
                 "worst[protocol!=m3u8][protocol!=m3u8_native]"
                 "worst[format_id!=hls-meta]")
               "/")
  "LQ format selectors for thumbnails extraction.
Preferring webm for cleaner cuts at keyframes.")

(defvar ytdl-format/hd
  (string-join
   '("bestvideo[height<=?1080][protocol!=http_dash_segments]+bestaudio[protocol!=http_dash_segments]"
     "best[height<=?1080]")
   "/"))

(defun ytdl-detach-process (cmd)
  (if (executable-find "setsid")
      (cons "setsid" cmd)
    cmd))

;;;###autoload
(defun ytdl-get-json (url &optional fmt)
  (with-temp-buffer
      (apply #'call-process ytdl-command nil t nil
             `("--no-warnings" "-J" "--flat-playlist" "--no-playlist"
                               ,@(when fmt
                                   (list (format "--format=%s" fmt)))
                               "--no-mark-watched" "--no-color"
                               "--" ,url))
    (goto-char (point-min))
    (json-read)))

(defmacro ytdl--define-json-refs (&rest keys)
  `(progn
     ,@(mapcar (lambda (key)
                 (let ((funsym (intern (format "ytdl-json/%s" key)))
                       (keysym (intern (format ".%s" key))))
                   `(defun ,funsym (json)
                      (let-alist json ,keysym))))
               keys)))

(ytdl--define-json-refs
 extractor id title webpage_url duration start_time end_time thumbnail
 formats requested_formats format_note url ext
 ;; for playlists
 _type entries)

(defmacro ytdl--define-extractors (&rest extractors)
  `(progn
     ,@(mapcar (lambda (extractor)
                 (let ((funsym (intern (format "ytdl-%s-p" extractor)))
                       (extstr (symbol-name extractor)))
                   `(defun ,funsym (json)
                      (string= (ytdl-json/extractor json) ,extstr))))
               extractors)))

(ytdl--define-extractors youtube twitter)

(defun ytdl-requested-formats (json)
  (or (let-alist json
        (mapcar #'identity .requested_formats)) ;vec->list
      (list json)))

(defun ytdl-requested-video (json)
  (car (ytdl-requested-formats json)))

(defun ytdl-video-file (json)
  (format "%s.%s"
          (expand-file-name (md5 (ytdl-json/webpage_url json))
                            temporary-file-directory)
          (ytdl-json/ext (ytdl-requested-video json))))

(defun ytdl-download-video (json callback)
  (ytdl-download (ytdl-requested-video json)
                 (ytdl-video-file json)
                 callback))

(defun ytdl--dl-hls (fmt file)
  (let-alist fmt
    (list "ffmpeg" "-y"
          "-headers" (mapconcat (pcase-lambda (`(,k . ,v))
                                  (format "%s: %s\r\n" k v))
                                .http_headers "")
          "-i" .url
          "-c" "copy"
          "-f" "mp4"
          "-bsf:a" "aac_adtstoasc"
          file)))

(defun ytdl--dl-http (fmt file)
  (let ((script (make-temp-file "ytdl")))
    (with-temp-file script
      (insert (format "wget --no-check-certificate -nv -O %s \\\n" file))
      (let-alist fmt
        (cl-loop for (k . v) in .http_headers
                 do (insert (format "  --header=\"%s: %s\" \\\n"
                                    k
                                    (shell-quote-argument v))))
        (if (string= .protocol "http_dash_segments")
            (let ((url .fragment_base_url))
             (cl-loop for i downfrom (length .fragments)
                      for frag across .fragments
                      do (insert
                          (format "  %S %s\n"
                                  (concat url (alist-get 'path frag))
                                  (if (= i 1) "" "\\")))))
          (insert (format "  %S\n" .url))))
      (insert "rm $0\n"))
    (list shell-file-name script)))

(defun ytdl-protocol-hls-p (json)
  (let-alist json
    (string-match-p "\\`m3u8" .protocol)))

(defun ytdl-download (fmt file callback)
  (if (and (file-exists-p file)
           (> (file-attribute-size (file-attributes file)) 0))
      (funcall callback file)
    (process-put
     (make-process :name "ytdl-dl"
                   :buffer (generate-new-buffer "*ytdl-dl*")
                   :command (cond ((ytdl-protocol-hls-p fmt)
                                   (ytdl--dl-hls fmt file))
                                  (t (ytdl--dl-http fmt file)))
                   :sentinel (lambda (proc signal)
                               (funcall (process-get proc :spinner))
                               (if (zerop (process-exit-status proc))
                                   (progn
                                     (kill-buffer (process-buffer proc))
                                     (funcall callback file))
                                 (pop-to-buffer (process-buffer proc))
                                 (error "Download failed: %s" signal))))
     :spinner (spinner-start))))

(defun ytdl--format (json cmp pred)
  (let ((fmts (ytdl-json/formats json)))
    (or (seq-find pred
                  (seq-sort-by (lambda (fmt)
                                 (alist-get 'height fmt))
                               cmp
                               (seq-filter
                                (lambda (fmt)
                                  (let-alist fmt
                                    (and .height
                                         ;; reject m3u8 format dumped
                                         ;; by dailymotion, vimeo etc.
                                         (or (not
                                              (string-match-p "^m3u8" .protocol))
                                             (ytdl-twitter-p json)))))
                                fmts)))
        (elt fmts 0))))

(defun ytdl-format-mq (json)
  (ytdl--format json
                #'>
                (lambda (fmt)
                  (let-alist fmt
                    (and (<= .height 480)
                         (if (ytdl-youtube-p json)
                             ;; We choose webm for the sake of instant seeks
                             (string= .ext "webm")
                           t))))))

(defun ytdl-format-sd (json)
  (ytdl--format json
                #'<
                (lambda (fmt)
                  (let-alist fmt
                    (if (ytdl-youtube-p json)
                        (and (> .height 144)
                             (not (member .ext '("3gp" "flv"))))
                      (>= .height 144))))))

(provide 'ytdl-lib)
;;; ytdl-lib.el ends here
