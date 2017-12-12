;;; ffedit --- Major mode for editing videos -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'ffmpeg-join)
(require 'ffprobe)
(require 'ytdl-lib)
(require 'ffap)

(defvar ffedit-debug nil)

(defvar ffedit-find-video-function #'ffedit-find-video-default)
(defvar ffedit-preview-params/mpv '("--no-resume-playback"))

(defvar-local ffedit-video-path nil)
(defvar-local ffedit-video-info nil)
(defvar-local ffedit-ffmpeg-log nil)
(defvar-local ffedit-sess-file nil)
(defvar ffedit--session-name ".sess")
(defvar-local ffedit--pts-list nil)
(defvar-local ffedit--editable nil)

(defmacro ffedit--assert-buffer ()
  '(unless (derived-mode-p 'ffedit-mode)
    (user-error "Not in ffedit mode")))

(defmacro ffedit-defcommand (name &rest body)
  (declare (indent 1))
  (let ((body
         (if (= (length body) 1)
             `(call-interactively #',(car body))
           (cons 'progn body))))
    `(defun ,(intern (format "ffedit-%s" name)) ()
       (interactive)
       (ffedit--assert-buffer)
       (unless ffedit--editable
         (user-error "Not editable yet"))
       (let ((buffer-read-only nil))
         ,body))))

(defun ffedit--temp-dir (file)
  (let ((default-directory temporary-file-directory)
        (path (md5 file)))
    (unless (file-directory-p path)
      (mkdir path))
    (expand-file-name path)))

(defun ffedit--segment (p)
  (when-let* ((pts (get-text-property p 'pts)))
    (cons pts
          (get-text-property p 'end-pts))))

(defun ffedit--reduce-segments (segs)
  (nreverse
   (seq-reduce (lambda (acc elt)
                 (if (= (car elt) (cdar acc))
                     (cons (cons (caar acc) (cdr elt))
                           (cdr acc))
                   (cons elt acc)))
               (cdr segs)
               (list (car segs)))))

(defun ffedit--parse-timebase ()
  (with-current-buffer (find-file-noselect ffedit-ffmpeg-log)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "config in time_base: *\\([0-9/]+\\)" nil t)
        (match-string 1)))))

(defun ffedit--update-timebase (time-base)
  ;; Ffprobe and ffmpeg may report different values on `time_base'.
  ;; Prefer the latter.
  (let-hash ffedit-video-info
    (unless (string= time-base .time_base)
      (message "[FF]Updating timebase %s->%s" time-base .time_base)
      (puthash "time_base" time-base ffedit-video-info))))

(defun ffedit--thumbs-pre (video thumbs-buf)
  (with-current-buffer thumbs-buf
    (let-alist video
      (let ((canon (or .canonical-path .path)))
        (ffedit-mode)
        (setq default-directory (ffedit--temp-dir canon))
        (setq ffedit-video-path canon
              ffedit-video-info (ffprobe-video-info .path)
              ffedit-ffmpeg-log (expand-file-name
                                 (md5 (format "%s%s"
                                              canon
                                              (file-attribute-size
                                               (file-attributes canon)))))
              ffedit-sess-file (expand-file-name ffedit--session-name))))))

(defun ffedit--thumbs-post (thumbs-buf frame)
  (with-selected-frame frame
    (with-current-buffer thumbs-buf
      (ffedit--update-timebase (ffedit--parse-timebase))
      (unless (file-exists-p ffedit-sess-file)
        (ffedit-save-session))
      (setq buffer-undo-list nil
            ffedit--editable t)
      (when (= (point) (point-max))
        (goto-char (point-min)))
      (pop-to-buffer thumbs-buf))))

(defun ffedit--add-thumb (thumbs-buf thumb pts)
  (with-current-buffer thumbs-buf
    (let ((buffer-read-only nil))
      (when-let* ((prev-pts (car ffedit--pts-list)))
        (when (equal prev-pts (get-text-property (1- (point-max)) 'pts))
          (put-text-property (1- (point-max)) (point-max) 'end-pts pts)))
      (push pts ffedit--pts-list)
      (when thumb
        (save-excursion
          (goto-char (point-max))
          (insert
           (propertize " "
                       'display (create-image thumb nil nil :relief 2 :margin 2)
                       'pts pts)))))))

(defun ffedit--add-thumbs (thumbs-buf &optional pos)
  (goto-char (or pos (point-min)))
  (while (search-forward-regexp
          "^\\[Parsed_showinfo.+?\\] n:[[:space:]]*\\([0-9]+\\).+?pts:[[:space:]]*\\([0-9]+\\)" nil t)
    (ffedit--add-thumb thumbs-buf
                       (expand-file-name
                        (format "%s.jpg"
                                (1+ (string-to-number (match-string 1)))))
                       (string-to-number (match-string 2)))))

(defun ffedit-generate-thumbs (video &optional refresh)
  (let-alist video
    (let ((thumbs-buf (generate-new-buffer (format "*%s*" .title)))
          (frame (window-frame)))
      (ffedit--thumbs-pre video thumbs-buf)
      (if (and (not refresh)
               (or (ffedit-load-session thumbs-buf)
                   (ffedit-load-cache thumbs-buf)))
          (ffedit--thumbs-post thumbs-buf frame)
        (let ((default-directory
               (buffer-local-value 'default-directory thumbs-buf)))
          (make-process :name "ffmpeg-iframes"
                        :buffer (generate-new-buffer "*ffmpeg-iframes*")
                        :command (list "ffmpeg" "-nostats"
                                       ;; "-debug_ts"
                                       ;; "-fdebug" "ts"
                                       "-i" .path
                                       "-vf" "select=eq(pict_type\\,I),showinfo"
                                       "-vsync" "2"
                                       "-s" "160x90"
                                       "-f" "image2"
                                       "-y" "%d.jpg")
                        :filter
                        (lambda (proc str)
                          (when (buffer-live-p (process-buffer proc))
                            (with-current-buffer (process-buffer proc)
                              (goto-char (process-mark proc))
                              (insert str)
                              (save-mark-and-excursion
                                (ffedit--add-thumbs thumbs-buf
                                                    (process-mark proc)))
                              (set-marker (process-mark proc) (point-max)))))
                        :sentinel
                        (lambda (proc _)
                          (if (zerop (process-exit-status proc))
                              (progn
                                (with-current-buffer (process-buffer proc)
                                  (write-region
                                   (point-min) (point-max)
                                   (buffer-local-value 'ffedit-ffmpeg-log
                                                       thumbs-buf))
                                  (ffedit--thumbs-post thumbs-buf frame))
                                (kill-buffer (process-buffer proc)))
                            (pop-to-buffer (process-buffer proc))
                            (error "FFMpeg thumbs failed")))))))))



(defun ffedit--edl-escape (url)
  (format "%%%d%%%s" (string-bytes url) url))

(defun ffedit-edl-segment (path seg)
  (concat (ffedit--edl-escape path)
          (pcase seg
            (`(,start . ,end)
              (concat (format ",start=%s" start)
                      (and end
                           (format ",length=%s" (- end start))))))))

(defun ffedit--dash-segments (fmt segs)
  (let-hash fmt
    (cl-loop with starts = nil
             for d across .timeline
             for u across .segment_urls
             while segs
             if (and (> (+ total-time d) (caar segs))
                     (or (null (cdar segs))
                         (< total-time (cdar segs))))
               do (let ((offset (- total-time (caar segs))))
                    (when (<= offset 0)
                      (push (/ (+ edit-time (abs offset)) .timescale) starts))
                    (when (and (cdar segs)
                               (>= (+ total-time d) (cdar segs)))
                      (setq segs (cdr segs))))
               and collect u into ul
               and sum d into edit-time
             sum d into total-time
             finally return (cons ul (nreverse starts)))))

(defun ffedit--dash-download (fmt file dsegs)
  ;; TODO: Do this async'ly. Possibly useful info:
  ;; (info "(elisp)Accepting Output")
  (message "[ds]%S" dsegs)
  (let-hash fmt
    (when (or (not (file-exists-p file))
              (zerop (file-attribute-size (file-attributes file))))
      (let ((script (make-temp-file "edl")))
        (with-temp-file script
          (insert (format "wget -O %s \\\n" file))
          (cl-loop for (k . v) in .http_headers
                   do (insert (format "  --header=\"%s: %s\" \\\n" k v)))
          ;; (info "(cl)Loop Facility")
          (cl-loop for i downfrom (length (car dsegs))
                   for path in (cons .initialization_url (car dsegs))
                   do (insert
                       (format "  %S %s\n"
                               (concat .url path) (if (zerop i) "" "\\")))))
        (call-process shell-file-name script)))))

(defun ffedit--dash-segs-edl (url fmt segs)
  (let-hash fmt
    (let ((file
            (expand-file-name (format "%s.%s"
                                      (md5 (format "%s%s%s" url .format_id segs))
                                      .ext)
                              temporary-file-directory))
          (dsegs (ffedit--dash-segments
                  fmt
                  (mapcar (pcase-lambda (`(,start . ,end))
                            (cons (* start .timescale)
                                  (and end
                                       (* end .timescale))))
                          segs))))
      (ffedit--dash-download fmt file dsegs)
      (mapconcat (apply-partially #'concat
                                  (ffedit--edl-escape file))
                 (cl-mapcar (lambda (start dur)
                              ;; TODO: enable seek
                              (concat (format ",start=%s" start)
                                      (when nil
                                        (format ",length=%s" dur))))
                            (cdr dsegs)
                            (mapcar (pcase-lambda (`(,start . ,end))
                                      (and end (- end start)))
                                    segs))
                 ";"))))

(defun ffedit-preview--edl-uri (url fmt segs)
  (let-hash fmt
    (concat "edl://"
            (if (string= .protocol "http_dash_segments")
                (ffedit--dash-segs-edl url fmt segs)
              (string-join (mapcar
                            (apply-partially #'ffedit-edl-segment .url)
                            segs)
                           ";")))))

;; (defun mpv-play-dash (fmt)
;;   (let-alist fmt
;;     (make-process :name "play-dash"
;;                   :buffer (generate-new-buffer "*play-dash*")
;;                   :command
;;                   (list "mpv" "--no-resume-playback"
;;                         "--ytdl-raw-options=no-mark-watched="
;;                         (concat "edl://"
;;                                 (mapconcat (lambda (path)
;;                                              (ffedit--edl-escape
;;                                               (concat .url path)))
;;                                            (vconcat (vector .initialization_url)
;;                                                     .segment_urls)
;;                                            ";"))))))

(defun ffedit-preview-sources/mpv (path segs)
  (if (ffap-url-p path)
      (let ((json (ytdl-get-json path ytdl-format/hd)))
        (cons (format "--title=%S" (ytdl-json/title json))
              (mapcar (lambda (fmt)
                        (concat
                         (when (string= (ytdl-json/format_note fmt) "DASH audio")
                           "--audio-file=")
                         (ffedit-preview--edl-uri path fmt segs)))
                      (ytdl-requested-formats json))))
    (list
     (ffedit-preview--edl-uri nil (list (cons 'url path)) segs))))

;; (defun ffedit-echo (x) (message "[FFE]%S" x) x)

(defun ffedit-preview-command/mpv (path segs)
  (pp `(ffedit-play-edl ,path ',segs))
  (append '("mpv")
          ffedit-preview-params/mpv
          (when ffedit-debug '("-v"))
          (ffedit-preview-sources/mpv path segs)))

(defun ffedit--preview-segments (begin end)
  (let ((segs
         (thread-last (number-sequence begin end)
           (mapcar #'ffedit--segment)
           (delq nil))))
    (if (null segs)
        (user-error "Invalid region specified: %S" (cons begin end))
      (mapcar (pcase-lambda (`(,start . ,end))
                (cons (ffprobe-pts-starttime ffedit-video-info start)
                      (and end
                           (ffprobe-pts-endtime ffedit-video-info end t))))
              ;; TODO should be done in the last minute, not
              ;; here, to reduce rounding errors.
              (ffedit--reduce-segments segs)))))

(defun ffedit-preview (&optional arg)
  "Play the segment at point with mpv.
With prefix argument ARG, play through to the end."
  (interactive "P")
  (ffedit-play-edl ffedit-video-path
                   (if (region-active-p)
                       (ffedit--preview-segments (region-beginning) (region-end))
                     (ffedit--preview-segments (point)
                                               (if arg (point-max) (point))))))

(defun ffedit-preview-all ()
  "Play the current video buffer with mpv."
  (interactive)
  (ffedit-play-edl ffedit-video-path
                   (ffedit--preview-segments (point-min) (point-max))))

;;;###autoload
(defun ffedit-play-edl (path segs)
  (make-process :name "ffedit-preview"
                :buffer (generate-new-buffer "*ffedit-preview*")
                :command
                (ytdl-detach-process (ffedit-preview-command/mpv path segs))
                :sentinel
                (lambda (proc signal)
                  ;; (message "[FFE]%s: %s" (process-exit-status proc) path)
                  (if (zerop (process-exit-status proc))
                      (kill-buffer (process-buffer proc))
                    (pop-to-buffer (process-buffer proc))
                    (error "FFEdit-preview failed: %s" signal)))))



(defun ffedit-save-session ()
  "Save the current edit state."
  (interactive)
  (ffedit--assert-buffer)
  (let ((sess (buffer-string)))
    (with-temp-file ffedit-sess-file
      (insert (prin1-to-string sess)))))

(defun ffedit--load-session-internal (sess-file thumbs-buf)
  (let ((sess-data
         (with-current-buffer (find-file-noselect sess-file)
           (save-excursion
             (goto-char (point-min))
             (prog1 (read (current-buffer))
               (kill-buffer))))))
    (with-current-buffer thumbs-buf
      (let ((buffer-read-only nil))
        (insert sess-data)))))

(defun ffedit-load-session (thumbs-buf)
  (let ((sess-file
         (buffer-local-value 'ffedit-sess-file thumbs-buf)))
    (when (file-exists-p sess-file)
      (ffedit--load-session-internal sess-file thumbs-buf)
      t)))

(defun ffedit-load-cache (thumbs-buf)
  (let ((log-file (buffer-local-value 'ffedit-ffmpeg-log thumbs-buf)))
    (when (file-exists-p log-file)
      (let ((log-buf (find-file-noselect log-file)))
        (with-current-buffer thumbs-buf
          ;; (Re)initialize thumbs buf
          (setq ffedit--pts-list nil))
        (with-current-buffer log-buf
          (ffedit--add-thumbs thumbs-buf))
        (kill-buffer log-buf))
      t)))



(defun ffedit-save-remote-video (url segs callback)
  (let* ((json (ytdl-get-json url ytdl-format/hd))
         (fmts (ytdl-requested-formats json)))
      (let-hash json
        (let* ((frags
                (cl-loop for i from 1 to (length segs)
                         collect
                         (expand-file-name
                          (format "%s-%s-%d.%s" .extractor .display_id i .ext))))
               (outparams
                (append '("-c" "copy" "-avoid_negative_ts" "1" "-sn")
                        (cond ((ytdl-protocol-hls-p (car fmts))
                               '("-bsf:a" "aac_adtstoasc"))
                              (t nil))))
               (cmdline
                (append (list "ffmpeg" "-nostats" "-y"
                              "-headers" (ytdl-http-headers (car fmts)))
                        (mapcan (lambda (fmt)
                                  (let-hash fmt
                                    (list "-i" .url)))
                                fmts)
                        (ffmpeg-normalize-params
                         (cl-mapcan (lambda (v c)
                                      (append outparams
                                              (list "-ss"
                                                    (ffprobe-pts-starttime
                                                     ffedit-video-info (car c)))
                                              (when (cdr c)
                                                (list "-to"
                                                      (ffprobe-pts-endtime
                                                       ffedit-video-info (cdr c))))
                                              (list v)))
                                    frags segs)))))
          (message "[FFS] %S" cmdline)
          (make-process :name "ffedit-save"
                        :buffer (generate-new-buffer "*ffedit-save*")
                        :command cmdline
                        :sentinel (lambda (proc signal)
                                    (if (zerop (process-exit-status proc))
                                        (progn
                                          ;; (kill-buffer (process-buffer proc))
                                          (funcall callback frags))
                                      (pop-to-buffer (process-buffer proc))
                                      (error "Save failed: %s" signal))))))))

(declare-function async-start "ext:async")

(defun ffedit-save-video ()
  "Save the current buffer as a video file."
  (interactive)
  (ffedit--assert-buffer)
  (let ((video ffedit-video-path)
        (dir default-directory)
        (segs
         (ffedit--reduce-segments
          (delq nil
                (mapcar #'ffedit--segment
                        (number-sequence (point-min) (point-max)))))))
    (if (ffap-url-p video)
        (ffedit-save-remote-video video segs #'ffmpeg-join)
      (ffmpeg-join
       (ffmpeg-split-at-chapters-internal
        video dir
        (list ;; (cons 'seek-timestamp t)
         (cons 'pts-p t)
         (cons 'chapters segs)))))))



(ffedit-defcommand revert-buffer
  (erase-buffer)
  (or (ffedit-load-session (current-buffer))
      (ffedit-load-cache (current-buffer))))

(ffedit-defcommand delete-segment delete-char)
(ffedit-defcommand kill-line kill-line)
(ffedit-defcommand kill-region kill-region)
(ffedit-defcommand undo undo)
(ffedit-defcommand yank yank)

(defvar ffedit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-c C-c") #'ffedit-save-video)
    (define-key map [remap save-buffer] #'ffedit-save-session)
    (define-key map [remap delete-char] #'ffedit-delete-segment)
    (define-key map [remap kill-line] #'ffedit-kill-line)
    (define-key map [remap kill-region] #'ffedit-kill-region)
    (define-key map [remap yank] #'ffedit-yank)
    (define-key map [remap undo] #'ffedit-undo)
    (define-key map (kbd "RET") #'ffedit-preview)
    (define-key map (kbd "C-c C-o") #'ffedit-preview-all)
    (define-key map "g" #'ffedit-revert-buffer)
    map))

(define-derived-mode ffedit-mode special-mode
  "ffedit"
  "Major mode for editing videos."
  ;; (setq-local revert-buffer-function #'ffedit-revert-buffer)
  )

(defun ffedit-find-video-default ()
  (find-file-read-args "Find video: " (confirm-nonexistent-file-or-buffer)))

;;;###autoload
(defun ffedit (path &optional refresh)
  "Edit video file PATH.
If REFRESH is non-nil, discard cache files, if any."
  (interactive (list (funcall ffedit-find-video-function) current-prefix-arg))
  (ffedit-generate-thumbs `((path . ,path)
                            (title . ,(file-name-base path)))
                          refresh))

(declare-function youtube-dl-url "youtube-dl")

;;;###autoload
(defun ffedit-url (url)
  "Edit online video URL."
  (interactive (list (youtube-dl-url)))
  (let ((json (ytdl-get-json url ytdl-format/lq)))
    (ytdl-download-video json
                         (lambda (cache)
                           (ffedit-generate-thumbs
                            `((canonical-path . ,(ytdl-json/webpage_url json))
                              (path . ,cache)
                              (title . ,(ytdl-json/title json))))))))

;;;###autoload
(defun ffedit-export-url ()
  (let ((urlobj (url-generic-parse-url ffedit-video-path)))
    (if (string-match-p (rx "youtube.com") (url-host urlobj))
        (let ((segs (ffedit--preview-segments (point-min) (point-max))))
          (setf (url-filename urlobj)
                (format "%s?%s"
                        (car (url-path-and-query urlobj))
                        (url-build-query-string
                         `(,@(url-parse-query-string
                              (cdr (url-path-and-query urlobj)))
                             (t ,(floor (caar segs)))
                             ,@(when-let* ((end (cdar (last segs))))
                                `((end ,(ceiling end))))))))
          (url-recreate-url urlobj))
      ffedit-video-path)))

(provide 'ffedit)
;;; ffedit.el ends here
