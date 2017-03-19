;;; org-ytdl --- Manage favorite videos with Org -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'org-capture)
(require 'org-element)
(require 'async)
(require 'ytdl-lib)
(require 'youtube-dl)
(require 'ffmpeg)

(defvar org-ytdl-template
  "* %(org-ytdl-capture)  %^g\n%(org-ytdl-run-template-hook)\n%?\n%U")

(defvar org-ytdl-template-hook nil)
(defvar org-ytdl-finalize-hook nil)

;; (ytdl--define-extractors vimeo)

(defun org-ytdl--dir ()
  (let ((dir (expand-file-name "thumbs" org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun org-ytdl--thumb-url (json)
  (when-let* (thumb-url (ytdl-json/thumbnail json))
    (if (ytdl-youtube-p json)
        (replace-regexp-in-string "maxres" "hq" thumb-url)
      thumb-url)))

(defun org-ytdl--thumb-ext (json)
  (let ((thumb-url (ytdl-json/thumbnail json)))
    (if thumb-url
        (pcase (ytdl-json/extractor json)
          ((or "vimeo" "linelive") "jpg")
          (_
           (thread-first thumb-url
             url-generic-parse-url
             url-path-and-query
             car
             image-type-from-file-name)))
      "jpg")))

(defun org-ytdl--thumb-file (json)
  (abbreviate-file-name
   (expand-file-name
    (convert-standard-filename
     (format "%s-%s-%s.%s"
             (ytdl-json/extractor json)
             (or (ytdl-json/id json)
                 (ytdl-json/title json))
             (floor (float-time))
             (org-ytdl--thumb-ext json)))
    (org-ytdl--dir))))

(defun org-ytdl-setup-hooks (json)
  (let ((thumb-file (org-ytdl--thumb-file json)))
    (setq org-ytdl-template-hook
          (lambda ()
            (format "[[%s]]" thumb-file)))
    (setq org-ytdl-finalize-hook
          (lambda ()
            (org-ytdl-generate-thumbnail json thumb-file)))))

(defun org-ytdl-run-template-hook ()
  (when org-ytdl-template-hook
    (prog1 (funcall org-ytdl-template-hook)
      (setq org-ytdl-template-hook nil))))

;;;###autoload
(defun org-ytdl-run-finalize-hook ()
  (when org-ytdl-finalize-hook
    (funcall org-ytdl-finalize-hook)
    (setq org-ytdl-finalize-hook nil)))

(defun org-ytdl-raw-link (link)
  (pcase (org-element-property :type link)
    ("http" (org-element-property :raw-link link))
    ("ytdl" (org-element-property :path link))))

;;;###autoload
(defun org-ytdl-link-at-point ()
  (when-let* (elem (and (derived-mode-p 'org-mode)
                        (org-element-lineage (org-element-context)
                                             '(headline link)
                                             t)))
    (pcase (org-element-type elem)
      (`link
       (org-ytdl-raw-link elem))
      (`headline
       ;; Handle my org-ytdl buffer where each heading consists of a video link.
       (with-temp-buffer
         (insert (org-element-property :raw-value elem))
         (goto-char (point-min))
         (org-ytdl-raw-link (org-element-link-parser)))))))

(defun org-ytdl--find-link (contents type)
  (when-let* (element
              (seq-find (lambda (element)
                          (and (eq (org-element-type element) 'link)
                               (string= (org-element-property :type element)
                                        type)))
                        contents))
    element))

(defun org-ytdl-current-content ()
  (when-let* (head
              (save-excursion
                (unless (org-at-heading-p)
                  (org-back-to-heading t))
                (org-element-at-point)))
    (org-element--parse-objects (org-element-property :begin head)
                                (org-element-property :end head)
                                nil
                                (org-element-restriction 'headline))))
;;;###autoload
(defun org-ytdl-update-thumbnail ()
  "Regenerate video preview image."
  (interactive)
  (when-let* ((ctx (org-ytdl-current-content))
              (ytdl-link (org-ytdl--find-link ctx "ytdl"))
              (thumb-link (org-ytdl--find-link ctx "file")))
    (let* ((json
            (ytdl-get-json (org-element-property :path ytdl-link)
                           ytdl-format/lq))
           (time
            (org-ytdl-time-spec
             (read-string "Enter time [?h?m?s]: ")))
           (start-t
            (or (car time) (ytdl-json/start_time json)))
           (end-t
            (or (cadr time) (ytdl-json/end_time json))))
      (let* ((ov
              (make-overlay (org-element-property :begin thumb-link)
                            (org-element-property :end thumb-link)))
             (buf (current-buffer))
             (callback
              (lambda (proc _signal)
                (when (zerop (process-exit-status proc))
                  (with-current-buffer buf
                    (org-display-inline-images nil t
                                               (overlay-start ov)
                                               (overlay-end ov))))
                (delete-overlay ov))))
        (if (and start-t (not end-t))
            (ffmpeg-screenshot
             start-t
             (ytdl-json/url (ytdl-format-mq json))
             (org-element-property :path thumb-link)
             callback)
          (org-ytdl-generate-preview json
                                     (org-element-property :path thumb-link)
                                     start-t end-t callback))))))

(defmacro org-ytdl-display-thumbnail (&rest body)
  (let ((begvar (make-symbol "beg"))
        (endvar (make-symbol "end"))
        (bufvar (make-symbol "buf")))
    `(let ((,begvar
            (marker-position (org-capture-get :begin-marker 'local)))
           (,endvar
            (marker-position (org-capture-get :end-marker 'local)))
           (,bufvar
            (org-capture-target-buffer (cadr (org-capture-get :target)))))
       (lambda (proc _signal)
         (when (zerop (process-exit-status proc))
           (with-current-buffer ,bufvar
             ,@body
             (org-display-inline-images nil t ,begvar ,endvar)))))))

;; Entry point
;;;###autoload
(defun org-ytdl-capture ()
  (let* ((url
          (with-current-buffer (org-capture-get :original-buffer)
            (youtube-dl-url (plist-get org-store-link-plist :url))))
         (json
          (ytdl-get-json url ytdl-format/lq)))
    (org-ytdl-setup-hooks json)
    (org-make-link-string (format "ytdl:%s" url)
                          (ytdl-json/title json))))

(defun org-ytdl--parse-time (time)
  (cl-labels ((parse (unit factor)
                (if (string-match (format "\\([0-9]+\\)%s" unit) time)
                    (* (string-to-number (match-string 1 time))
                       factor)
                  0)))
    (+ (parse "h" 3600)
       (parse "m" 60)
       (parse "s?$" 1))))       ;treat bare numbers as seconds

(defun org-ytdl-time-spec (spec)
  (sort (mapcar #'org-ytdl--parse-time
                (split-string spec "-" t))
        #'<))

(defun org-ytdl-playlist-preview (url video-file thumb-file callback)
  (async-start
   `(lambda ()
      ,(async-inject-environment "\\`load-path\\'")
      (require 'org-ytdl)
      (let* ((default-directory temporary-file-directory)
             (infile (expand-file-name (format "thumbs-%s.txt" (float-time)))))
        (with-temp-file infile
          (cl-loop for entry across
                             (ytdl-json/entries (ytdl-get-json ,url))
                   with temps = nil
                   do (condition-case nil
                          (let* ((json
                                  (ytdl-get-json (cdr (assq 'url entry))))
                                 (thumb-file
                                  (file-name-nondirectory
                                   (org-ytdl--thumb-file json))))
                            (push thumb-file temps)
                            (call-process-shell-command
                             (format "wget %S -q -O %S"
                                     (org-ytdl--thumb-url json)
                                     thumb-file))
                            (insert
                             (format "file %s\nduration %.1f\n"
                                     thumb-file
                                     (max 1
                                          (/ (ytdl-json/duration json)
                                             30)))))
                        (json-readtable-error nil))
                   finally return (cons infile
                                        (mapcar #'expand-file-name temps))))))
   (lambda (temps)
     (call-process "ffmpeg" nil nil nil
                   "-y" "-f" "concat" "-i" (car temps)
                   "-vf" "fps=10" "-pix_fmt" "yuv420p"
                   video-file)
     (mapc #'delete-file temps)
     (ffmpeg-preview video-file thumb-file callback))))

(defun org-ytdl--video-file (origin ext)
  (expand-file-name
   (concat (file-name-sans-extension (file-name-nondirectory origin))
           "." ext)
   temporary-file-directory))

(defun org-ytdl-generate-preview (json thumb-file &optional start end callback)
  ;; Generate preview pic a la Dailymotion:
  ;; https://static2-ssl.dmcdn.net/static/video/655/848/180848556:jpeg_preview_contact.jpg?20150729182120
  (let ((start (or start (ytdl-json/start_time json)))
        (end (or end (ytdl-json/end_time json)))
        (callback (or callback (org-ytdl-display-thumbnail))))
    (ytdl-download-video
     json
     (lambda (video)
       (ffmpeg-preview video thumb-file callback start end)))))

(defun org-ytdl-generate-thumbnail (json thumb-file)
  (let ((start (ytdl-json/start_time json))
        (end (ytdl-json/end_time json)))
    (cond ((ytdl-json/_type json)
           (let ((video-file
                  (org-ytdl--video-file thumb-file "mp4")))
             (org-ytdl-playlist-preview (ytdl-json/webpage_url json)
                                        video-file
                                        thumb-file
                                        (org-ytdl-display-thumbnail
                                         (delete-file video-file)))))
          ((and start (not end))
           (ffmpeg-screenshot (1+ start)
                              (assoc-default 'url (ytdl-format-mq json))
                              thumb-file
                              (org-ytdl-display-thumbnail)))
          (t
           (org-ytdl-generate-preview json thumb-file start end)))))

;;;###autoload
(defun org-ytdl-display-inline-image (state)
  (when (eq state 'subtree)
    (org-display-inline-images nil t
                               (save-excursion
                                 (outline-end-of-heading)
                                 (point))
                               (save-excursion
                                 (org-end-of-subtree t t)
                                 (when (bolp)
                                   (backward-char))
                                 (point)))))

;;;###autoload
(defun org-ytdl-link-handler (link)
  (youtube-dl/play (replace-regexp-in-string "^//" "ytdl:\\&" link)))

;;;###autoload(with-eval-after-load 'org (org-link-set-parameters "ytdl" :follow #'org-ytdl-link-handler) (add-hook 'org-capture-before-finalize-hook #'org-ytdl-run-finalize-hook) (add-hook 'org-cycle-hook #'org-ytdl-display-inline-image))

(provide 'org-ytdl)
;;; org-ytdl.el ends here
