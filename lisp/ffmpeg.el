;;; ffmpeg --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; http://blog.albertarmea.com/post/91024308173/losslessly-splitting-videos-by-chapter-using-bash

;;; Code:

;; (eval-when-compile (require 'subr-x))
(require 'seq)
(require 'ffprobe)

(defvar ffmpeg-command "ffmpeg")

(defmacro ffmpeg-parse (&rest items)
  `(list
    ,@(mapcar (lambda (item)
                (let ((fname (intern (format "ffmpeg--%s" item))))
                  `(cons ',item
                         (save-excursion
                           (save-match-data
                             (,fname))))))
              items)))

(defun ffmpeg--chapters ()
  (let ((chapters nil))
    (goto-char (point-max))
    (re-search-backward "^Output #0")
    (while (re-search-backward "Chapter .*?start \\(.+?\\), end \\(.+\\)" nil t)
      (push (cons (match-string 1) (match-string 2))
            chapters))
    chapters))

(defun ffmpeg--duration ()
  (when (re-search-forward "Duration: \\([0-9]+\\):\\([0-9]+\\):\\([.0-9]+\\)")
    (+ (* (string-to-number (match-string 1)) 3600)
       (* (string-to-number (match-string 2)) 60)
       (string-to-number (match-string 3)))))

(defun ffmpeg--fps ()
  (when (re-search-forward "\\<fps, *\\([0-9.]+\\)")
    (string-to-number (match-string 1))))

(defun ffmpeg--frame-count ()
  (when (re-search-forward "^frame= *\\([0-9]+\\)")
    (string-to-number (match-string 1))))

;;(defvar ffmpeg-process-coding-system nil)

;; (defmacro ffmpeg--process-coding-system-hack (expr)
;;   ;; To handle file names with Japanese characters on w32 (msys2
;;   ;; specifically) environment, process-coding-system has to be
;;   ;; '(utf-8-dos . shift_jis-dos)
;;   `(if ffmpeg-process-coding-system
;;        (let ((process-coding-system-alist nil)
;;              (default-process-coding-system ffmpeg-process-coding-system))
;;          ,expr)
;;      ,expr))

;;;###autoload
(defun ffmpeg-video-info (file)
  (with-temp-buffer
    (call-process-shell-command
     (format "ffmpeg -nostats -i \"%s\" -vcodec copy -f rawvideo -y /dev/null"
             file)
     nil '(t t))
    ;;(print (buffer-string))
    (goto-char (point-min))
    (ffmpeg-parse duration fps frame-count chapters)))

(defvar ffmpeg-preview-height 600)
(defvar ffmpeg-preview-tile-height nil)

(defun ffmpeg--preview-filter/between (info start end)
  (if (or start end)
      (let ((start (or start 0))
            (end (or end (alist-get 'duration info))))
        (format "between(t\\,%d\\,%d)*" start end))
    ""))

(defun ffmpeg--preview-filter/tiles (duration)
  ;; Calc so that 1 hour+ videos will have maximum 10x10 tiles. The
  ;; minimum will be 5x5.
  (floor (* (log (max 60 (min 3600 duration)))
            (/ 10 (log 3600)))))

(defun ffmpeg--preview-filter/period (info duration tiles)
  (format "not(mod(n\\,%d))"
          (floor (/ (* duration (alist-get 'fps info))
                    (expt tiles 2)))))

(defun ffmpeg--preview-filter/tile-height (tiles)
  (or ffmpeg-preview-tile-height
      (/ ffmpeg-preview-height tiles)))

(defun ffmpeg--preview-filter (info start end)
  (let* ((duration (- (or end (alist-get 'duration info))
                      (or start 0)))
         ;; Square root of tiles
         (tiles (ffmpeg--preview-filter/tiles duration)))
    (format "select=%s%s,scale=-1:%d,tile=%dx%d"
            (ffmpeg--preview-filter/between info start end)
            (ffmpeg--preview-filter/period info duration tiles)
            (ffmpeg--preview-filter/tile-height tiles)
            tiles tiles)))

(defun ffmpeg-animate (dir &optional dur fps palindrome)
  (let ((default-directory dir)
        (infile "catpls.txt"))
    (with-temp-file infile
      (dolist (img (let ((imgs
                          (directory-files dir nil
                                           (rx "." (or "jpg" "bmp" "png") eos))))
                     (append imgs (when palindrome (reverse imgs)))))
        (insert (format "file %s\nduration %.1f\n" img (or dur 0.1)))))
    (call-process "ffmpeg" nil "*ffmpeg*" nil
                  "-f" "concat"
                  "-i" infile
                  "-c:v" "libx264"
                  "-vf" (format "scale=1280:-2,fps=%d" (or fps 10))
                  "-pix_fmt" "yuv444p"
                  "-y"
                  (format "%s.mp4"
                          (file-name-base (directory-file-name dir))))))

(defun ffmpeg-preview (video preview &optional callback start end)
  (let ((info (ffmpeg-video-info video)))
    (make-process :name "ffmpeg-preview"
                  ;;:buffer "debug-ffmpeg"
                  :command (list ffmpeg-command "-y" "-i" video
                                 "-vf"
                                 (ffmpeg--preview-filter info start end)
                                 "-frames:v" "1"
                                 (expand-file-name preview))
                  :sentinel callback)))

(defun ffmpeg-screenshot (time video image &optional callback)
  ;;ffmpeg -ss 1370 -i `youtube-dl -g http://www.youtube.com/watch?v=dZVHLcImPc8` -vframes 1 -an -y %d.jpg
  (make-process :name "ffmpeg-screenshot"
                :command (list ffmpeg-command "-y"
                               "-ss" (number-to-string time)
                               "-i" video
                               "-vframes" "1" "-an"
                               (expand-file-name image))
                :sentinel callback))

(defun ffmpeg--split-ext (video)
  (let ((ext (file-name-extension video)))
    (if (string-match-p (rx bos (or "mp4" "m4v" "flv" "f4v") eos)
                        ext)
        "mp4"
      ext)))

(defun ffmpeg-normalize-params (params)
  (mapcar (lambda (param)
            (if (stringp param)
                param
              (format "%s" param)))
          (delq nil params)))

(defun ffmpeg-split-at-chapters-internal (video tmpdir info)
  (let ((base (file-name-base video))
        (ext (ffmpeg--split-ext video))
        (json (ffprobe-video-info video))
        (default-directory tmpdir))
    (let ((frags
           (cl-loop for i from 1 to (length (alist-get 'chapters info))
                    collect (format "%s-%d.%s" base i ext)))
          (pts-p (alist-get 'pts-p info))
          (inparams
           (append (list "-i" video "-y")
                   ;; (when-let (start (ffprobe-start-offset json))
                   ;;   (list "-itsoffset" start))
                   ))
          (outparams '("-c" "copy" "-avoid_negative_ts" "1" "-sn"))
          (buf (generate-new-buffer "*ffsc*")))
      (if (zerop
           (apply #'call-process
                  ffmpeg-command nil buf nil
                  (ffmpeg-normalize-params
                   (append
                    inparams
                    (cl-mapcan (lambda (v c)
                                 (append outparams
                                         (list "-ss"
                                               (if pts-p
                                                   (ffprobe-pts-starttime json
                                                                          (car c))
                                                 (car c)))
                                         (when (cdr c)
                                           (list "-to"
                                                 (if pts-p
                                                     (ffprobe-pts-endtime json
                                                                          (cdr c))
                                                   (cdr c))))
                                         (list v)))
                               frags
                               (alist-get 'chapters info))))))
          (progn
            (kill-buffer buf)
            frags)
        (pop-to-buffer buf)
        (error "FFMPEG split failed")))))

;; (defun ffmpeg-split-at-chapters-internal (video tmpdir info)
;;   (let ((base (file-name-base video))
;;         (ext (ffmpeg--split-ext video))
;;         (json (ffprobe-video-info video))
;;         (pts-p (cdr (assq 'pts-p info)))
;;         (default-directory tmpdir))
;;     (cl-loop
;;        for i from 1
;;        for c in (cdr (assq 'chapters info))
;;        collect
;;          (let ((v (format "%s-%d.%s" base i ext))
;;                (buf (generate-new-buffer "*ffsc*")))
;;            (if (zerop
;;                 (apply #'call-process
;;                        ffmpeg-command nil buf nil
;;                        (ffmpeg-normalize-params
;;                         `(,@(when (cdr (assq 'seek-timestamp info))
;;                               '("-seek_timestamp" "1"))
;;                             ,@(when-let ((start (ffprobe-start-offset json)))
;;                                 (list "-itsoffset" start))
;;                             ;; "-noaccurate_seek"
;;                             "-ss"
;;                             ,(if pts-p
;;                                  (ffprobe-pts-starttime json (car c))
;;                                (car c))
;;                             "-i" ,video
;;                             "-c" "copy"
;;                             ,@(when (cdr c)
;;                                 (list "-to"
;;                                       (if pts-p
;;                                           (ffprobe-pts-endtime json (cdr c))
;;                                         (cdr c))))
;;                             "-copyts" "-start_at_zero"
;;                             ;; "-avoid_negative_ts" "1"
;;                             "-y" ,v))))
;;                (progn
;;                  (kill-buffer buf)
;;                  v)
;;              (pop-to-buffer buf)
;;              (error "FFMPEG split failed"))))))

(declare-function async-start "ext:async")

(defun ffmpeg-split-at-chapters (video tmpdir info)
  (async-start
   (lambda ()
     (require 'ffmpeg)
     ;; (ffmpeg-split-at-chapters-internal ,video ,tmpdir ',info)
     (ffmpeg-split-at-chapters-internal video tmpdir info))
   (lambda (results)
     (message "[ffmpeg-split] %S" results)
     results)))

(defvar ffmpeg-split-interval 40
  "Specify video segment duration in minutes.")

;;;###autoload
(defun ffmpeg-split (video)
  (interactive "fVideo: ")
  (let ((info (ffmpeg-video-info video)))
    (when (> (alist-get 'duration info)
             ffmpeg-split-interval)
      (let* ((base (md5 (file-name-base video)))
             (dir (file-name-directory video))
             (tmpdir (expand-file-name base dir)))
        (unless (file-directory-p tmpdir)
          (mkdir tmpdir))
        (if (alist-get 'chapters info)
            (ffmpeg-split-at-chapters video tmpdir info)
          (make-process :name "ffmpeg-split"
                        :command
                        (list ffmpeg-command "-i" video
                              "-c" "copy"
                              "-f" "segment"
                              "-segment_time" (number-to-string
                                               (* ffmpeg-split-interval 60))
                              "-reset_timestamps" "1"
                              "-map" "0"
                              (expand-file-name (format "%s-%%d.mp4" base)
                                                tmpdir))
                        :sentinel
                        (lambda (proc _signal)
                          (if (zerop (process-exit-status proc))
                              (message "[FFMPEG-SPLIT] finished successfully")
                            (error "[FFMPEG-SPLIT] failed with exit code %d"
                                   (process-exit-status proc))))))))))

; ((duration . 50.57) (chapters) (frame-count . 1517))

;; (ffmpeg-video-info (expand-file-name
;;                                         ;"横アリの中心でさゆを叫ぶまーちゃん-sm25549980.mp4"
;;                     "Morning Musume｡'14 - Special Event in Shinagawa (2014-11-24).mp4"
;;                     "~/Downloads"))

;; '((duration . 3816.7)
;;   (chapters
;;    ("0.000000" . "318.276000")
;;    ("318.276000" . "558.716000")
;;    ("558.716000" . "2216.706000")
;;    ("2216.706000" . "2464.453000")
;;    ("2464.453000" . "2707.930000")
;;    ("2707.930000" . "2946.068000")
;;    ("2946.068000" . "3199.588000")
;;    ("3199.588000" . "3544.065000")
;;    ("3544.065000" . "3768.056000")
;;    ("3768.056000" . "3816.704000")))

(provide 'ffmpeg)
;;; ffmpeg.el ends here
