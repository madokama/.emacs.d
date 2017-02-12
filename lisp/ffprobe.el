;;; ffprobe --- description

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'json)

(defun ffprobe-video (video)
  ;; TODO Parse time_base from ffmpeg dumps
  (when-let* (probe (executable-find "ffprobe"))
    (with-temp-buffer
      (call-process probe nil t nil
                    "-v" "error"
                    "-show_streams"
                    "-select_streams" "v:0"
                    "-of" "json"
                    video)
      (goto-char (point-min))
      (json-read))))

(defun ffprobe-video-info (video)
  (let-alist (ffprobe-video video)
    (cons (cons 'pts-shift (ffprobe-first-frame-pts video))
          (aref .streams 0))))

(defun ffprobe-first-frame-pts (video)
  (with-temp-buffer
    (call-process "ffmpeg" nil t nil
                  "-ss" "0"
                  "-i" video
                  "-debug_ts" "-nostats"
                  "-f" "image2"
                  "-y" "/dev/null")
    ;; (message "%s" (buffer-string))
    (goto-char (point-min))
    ;; demuxer -> ist_index:0 type:video next_dts:NOPTS next_dts_time:NOPTS next_pts:NOPTS next_pts_time:NOPTS pkt_pts:0 pkt_pts_time:0 pkt_dts:-2002 pkt_dts_time:-0.0667333 off:0 off_time:0
    ;; FIXME: dunno what I'm doing
    (when (search-forward-regexp "^demuxer -> .*?pkt_pts:\\([-0-9]+\\).*?pkt_dts:\\([-0-9]+\\)" nil t)
      (- (string-to-number (match-string 1))
         (string-to-number (match-string 2))))))

(defun ffprobe--as-number (num)
  (if (stringp num)
      (string-to-number num)
    num))

(defun ffprobe--compute-ratio (ratio &optional inverse)
  (pcase (mapcar #'ffprobe--as-number ratio)
    (`(,n ,d)
      (if inverse
          (/ (* d 1.0) n)
        (/ n (* d 1.0))))))

(defun ffprobe--eval-ratio (rat-str &optional inverse)
  (ffprobe--compute-ratio (split-string rat-str "/") inverse))

(defun ffprobe-time-base (json)
  (let-alist json
    (ffprobe--eval-ratio .time_base)))

(defun ffprobe-frame-delta (json)
  (let-alist json
    (ffprobe--compute-ratio (list .duration_ts .nb_frames))))

(defun ffprobe-pts-time (json pts &optional delta)
  (when-let* (time-base (ffprobe-time-base json))
    (+ (* pts time-base) (or delta 0))))

(defun ffprobe-pts-shift (json)
  (let-alist json
    (or .pts-shift 0)))

(defun ffprobe-has-bframe-p (json)
  (let-alist json
    (< 0 .has_b_frames)))

(defun ffprobe-start-pts (json)
  (or (let-alist json .start_pts) 0))

(defun ffprobe-start-offset (json)
  (let ((start (ffprobe-start-pts json)))
    (unless (zerop start)
      (ffprobe-pts-time json start))))

(defun ffprobe--pad-offset (json)
  (let ((offset (ffprobe-start-pts json)))
    (if (zerop offset)
        (ffprobe-pts-shift json)
      offset)))

(defun ffprobe-pts-starttime (json pts)
  (ffprobe-pts-time json
                    (+ pts (ffprobe--pad-offset json))))

(defun ffprobe-pts-endtime (json pts &optional excl)
  (ffprobe-pts-starttime json
                         (- pts (if excl 1 0))))

;; (defun ffmpeg--pts-duation (json seg)
;;   (ffprobe-pts-time json (- (cdr seg) (car seg)
;;                             (* 6 (ffprobe-frame-delta json)))))

(provide 'ffprobe)
;;; ffprobe.el ends here
