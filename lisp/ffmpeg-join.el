;;; ffmpeg-join --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'ffmpeg)
(require 'seq)

(defun ffmpeg-join-name (parts ext)
  (format "%s.%s"
          (if-let ((name
                    (apply #'fill-common-string-prefix
                           (mapcar #'file-name-base (seq-take parts 2)))))
              (concat (replace-regexp-in-string "_part$" "" name)
                      "joined")
            (format "%s-joined" (file-name-base (car parts))))
          ext))

(defun ffmpeg-join-sort (parts)
  (mapcar #'cdr
          (sort (mapcar (lambda (part)
                          (when (string-match "\\([0-9]+\\)\\.[[:alnum:]]+$" part)
                            (cons (string-to-number (match-string 1 part))
                                  part)))
                        parts)
                (lambda (a b)
                  (< (car a) (car b))))))

(defun ffmpeg-join-normalize-filename (file)
  (let ((newname
         (replace-regexp-in-string "['\" ]" "_"
                                   (file-name-nondirectory file))))
    (rename-file file
                 (expand-file-name newname (file-name-directory file))
                 t)
    newname))

(defmacro ffmpeg-join-with-script-file (dir &rest body)
  (declare (indent 1))
  (let ((svar (make-symbol "script")))
    `(let ((,svar (make-temp-file "ffmpeg-join-")))
       (with-temp-file ,svar
         (set-buffer-multibyte 'to)
         (insert (format "cd %S\n" ,dir))
         ,@body
         (insert "rm $0\n"))
       (make-process :name "ffmpeg-join"
                     :buffer "*ffmpeg-join*"
                     :command (list shell-file-name ,svar)
                     :sentinel
                     (lambda (proc signal)
                       (if (zerop (process-exit-status proc))
                           (message "[FFMPEG-JOIN] finished successfully")
                         (error "[FFMPEG-JOIN] failed with exit code %d"
                                (process-exit-status proc))))))))

(defun ffmpeg-join-ts (parts dir)
  ;;ffmpeg -i "concat:input1.ts|input2.ts|input3.ts" -c copy output.ts
  (let ((joined
         (ffmpeg-join-name parts
                           (file-name-extension (car parts)))))
    (ffmpeg-join-with-script-file dir
      (insert
       (format "ffmpeg -i \"concat:%s\" -c copy %s\n"
               (mapconcat #'identity parts "|")
               joined)))))

;; (defun ffmpeg-join-fifo (parts joined)
;;   (let ((fifo-names (mapcar #'file-name-base parts)))
;;     (insert
;;      (format "mkfifo %s\n" (mapconcat #'identity fifo-names " ")))
;;     (cl-loop for part in parts
;;              for fifo in fifo-names
;;              do (insert
;;                  (format
;;                   "ffmpeg -i %s -c copy -bsf:v h264_mp4toannexb -f mpegts %s 2> /dev/null &\n"
;;                   part fifo)))
;;     (insert
;;      (format "ffmpeg -i \"concat:%s\" -c copy -bsf:a aac_adtstoasc -f mpegts %s\n"
;;              (mapconcat #'identity fifo-names "|")
;;              joined))
;;     (insert (format "rm %s\n" (mapconcat #'identity fifo-names " ")))))

;; (defun ffmpeg-join-mp4 (parts dir)
;;   (let ((joined (ffmpeg-join-name parts "mp4"))
;;         ;;(bsf-v (ffmpeg-join-filter-in (car parts)))
;;         )
;;     (ffmpeg-join-with-script-file dir
;;       (ffmpeg-join-fifo parts joined))))

;; $ ffmpeg -f concat -i mylist.txt -c copy output3.webm
;; ==
;; And here is mylist.txt:
;; ==
;; file 'tmpD08D.webm'
;; file 'tmpD08E.webm'
;; file 'tmpD08F.webm'
;; file 'tmpD090.webm'
;; file 'tmpD091.webm'
;; file 'tmpD0A1.webm'

(defun ffmpeg-join-generic (parts dir)
  ;; (message "%S" (cons dir parts))
  (let ((default-directory dir))
    (if (= (length parts) 1)
        (rename-file (car parts)
                     (replace-regexp-in-string
                      "-[0-9]+\\(\\.[[:alnum:]]+\\)$" "\\1" (car parts))
                     t)
      (let ((pls "cat.txt")
            (buf (generate-new-buffer "*ffjg*")))
        (with-temp-file pls
          (dolist (part parts)
            (insert (format "file '%s'\n" part))))
        (if (zerop
             (call-process ffmpeg-command nil buf nil
                           "-f" "concat"
                           "-safe" "0"
                           "-i" pls
                           "-c" "copy"
                           "-y"
                           (ffmpeg-join-name parts
                                             (file-name-extension (car parts)))))
            (kill-buffer buf)
          (pop-to-buffer buf)
          (error "FFMpeg join failed"))))))

(defun ffmpeg-join-mp4 (parts dir)
  (let ((ts (ffmpeg-join-name parts "ts")))
    (ffmpeg-join-with-script-file dir
      (when (file-exists-p (expand-file-name ts dir))
        (insert (format "rm %S\n" ts)))
      (dolist (part parts)
        (insert
         (format "ffmpeg -v warning -i %S -c copy -bsf:v h264_mp4toannexb -f mpegts - >> %S\n"
                 part ts)))
      (insert (format "ffmpeg -i %S -c copy -bsf:a aac_adtstoasc -y %S\n"
                      ts (ffmpeg-join-name parts "mp4"))
              (format "rm %S\n" ts)))))

;; https://trac.ffmpeg.org/wiki/Concatenate

(defun ffmpeg-join (parts)
  (when (> (length parts) 1)
    (let ((dir (or (file-name-directory (car parts))
                   default-directory))
          (parts
           (ffmpeg-join-sort
            (mapcar #'ffmpeg-join-normalize-filename parts)))
          (ext (file-name-extension (car parts)))
          (case-fold-search t))
      (cond ((string-match-p (rx (or "ts" "mpg")) ext)
             (ffmpeg-join-ts parts dir))
            ((string-match-p (rx (or "flv" "mp4")) ext)
             (ffmpeg-join-mp4 parts dir))
            (t
             (ffmpeg-join-generic parts dir)
             ;; (error "Unsupported format: %s" ext)
             )))))

(provide 'ffmpeg-join)
;;; ffmpeg-join.el ends here
