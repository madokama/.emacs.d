;;; hydra-dired --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'dired)
(require 'seq)
(require 'hydra-misc)

;;;###autoload
(defvar dired--video-extensions
  (rx "." (or "mp4" "m4v" "mkv" "flv" "webm" "wmv" "avi" "vob") eos))

;; http://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  (interactive)
  (when-let* ((du (executable-find "du"))
              (files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process du nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "^\\([0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-ffmpeg-filter-videos ()
  (seq-filter (apply-partially #'string-match-p dired--video-extensions)
              (dired-get-marked-files)))

(declare-function ffmpeg-video-info "ffmpeg")
(declare-function ffmpeg-split "ffmpeg")
(declare-function ffmpeg-join "ffmpeg-join")
(declare-function ffedit "ffedit")

(defun dired-ffmpeg-video-info ()
  (interactive)
  (when (require 'ffmpeg nil t)
    (dolist (v (dired-ffmpeg-filter-videos))
      (pp (ffmpeg-video-info v)))))

(defun dired-ffmpeg-split ()
  (interactive)
  (when (require 'ffmpeg nil t)
    (mapc #'ffmpeg-split (dired-ffmpeg-filter-videos))))

(defun dired-ffmpeg-join ()
  (interactive)
  (when (require 'ffmpeg-join nil t)
    (ffmpeg-join (dired-ffmpeg-filter-videos))))

(defun dired-ffedit ()
  (interactive)
  (when (require 'ffedit nil t)
    (ffedit (car (dired-ffmpeg-filter-videos)))))

(defun hydra-dired-mode-docstring ()
  (concat (mapconcat
           #'identity
           `(,(format "%s: size" (propertize "dz" 'face 'hydra-face-red))
              ,@(when-let* ((vids (dired-ffmpeg-filter-videos)))
                  (list (format "%s: info"
                                (propertize "di" 'face 'hydra-face-red))
                        (format "%s: edit"
                                (propertize "de" 'face 'hydra-face-red))
                        (if (= (length vids) 1)
                            (format "%s: split"
                                    (propertize "ds" 'face 'hydra-face-red))
                          (format "%s: join"
                                  (propertize "dj" 'face 'hydra-face-red))))))
           ", ")
          "\n"
          (eval hydra-launch/hint)))

(defhydra hydra-dired-mode (:hint nil :exit t :inherit (hydra-launch/heads))
  ("dz" dired-get-size)
  ("di" dired-ffmpeg-video-info)
  ("de" dired-ffedit)
  ("dj" dired-ffmpeg-join)
  ("ds" dired-ffmpeg-split))

(setq hydra-dired-mode/hint '(hydra-dired-mode-docstring))

(provide 'hydra-dired)
;;; hydra-dired.el ends here
