;;; alpha-frame --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)                       ; for cl-list*

;; https://technet.microsoft.com/pt-br/library/ms632600.aspx

(defvar alpha-frame-use-nircmd (not (null (executable-find "nircmd"))))

;; (defvar alpha-frame-size-param
;;   (split-string (format "setsize 0 0 %d %d"
;;                         (display-pixel-width)
;;                         (1- (display-pixel-height)))
;;                 " "))

(defun alpha-frame-nircmd (proc-id param-list)
  (when param-list
    (let ((param (car param-list)))
      (make-process :name "nircmd"
                    :command (cl-list* "nircmd.exe" "win" (car param)
                                       "process" (alpha-frame-pid proc-id)
                                       (cdr param))
                    :sentinel
                    (lambda (proc _signal)
                      (when (zerop (process-exit-status proc))
                        (alpha-frame-nircmd proc-id (cdr param-list))))))))

(defun alpha-frame-pid (pid)
  (format "/%s" pid))

(defvar alpha-frame-border-delete
  (eval-when-compile
    (format "0x%08X"
            (logior    ;; #x00000000 ;WS_OVERLAPPED
             #x00C00000                 ;WS_CAPTION
             #x00080000                 ;WS_SYSMENU
             #x00040000                 ;WS_THICKFRAME
             #x00020000                 ;WS_MINIMIZEBOX
             #x00010000                 ;WS_MAXIMIZEBOX
             #x00200000                 ;WS_VSCROLL
             #x00100000                 ;WS_HSCROLL
             ))))

(defun alpha-frame-delete-border (pid)
  (alpha-frame-nircmd pid `(("-style" ,alpha-frame-border-delete))))

(defun alpha-frame-init ()
  (if alpha-frame-use-nircmd
      (alpha-frame-nircmd (emacs-pid)
                          `(("-style" ,alpha-frame-border-delete)
                            ("max")
                            ;; ,alpha-frame-size-param
                            ))
    (modify-frame-parameters nil
                             '((fullscreen . fullboth))))
  (set-frame-parameter nil 'alpha .713))

;;;###autoload
(defun alpha-frame-max ()
  "Maximize the current frame."
  (interactive)
  (if alpha-frame-use-nircmd
      (alpha-frame-nircmd (emacs-pid) '(("max")))
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun alpha-frame-trans-set (arg)
  (set-frame-parameter nil 'alpha (min 1.0 arg)))

;;;###autoload
(defun alpha-frame-trans-inc (step)
  "Increase frame transparency with rate STEP."
  (interactive "p")
  (alpha-frame-trans-set (- (or (frame-parameter nil 'alpha) 1)
                      (/ step 100.0))))

;;;###autoload
(defun alpha-frame-trans-dec (step)
  "Decrease frame transparency with rate STEP."
  (interactive "p")
  (alpha-frame-trans-inc (- step)))

;;;###autoload
(defun alpha-frame-opaque ()
  "Make EMACS frame opaque."
  (interactive)
  (alpha-frame-trans-set 1.0))

(provide 'alpha-frame)
;;; alpha-frame.el ends here
