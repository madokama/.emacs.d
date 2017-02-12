;; -*- lexical-binding: nil; byte-compile-warnings: nil; -*-

;; https://github.com/Bruce-Connor/spinner.el
(require 'spinner)

(defvar progchar-repeat-interval 1)

(defun progchar (&optional msg)
  (let ((count (cl-gensym "pc-count"))
        (timer (cl-gensym "pc-timer")))
    (eval
     `(progn
        ;; NOTE: `count' must be global since it has to be embedded in
        ;; s-exp as a quoted symbol.
        (defvar ,count 0)
        (defvar ,timer
          (run-with-timer 0.1 progchar-repeat-interval
                          (lambda ()
                            (setq ,count (+ 1 ,count))
                            (force-mode-line-update))))
        (let ((chars
               (cdr (elt spinner-types
                         (random (length spinner-types))))))
          (push (list ',timer
                      `(:eval
                        (propertize (elt ,chars (% ,',count ,(length chars)))
                                    'help-echo ,,msg)))
                global-mode-string))
        (lambda (&optional arg)
          (cancel-timer ,timer)
          (unless arg
            ;; Non-nil ARG should signify failure on the caller site, in
            ;; which case we simply stop the progress char to indicate
            ;; that.
            (setq global-mode-string
                  (seq-remove (lambda (mode)
                                (and (consp mode)
                                     (symbolp (car mode))
                                     (string= (symbol-name ',timer)
                                              (symbol-name (car mode)))))
                              global-mode-string))
            (setq ,count nil
                  ,timer nil)))))))

(provide 'progchar)
