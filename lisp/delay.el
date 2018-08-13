;;; delay --- Provide delayed evaluation mechanism -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs))

(cl-defstruct promise
  value forced)
;;;###autoload(autoload 'promise-p "delay")

;;;###autoload
(defun force (p)
  (cl-assert (promise-p p) t)           ; FIXME should return P as is?
  (if (promise-forced p)
      (promise-value p)
    (prog1
        (setf (promise-value p) (funcall (promise-value p)))
      (setf (promise-forced p) t))))

;;;###autoload
(defmacro delay (&rest body)
  `(make-promise :value (lambda () ,@body)))

(provide 'delay)
;;; delay.el ends here
