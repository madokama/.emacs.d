
(eval-when-compile (require 'seq))

(defmacro major-diminish (&rest args)
  `(progn
     ,@(mapcar (lambda (mode)
                 `(advice-add ',(car mode) :after
                              (lambda ()
                                "Diminish mode name"
                                (setq mode-name ,(cadr mode)))))
               (seq-partition args 2))))

(provide 'major-diminish)
