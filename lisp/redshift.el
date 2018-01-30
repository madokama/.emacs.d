;;; redshift --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun redshift ()
  (when (and (executable-find "setsid")
             (executable-find "redshift")
             (not
              (cl-find-if (lambda (pid)
                            (string-match-p (rx bos "redshift" eow)
                                            (alist-get 'comm
                                                       (process-attributes pid))))
                          (list-system-processes))))
    (call-process "setsid" nil nil nil "redshift")))

(redshift)

(provide 'redshift)
;;; redshift.el ends here
