
(require 'ivy)
(require 'magit)

;;;###autoload
(defun counsel-magit-repos ()
  (interactive)
  (ivy-read "Repos: "
            (mapcar (lambda (repo)
                      (cons (file-name-nondirectory repo) repo))
                    (cl-remove-if-not
                     (lambda (dir)
                       (and (file-directory-p dir)
                            (file-directory-p (expand-file-name ".git" dir))))
                     (append (cddr (directory-files "~/git" t))
                             (cddr (directory-files
                                    (expand-file-name "site-lisp"
                                                      user-emacs-directory)
                                    t)))))
            :action (lambda (datum)
                      (magit-status-internal (cdr datum)))))

(provide 'counsel-magit)
