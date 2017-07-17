;;; ob-async --- Asynchronous support for org babel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org)
(require 'async)

(defun org-babel--make-source-overlay (ctx)
  "Create overlay on source CTX to inhibit multiple executions."
  (let ((ov
         (make-overlay (org-element-property :begin ctx)
                       (org-element-property :end ctx)))
        (map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
      (lambda ()
        (interactive)
        (message "Please wait until the work is finished.")))
    (overlay-put ov 'keymap map)
    ov))

;; todo
;;  (defconst org-babel-common-header-args-w-values
;; -  '((cache	. ((no yes)))
;; +  '((async      . ((yes no)))
;; +    (cache	. ((no yes)))

;;;###autoload
(defun org-babel-execute-async ()
  (when (org-in-src-block-p)
    (let* ((info (org-babel-get-src-block-info))
           (params (nth 2 info)))
      (when (cl-equalp (alist-get :async params) "yes")
        (let ((file buffer-file-name)  ;TODO handle case when it's nil
              (sid (md5 (pp-to-string info)))
              (frame (window-frame))
              (ov (org-babel--make-source-overlay (org-element-context))))
          (overlay-put ov 'face 'secondary-selection)
          (message "Executing async %s code block%s..."
                   (nth 0 info)
                   (let ((name (nth 4 info)))
                     (if name (format " (%s)" name) "")))
          (async-start
           `(lambda ()
              ,(async-inject-variables
                (rx bos (or "load-path" "org-babel-library-of-babel") eos))
              (require 'org)
              (org-babel-do-load-languages 'org-babel-load-languages
                                           ',org-babel-load-languages)
              (with-current-buffer (find-file-noselect ,file)
                (goto-char ,(nth 5 info))
                (let ((org-confirm-babel-evaluate nil))
                  (cons (org-babel-execute-src-block
                         nil ',info
                         ',(progn
                             (nconc (assq :result-params params) '("silent"))
                             params))
                        (let ((ebuf (get-buffer org-babel-error-buffer-name)))
                          (when ebuf
                            (with-current-buffer ebuf
                              (buffer-string))))))))
           (pcase-lambda (`(,result . ,error))
             (unwind-protect
                  (with-selected-frame frame
                    (if error
                        (org-babel-eval-error-notify nil error)
                      (display-buffer
                       (with-current-buffer
                           (generate-new-buffer (format "*ob-result:%s*" sid))
                         (insert (if (stringp result)
                                     result
                                   (pp-to-string result)))
                         (special-mode)
                         (current-buffer))
                       '(display-buffer-in-side-window))))
               (delete-overlay ov)))))
        t))))

;;;###autoload(add-hook 'org-ctrl-c-ctrl-c-hook #'org-babel-execute-async)

(provide 'ob-async)
;;; ob-async.el ends here
