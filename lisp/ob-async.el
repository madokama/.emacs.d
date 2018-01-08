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

(defun org-babel--async-buffer (info)
  (let ((buf (generate-new-buffer
              (format "*ob-result:%s*" (md5 (pp-to-string info))))))
    (prog1 buf
      (with-current-buffer buf
        (special-mode)
        (setq buffer-read-only nil))
      (display-buffer buf '(display-buffer-in-side-window)))))

(defun org-babel--async-show-progress (buf output)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert output))))

;;;###autoload
(defun org-babel-execute-async ()
  (when (org-in-src-block-p)
    (let* ((info (org-babel-get-src-block-info))
           (lang (nth 0 info))
           (params (nth 2 info))
           (name (nth 4 info))
           (quiet (cl-member-if (lambda (param)
                                  (string= param "none"))
                                (alist-get :result-params params))))
      (when (cl-equalp (alist-get :async params) "yes")
        (let ((file buffer-file-name)  ;TODO handle case when it's nil
              (buf (unless quiet (org-babel--async-buffer info)))
              (frame (window-frame))
              (ov (org-babel--make-source-overlay (org-element-context))))
          (overlay-put ov 'face 'secondary-selection)
          (message "Executing async %s code block%s..."
                   lang
                   (if name (format " (%s)" name) ""))
          (pcase lang
            ((or "sh" "cmd") (org-babel-execute-async:shell lang info buf ov))
            (_
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
                       (cond (error
                              (org-babel-eval-error-notify nil error))
                             (result
                              (unless quiet
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert (if (stringp result)
                                              result
                                            (pp-to-string result))))))
                             ;; (t (kill-buffer buf))
                             ))
                  (delete-overlay ov))))
             ;; Report progress
             (unless quiet
               (set-process-filter async--procvar
                                   (lambda (proc output)
                                     (org-babel--async-show-progress buf output)
                                     (with-current-buffer (process-buffer proc)
                                       (insert output))))))))
        t))))

;;;###autoload(add-hook 'org-ctrl-c-ctrl-c-hook #'org-babel-execute-async)



(require 'ob-shell)

(defun ob-async--shell-cmdline (lang script)
  (pcase lang
    ("cmd"
     (let ((bat (concat script ".bat")))
       (rename-file script bat)
       (list "cmd.exe" "/C" bat)))
    ("sh" (list lang "--noediting" script))))

(defun org-babel-execute-async:shell (lang info buf ov)
  (let* ((params (nth 2 info))
         (body
          (let ((coderef (nth 6 info))
                (expand
                 (if (org-babel-noweb-p params :eval)
                     (org-babel-expand-noweb-references info)
                   (nth 1 info))))
            (if (not coderef)
                expand
              (replace-regexp-in-string
               (org-src-coderef-regexp coderef) "" expand nil nil 1))))
         ;; TODO handle these parameters.
         ;; (session (org-babel-sh-initiate-session (alist-get :session params)))
         ;; (stdin (when-let* ((stdin (alist-get :stdin params)))
         ;;          (org-babel-sh-var-to-string (org-babel-ref-resolve stdin))))
         ;; (cmdline (alist-get :cmdline params))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:shell params))))
    (let ((script (make-temp-file "ob-sh"))
          (default-directory (or (alist-get :dir params) default-directory)))
      (with-temp-file script
        (insert full-body))
      (set-file-modes script #o755)
      (make-process :name "ob-sh"
                    :buffer buf
                    :stderr buf
                    :command (ob-async--shell-cmdline lang script)
                    :filter (when buf
                              (lambda (_proc output)
                                (org-babel--async-show-progress buf output)))
                    :sentinel (lambda (proc _signal)
                                ;; Make error messages visible.
                                (unless (zerop (process-exit-status proc))
                                  (when (buffer-live-p buf)
                                    (with-current-buffer buf
                                      (goto-char (point-min)))))
                                (delete-overlay ov)
                                (delete-file script)
                                (kill-process proc))))))

(provide 'ob-async)
;;; ob-async.el ends here
