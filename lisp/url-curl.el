;;; url-curl.el --- url-http replacement  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'url-http)
(require 'url-cache)

(defun url-curl-response ()
  (save-match-data
    (when (search-forward-regexp "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

;;;###autoload
(defun url-curl-cookie ()
  (url-do-setup)
  (concat url-cookie-file ".curl"))

;; Cookie utility
(defun url-curl-extract-cookies (host keys)
  (string-join
   (mapcar (pcase-lambda (`(,k . ,v))
             (format "%s=%s;" k v))
           (with-current-buffer
               (find-file-noselect (expand-file-name (url-curl-cookie)))
             (goto-char (point-min))
             (prog1
                 (cl-loop while (re-search-forward
                                 (format "^%s\t.+?\t\\([^\t]+\\)\t\\([^\t]+\\)$"
                                         (regexp-quote host))
                                 nil t)
                          when (member (match-string 1) keys)
                            collect (cons (match-string 1) (match-string 2)))
               (kill-buffer (current-buffer)))))
   " "))

(defun url-curl--args (url referer)
  (let ((cache (url-cache-create-filename url)))
    `("-X" ,url-request-method
           "-L" "-s" "--http1.1" "--compressed"
           "-D" "-"
           "-A" ,(url-http--user-agent-default-string)
           ,@(when referer
               (list "-e" referer))
           ,@(unless (string= url-request-method "HEAD")
               (when (url-cache-prepare cache)
                 (list "-o" cache)))
           ,@(when-let* ((cookie (expand-file-name (url-curl-cookie))))
               (list "-b" cookie "-c" cookie))
           ,@(mapcan (pcase-lambda (`(,name . ,value))
                       (list "-H" (format "%s: %s" name value)))
                     url-request-extra-headers)
           ,@(when url-request-data
               (list "-H" (format "Content-Length: %d"
                                  (string-bytes url-request-data))
                     "-d" url-request-data))
           ,@(when (string= url-request-method "GET")
               (list "-z" cache)))))

(defun url-curl--clean-header ()
  (goto-char (point-min))
  (when (looking-at-p "^$")
    (kill-line)))

(defvar url-http-response-status)

(defun url-curl--parse-header ()
  (url-curl--clean-header)
  ;; Adapted from url-http.el
  (mail-narrow-to-head)
  (let ((code (url-curl-response))
        (status nil))
    (setq-local url-http-response-status code)
    ;; (setq-local elfeed-curl-status-code code)
    (cl-case (/ code 100)
      (3                                ; Redirection
       ;; 300 Multiple choices
       ;; 301 Moved permanently
       ;; 302 Found
       ;; 303 See other
       ;; 304 Not modified
       ;; 305 Use proxy
       ;; 307 Temporary redirect
       (when-let* ((loc (or (mail-fetch-field "Location")
                            (mail-fetch-field "URI"))))
         (setq status (list :redirect loc))
         (delete-region (point-min) (point-max))))
      (4
       (setq status (list :error (list 'error 'http code))))
      (5
       (setq status (list :error (list 'error 'http code)))))
    (widen)
    status))

(defun url-curl-parse-headers (status)
  (let ((status1 (url-curl--parse-header)))
    (if (eq (car status1) :redirect)
        ;; TODO: May blow the stack when infinite redirects encountered
        (url-curl-parse-headers (append status1 status))
      (append status1 status))))

(declare-function eww-current-url "eww")
(declare-function elfeed-entry-link "ext:elfeed-show")
(defvar elfeed-show-entry)

(defun url-curl-referer ()
  (cond ((derived-mode-p 'eww-mode)
         (eww-current-url))
        ((derived-mode-p 'elfeed-show-mode)
         (elfeed-entry-link elfeed-show-entry))))

(defun url-curl-sync (url &optional callback)
  (let ((referer (url-curl-referer)))
    (with-temp-buffer
      (setq-local url-request-method (or url-request-method "GET"))
      (apply #'call-process "curl" nil t nil
             `(,@(url-curl--args url referer)
                 ,(if (url-p url)
                      (url-recreate-url url)
                    url)))
      (let ((cache (url-cache-create-filename url))
            (status (url-curl-parse-headers nil))
            (inhibit-message t))
        (when (plist-get status :error)
          (ignore))
        (unless (= url-http-response-status 304)
          (write-region (point-min) (point-max) (concat cache ".h")))
        (when (file-exists-p cache)
          (with-current-buffer (find-file-noselect cache)
            (prog1 (if callback
                       (progn
                         (goto-char (point-min))
                         (funcall callback))
                     (buffer-substring-no-properties (point-min) (point-max)))
              (kill-buffer))))))))

(defun url-curl (url callback cbargs &rest _)
  (let ((referer (url-curl-referer))
        (buffer
         (generate-new-buffer
          (format " *http %s:%d*" (url-host url) (url-port url)))))
    (with-current-buffer buffer
      (mm-disable-multibyte)
      (setq url-current-object url)
      (setq-local url-request-method (or url-request-method "GET"))
      (make-process :name "url-curl"
                    :buffer buffer
                    :command `("curl" ,@(url-curl--args url referer)
                                      ,(url-recreate-url url))
                    :coding 'binary
                    :connection-type 'pipe
                    :filter
                    (lambda (proc str)
                      ;; NOTE processing header only.
                      (with-current-buffer (process-buffer proc)
                        (insert (replace-regexp-in-string "\r" "" str))))
                    :sentinel
                    (lambda (proc _signal)
                      (when (zerop (process-exit-status proc))
                        (with-current-buffer (process-buffer proc)
                          (let* ((cache (url-cache-create-filename url))
                                 (header (concat cache ".h"))
                                 (inhibit-message t))
                            (setf (car cbargs)
                                  (nconc (url-curl-parse-headers nil)
                                         (car cbargs)))
                            (if (= url-http-response-status 304)
                                (insert-file-contents-literally header nil nil nil t)
                              (write-region (point-min) (point-max) header))
                            (save-excursion
                              (goto-char (point-max))
                              (insert-file-contents-literally cache))
                            (when-let* ((redir (plist-get (car cbargs) :redirect)))
                              (message "[%s -> %s]" (url-recreate-url url) redir))
                            (apply callback cbargs)))))))
    buffer))

(advice-add 'url-http :override #'url-curl)
;; (advice-remove 'url-http #'url-curl)

(provide 'url-curl)
;;; url-curl.el ends here
