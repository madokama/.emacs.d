;;; url-curl.el --- url-http replacement  -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'url-http)
(require 'url-cache)

(defvar url-curl-default-ua-regexp
  (rx bow (or "google.com") eos))


;; Lock system

(defvar url-curl--mutex (make-mutex))
(defvar url-curl--connections (make-hash-table :test 'equal))

(defsubst url-curl--lock ()
  (mutex-lock url-curl--mutex))

(defsubst url-curl--unlock ()
  (mutex-unlock url-curl--mutex))

(defun url-curl--cache-lock (cache)
  (url-curl--lock)
  (mutex-lock (or (gethash cache url-curl--connections)
                  (puthash cache (make-mutex cache) url-curl--connections)))
  (url-curl--unlock))

(defun url-curl--cache-unlock (cache)
  (url-curl--lock)
  (when-let ((m (gethash cache url-curl--connections)))
    (remhash cache url-curl--connections)
    (mutex-unlock m))
  (url-curl--unlock))



(defun url-curl-response ()
  (save-match-data
    (when (search-forward-regexp "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

;;;###autoload
(defun url-curl-cookie ()
  (url-do-setup)
  (concat url-cookie-file ".curl"))

(defun url-curl--host-regexp (host)
  (if (listp host)
      (regexp-opt host)
    (regexp-quote host)))

;; Cookie utility
(defun url-curl-extract-cookies (host keys)
  (string-join
   (mapcar (pcase-lambda (`(,k . ,v))
             (format "%s=%s;" k v))
           (with-current-buffer
               (find-file-noselect (expand-file-name (url-curl-cookie)))
             (goto-char (point-min))
             (prog1
                 (cl-delete-duplicates
                  (cl-loop while (re-search-forward
                                  (format "^[^\t]+?%s\t.+?\t\\([^\t]+\\)\t\\([^\t\n]+\\)$"
                                          (url-curl--host-regexp host))
                                  nil t)
                           when (member (match-string 1) keys)
                             collect (cons (match-string 1) (match-string 2)))
                  :test (lambda (a b)
                          (string= (car a) (car b)))
                  ;; :from-end t
                  )
               (kill-buffer (current-buffer)))))
   " "))

(defun url-curl--args (url referer)
  (let ((cache (url-cache-create-filename url)))
    `("-X" ,url-request-method
           "-L" "-s" "--http1.1" "--compressed"
           "--fail-early"
           "-D-"
           ,@(cond
               ((string-match-p url-curl-default-ua-regexp (url-host url))
                (list "-A" (url-http--user-agent-default-string))))
           ,@(when referer
               (list "-e" referer))
           ,@(unless (string= url-request-method "HEAD")
               (when (url-cache-prepare cache)
                 (list "-o" cache)))
           ,@(when-let ((use-cookies (url-use-cookies url))
                        (cookie (expand-file-name (url-curl-cookie))))
               (list "-b" cookie ;; "-c" cookie
                     ))
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
    (cl-case (/ code 100)
      (3                                ; Redirection
       ;; 300 Multiple choices
       ;; 301 Moved permanently
       ;; 302 Found
       ;; 303 See other
       ;; 304 Not modified
       ;; 305 Use proxy
       ;; 307 Temporary redirect
       (when-let ((loc (or (mail-fetch-field "Location")
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
  (let* ((url
          ;; `url-curl--args' expects this to be url-object.
          (if (url-p url)
              url
            (url-generic-parse-url url)))
         (referer (url-curl-referer))
         (cache (url-cache-create-filename url)))
    (with-temp-buffer
      (setq-local url-request-method (or url-request-method "GET"))
      (apply #'call-process "curl" nil t nil
             `(,@(url-curl--args url referer)
                 ,(url-recreate-url url)))
      (let ((status (url-curl-parse-headers nil))
            (inhibit-message t))
        (when (plist-get status :error)
          (ignore))
        (unless (= url-http-response-status 304)
          (goto-char (point-max))
          (insert-file-contents-literally cache)
          (write-region (point-min) (point-max) cache nil 5))))
    (when (file-exists-p cache)
      (with-current-buffer (find-file-noselect cache)
        (goto-char (point-min))
        (search-forward "\n\n")
        (unwind-protect
             (if callback
                 (funcall callback)
               (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer))))))



(declare-function curl-get "ext:curl-core")
(declare-function curl-load-cookies "ext:curl-core")

(defvar url-curl-libcurl nil)

(defun url--libcurl-internal (url referer cache buffer callback cbargs)
  (url-cache-prepare cache)
  (when-let ((status
              (curl-get url
                        (vconcat
                         (cond
                           ((string-match-p url-curl-default-ua-regexp
                                            (url-host url))
                            (list (format "User-Agent: %s"
                                          (url-http--user-agent-default-string)))))
                         (when referer
                           (list (format "Referer: %s" referer)))
                         (mapcar (pcase-lambda (`(,name . ,value))
                                   (format "%s: %s" name value))
                                 url-request-extra-headers))
                        cache)))
    (setf (car cbargs) (nconc status (car cbargs))))
  (when (file-exists-p cache)
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally cache)
      (apply callback cbargs))))

;; (defun url-libcurl-post-data (url callback cbargs &rest _)
;;   ;; TODO "multipart/form-data" for file uploads
;;   )

(defun url-libcurl (url callback cbargs &rest _)
  (let ((referer (url-curl-referer))
        (buffer
         (generate-new-buffer
          (format " *http %s:%d*" (url-host url) (url-port url))))
        (cache (url-cache-create-filename url)))
    (if (url-asynchronous url)
        (make-thread
         (lambda ()
           (url--libcurl-internal url referer cache buffer callback cbargs)))
      (url--libcurl-internal url referer cache buffer callback cbargs))
    buffer))

(defun url-libcurl-load-cookies ()
  (let ((jar (expand-file-name (url-curl-cookie))))
    (when (file-readable-p jar)
      (curl-load-cookies jar))))

(when (and url-curl-libcurl
           (require 'curl-core nil t))
  (url-libcurl-load-cookies)
  (add-hook 'cookie-sync-hooks #'url-libcurl-load-cookies))

(defun url-curl--senginel (url cache callback cbargs)
  (lambda (proc _signal)
    (when (and (zerop (process-exit-status proc))
               (file-exists-p cache))
      (unwind-protect
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-message t))
               (setf (car cbargs)
                     (nconc (url-curl-parse-headers nil)
                            (car cbargs)))
               (let ((header (buffer-string)))
                 (insert-file-contents-literally cache nil nil nil t)
                 (unless (= url-http-response-status 304)
                   (goto-char (point-min))
                   (unless (looking-at-p "HTTP/")
                     ;; We download header and body separetely for
                     ;; ease of header processing. Here we prepend
                     ;; header back to the body, the format url libs
                     ;; expect.
                     (insert header))
                   (write-region (point-min) (point-max) cache nil 5)))
               (when-let ((redir (plist-get (car cbargs) :redirect)))
                 (message "[%s -> %s]" (url-recreate-url url) redir))
               (apply callback cbargs)))
        (url-curl--cache-unlock cache)))))

(defun url-curl (url callback cbargs &rest _)
  (let ((cache (url-cache-create-filename url)))
    (url-curl--cache-lock cache)
    (let ((referer (url-curl-referer))
          (buffer
           (generate-new-buffer
            (format " *http %s:%d*" (url-host url) (url-port url)))))
      (with-current-buffer buffer
        (mm-disable-multibyte)
        (setq url-current-object url)
        (setq-local url-request-method (or url-request-method "GET"))
        (make-process
         :name "url-curl"
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
         :sentinel (url-curl--senginel url cache callback cbargs)))
      buffer)))

(advice-add 'url-http :override #'url-curl)
;; (advice-remove 'url-http #'url-curl)

(provide 'url-curl)
;;; url-curl.el ends here
