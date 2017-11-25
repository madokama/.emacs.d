;;; jcom --- jcom (catv) remote reservation library  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'dom)
(require 'json)
(require 'url-curl)
(require 'async)

(defgroup jcom nil
  "Abema TV schedules."
  :prefix "jcom-"
  :group 'external
  :group 'comm)

(defcustom jcom-device-name "stb1"
  "Name of the primary JCOM recording device."
  :type 'string)

(defcustom jcom-record-mode "DR"
  "Preferred recording mode."
  :type '(choice
          (const "DR")
          (const "2倍")
          (const "3倍")
          (const "5倍")
          (const "7倍")))

(defvar jcom-cookie nil)

(defun jcom--prepare-cookie ()
  (let ((tmp (make-temp-file "jcom")))
    (with-temp-buffer
      (insert-file-contents-literally (expand-file-name (url-curl-cookie)))
      (goto-char (point-min))
      (while (re-search-forward (rx bol "#") nil t))
      (re-search-forward "\\(?:\r?\n\\)+")
      (keep-lines (rx bol (opt ".tv") ".myjcom.jp"))
      (write-region nil nil tmp))
    (setq jcom-cookie tmp)))

(defun jcom-refresh-cookie ()
  (when jcom-cookie
    (delete-file jcom-cookie)
    (setq jcom-cookie nil))
  (jcom--prepare-cookie))

(defun jcom--http (url &optional headers data)
  (unless jcom-cookie
    (jcom--prepare-cookie))
  (sleep-for (random 10) 600)
  (apply #'call-process "curl" nil t nil
         url
         "-sL" "--compressed" "-b" jcom-cookie "-c" jcom-cookie
         "-A" url-user-agent
         "-H" "DNT: 1"
         (nconc (mapcan (pcase-lambda (`(,k . ,v))
                          (list "-H" (format "%s: %s" k v)))
                        headers)
                (when data
                  (list
                   "-H" "Content-Type: application/x-www-form-urlencoded; charset=UTF-8"
                   "-d" data)))))

(defun jcom--ajax-headers (referer)
  `(("X-Requested-With" . "XMLHttpRequest")
    ("Referer" . ,referer)))

(defun jcom-ajax-get (url referer)
  (jcom--http url (jcom--ajax-headers referer)))

(defun jcom-ajax-post (url referer data)
  (jcom--http url (jcom--ajax-headers referer) data))

(defun jcom-reserve-list ()
  (with-temp-buffer
    (save-match-data
      (let ((location "https://tv.myjcom.jp/jcom-pc/remoteRecReservelist.action"))
        (jcom--http location)
        (erase-buffer)
        (jcom-ajax-get "https://tv.myjcom.jp/jcom-pc/remoteRecList.action?recListType=1&limit=100"
                       location)
        (goto-char (point-min))
        (cl-loop while (re-search-forward (rx "detail.action?"
                                              (group (+ (not (any "\"")))))
                                          nil t)
                 collect (let ((params
                                (url-parse-query-string (match-string 1))))
                           (cl-flet ((get (k)
                                       (cadr (assoc k params))))
                             (let ((serviceCode
                                    (split-string (get "serviceCode") "_")))
                               ;; Collect program data to later weed
                               ;; out reserved programs from the wish
                               ;; list. Since `eventId' is not unique
                               ;; enough, we record several other
                               ;; parameters as well.
                               `((channelType . ,(get "channelType"))
                                 (serviceId . ,(car serviceCode))
                                 (networkId . ,(cadr serviceCode))
                                 (eventId . ,(get "eventId")))))))))))

(defsubst jcom--parse-html-region (begin end)
  (thread-last (libxml-parse-html-region begin end)
    caddr caddr))

(defun jcom--device-params ()
  (goto-char (point-min))
  (when (re-search-forward (rx "<div id=\"recDialogContents" (+? anything) "\n</div>")
                           nil t)
    (let* ((dom
            (jcom--parse-html-region (match-beginning 0) (match-end 0)))
           (device
            (dom-attr (seq-find (lambda (dom)
                                  (string= jcom-device-name
                                           (car (dom-strings dom))))
                                (dom-by-tag (dom-by-id dom "deviceListNumber")
                                            'option))
                      'value))
           (mode
            (dom-attr (seq-find (lambda (dom)
                                  (string= jcom-record-mode
                                           (car (dom-strings dom))))
                                (dom-by-tag (dom-by-class dom
                                                          (format "mode%s" device))
                                            'option))
                      'value))
           (flag (or (dom-attr (dom-by-id dom "diskSpaceError")
                               'value)
                     "false")))
      `((deviceListNumber ,device)
        (desiredQueryHardDiskId "")
        (desiredQueryMode ,mode)
        (diskSpaceErrorFlg ,flag)
        (withinPeriod "true")))))

(defun jcom--wish-list ()
  (goto-char (point-min))
  (cl-loop while (re-search-forward "doRemoteRec(\\(.+?}\\)" nil t)
           collect (thread-last (jcom--parse-html-region
                                 (match-beginning 1) (match-end 1))
                     caddr
                     json-read-from-string)))

;;;###autoload
(defun jcom-wish-list ()
  (with-temp-buffer
    (save-match-data
      (jcom--http "https://tv.myjcom.jp/wishList.action")
      (let ((params (jcom--device-params))
            (progs (jcom--wish-list))
            (search (jcom-search-list))
            (reserved (jcom-reserve-list)))
        (list (cons 'common params)
              (cons 'cookie jcom-cookie)
              (cons 'programs
                    ;; Remove already reserved programs
                    (cl-delete-if
                     (lambda (prog)
                       (seq-find (lambda (res)
                                   (seq-every-p (pcase-lambda (`(,k . ,v))
                                                  (string= v (alist-get k prog)))
                                                res))
                                 reserved))
                     (cl-delete-duplicates
                      (nconc progs search)
                      :test (lambda (a b)
                              (string= (alist-get 'programId a)
                                       (alist-get 'programId b)))
                      :from-end t))))))))

(defun jcom--end-time (params)
  (let-alist params
    (save-match-data
      (format-time-string
       "%H%M%S"
       (time-add
        (and (string-match "^\\(..\\)\\(..\\)" .airTime)
             (seconds-to-time
              (+ (* 3600 (string-to-number (match-string 1 .airTime)))
                 (* 60 (string-to-number (match-string 2 .airTime))))))
        (apply #'encode-time
               (mapcar #'string-to-number
                       (nconc (and (string-match "^\\(..\\)\\(..\\)" .startTime)
                                   (list "0"
                                         (match-string 2 .startTime)
                                         (match-string 1 .startTime)))
                              (and (string-match "^\\(....\\)\\(..\\)\\(..\\)$"
                                                 .date)
                                   (list (match-string 3 .date)
                                         (match-string 2 .date)
                                         (match-string 1 .date)))))))))))

(defun jcom--build-reserve-form (params prog)
  (url-build-query-string
   (let-alist prog
     `((broadCastType , .channelType)
       (startDate , .date)
       (startTime , .startTime)
       (endTime ,(jcom--end-time prog))
       (duration , .airTime)
       (networkId , .networkId)
       (serviceId , .serviceId)
       (eventId , .eventId)
       (title , .title)
       (genreId , .genreId)
       (chName ,(concat .channelName .channelNo))
       ,@params))
   nil t))

(defun jcom--reserve-success-p (result)
  (string= "リモート録画予約が完了しました。" (caddr (cadr result))))

(defun jcom--reserve1 (params)
  (lambda (prog)
    (let-alist prog
      (with-temp-buffer
        (jcom-ajax-post "https://tv.myjcom.jp/remoteRecSubmit.action"
                        "https://tv.myjcom.jp/wishList.action"
                        (jcom--build-reserve-form params prog))
        (let ((dom
               (libxml-parse-html-region (point-min) (point-max))))
          (list .title
                (car (dom-by-id dom "recResultTitle"))
                (car (dom-by-class dom "cont"))))))))

;;;###autoload
(defun jcom-make-reservation (data)
  (let-alist data
    (when .cookie
      (setq jcom-cookie .cookie))
    (mapcar (jcom--reserve1 .common) .programs)))

(defun jcom-search-id (id)
  (with-temp-buffer
    (jcom--http (format "https://tv.myjcom.jp/mySearch.action?searchId=%s&p=1" id))
    (let ((dom
           (thread-first (libxml-parse-html-region (point-min) (point-max))
             (dom-by-id "resultArea")
             (dom-by-tag 'tbody)
             (dom-by-tag 'table))))
      (mapcar (lambda (dom)
                (let ((json
                       (json-read-from-string
                        (dom-attr (dom-by-class dom "inner")
                                  'data-program-json)))
                      (desc (thread-first (dom-by-class dom "desbox")
                              (dom-by-tag 'dd)
                              dom-strings
                              car)))
                  (nconc json (list (cons 'commentary desc)))))
              (cl-delete-if-not
               (lambda (dom)
                 (seq-find (lambda (dom)
                             (when-let* ((onclick (dom-attr dom 'onclick)))
                               (string-match-p "^doRemoteRec" onclick)))
                           (dom-by-tag (dom-by-class dom "resBox02") 'img)))
               dom)))))

(defun jcom-search-list ()
  (with-temp-buffer
    (jcom--http "https://tv.myjcom.jp/jcom-pc/mySearchList.action")
    (mapcan (lambda (dom)
              (jcom-search-id (dom-attr (dom-by-tag dom 'input) 'value)))
            (dom-by-class (libxml-parse-html-region (point-min) (point-max))
                          "mySearchListBox"))))

(defun jcom-program-page (prog)
  (let-alist prog
    (format "https://tv.myjcom.jp/jcom-pc/detail.action?channelType=%s&serviceCode=%s_%s&eventId=%s&programDate=%s"
            .channelType .serviceId .networkId .eventId .date)))

;;; Major mode

(defvar-local jcom-meta nil)

(defvar jcom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jcom-mode-enter)
    (define-key map (kbd "m") #'jcom-mode-mark)
    (define-key map (kbd "t") #'jcom-mode-mark-all)
    (define-key map (kbd "R") #'jcom-mode-do-reserve)
    (define-key map (kbd "C-d") #'jcom-mode-delete)
    map))

(defun jcom--program-at-point ()
  (get-text-property (point) 'jcom))

(defun jcom--marked-programs ()
  ;; List program at point if none marked.
  (or (save-excursion
        (goto-char (point-min))
        (cl-loop while (not (eobp))
                 if (eq (following-char) ?\*)
                   collect (jcom--program-at-point)
                 end
                 do (goto-char (1+ (line-end-position)))))
      (when-let* ((prog (jcom--program-at-point)))
        (list prog))))

(defun jcom-mode-mark ()
  "Mark/unmark the program at point."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (line-beginning-position))
      (let ((mark (following-char)))
        (subst-char-in-region (point) (1+ (point))
                              mark
                              (if (eq mark ?\ ) ?\* ?\ )
                              t)))))

(defun jcom-mode-mark-all ()
  "Toggle marks for all programs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (jcom-mode-mark)
      (goto-char (1+ (line-end-position))))))

(defun jcom-mode-do-reserve ()
  "Reserve marked programs.  If none, reserve the program at point."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'jcom)
      (jcom-make-reservation
       ',(list (cons 'common jcom-meta)
               (cons 'cookie jcom-cookie)
               (cons 'programs (jcom--marked-programs)))))
   (lambda (results)
     (if (seq-every-p #'jcom--reserve-success-p results)
         (message "[JCOM] Done.")
       (error "[JCOM] %S" results)))))

(define-derived-mode jcom-mode
  special-mode "JCOM"
  "Major mode for JCOM online reservation interface.

\\{jcom-mode-map}"
  (setq-local revert-buffer-function #'jcom-list-programs))

(defun jcom-mode-enter ()
  "Reserve program at point."
  (interactive)
  (browse-url (jcom-program-page (jcom--program-at-point))))

(defun jcom-mode-delete ()
  "Delete program at point."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (1+ (line-end-position)))))

;;;###autoload
(defun jcom-list-programs ()
  "Show JCOM TV programs for online reservation."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'jcom)
      (jcom-wish-list))
   (lambda (result)
     (let-alist result
       (if .programs
           (let ((buf (get-buffer-create "*jcom schedule*")))
             (with-current-buffer buf
               (unless (derived-mode-p 'jcom-mode)
                 (jcom-mode))
               (setq jcom-meta .common)
               (setq jcom-cookie .cookie)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (mapc (lambda (prog)
                         (let-alist prog
                           (insert
                            (propertize (format " %s|%s\n" .channelName .title)
                                        'jcom prog
                                        'help-echo .commentary))))
                       .programs))
               (goto-char (point-min)))
             (pop-to-buffer buf))
         (message "No programs found."))))))

(provide 'jcom)
;;; jcom.el ends here
