;;; jcom --- jcom (catv) remote reservation library  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'dom)
(require 'json)
(require 'cookie-sync)
(require 'async)
(require 'url-expand)

(defgroup jcom nil
  "JCOM TV schedules."
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

(defun jcom-prepare-cookie ()
  (let ((tmp (make-temp-file "jcom")))
    (cookie-sync-try (cookie-sync-params-list '(".myjcom.jp" ".tv.myjcom.jp"))
                     tmp)
    (setq jcom-cookie tmp)))

(defun jcom--async-common ()
  (list (async-inject-variables "\\`\\(?:load-path\\|url-user-agent\\)\\'")
        '(require 'jcom)))

(defvar url-user-agent)

(defun jcom--http (url &optional headers data)
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

(defsubst jcom--dom-string (dom)
  (car (dom-strings dom)))

(defun jcom--parse-reserve-params (url)
  (when (stringp url)
    (let ((params
           (url-parse-query-string
            (cadr (split-string url "\\?")))))
      (cl-flet ((get (k)
                  (cadr (assoc k params))))
        (let ((serviceCode
               (split-string (get "serviceCode") "_")))
          ;; Collect program data to later weed out reserved programs
          ;; from the wish list. Since `eventId' is not unique enough,
          ;; we record several other parameters as well.
          `((date . ,(get "programDate"))
            (channelType . ,(get "channelType"))
            (serviceId . ,(car serviceCode))
            (networkId . ,(cadr serviceCode))
            (eventId . ,(get "eventId"))))))))

(defun jcom--reserved-prog/url (dom)
  "Collect data from the program url in DOM."
  (thread-first dom
    (dom-by-class (rx bos "prName"))
    (dom-by-tag 'a)
    (dom-attr 'href)
    jcom--parse-reserve-params))

(defun jcom--reserved-prog/dom (dom)
  "Collect the rest of metadata from #recDataN-N in DOM."
  (let ((data
         (dom-by-id dom (rx bos "recData"))))
    (cl-flet ((get (k)
                (jcom--dom-string (dom-by-class data k))))
      `((title . ,(get "recTitle"))
        (startTime . ,(get "recStart"))
        (endTime . ,(get "recEnd"))
        (airTime . ,(get "recMins"))
        (channelName . ,(get "recChName"))
        (channelNo . ,(get "recChNo"))
        (itemId . ,(get "recItemId"))))))

(defun jcom--reserved-prog-params (dom)
  (nconc (jcom--reserved-prog/url dom)
         (jcom--reserved-prog/dom dom)))

(defun jcom-reserve-list ()
  (with-temp-buffer
    (save-match-data
      (let ((location "https://tv.myjcom.jp/jcom-pc/remoteRecReservelist.action"))
        (jcom--http location)
        (erase-buffer)
        (jcom-ajax-get "https://tv.myjcom.jp/jcom-pc/remoteRecList.action?recListType=1&limit=100"
                       location)
        (if-let ((progs
                  (cl-delete-if
                   (lambda (dom)
                     (dom-by-tag dom 'th))
                   (thread-first (libxml-parse-html-region (point-min) (point-max))
                     (dom-by-id "recList")
                     car
                     (dom-by-tag 'tr)))))
            (mapcar #'jcom--reserved-prog-params progs)
          (message "WARN:%s" (buffer-string))
          nil)))))

(defsubst jcom--parse-html-region (begin end)
  (thread-last (libxml-parse-html-region begin end nil)
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
                                           (jcom--dom-string dom)))
                                (dom-by-tag (dom-by-id dom "deviceListNumber")
                                            'option))
                      'value))
           (mode
            (dom-attr (seq-find (lambda (dom)
                                  (string= jcom-record-mode
                                           (jcom--dom-string dom)))
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

(defun jcom-wish-list ()
  (with-temp-buffer
    (save-match-data
      (jcom--http "https://tv.myjcom.jp/wishList.action")
      (let ((stb (jcom--device-params))
            (progs (jcom--wish-list))
            (search (jcom-search-list))
            (reserved (jcom-reserve-list)))
        (list (cons 'stb stb)
              (cons 'cookie jcom-cookie)
              (cons 'reserved reserved)
              (cons 'programs
                    (seq-sort-by
                     (lambda (prog)
                       (let-alist prog
                         (if .date
                             (string-to-number .date)
                           0)))
                     #'<
                     ;; Remove already reserved programs
                     (cl-delete-if
                      (lambda (prog)
                        (seq-find (lambda (res)
                                    (cl-every
                                     (lambda (k)
                                       (string= (alist-get k res)
                                                (alist-get k prog)))
                                     '(date channelType serviceId networkId eventId)))
                                  reserved))
                      (cl-delete-duplicates
                       (nconc progs search)
                       :test (lambda (a b)
                               (and (string= (alist-get 'programId a)
                                             (alist-get 'programId b))
                                    (string= (alist-get 'eventId a)
                                             (alist-get 'eventId b))))
                       :from-end t)))))))))

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

(defun jcom--build-reserve-form (stb prog)
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
       ,@stb))
   nil t))

(defun jcom--reserve-success-p (result)
  (string= "リモート録画予約が完了しました。" (cadr result)))

(defun jcom--reserve1 (stb)
  (lambda (prog)
    (let-alist prog
      (with-temp-buffer
        (jcom-ajax-post "https://tv.myjcom.jp/remoteRecSubmit.action"
                        "https://tv.myjcom.jp/wishList.action"
                        (jcom--build-reserve-form stb prog))
        (let ((dom
               (libxml-parse-html-region (point-min) (point-max) nil)))
          (list .title
                (jcom--dom-string (car (dom-by-id dom "recResultTitle")))
                (car (dom-by-class dom "cont"))))))))

(defun jcom-make-reservation (data)
  (let-alist data
    (when .cookie
      (setq jcom-cookie .cookie))
    (cons (cons 'results
                (mapcar (jcom--reserve1 .stb) .programs))
          data)))

(defun jcom--search-keyword (dom)
  "Extract search condition from the DOM of the searchId page."
  (seq-let (key _date _chan genre)
      (mapcar #'jcom--dom-string
              (thread-first dom
                (dom-by-class "\\`condition\\'")
                (dom-by-class "value")))
    (if (string= key "なし")
        genre
      key)))

(defun jcom-search-id (id)
  (with-temp-buffer
    (jcom--http (format "https://tv.myjcom.jp/mySearch.action?searchId=%s&p=1" id))
    (let* ((dom
            (libxml-parse-html-region (point-min) (point-max) nil))
           (key (jcom--search-keyword dom))
           (dom-result
            (thread-first dom
              (dom-by-id "resultArea")
              (dom-by-tag 'tbody)
              (dom-by-tag 'table))))
      (message "INFO:%s" key)
      (mapcar (lambda (dom)
                (let ((json
                       (json-read-from-string
                        (dom-attr (dom-by-class dom "inner")
                                  'data-program-json)))
                      (desc (thread-first (dom-by-class dom "desbox")
                              (dom-by-tag 'dd)
                              jcom--dom-string)))
                  (nconc json (list (cons 'commentary desc)
                                    (cons 'keyword key)))))
              (cl-delete-if-not
               (lambda (dom)
                 (seq-find (lambda (dom)
                             (when-let ((onclick (dom-attr dom 'onclick)))
                               (string-match-p "^doRemoteRec" onclick)))
                           (dom-by-tag (dom-by-class dom "resBox02") 'img)))
               dom-result)))))

(defun jcom-search-list ()
  (with-temp-buffer
    (jcom--http "https://tv.myjcom.jp/jcom-pc/mySearchList.action")
    (mapcan (lambda (dom)
              (jcom-search-id (dom-attr (dom-by-tag dom 'input) 'value)))
            (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil)
                          "mySearchListBox"))))

(defun jcom-program-page (prog)
  (let-alist prog
    (format "https://tv.myjcom.jp/jcom-pc/detail.action?channelType=%s&serviceCode=%s_%s&eventId=%s&programDate=%s"
            .channelType .serviceId .networkId .eventId .date)))

;;; Major mode

(defvar-local jcom-stb nil)

(defvar-local jcom-reserved nil)

(defvar jcom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jcom-mode-enter)
    (define-key map (kbd "m") #'jcom-mode-mark)
    (define-key map (kbd "n") #'jcom-mode-next-line)
    (define-key map (kbd "p") #'jcom-mode-prev-line)
    (define-key map (kbd "t") #'jcom-mode-mark-all)
    (define-key map (kbd "U") #'jcom-mode-unmark-all)
    (define-key map (kbd "R") #'jcom-mode-reserve)
    (define-key map (kbd "C-d") #'jcom-mode-delete)
    (define-key map [remap undo] #'jcom-mode-undo)
    map))

(defun jcom-program-at-point ()
  (get-text-property (point) 'jcom))

(defun jcom-program-marked-p ()
  (= (char-after (line-beginning-position)) ?*))

(defun jcom-marked-programs ()
  ;; List program at point if none marked.
  (or (save-excursion
        (goto-char (point-min))
        (cl-loop while (not (eobp))
                 if (jcom-program-marked-p)
                   collect (jcom-program-at-point)
                 end
                 do (forward-line 1)))
      (when-let ((prog (jcom-program-at-point)))
        (list prog))))

(defun jcom-mode-iterate (fn)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (funcall fn)
        (forward-line 1)))))

(defun jcom-mode-toggle-mark ()
  (let* ((bol (line-beginning-position))
         (mark (char-after bol)))
    (subst-char-in-region bol (1+ bol)
                          mark (if (eq mark ? ) ?* ? ) t)))

(defun jcom-mode-mark ()
  "Mark/unmark the program at point."
  (interactive)
  (let ((inhibit-read-only t))
    (jcom-mode-toggle-mark)
    (forward-line 1)))

(defun jcom-mode-mark-all ()
  "Toggle marks for all programs."
  (interactive)
  (jcom-mode-iterate
   (lambda ()
     (jcom-mode-toggle-mark))))

(defun jcom-mode-unmark-all ()
  "Remove all marks."
  (interactive)
  (jcom-mode-iterate
   (lambda ()
     (when (jcom-program-marked-p)
       (jcom-mode-toggle-mark)))))

(defun jcom-mode-next-line ()
  "Move cursor down."
  (interactive)
  (forward-line 1))

(defun jcom-mode-prev-line ()
  "Move cursor up."
  (interactive)
  (forward-line -1))

(defun jcom-mode-undo ()
  "Undo in a JCOM buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(defun jcom--with-warn-buffer (output)
  (with-current-buffer
      (pop-to-buffer "*jcom warning*")
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (functionp output)
          (funcall output)
        (insert output)))))

(defun jcom--report-reserve-error (result)
  (jcom--with-warn-buffer
   (lambda ()
     (shr-insert-document
      `(body
        nil
        ,@(mapcan
           (pcase-lambda (`(,title ,msg ,detail))
             `((h1 nil ,msg)
               (h2 nil "番組: " ,title)
               (p nil ,(jcom--dom-string (dom-by-class detail "errMessage")))
               (ul nil
                   ,@(mapcar
                      (lambda (dom)
                        (let ((a (dom-by-tag dom 'a)))
                          `(li nil
                               (a ((href . ,(url-expand-file-name (dom-attr a 'href)
                                                                  "https://tv.myjcom.jp/")))
                                  ,(jcom--dom-string a)))))
                      (dom-by-class detail "recordedDetail")))))
           result))))))

(defun jcom-mode-reserve ()
  "Reserve marked programs.  If none, reserve the program at point."
  (interactive)
  (message "[JCOM] Making reservations...")
  (async-start
   `(lambda ()
      ,@(jcom--async-common)
      (jcom-make-reservation
       ',(list (cons 'stb jcom-stb)
               (cons 'reserved jcom-reserved)
               (cons 'cookie jcom-cookie)
               (cons 'programs (jcom-marked-programs)))))
   (lambda (data)
     (let-alist data
       (if-let ((failed
                 (cl-delete-if #'jcom--reserve-success-p .results)))
           (jcom--report-reserve-error failed)
         (message "[JCOM] Done."))))))


(define-derived-mode jcom-mode
  special-mode "JCOM"
  "Major mode for JCOM online reservation interface.

\\{jcom-mode-map}"
  (setq-local revert-buffer-function #'jcom-list-programs))

(defun jcom-mode-enter ()
  "Reserve program at point."
  (interactive)
  (browse-url (jcom-program-page (jcom-program-at-point))))

(defun jcom-mode-delete ()
  "Delete program at point."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (1+ (line-end-position)))))

(defvar jcom-program-format-function #'jcom-program-format-default)

(defun jcom-program-format-default (prog)
  (let-alist prog
    (format "%s|[%s]%s" .channelName (or .keyword "") .title)))

;;;###autoload
(defun jcom-list-programs (&rest _)
  "Show JCOM TV programs for online reservation."
  (interactive)
  (jcom-prepare-cookie)
  (async-start
   `(lambda ()
      ,@(jcom--async-common)
      (setq jcom-cookie ,jcom-cookie)
      (jcom-wish-list))
   (lambda (result)
     (let-alist result
       (if .programs
           (let ((buf (get-buffer-create "*jcom schedule*")))
             (with-current-buffer buf
               (unless (derived-mode-p 'jcom-mode)
                 (jcom-mode))
               (setq jcom-stb .stb
                     jcom-reserved .reserved)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (mapc (lambda (prog)
                         (let-alist prog
                           (insert
                            (propertize
                             (concat " "
                                     (funcall jcom-program-format-function prog))
                             'jcom prog
                             'help-echo .commentary)
                            "\n")))
                       .programs))
               (goto-char (point-min)))
             (pop-to-buffer buf))
         (message "[JCOM] No programs found.")))))
  (set-process-filter async--procvar
                      (lambda (proc output)
                        (save-match-data
                          (cond ((string-prefix-p "INFO:" output)
                                 (let ((message-log-max nil))
                                   (message "[JCOM:P]%s"
                                            (string-trim-right
                                             (substring output 5)))))
                                ((string-prefix-p "WARN:" output)
                                 (jcom--with-warn-buffer (substring output 5)))
                                (t
                                 (with-current-buffer (process-buffer proc)
                                   (insert output))))))))

(provide 'jcom)
;;; jcom.el ends here
