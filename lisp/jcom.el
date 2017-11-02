;;; jcom --- jcom (catv) remote reservation library  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'dom)
(require 'json)
(require 'url-curl)

(defvar jcom-device-name "stb1")
(defvar jcom-record-mode "DR")

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
  (apply #'call-process "curl" nil t nil
         url
         "-sL" "--compressed" "-b" jcom-cookie "-c" jcom-cookie
         "-H" "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:55.0) Gecko/20100101 Firefox/55.0"
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
      (jcom--http "https://tv.myjcom.jp/wishList.action?limit=100")
      (let ((params (jcom--device-params))
            (progs (jcom--wish-list))
            (search (jcom-search-list))
            (reserved (jcom-reserve-list)))
        (list (cons 'common params)
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
  (let ((date (alist-get 'date params))
        (start (alist-get 'startTime params))
        (dur (alist-get 'airTime params)))
    (save-match-data
      (format-time-string
       "%H%M%S"
       (time-add
        (and (string-match "^\\(..\\)\\(..\\)" dur)
             (seconds-to-time
              (+ (* 3600 (string-to-number (match-string 1 dur)))
                 (* 60 (string-to-number (match-string 2 dur))))))
        (apply #'encode-time
               (mapcar #'string-to-number
                       (nconc (and (string-match "^\\(..\\)\\(..\\)" start)
                                   (list "0"
                                         (match-string 2 start)
                                         (match-string 1 start)))
                              (and (string-match "^\\(....\\)\\(..\\)\\(..\\)$"
                                                 date)
                                   (list (match-string 3 date)
                                         (match-string 2 date)
                                         (match-string 1 date)))))))))))

(defun jcom--build-reserve-form (params prog)
  (url-build-query-string
   `((broadCastType ,(alist-get 'channelType prog))
     (startDate ,(alist-get 'date prog))
     (startTime ,(alist-get 'startTime prog))
     (endTime ,(jcom--end-time prog))
     (duration ,(alist-get 'airTime prog))
     (networkId ,(alist-get 'networkId prog))
     (serviceId ,(alist-get 'serviceId prog))
     (eventId ,(alist-get 'eventId prog))
     (title ,(alist-get 'title prog))
     (genreId ,(alist-get 'genreId prog))
     (chName ,(concat (alist-get 'channelName prog)
                      (alist-get 'channelNo prog)))
     ,@params)
   nil t))

(defun jcom--reserve-success-p (result)
  (string= "リモート録画予約が完了しました。" (caddr (cadr result))))

;;;###autoload
(defun jcom-make-reservation (data)
  (let ((results
         (mapcar (lambda (prog)
                   (sleep-for (random 10) 600)
                   (with-temp-buffer
                     (jcom-ajax-post "https://tv.myjcom.jp/remoteRecSubmit.action"
                                     "https://tv.myjcom.jp/wishList.action?limit=100"
                                     (jcom--build-reserve-form
                                      (alist-get 'common data)
                                      prog))
                     (let ((dom
                            (libxml-parse-html-region (point-min) (point-max))))
                       (list (alist-get 'title prog)
                             (car (dom-by-id dom "recResultTitle"))
                             (car (dom-by-class dom "cont"))))))
                 (alist-get 'programs data))))
    (or (seq-every-p #'jcom--reserve-success-p results)
        results)))

(defun jcom-search-id (id)
  (sleep-for (random 10) 600)
  (with-temp-buffer
    (save-match-data
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
                 dom))))))

(defun jcom-search-list ()
  (with-temp-buffer
    (save-match-data
      (jcom--http "https://tv.myjcom.jp/jcom-pc/mySearchList.action")
      (mapcan (lambda (dom)
                (jcom-search-id (dom-attr (dom-by-tag dom 'input) 'value)))
              (dom-by-class (libxml-parse-html-region (point-min) (point-max))
                            "mySearchListBox")))))

(provide 'jcom)
;;; jcom.el ends here
