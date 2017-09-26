;;; abema --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'json)

(defvar browse-url-generic-program)
(defvar abema-watch-browser browse-url-generic-program)

(defvar abema-json nil)
(defvar abema-auth nil)

;;

(defun abema-date (time)
  (format-time-string "%Y%m%d" time))

(defun abema-today ()
  (abema-date (current-time)))

(defun abema-tomorrow ()
  (abema-date (time-add (current-time) (* 24 60 60))))

(defun abema-schedule-json (from to chan)
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-s" "--compressed"
                  "-H" "Host: api.abema.io"
                  "-H" "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:53.0) Gecko/20100101 Firefox/53.0"
                  "-H" "Accept: */*"
                  "-H" "Accept-Language: en-US,en;q=0.7,ja;q=0.3"
                  "-H" "Content-Type: application/json"
                  "-H" (format "Authorization: bearer %s" abema-auth)
                  "-H" "Origin: https://abema.tv"
                  "-H" "DNT: 1"
                  "-H" "Connection: keep-alive"
                  (format "https://api.abema.io/v1/media?dateFrom=%s&dateTo=%s&channelIds=%s"
                          from to chan))
    (goto-char (point-min))
    (json-read)))

(defun abema-token ()
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-s" "--compressed"
                  "-H" "Host: api.abema.io"
                  "-H" "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:53.0) Gecko/20100101 Firefox/53.0"
                  "-H" "Accept: */*"
                  "-H" "Accept-Language: en-US,en;q=0.7,ja;q=0.3"
                  "-H" "Content-Type: application/json"
                  "-H" (format "Authorization: bearer %s" abema-auth)
                  "-H" "Origin: https://abema.tv"
                  "-H" "DNT: 1"
                  "-H" "Connection: keep-alive"
                  "-H" "Cache-Control: max-age=0"
                  "https://api.abema.io/v1/media/token?osName=pc&osVersion=1.0.0&osLang=en-US&osTimezone=Asia%2FTokyo&appVersion=v3.2.3")
    (goto-char (point-min))
    (json-read)))

(defun abema-fresh-p (chan)
  (and abema-json
       (assq chan abema-json)
       (time-less-p (current-time)
                    (time-add (get chan 'abema) (* 24 60 60)))))

(defun abema-json (chan)
  (unless (abema-fresh-p chan)
    (setq abema-json
          (cons (cons chan
                      (alist-get 'channelSchedules
                                 (abema-schedule-json
                                  (abema-today) (abema-tomorrow) chan)))
                (assq-delete-all chan abema-json)))
    (put chan 'abema (current-time)))
  (alist-get chan abema-json))

(defun abema-program (json)
  (let-alist json
    (list .startAt
          .endAt
          .title
          (format "https://abema.tv/channels/%s/slots/%s" .channelId .id)
          (thread-last (aref .programs 0)
            (alist-get 'credit)
            (alist-get 'casts)))))

;;; Entry points

(defun abema-schedule (chan)
  (let ((now (current-time)))
    (seq-drop-while (lambda (prog)
                      (time-less-p (cadr prog) now))
                    ;; FIXME can't do this inside abema-json
                    (mapcan (lambda (date)
                              (mapcar #'abema-program
                                      (alist-get 'slots date)))
                            (abema-json chan)))))

(defun abema-watch (url)
  (let ((browse-url-generic-program abema-watch-browser))
    (browse-url-generic url)))

(provide 'abema)
;;; abema.el ends here