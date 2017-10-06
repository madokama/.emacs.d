;;; mpa --- $ mpv --no-video --shuffle -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'multi-term)

(defvar mpa-scrapers-alist nil)

;;;###autoload
(defun mpa-play (lst)
  (mpa-stop)
  (let ((proc
         (with-current-buffer
             (make-term "mpa" multi-term-program nil multi-term-program-switches)
           (multi-term-internal)
           (get-buffer-process (current-buffer)))))
    (process-send-string proc
                         (concat
                          (string-join
                           (cl-list* "mpv" "--no-video" "--shuffle"
                                     "--keep-open=no" "--volume=80"
                                     "--no-resume-playback"
                                     "--msg-level=all=no,ytdl_hook=info"
                                     "--ytdl-raw-options=no-mark-watched="
                                     lst)
                           " ")
                          "\n"))
    (set-process-filter proc #'mpa--filter)))

(defun mpa--filter (_proc output)
  (save-match-data
    (when (string-match (rx "[ytdl_hook] " (group (* (not (any control))))) output)
      (message "[mpa] %s" (decode-coding-string (match-string 1 output)
                                                locale-coding-system)))))

(defun mpa--current-process ()
  (car
   (cl-delete-if-not (lambda (p)
                       (and (string-match-p (rx bos "mpa" eow) (process-name p))
                            (eq (process-status p) 'run)))
                     (process-list))))

(defun mpa--send-key (key)
  "Send input KEY to the mpa process.
Return K if successfull, nil otherwise."
  (when-let* ((p (mpa--current-process)))
    (with-current-buffer (process-buffer p)
      (term-send-raw-string key))
    key))

(defun mpa-next ()
  (mpa--send-key ">"))

(defun mpa-stop ()
  (when-let* ((p (mpa--current-process)))
    (delete-process p)
    (kill-buffer (process-buffer p))))

(declare-function org-make-link-string "org")

(defun mpa-recv-url (send)
  (let ((kill-ring nil))
    (and (funcall send "y")
         (cl-loop for i from 10 downto 0
                  do (sleep-for 0.5)
                  if (ignore-errors (current-kill 0))
                    return (current-kill 0)
                  else if (zerop i) return nil))))

(defun mpa-now-playing ()
  (when-let* ((url (mpa-recv-url #'mpa--send-key)))
    (let-alist (ytdl-get-json url)
      (insert "\n"
              (org-make-link-string url .title)
              "\n#+BEGIN_QUOTE\n"
              .description
              "\n#+END_QUOTE\n"))))

;;;###autoload
(defun mpa-scrape (&rest urls)
  (mapcan (lambda (url)
            (seq-some (pcase-lambda (`(,pat . ,fn))
                        (when (string-match-p pat url)
                          (with-temp-buffer
                            (call-process "curl" nil t nil "-s" url)
                            (goto-char (point-min))
                            (save-match-data
                              (funcall fn)))))
                      mpa-scrapers-alist))
          urls))

(defun mpa-scrape-pianeys ()
  (cl-loop while (re-search-forward "<td class=\"movie\"><a href=\"\\(.+?\\)\""
                                    nil t)
           collect (match-string 1)))

(add-to-list 'mpa-scrapers-alist '("\\<pianeys\\.com" . mpa-scrape-pianeys))

(provide 'mpa)
;;; mpa.el ends here
