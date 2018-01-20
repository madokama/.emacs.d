;;; mpa --- $ mpv --no-video --shuffle -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'multi-term)

(defgroup mpa ()
  "Audio player interface for mpv."
  :prefix "mpa-"
  :group 'multimedia)

(defcustom mpa-shuffle t
  "Special option to turn shuffle mode on or off."
  :type 'boolean)

(defcustom mpa-options nil
  "Command-line options to pass to mpv."
  :type '(repeat string))

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
                           `("mpv" ,@(when mpa-shuffle '("--shuffle"))
                                   ,@mpa-options
                                   "--no-video" "--keep-open=no"
                                   ;; "--msg-level=all=no,ytdl_hook=info"
                                   ,@(mpa-scrape lst))
                           " ")
                          "\n"))
    (set-process-filter proc #'mpa--filter)))

(defun mpa--filter (_proc output)
  (save-match-data
    (cond ((string-match (rx "Volume: " (group (+ digit))) output)
           (message "[mpa] vol: %s" (match-string 1 output)))
          ((string-match (rx "[ytdl_hook] " (group (+? anything)) "[0K")
                         output)
           (message "[mpa] %s"
                    (decode-coding-string
                     (replace-regexp-in-string (rx (or "[0K" "\r" "\n"))
                                               ""
                                               (match-string 1 output))
                     locale-coding-system))))))

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

(defun mpa-interact ()
  (cl-block nil
    (while t
      (if-let* ((char (read-char "mpa>")))
          (progn
            (mpa--send-key (string char))
            (when (memq char '(?q ?Q))
              (cl-return)))
        (cl-return)))))

(defun mpa-next ()
  (mpa--send-key ">"))

(defun mpa-pause ()
  (mpa--send-key " "))

(defun mpa-volume-down ()
  (mpa--send-key "/"))

(defun mpa-volume-up ()
  (mpa--send-key "*"))

(defun mpa-loop ()
  (mpa--send-key "L"))

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
    (let-hash (ytdl-get-json url)
      (insert "\n"
              (org-make-link-string url .title)
              "\n#+BEGIN_QUOTE\n"
              .description
              "\n#+END_QUOTE\n"))))

(defun mpa-scrape (urls)
  (mapcan (lambda (url)
            (or (seq-some (pcase-lambda (`(,pat . ,fn))
                            (when (string-match-p pat url)
                              (with-temp-buffer
                                (call-process "curl" nil t nil "-s" url)
                                (goto-char (point-min))
                                (save-match-data
                                  (funcall fn)))))
                          mpa-scrapers-alist)
                (list url)))
          urls))

(defun mpa-scrape-pianeys ()
  (cl-loop while (re-search-forward "<td class=\"movie\"><a href=\"\\(.+?\\)\""
                                    nil t)
           collect (match-string 1)))

(add-to-list 'mpa-scrapers-alist '("\\<pianeys\\.com" . mpa-scrape-pianeys))

(provide 'mpa)
;;; mpa.el ends here
