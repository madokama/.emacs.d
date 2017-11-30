;;; ping --- ping minor mode for general network connectivity -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'async)

(defgroup ping nil
  "Ping monitor."
  :prefix "ping-"
  :group 'processes)

(defcustom ping-address "192.168.0.1"
  "Default ping address."
  :type 'string)

(defun ping-responsive-p (addr)
  (with-temp-buffer
    (call-process "ping" nil t nil addr)
    (goto-char (point-min))
    (search-forward "(0% loss)" nil t)))

(defvar ping-mode-lighter nil)
;;;###autoload (put 'ping-mode-lighter 'risky-local-variable t)

;;;###autoload
(define-minor-mode ping-mode
    "Toggle ping monitor for general network connectivity."
  :global t
  :group 'ping
  :require 'ping
  (if ping-mode
      (progn
        (setq ping-mode-lighter "")
        (unless global-mode-string
          (setq global-mode-string '("")))
        (add-to-list 'global-mode-string 'ping-mode-lighter t)
        (ping-monitor))
    (delq 'ping-mode-lighter global-mode-string)))

(defun ping-monitor ()
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`load-path\\'")
      (require 'ping)
      (ping-responsive-p ,ping-address))
   (lambda (result)
     (if result
         (setq ping-mode-lighter "")
       (setq ping-mode-lighter
             (propertize (if (char-displayable-p #xf071)
                             (string #xf071)
                           " No connection")
                         'face 'error
                         'help-echo "No network connection")))
     (when ping-mode
       (run-with-idle-timer 60 nil #'ping-monitor)))))

(provide 'ping)
;;; ping.el ends here
