;;; le-extempore --- lispy support for Extempore -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lispy)
(require 'extempore-mode)

;; Async eval

(defun lispy-eval-extempore (&optional _plain)
  (let ((bnd (lispy--bounds-dwim)))
    (extempore-send-region (car bnd) (cdr bnd))))

(add-to-list 'lispy-eval-alist '(extempore-mode lispy-eval-extempore le-extempore))

;; Sync eval

(defvar lispy--eval-extempore-timeout 10
  "Timeout in seconds for synchronous evaluation.")

(defun lispy--eval-extempore-running-p (host port)
  (and (bound-and-true-p extempore-buffer)
       (comint-check-proc extempore-buffer)
       (get-buffer-process (format "*extempore REPL<%s:%d>*" host port))))

(defun lispy--eval-extempore-check-result (result)
  ;; toplevel::0::eval: unbound variable: lily-command-alist
  (if (string-match-p (rx "::" (1+ digit) "::eval:") result)
      (prog1 ""
        (message result))
    result))

(defun lispy--eval-extempore (e-str)
  (let ((host (or (car extempore-connect-host-history-list)
                  extempore-default-host))
        (port
         (string-to-number (or (car extempore-connect-port-history-list)
                               extempore-default-port))))
    (when-let* ((proc
                 (or (lispy--eval-extempore-running-p host port)
                     (save-window-excursion
                       (extempore-repl host port)
                       (sleep-for 0.2) ; needed for some obscure reason
                       (lispy--eval-extempore-running-p host port)))))
      (with-current-buffer (process-buffer proc)
        (extempore-repl-send proc e-str)
        (with-timeout (lispy--eval-extempore-timeout)
          (accept-process-output proc)
          (goto-char (point-max))
          (save-match-data
            (when (re-search-backward
                   (rx bol
                       (group (eval (symbol-name extempore-repl-current-language))
                              "<" (1+ digit) ">")
                       " => "
                       (group (+? anything))
                       (backref 1))
                   nil t)
              (lispy--eval-extempore-check-result
               (string-trim (match-string-no-properties 2))))))))))

(provide 'le-extempore)
;;; le-extempore.el ends here
