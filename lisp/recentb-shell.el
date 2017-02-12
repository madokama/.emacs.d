;;; recentb-shell --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentb)

(recentb-define-mode shell-mode
  :history recentb-shell-history-function
  :candidate recentb-shell-candidate)

(defvar recentb-shell-action-function #'recentb-shell-action-default)
(defvar recentb-shell-history-function #'recentb-shell-history-default)

(defun recentb-shell-candidate (item)
  (unless (get-buffer item)
    (propertize item 'recentb `(,recentb-shell-action-function ,item))))

(defun recentb-shell-action-default (item)
  (shell item))

(defun recentb-shell-history-default ()
  '("*shell*"))

(defvar explicit-shell-file-name)
(defvar explicit-powershell-args)
(defvar multi-term-program)
(defvar multi-term-program-switches)

(declare-function multi-term-internal "ext:multi-term")

(defmacro recentb-with-env (env &rest body)
  (declare (indent 1))
  `(progn
     (let ,env
       ,@body)
     ,@(mapcar (pcase-lambda (`(,var ,val))
                 `(setq-local ,var ,val))
               env)))

(defun recentb-shell-action-msys2 (item)
  (pcase (and (string-match "shell:\\([^*]+\\)" item)
              (match-string 1 item))
    ("msys2"
     (recentb-with-env ((process-environment
                         (cons "MSYSTEM=MSYS" process-environment)))
       (shell item)))
    ("w32"
     (recentb-with-env ((explicit-shell-file-name "cmdproxy")
                        (default-process-coding-system '(utf-8-dos . utf-8-dos)))
       (shell item)))
    ("powershell"
     (recentb-with-env ((explicit-shell-file-name "powershell")
                        (explicit-powershell-args '())
                        (default-process-coding-system '(utf-8-dos . utf-8-dos)))
       (shell item)))
    ("term"
     (when (require 'multi-term nil t)
       (let ((term
              (make-term (replace-regexp-in-string (rx "*") "" item)
                         multi-term-program nil multi-term-program-switches)))
         (with-current-buffer term
           (multi-term-internal))
         (pop-to-buffer-same-window term))))
    (_
     (shell item))))

(defun recentb-shell-history-msys2 ()
  '("*shell*" "*shell:term*" "*shell:msys2*" "*shell:w32*" "*shell:powershell*"))

(provide 'recentb-shell)
;;; recentb-shell.el ends here
