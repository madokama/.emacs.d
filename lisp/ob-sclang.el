;;; ob-sclang.el --- org-babel functions for sclang evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it
;; is a template for use in adding new language support to Org-babel.
;; Good first steps are to copy this file to a file named by the
;; language you are adding, and then use `query-replace' to replace
;; all strings of "template" in this file with the name of your new
;; language.
;;
;; If you have questions as to any of the portions of the file defined
;; below please look to existing language support for guidance.
;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at http://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'sclang)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("sclang" . "sc"))

(defvar org-babel-default-header-args:sclang
  '((:prologue . "(") (:epilogue . ")")))

(defconst org-babel-sclang-eoe "org_babel_sclang_eoe")

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:sclang' function below.
;; (defun org-babel-expand-body:sclang (body params)
;;   "Expand BODY according to PARAMS, return the expanded body."

;;   (let ((vars (org-babel--get-vars params)))
;;     (with-output-to-string
;;       (princ "(\n")
;;       (mapc (lambda (pair)
;;               (princ
;;                (format "%s=%S;\n"
;;                        (car pair)
;;                        (org-babel-sclang-var-to-sclang (cdr pair)))))
;;             vars)
;;       (princ body)
;;       (princ "\n)"))))

(defun org-babel-variable-assignments:sclang (params)
  (mapcar (lambda (pair)
            (format "%s=%s;"
                    (car pair)
                    (org-babel-sclang-var-to-sclang (cdr pair))))
          (org-babel--get-vars params)))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:sclang (body params)
  "Execute a block of Sclang code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((session
          (org-babel-sclang-initiate-session (assoc-default :session params)))
         (result-params (assoc-default :result-params params))
         ;;(result-type (assoc-default :result-type params))
         (full-body
          (org-babel-expand-body:generic
           body params (org-babel-variable-assignments:sclang params))))
    (org-babel-sclang-evaluate session full-body result-params)))

(defun org-babel-sclang-evaluate (session body &optional result-params)
  (with-current-buffer session
    (save-match-data
      (let* ((process (get-buffer-process session))
             (start (marker-position (process-mark process))))
        (mapc (lambda (expr)
                (process-send-string
                 process
                 (concat expr
                         sclang-token-interpret-print-cmd-line))
                (sleep-for 0.5))
              (list body
                    (format "%S;" org-babel-sclang-eoe)))
        (unless (member "none" result-params)
          (goto-char start)
          (let ((end
                 (catch 'eoe
                   (while (accept-process-output process 10)
                     (when (search-forward org-babel-sclang-eoe nil t)
                       (throw 'eoe (match-beginning 0)))))))
            (buffer-substring-no-properties start (or end (point-max)))))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
;; (defun org-babel-prep-session:sclang (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS."
;;   )

(defun org-babel-sclang-var-to-sclang (var)
  "Convert an elisp var into a string of sclang source code
specifying a var of the same value."
  (format "%S" var))

;; (defun org-babel-sclang-table-or-string (results)
;;   "If the results look like a table, then convert them into an
;; Emacs-lisp table, otherwise return the results as a string."
;;   )

(defun org-babel-sclang-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  ;;(require 'sclang-interp)
  (let ((buffer (if (string= session "none")
		    "*sclang*"
		  session)))
    (or (org-babel-comint-buffer-livep buffer)
	(let ((process
	       (make-process :name "sclang"
			     :buffer buffer
			     :command (cons sclang-program (sclang-make-options))
			     :noquery t)))
	  (process-buffer process)))))

(provide 'ob-sclang)
;;; ob-sclang.el ends here
