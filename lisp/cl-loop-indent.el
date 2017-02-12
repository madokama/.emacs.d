;;; cl-loop-indent --- loop macro support imported from slime/contrib -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar lisp-loop-indent-subclauses t
  "Whether or not to indent loop subclauses.")
  
(defvar lisp-simple-loop-indentation)

(defvar lisp-loop-clauses-indentation 2
  "Indentation of loop clauses if `loop' is immediately followed by a newline.")

(defvar lisp-loop-body-forms-indentation 3
  "Indentation of loop body clauses.")

(defvar lisp-loop-indent-body-forms-relative-to-loop-start nil
  "When true, indent loop body clauses relative to the open paren of the loop
form, instead of the keyword position.")

(defvar lisp-loop-indent-forms-like-keywords nil
  "Whether or not to indent loop subforms just like
loop keywords. Only matters when `lisp-loop-indent-subclauses'
is nil.")

;;;; LOOP indentation, the complex version -- handles subclause indentation

;; Regexps matching various varieties of loop macro keyword ...
(defvar common-lisp-body-introducing-loop-macro-keyword
  "\\(#?:\\)?\\(do\\(ing\\)?\\|finally\\|initially\\)"
  "Regexp matching loop macro keywords which introduce body forms.")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar common-lisp-prefix-loop-macro-keyword
  "\\(#?:\\)?\\(and\\|else\\)"
  "Regexp matching loop macro keywords which are prefixes.")

(defvar common-lisp-indent-clause-joining-loop-macro-keyword
  "\\(#?:\\)?and"
  "Regexp matching 'and', and anything else there ever comes to be like it.")

(defvar common-lisp-indent-indented-loop-macro-keyword
  "\\(#?:\\)?\\(\\(up\\|down\\)?(from\\|to)\\|below\\|above\\|in\\(to\\)?\\|\
on\\|=\\|then\\|across\\|being\\|each\\|the\\|of\\|using\\|\
\\(present-\\|external-\\)?symbols?\\|fixnum\\|float\\|t\\|nil\\|of-type\\)"
  "Regexp matching keywords introducing loop subclauses.
Always indented two.")

(defvar common-lisp-indenting-loop-macro-keyword
  "\\(#?:\\)?\\(when\\|unless\\|if\\)"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented.")

(defvar common-lisp-loop-macro-else-keyword "\\(#?:\\)?else")

;;;; LOOP indentation, the simple version

(defun common-lisp-loop-type (loop-start)
  "Returns the type of the loop form at LOOP-START.
Possible types are SIMPLE, SIMPLE/SPLIT, EXTENDED, and EXTENDED/SPLIT. */SPLIT
refers to extended loops whose body does not start on the same line as the
opening parenthesis of the loop."
  (let (comment-split)
    (condition-case ()
        (save-excursion
          (goto-char loop-start)
          (let ((line (line-number-at-pos))
                (maybe-split t))
            (forward-char 1)
            (forward-sexp 1)
            (save-excursion
              (when (looking-at "\\s-*\\\n*;")
                (search-forward ";")
                (backward-char 1)
                (if (= line (line-number-at-pos))
                    (setq maybe-split nil)
                  (setq comment-split t))))
            (forward-sexp 1)
            (backward-sexp 1)
            (if (eql (char-after) ?\()
                (if (or (not maybe-split) (= line (line-number-at-pos)))
                    'simple
                  'simple/split)
              (if (or (not maybe-split) (= line (line-number-at-pos)))
                  'extended
                'extended/split))))
      (error
       (if comment-split
           'simple/split
         'simple)))))

(defun common-lisp-trailing-comment ()
  (ignore-errors
    ;; If we had a trailing comment just before this, find it.
    (save-excursion
      (backward-sexp)
      (forward-sexp)
      (when (looking-at "\\s-*;")
        (search-forward ";")
        (1- (current-column))))))

;;;###autoload
(defun lisp-indent-loop (path state indent-point sexp-column normal-indent)
  (if (cdr path)
      normal-indent
    (let* ((loop-start (elt state 1))
           (type (common-lisp-loop-type loop-start)))
      (cond ((and lisp-loop-indent-subclauses
                  (member type '(extended extended/split)))
             (list (common-lisp-indent-loop-macro-1 state indent-point)
                   (common-lisp-indent-parse-state-start state)))
            (t
             (common-lisp-loop-part-indentation indent-point state type))))))

;;; Attempt to indent the loop macro ...

(defun common-lisp-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun common-lisp-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

(defun common-lisp-loop-part-indentation (indent-point state &optional type)
  "Compute the indentation of loop form constituents."
  (let* ((loop-start (elt state 1))
         (type (or type (common-lisp-loop-type loop-start)))
         (loop-indentation (save-excursion
                             (goto-char loop-start)
                             (if (eq type 'extended/split)
                                 (- (current-column) 4)
                               (current-column))))
         (indent nil)
         (re "\\(\\(#?:\\)?\\sw+\\|)\\|\n\\)"))
    (goto-char indent-point)
    (back-to-indentation)
    (cond ((eq type 'simple/split)
           (+ loop-indentation lisp-simple-loop-indentation))
          ((eq type 'simple)
           (+ loop-indentation 6))
          ;; We are already in a body, with forms in it.
          ((and (not (looking-at re))
                (save-excursion
                  (while (and (ignore-errors (backward-sexp) t)
                              (not (looking-at re)))
                    (setq indent (current-column)))
                  (when (and indent
                             (looking-at
                              common-lisp-body-introducing-loop-macro-keyword))
                    t)))
           (list indent loop-start))
          ;; Keyword-style or comment outside body
          ((or lisp-loop-indent-forms-like-keywords
               (looking-at re)
               (looking-at ";"))
           (if (and (looking-at ";")
                    (let ((p (common-lisp-trailing-comment)))
                      (when p
                        (setq loop-indentation p))))
               (list loop-indentation loop-start)
             (list (+ loop-indentation 6) loop-start)))
          ;; Form-style
          (t
           (list (+ loop-indentation 9) loop-start)))))

(defun common-lisp-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
     ;; Find first clause of loop macro, and use it to establish
     ;; base column for indentation
     (goto-char (common-lisp-indent-parse-state-start parse-state))
     (let ((loop-start-column (current-column)))
       (common-lisp-loop-advance-past-keyword-on-line)

       (when (eolp)
         (forward-line 1)
         (end-of-line)
         ;; If indenting first line after "(loop <newline>"
         ;; cop out ...
         (if (<= indent-point (point))
             (throw 'return-indentation (+ lisp-loop-clauses-indentation
                                           loop-start-column)))
         (back-to-indentation))

       (let* ((case-fold-search t)
              (loop-macro-first-clause (point))
              (previous-expression-start
                (common-lisp-indent-parse-state-prev parse-state))
              (default-value (current-column))
              (loop-body-p nil)
              (loop-body-indentation nil)
              (indented-clause-indentation (+ 2 default-value)))
         ;; Determine context of this loop clause, starting with the
         ;; expression immediately preceding the line we're trying to indent
         (goto-char previous-expression-start)

         ;; Handle a body-introducing-clause which ends a line specially.
         (if (looking-at common-lisp-body-introducing-loop-macro-keyword)
             (let ((keyword-position (current-column)))
               (setq loop-body-p t)
               (setq loop-body-indentation
                     (if (common-lisp-loop-advance-past-keyword-on-line)
                         (current-column)
                       (back-to-indentation)
                       (if (/= (current-column) keyword-position)
                           (+ 2 (current-column))
                         (+ lisp-loop-body-forms-indentation
                            (if lisp-loop-indent-body-forms-relative-to-loop-start
                                loop-start-column
                              keyword-position))))))

           (back-to-indentation)
           (if (< (point) loop-macro-first-clause)
               (goto-char loop-macro-first-clause))
           ;; If there's an "and" or "else," advance over it.
           ;; If it is alone on the line, the next "cond" will treat it
           ;; as if there were a "when" and indent under it ...
           (let ((exit nil))
             (while (and (null exit)
                         (looking-at common-lisp-prefix-loop-macro-keyword))
                    (if (null (common-lisp-loop-advance-past-keyword-on-line))
                        (progn (setq exit t)
                               (back-to-indentation)))))

           ;; Found start of loop clause preceding the one we're
           ;; trying to indent. Glean context ...
           (cond
             ((looking-at "(")
              ;; We're in the middle of a clause body ...
              (setq loop-body-p t)
              (setq loop-body-indentation (current-column)))
             ((looking-at common-lisp-body-introducing-loop-macro-keyword)
              (setq loop-body-p t)
              ;; Know there's something else on the line (or would
              ;; have been caught above)
              (common-lisp-loop-advance-past-keyword-on-line)
              (setq loop-body-indentation (current-column)))
             (t
              (setq loop-body-p nil)
              (if (or (looking-at common-lisp-indenting-loop-macro-keyword)
                      (looking-at common-lisp-prefix-loop-macro-keyword))
                  (setq default-value (+ 2 (current-column))))
              (setq indented-clause-indentation (+ 2 (current-column)))
              ;; We still need loop-body-indentation for "syntax errors" ...
              (goto-char previous-expression-start)
              (setq loop-body-indentation (current-column)))))

         ;; Go to first non-blank character of the line we're trying
         ;; to indent. (if none, wind up poised on the new-line ...)
         (goto-char indent-point)
         (back-to-indentation)
         (cond
           ((looking-at "(")
            ;; Clause body ...
            loop-body-indentation)
           ((or (eolp) (looking-at ";"))
            ;; Blank line.  If body-p, indent as body, else indent as
            ;; vanilla clause.
            (if loop-body-p
                loop-body-indentation
              (or (and (looking-at ";") (common-lisp-trailing-comment))
                  default-value)))
           ((looking-at common-lisp-indent-indented-loop-macro-keyword)
            indented-clause-indentation)
           ((looking-at common-lisp-indent-clause-joining-loop-macro-keyword)
            (let ((stolen-indent-column nil))
              (forward-line -1)
              (while (and (null stolen-indent-column)
                          (> (point) loop-macro-first-clause))
                     (back-to-indentation)
                     (if (and (< (current-column) loop-body-indentation)
                              (looking-at "\\(#?:\\)?\\sw"))
                         (progn
                           (if (looking-at common-lisp-loop-macro-else-keyword)
                               (common-lisp-loop-advance-past-keyword-on-line))
                           (setq stolen-indent-column
                                 (current-column)))
                       (forward-line -1)))
              (if stolen-indent-column
                  stolen-indent-column
                default-value)))
           (t default-value)))))))

(defun common-lisp-loop-advance-past-keyword-on-line ()
  (forward-symbol 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (if (eolp)
      nil
    (current-column)))

(provide 'cl-loop-indent)
;;; cl-loop-indent.el ends here
