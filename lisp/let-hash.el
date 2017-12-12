;;; let-hash --- let-alist for hash-tables -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'let-alist)

(defun let-hash--deep-dot-search (data)
  "Return alist of symbols inside DATA that start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
    ((symbolp data)
     (let ((name (symbol-name data)))
       (when (string-match "\\`\\." name)
         ;; Return the cons cell inside a list, so it can be appended
         ;; with other results in the clause below.
         (list (cons data (intern (replace-match "" nil nil name)))))))
    ((not (consp data)) nil)
    ((eq (car data) 'let-hash)
     ;; For nested ‘let-hash’ forms, ignore symbols appearing in the
     ;; inner body because they don’t refer to the hash currently
     ;; being processed.  See Bug#24641.
     (let-hash--deep-dot-search (cadr data)))
    (t (append (let-hash--deep-dot-search (car data))
               (let-hash--deep-dot-search (cdr data))))))

(defun let-hash--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (let-alist--remove-dot symbol))
         (name (symbol-name clean)))
    (if (string-match-p "\\`\\." name)
        clean
      (let-hash--list-to-sexp
       (mapcar #'intern (nreverse (split-string name "\\.")))
       variable))))

(defun let-hash--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `gethash' on VAR."
  `(let ((hash ,(if (cdr list)
                    (let-hash--list-to-sexp (cdr list) var)
                  var)))
     (and (hash-table-p hash)
          (gethash ,(symbol-name (car list)) hash))))

;;;###autoload
(defmacro let-hash (hash &rest body)
  (declare (indent 1) (debug t))
  (let ((var (make-symbol "hash")))
    `(let ((,var ,hash))
       (let ,(mapcar (lambda (x) `(,(car x) ,(let-hash--access-sexp (car x) var)))
                     (delete-dups (let-hash--deep-dot-search body)))
         ,@body))))

(provide 'let-hash)
;;; let-hash.el ends here
