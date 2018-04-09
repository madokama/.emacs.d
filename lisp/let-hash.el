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
       (when (string-prefix-p "." name)
         ;; Return the cons cell inside a list, so it can be appended
         ;; with other results in the clause below.
         (list (cons data (intern (substring name 1)))))))
    ((not (consp data)) nil)
    ((eq (car data) 'let-hash)
     ;; For nested ‘let-hash’ forms, ignore symbols appearing in the
     ;; inner body because they don’t refer to the hash currently
     ;; being processed.  See Bug#24641.
     (let-hash--deep-dot-search (cadr data)))
    (t (append (let-hash--deep-dot-search (car data))
               (let-hash--deep-dot-search (cdr data))))))

(defun let-hash--gethash-sexp (key hash)
  `(when (hash-table-p ,hash)
     (if (eq (hash-table-test ,hash) 'eq)
         (gethash ',(intern key) ,hash)
       (gethash ,key ,hash))))

(defun let-hash--sexp (key hash let-p)
  (let ((sexp (if (symbolp hash)
                  (let-hash--gethash-sexp key hash)
                (let ((hvar (make-symbol "hash")))
                  `(let ((,hvar ,hash))
                     ,(let-hash--gethash-sexp key hvar))))))
    (if let-p
        sexp
      (let ((vvar (make-symbol "val")))
        `(let ((,vvar ,sexp))
           (if (memq ,vvar '(:null :false))
               nil
             ,vvar))))))

(defun let-hash--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let-hash--list-to-sexp
   (nreverse (split-string (symbol-name symbol) "\\."))
   variable))

(defun let-hash--list-to-sexp (lst variable &optional let-p)
  "Turn symbols LST into recursive calls to `gethash' on VARIABLE.
Optional argument LET-P indicates the sexp will be used in an
intermediate `let' binding."
  (if (cdr lst)
      (let-hash--sexp (car lst)
                      (let-hash--list-to-sexp (cdr lst) variable t)
                      let-p)
    (let-hash--sexp (car lst) variable let-p)))

(defun let-hash--internal (var body)
  `(let ,(mapcar (lambda (x)
                   `(,(car x) ,(let-hash--access-sexp (cdr x) var)))
                 (delete-dups (let-hash--deep-dot-search body)))
     ,@body))

;;;###autoload
(defmacro let-hash (hash &rest body)
  (declare (indent 1) (debug t))
  (if (symbolp hash)
      (let-hash--internal hash body)
    (let ((var (make-symbol "hash")))
      `(let ((,var ,hash))
         ,(let-hash--internal var body)))))

;;;###autoload
(defmacro map-hash (binding hash &rest body)
  (declare (indent 2))
  `(cl-loop for ,(car binding) being the hash-keys of ,hash
              using (hash-values ,(cadr binding))
            collect (progn ,@body)))

;;;###autoload
(defmacro for-hash (binding hash &rest body)
  (declare (indent 2))
  `(cl-loop for ,(car binding) being the hash-keys of ,hash
              using (hash-values ,(cadr binding))
            do ,@body))

(provide 'let-hash)
;;; let-hash.el ends here
