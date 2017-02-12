;;; dlist --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; http://rosettacode.org/wiki/Doubly-linked_list/Definition#Common_Lisp

;;; Code:

(cl-defstruct (dlist (:constructor dlist-make)
                     (:conc-name dlist/))
  head tail)

(cl-defstruct (dlink (:constructor dlink-make)
                     (:conc-name dlink/))
  content prev next)

(defun list->dlist (lst)
  (let ((dl (dlist-make)))
    (mapc (lambda (x)
            (dlist-insert-tail dl x))
          lst)
    dl))

(defun dlist-insert-between (dlist before after data)
  "Insert a fresh link containing DATA after existing link BEFORE if not nil and before existing link AFTER if not nil"
  (let ((new-link (dlink-make :content data :prev before :next after)))
    (if (null before)
        (setf (dlist/head dlist) new-link)
      (setf (dlink/next before) new-link))
    (if (null after)
        (setf (dlist/tail dlist) new-link)
      (setf (dlink/prev after) new-link))
    new-link))

(defun dlist-insert-before (dlist dlink data)
  "Insert a fresh link containing DATA before existing link DLINK."
  (dlist-insert-between dlist (dlink/prev dlink) dlink data))

(defun dlist-insert-after (dlist dlink data)
  "Insert a fresh link containing DATA after existing link DLINK."
  (dlist-insert-between dlist dlink (dlink/next dlink) data))

(defun dlist-insert-head (dlist data)
  "Insert a fresh link containing DATA at the head of DLIST."
  (dlist-insert-between dlist nil (dlist/head dlist) data))

(defun dlist-insert-tail (dlist data)
  "Insert a fresh link containing DATA at the tail of DLIST."
  (dlist-insert-between dlist (dlist/tail dlist) nil data))

(defun dlist-remove-link (dlist dlink)
  "Remove link DLINK from DLIST and return its content."
  (let ((before (dlink/prev dlink))
        (after (dlink/next dlink)))
    (if (null before)
        (setf (dlist/head dlist) after)
      (setf (dlink/next before) after))
    (if (null after)
        (setf (dlist/tail dlist) before)
      (setf (dlink/prev after) before))))

(defun dlist-elements (dlist)
  "Returns the elements of DLIST as a list."
  (cl-labels ((extract-values (dlink acc)
                (if (null dlink)
                    acc
                  (extract-values (dlink/next dlink)
                                  (cons (dlink/content dlink) acc)))))
    (nreverse (extract-values (dlist/head dlist) nil))))

(provide 'dlist)
;;; dlist.el ends here
