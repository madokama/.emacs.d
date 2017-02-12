;;; elshogi-directions --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-core)

(defvar elshogi-directions-+
  '(elshogi-upward-index
    elshogi-leftward-index
    elshogi-rightward-index
    elshogi-downward-index))

(defvar elshogi-directions-x
  '(elshogi-upleft-index
    elshogi-upright-index
    elshogi-downleft-index
    elshogi-downright-index))

(defun elshogi-next-index (index next-file next-rank)
  (let ((file (funcall next-file (elshogi-index-file index)))
        (rank (funcall next-rank (elshogi-index-rank index))))
    (when (and (elshogi-valid-file-p file)
               (elshogi-valid-rank-p rank))
      (elshogi-calc-index file rank))))

(defun elshogi-upward (piece)
  (if (elshogi-piece-black-p piece) #'1- #'1+))

(defun elshogi-downward (piece)
  (if (elshogi-piece-black-p piece) #'1+ #'1-))

(defalias 'elshogi-leftward 'elshogi-upward)
(defalias 'elshogi-rightward 'elshogi-downward)

(defun elshogi-upward-index (index piece)
  (elshogi-next-index index
                      #'identity
                      (lambda (rank)
                        (funcall (elshogi-upward piece) rank))))

(defun elshogi-downward-index (index piece)
  (elshogi-next-index index
                      #'identity
                      (lambda (rank)
                        (funcall (elshogi-downward piece) rank))))

(defun elshogi-leftward-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-leftward piece) file))
                      #'identity))

(defun elshogi-rightward-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-rightward piece) file))
                      #'identity))

(defun elshogi-upleft-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-leftward piece) file))
                      (lambda (rank)
                        (funcall (elshogi-upward piece) rank))))

(defun elshogi-upright-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-rightward piece) file))
                      (lambda (rank)
                        (funcall (elshogi-upward piece) rank))))

(defun elshogi-downleft-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-leftward piece) file))
                      (lambda (rank)
                        (funcall (elshogi-downward piece) rank))))

(defun elshogi-downright-index (index piece)
  (elshogi-next-index index
                      (lambda (file)
                        (funcall (elshogi-rightward piece) file))
                      (lambda (rank)
                        (funcall (elshogi-downward piece) rank))))

(provide 'elshogi-directions)
;;; elshogi-directions.el ends here
