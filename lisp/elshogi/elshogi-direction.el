;;; elshogi-direction --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-core)

(defvar elshogi-direction-+
  '(elshogi-direction-upward-index
    elshogi-direction-leftward-index
    elshogi-direction-rightward-index
    elshogi-direction-downward-index))

(defvar elshogi-direction-x
  '(elshogi-direction-upleft-index
    elshogi-direction-upright-index
    elshogi-direction-downleft-index
    elshogi-direction-downright-index))

(defun elshogi-direction-next-index (index next-file next-rank)
  (let ((file (funcall next-file (elshogi-index-file index)))
        (rank (funcall next-rank (elshogi-index-rank index))))
    (when (and (elshogi-valid-file-p file)
               (elshogi-valid-rank-p rank))
      (elshogi-calc-index file rank))))

(defun elshogi-direction-upward (piece)
  (if (elshogi-piece-black-p piece) #'1- #'1+))

(defun elshogi-direction-downward (piece)
  (if (elshogi-piece-black-p piece) #'1+ #'1-))

(defalias 'elshogi-direction-leftward 'elshogi-direction-upward)
(defalias 'elshogi-direction-rightward 'elshogi-direction-downward)

(defun elshogi-direction-upward-index (index piece)
  (elshogi-direction-next-index index
                                #'identity
                                (lambda (rank)
                                  (funcall (elshogi-direction-upward piece) rank))))

(defun elshogi-direction-downward-index (index piece)
  (elshogi-direction-next-index index
                                #'identity
                                (lambda (rank)
                                  (funcall (elshogi-direction-downward piece) rank))))

(defun elshogi-direction-leftward-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-leftward piece) file))
                                #'identity))

(defun elshogi-direction-rightward-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-rightward piece) file))
                                #'identity))

(defun elshogi-direction-upleft-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-leftward piece) file))
                                (lambda (rank)
                                  (funcall (elshogi-direction-upward piece) rank))))

(defun elshogi-direction-upright-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-rightward piece) file))
                                (lambda (rank)
                                  (funcall (elshogi-direction-upward piece) rank))))

(defun elshogi-direction-downleft-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-leftward piece) file))
                                (lambda (rank)
                                  (funcall (elshogi-direction-downward piece) rank))))

(defun elshogi-direction-downright-index (index piece)
  (elshogi-direction-next-index index
                                (lambda (file)
                                  (funcall (elshogi-direction-rightward piece) file))
                                (lambda (rank)
                                  (funcall (elshogi-direction-downward piece) rank))))

(provide 'elshogi-direction)
;;; elshogi-direction.el ends here
