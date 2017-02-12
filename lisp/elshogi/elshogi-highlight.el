;;; elshogi-highlight --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'elshogi-game)
(require 'elshogi-candidates)

(defvar-local elshogi-highlight-ov-move nil)
(defvar-local elshogi-highlight-ov-selected nil)
(defvar-local elshogi-highlight-ov-candidates nil)

(defun elshogi-highlight-index->region (index)
  (if (consp index)
      (when-let* (pt (assoc-default 'point index))
        (list pt (1+ pt)))
    (when-let* (pt (elshogi-index->point index))
      (list pt (+ pt 2)))))

(defun elshogi-highlight-it (index &optional invert)
  (let ((overlay
         (apply #'make-overlay (elshogi-highlight-index->region index))))
    (overlay-put overlay 'face
                 (if invert
                     '(:inverse-video t)
                   'highlight))
    overlay))

(defsubst elshogi-unhighlight-it (overlay)
  (when overlay
    (delete-overlay overlay)))

(defun elshogi-unhighlight-move ()
  (elshogi-unhighlight-it elshogi-highlight-ov-move))

(defun elshogi-highlight-move (index)
  (elshogi-unhighlight-move)
  (setq elshogi-highlight-ov-move (elshogi-highlight-it index t)))

(defun elshogi-unhighlight-candidates ()
  (when elshogi-highlight-ov-candidates
    (mapc #'elshogi-unhighlight-it elshogi-highlight-ov-candidates)
    (setq elshogi-highlight-ov-candidates nil)))

(defun elshogi-highlight-candidates (candidates)
  (elshogi-unhighlight-candidates)
  (setq elshogi-highlight-ov-candidates
        (mapcar #'elshogi-highlight-it candidates)))

(defun elshogi-unhighlight-selected ()
  (elshogi-unhighlight-it elshogi-highlight-ov-selected))

(defun elshogi-highlight-selected (index)
  (elshogi-unhighlight-selected)
  (elshogi-unhighlight-candidates)
  (when index
    (when (consp index)
      (setq elshogi-highlight-ov-selected (elshogi-highlight-it index t)))
    (elshogi-highlight-candidates (elshogi-candidates-target index))))

(provide 'elshogi-highlight)
;;; elshogi-highlight.el ends here
