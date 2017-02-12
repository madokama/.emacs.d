;;; imgpop --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(remove-overlays)
(let ((ov (make-overlay (1- (point)) (line-end-position))))
  (overlay-put ov 'before-string " ")
  (overlay-put ov 'after-string "\n")
  (overlay-put ov
               'display
               (create-image "~/Downloads/Makino Maria-666848.jpg"
                             'imagemagick nil :width 140)))

(provide 'imgpop)
;;; imgpop.el ends here
