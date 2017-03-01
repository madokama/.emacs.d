;;; elshogi-mouse --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function elshogi-mouse-select-square "elshogi")

(defvar elshogi-mouse-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [mouse-2] #'elshogi-mouse-select-square)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun elshogi-mouse-read-props (ev &rest props)
  (let ((posn (event-start ev)))
    (with-current-buffer (window-buffer (posn-window posn))
      (mapcar (apply-partially #'get-text-property (posn-point posn))
              props))))

(defun elshogi-mouse-read-prop (ev prop)
  (car (elshogi-mouse-read-props ev prop)))

(provide 'elshogi-mouse)
;;; elshogi-mouse.el ends here
