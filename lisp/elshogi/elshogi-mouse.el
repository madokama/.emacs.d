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

(provide 'elshogi-mouse)
;;; elshogi-mouse.el ends here
