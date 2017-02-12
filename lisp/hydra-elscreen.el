;;; hydra-elscreen --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elscreen)

;;;###autoload(autoload 'hydra-elscreen/body "hydra-elscreen")
(defhydra hydra-elscreen (:idle 1.0 ;; global-map "C-z"
                          )
  "elscreen"
  ("n" elscreen-next
       ;; (progn
       ;;   (hydra-keyboard-quit)
       ;;   (elscreen-next))
       "next")
  ("p" elscreen-previous
       ;; (progn
       ;;   (hydra-keyboard-quit)
       ;;   (elscreen-previous))
       "prev")
  ("c" elscreen-create "create" :exit t)
  ("s" elscreen-split "split" :exit t)
  ("k" elscreen-kill
       ;; (progn
       ;;   (hydra-keyboard-quit)
       ;;   (elscreen-kill))
       "kill")
  ("C-a" elscreen-toggle "toggle" :exit t)
  ("z" nil))

(provide 'hydra-elscreen)
;;; hydra-elscreen.el ends here
