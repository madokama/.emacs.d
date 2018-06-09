;;; hydra-posframe --- posframe frontend for hydra -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'posframe)

(defgroup hydra-posframe nil
  "Using posframe to show hydra."
  :group 'hydra
  :prefix "hydra-posframe-")

(defface hydra-posframe
    '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used by the hydra-posframe."
  :group 'hydra-posframe)

(defvar hydra-posframe-buffer " *hydra-posframe-buffer*")

(defun hydra-posframe-show (str)
  (posframe-show hydra-posframe-buffer
                 :string str
                 :position (point)
                 :background-color (face-attribute 'hydra-posframe :background)
                 :foreground-color (face-attribute 'hydra-posframe :foreground)))

(defun hydra-posframe-hide ()
  (posframe-hide hydra-posframe-buffer))

(provide 'hydra-posframe)
;;; hydra-posframe.el ends here
