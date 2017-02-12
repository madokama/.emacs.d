;;; hydra-yas --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'yasnippet)

;;;###autoload(autoload 'hydra-yasnippet/body "hydra-yas")
(defhydra hydra-yasnippet (:color blue :hint t)
  "yasnippet"
  ;;("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet "ins")
  ("f" yas-visit-snippet-file "visit")
  ("n" yas-new-snippet "new")
  ;;("t" yas-tryout-snippet)
  ("l" yas-describe-tables "list"))

(provide 'hydra-yas)
;;; hydra-yas.el ends here
