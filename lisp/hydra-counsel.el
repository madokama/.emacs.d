;;; hydra-counsel --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload(autoload 'hydra-counsel-git/body "hydra-counsel")
(defhydra hydra-counsel-git (:hint nil :exit t)
    "
git: _f_ind _g_rep _l_og"
    ("f" counsel-git)
    ("g" counsel-git-grep)
    ("l" counsel-git-log))

(declare-function elfeed-feed-list "ext:elfeed")
(declare-function elfeed-update-feed "ext:elfeed")

(defun counsel-elfeed-update ()
  "Update RSS feeds with `counsel'."
  (interactive)
  (when (require 'elfeed nil t)
    (ivy-read "Elfeed: " (elfeed-feed-list)
              :action (lambda (datum)
                        (elfeed-update-feed datum)))))

;;;###autoload(autoload 'hydra-counsel/body "hydra-counsel")
(defhydra hydra-counsel (:hint nil :exit t)
  "
_i_menu, _l_ookup, _v_ar, li_b_
_h_ist, _p_roc
el_f_eed, _r_esume"
  ("b" counsel-find-library)
  ("f" counsel-elfeed-update)
  ("h" counsel-command-history)
  ("i" counsel-imenu)
  ;; ("k" counsel-ace-link)
  ("l" counsel-info-lookup-symbol)
  ("p" counsel-list-processes)
  ("r" ivy-resume)
  ("v" counsel-set-variable))

(provide 'hydra-counsel)
;;; hydra-counsel.el ends here
