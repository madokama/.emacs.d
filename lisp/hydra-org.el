;;; hydra-org --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'bind-key))
(require 'hydra)
(require 'org-element)

(defun hydra-org-on-src-block-p ()
  (memq (org-element-type (org-element-context))
        '(src-block fixed-width example-block)))

(defun hydra-org-next-heading-dwim (&optional arg)
  (interactive)
  (if (or (and (not arg) (hydra-org-on-src-block-p))
          (and arg (not (hydra-org-on-src-block-p))))
      (org-babel-next-src-block)
    (outline-next-heading)
    (when (outline-invisible-p)
      (org-reveal))))

(defun hydra-org-previous-heading-dwim (&optional arg)
  (interactive)
  (if (or (and (not arg) (hydra-org-on-src-block-p))
          (and arg (not (hydra-org-on-src-block-p))))
      (org-babel-previous-src-block)
    (outline-previous-heading)
    (when (outline-invisible-p)
      (org-reveal))))

(declare-function narrow-or-widen-dwim "init-util")
(declare-function ace-link-org "ext:ace-link")

(defhydra hydra-org-mode (org-mode-map "C-c" :hint nil)
  ("b" org-switchb :exit t)
  ("d" (unless (hydra-org-on-src-block-p)
         (org-down-element)))
  ("e" (if (hydra-org-on-src-block-p)
           (call-interactively #'org-ctrl-c-ctrl-c)
         (org-babel-execute-subtree)))
  ("g" org-babel-goto-named-src-block)
  ("h" (when (hydra-org-on-src-block-p)
         (org-babel-insert-header-arg)) :exit nil)
  ("l" org-mark-ring-goto)
  ("n" hydra-org-next-heading-dwim)
  ("N" (hydra-org-next-heading-dwim t))
  ("o" ace-link-org :exit t)
  ("p" hydra-org-previous-heading-dwim)
  ("P" (hydra-org-previous-heading-dwim t))
  ("r" (unless (hydra-org-on-src-block-p)
         (call-interactively #'org-set-property)))
  ("t" (when (org-at-heading-p)
         (call-interactively #'org-ctrl-c-ctrl-c)))
  ("T" org-babel-tangle :exit nil)
  ("u" org-up-element)
  ("v" (when (hydra-org-on-src-block-p)
         (org-edit-special)) :exit t)
  ("w" narrow-or-widen-dwim)
  ("<tab>" org-cycle)
  ("z" nil))

(defun hydra-org-mode-context ()
  (eval
   (hydra--format nil '(nil nil :hint nil)
                  (format "
_u_p%s _n_ext/_p_rev  _g_oto i_b_uf _l_:prev pos _o_:link
_e_val %s%s _T_angle"
                          (if (hydra-org-on-src-block-p)
                              ""
                            "/_d_own")
                          (if (hydra-org-on-src-block-p)
                              "_v_:edit _h_eader"
                            "p_r_op narro_w_")
                          (if (org-at-heading-p)
                              " _t_ag"
                            ""))
                  hydra-org-mode/heads)))

(setq hydra-org-mode/hint '(hydra-org-mode-context))

;; http://oremacs.com/2015/03/07/hydra-org-templates/
(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
  ("s" (org-hot-expand "<s"))
  ("e" (org-hot-expand "<e"))
  ("q" (org-hot-expand "<q"))
  ("v" (org-hot-expand "<v"))
  ("c" (org-hot-expand "<c"))
  ("l" (org-hot-expand "<l"))
  ("h" (org-hot-expand "<h"))
  ("a" (org-hot-expand "<a"))
  ("L" (org-hot-expand "<L"))
  ("i" (org-hot-expand "<i"))
  ("I" (org-hot-expand "<I"))
  ("H" (org-hot-expand "<H"))
  ("A" (org-hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun org-hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

(bind-key "<"
          (lambda ()
            (interactive)
            (if (looking-back "^" (point))
                (hydra-org-template/body)
              (self-insert-command 1)))
          org-mode-map)

(defhydra hydra-org-capture (:exit t)
  ("l" org-store-link "link")
  ("c" org-capture "capture")
  ;;("a" org-agenda "agenda")
  ("b" org-switchb "iswitchb")
  ("z" nil))

(provide 'hydra-org)
;;; hydra-org.el ends here
