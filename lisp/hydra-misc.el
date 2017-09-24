;;; hydra-misc --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hydra)

(declare-function elscreen-find-and-goto-by-buffer "ext:elscreen")
(declare-function elfeed "ext:elfeed")
(declare-function elfeed-search-buffer "ext:elfeed-search")

(declare-function open-junk-file "ext:open-junk-file")
(declare-function transpose-frame "ext:transpose-frame")
(declare-function winner-redo "winner")
(declare-function winner-undo "winner")
(declare-function w32-symon-mode "ext:w32-symon")

;; (defvar explicit-shell-file-name)

;; (defun shell-wrap (arg)
;;   "Wrapper function for `shell'.
;; When prefix argument ARG is non-nil, run MS-Windows command
;; prompt."
;;   (interactive "P")
;;   (if (and arg (eq system-type 'windows-nt))
;;       (let ((explicit-shell-file-name "cmdproxy"))
;;         (shell (get-buffer-create "*command prompt*")))
;;     (call-interactively #'shell)))

(declare-function alpha-frame-trans-inc "ext:alpha-frame")
(declare-function alpha-frame-trans-dec "ext:alpha-frame")
(declare-function alpha-frame-opaque "ext:alpha-frame")
(declare-function alpha-frame-max "ext:alpha-frame")

(defhydra hydra-alpha-frame ()
  "Frame"
  ("+" alpha-frame-trans-inc "trans+")
  ("=" alpha-frame-trans-inc)
  ("-" alpha-frame-trans-dec "trans-")
  ("o" alpha-frame-opaque "opaque")
  ("m" alpha-frame-max "max")
  ("z" nil))

(defhydra hydra-launch (:idle 1.0 :foreign-keys warn :exit t :hint nil)
  "
_b_rowse _e_xplore _f_eed   _j_unk   s_c_ratch
_m_agit  _M_agit
_w_:start menu _o_:monitor _h_:standby
"
  ("b" browse-url-at-point)
  ("c" pop-scratch)
  ("e" (start-process "explorer" nil "explorer" "."))
  ("f" (progn
         (require 'elfeed)
         (require 'elscreen)
         (elscreen-find-and-goto-by-buffer
          (elfeed-search-buffer) 'create)
         (elfeed)))
  ("h" (start-process "nircmd" nil "nircmd" "standby"))
  ("j" open-junk-file)
  ("m" counsel-magit-repos)
  ("M" magit-status)
  ("o" (start-process "nircmd" nil "nircmd" "monitor" "off"))
  ;; ("s" shell-wrap)
  ;; ("S" (let ((process-environment (cons "MSYSTEM=MSYS" process-environment)))
  ;;        (shell (get-buffer-create "*msys*"))))
  ("w" (start-process "nircmd" nil "nircmd" "sendkey" "lwin" "press"))
  ("z" nil))

;; (defhydra hydra-goto-line (goto-map ""
;;                            :pre (linum-mode 1)
;;                            :post (linum-mode -1))
;;   "goto-line"
;;   ("g" goto-line "go")
;;   ("m" set-mark-command "mark" :bind nil)
;;   ("z" nil))

(defvar hydra-goto-error-function nil)

(defhydra hydra-goto-error (:pre
                            (unless hydra-goto-error-function
                              (setq hydra-goto-error-function next-error-function)
                              (setq next-error-last-buffer nil))
                            :post (setq hydra-goto-error-function nil))
  "error"
  ("n" (let ((next-error-function hydra-goto-error-function))
         (next-error)))
  ("p" (let ((next-error-function hydra-goto-error-function))
         (previous-error)))
  ("z" nil))

(declare-function windmove-left "windmove")
(declare-function windmove-down "windmove")
(declare-function windmove-up "windmove")
(declare-function windmove-right "windmove")
(declare-function hydra-move-splitter-left "ext:hydra-examples")
(declare-function hydra-move-splitter-down "ext:hydra-examples")
(declare-function hydra-move-splitter-up "ext:hydra-examples")
(declare-function hydra-move-splitter-right "ext:hydra-examples")

;; http://oremacs.com/2015/02/03/one-hydra-two-hydra/
(defhydra hydra-window (:idle 1.0 :hint nil :color red)
  "
^Navigate^ ^Delete^   ^Max^     ^Split^    ^Move^    ^Misc.^
_o_: ace   _d_el      _m_ax     _x_: horz  _s_wap    _=_ balance
_b_uffer   _D_el ace  _M_ax ace _v_: vert  _t_rans   _a_ alpha
^^         ^^         ^^        ^^         ^^        _r_edo/_u_ndo
"
  ;; hidden keys
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ;; Navigate
  ("o" ace-window :exit nil)
  ("b" ivy-switch-buffer :exit t)
  ;; Delete
  ("d" delete-window)
  ("D" ace-delete-window)
  ;; Max
  ("m" delete-other-windows :exit t)
  ("M" ace-delete-other-windows :exit t)
  ;; Split
  ("x" (progn (split-window-below) (windmove-down)))
  ("v" (progn (split-window-right) (windmove-right)))
  ;; Move
  ("s" ace-swap-window)
  ("t" transpose-frame)
  ;; Misc.
  ("=" balance-windows)
  ("a" hydra-alpha-frame/body :exit t)
  ("r" winner-redo)
  ("u" winner-undo)
  ;;("q" nil)
  ("z" nil))

;; (defun toggle-selective-display ()
;;   (interactive)
;;   (set-selective-display
;;    (if (numberp selective-display)
;;        nil
;;      2)))

(defvar async-debug)
(defhydra hydra-toggler (:foreign-keys nil :hint nil)
  "
_a_ async-debug:         %(bound-and-true-p async-debug)
_d_ display-time-mode:   %`display-time-mode
_e_ debug-on-error:      %`debug-on-error
_h_ global-hl-line-mode: %`global-hl-line-mode
_l_ selective-display:   %`selective-display
_n_ narrow-or-widen:     %(buffer-narrowed-p)
_s_ w32-symon-mode:      %`w32-symon-mode
_t_ truncate-lines:      %`truncate-lines
_w_ which-function-mode: %`which-function-mode"
  ;;_a_ abbrev-mode:         %`abbrev-mode
  ;;_v_ volatile-highlights: %`volatile-highlights-mode
  ;;_x_ xref-etags-mode:     %`xref-etags-mode

  ("a" (when (boundp 'async-debug) (setq async-debug (not async-debug))))
  ("d" display-time-mode)
  ("e" toggle-debug-on-error)
  ("h" global-hl-line-mode)
  ("n" narrow-or-widen-dwim)
  ("l" (set-selective-display (if (numberp selective-display) nil 2)))
  ("s" w32-symon-mode)
  ("t" toggle-truncate-lines)
  ;;("v" volatile-highlights-mode)
  ("w" which-function-mode)
  ;;("x" xref-etags-mode)
  ("z" nil))

(defhydra hydra-apropos (:color blue :hint nil)
  "
_m_an _c_ommand
_a_propos _l_ibrary
_d_ocumentation _u_ser-option
_v_ariable valu_e_"
  ("m" man)
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))

(provide 'hydra-misc)
;;; hydra-misc.el ends here
