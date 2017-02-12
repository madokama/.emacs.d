;;; hydra-motion --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'hydra)

;;* Variables

(defvar hydra-motion-mode-is-helpful t)

(defvar hydra-motion-mode-lighter
  (concat " "
          (let ((c #x24c2))
            (if (char-displayable-p c)
                (string c)
              "hymn"))))

(defvar hydra-motion-mode-enabled nil)
(defvar hydra-motion-mode-meta-pressed nil)

(defvar hydra-motion-exclude-modes
  '(minibuffer-inactive-mode undo-tree-visualizer-mode pdf-view-mode dired-mode))

(defvar hydra-motion-conflicting-modes '(lispy-mode))

;;* Keymap

(defvar hydra-motion-keys
  '("C-SPC"
    ;; "C-a"
    ;; "C-e"
    "C-n"
    "C-p"
    "C-f"
    "C-b"
    "C-v"
    "M-f"
    "M-b"
    "M-v"))

(defvar hydra-motion--orig-binds
  (mapcar (lambda (key)
            (cons key
                  (lookup-key global-map
                              (if (vectorp key) key (kbd key)))))
          hydra-motion-keys))

(defhydra hydra-motion
    (:pre (progn
            (when-let* ((key (key-description (vector last-input-event)))
                        (fun (hydra-motion--find-orig key)))
              (setq hydra-motion-mode-meta-pressed (string-match-p "^M-" key))
              (ignore-errors (call-interactively fun))
              (setq current-prefix-arg nil))
            (hydra-motion/pre))
     :post (progn
             (setq hydra-motion-mode-meta-pressed nil)
             (hydra-motion/post))
     :hint nil)
  "
_p_rev/_n_ext _b_ack/_f_orw  _v_:Pg%(if hydra-motion-mode-meta-pressed 'Up 'Dn)"
  ("a" beginning-of-line)
  ("b" hydra-motion-backward)
  ("B" hydra-motion-scroll-right)
  ("d" delete-char)
  ("e" move-end-of-line)
  ("f" hydra-motion-forwad)
  ("F" hydra-motion-scroll-left)
  ("k" kill-whole-line)
  ("l" recenter-top-bottom)
  ("n" hydra-motion-next-line)
  ("N" (scroll-up 1))
  ("p" hydra-motion-prev-line)
  ("P" (scroll-down 1))
  ("v" hydra-motion-page)
  ("w" kill-region)
  ("y" yank)
  ("z" nil)
  ;; ("<SPC>" set-mark-command)
  ("C-u" universal-argument :exit t))

(defvar hydra-motion-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key hydra-motion-keys)
      (define-key map (kbd key) #'hydra-motion/body))
    map))

(defun hydra-motion--find-orig (key)
  (cdr (assoc key hydra-motion--orig-binds)))

;;* Motion commands

(defun hydra-motion/pre ()
  (setq hydra-motion-mode-enabled t))

(defun hydra-motion/post ()
  (setq hydra-motion-mode-enabled nil))

(defvar-local hydra-motion-next-line-action nil)
(defvar-local hydra-motion-prev-line-action nil)

(defun hydra-motion-next-line (arg)
  (interactive "p")
  (unless (or (pos-visible-in-window-p (point-max))
              (< (- (line-number-at-pos (point))
                    (line-number-at-pos (window-start)))
                 (/ (window-body-height) 2)))
    (scroll-up 1))
  (condition-case nil
      (line-move-1 arg)
    (end-of-buffer
     (run-hooks 'hydra-motion-next-line-action))))

(defun hydra-motion-prev-line (arg)
  (interactive "p")
  (unless (or (pos-visible-in-window-p (point-min))
              (> (- (line-number-at-pos (point))
                    (line-number-at-pos (window-start)))
                 (/ (window-body-height) 2)))
    (scroll-down 1))
  (condition-case nil
      (line-move-1 (- arg))
    (beginning-of-buffer
     (run-hooks 'hydra-motion-prev-line-action))))

(defun hydra-motion-forwad ()
  (interactive)
  (call-interactively
   (if hydra-motion-mode-meta-pressed
       #'forward-word
     #'forward-char)))

(defun hydra-motion-backward ()
  (interactive)
  (call-interactively
   (if hydra-motion-mode-meta-pressed
       #'backward-word
     #'backward-char)))

(defun hydra-motion-page (arg)
  (interactive "p")
  (condition-case nil
      (funcall
       (if hydra-motion-mode-meta-pressed
           #'scroll-down-command
         #'scroll-up-command)
       (- (* arg (window-body-height))
          next-screen-context-lines))
    (end-of-buffer
     (cl-case major-mode
       ;;(elfeed-show-mode (elfeed-show-next))
       (t (goto-char (point-max)))))
    (beginning-of-buffer
     (cl-case major-mode
       ;;(elfeed-show-mode (elfeed-show-prev))
       (t (goto-char (point-min)))))))

(defun hydra-motion-scroll-left (arg)
  (interactive "p")
  (scroll-left arg))

(defun hydra-motion-scroll-right (arg)
  (interactive "p")
  (scroll-right arg))

;;* Minor mode

(defun hydra-motion-update-lighter ()
  (when hydra-motion-mode-lighter
    (concat " "
            (propertize hydra-motion-mode-lighter
                        'face
                        (when hydra-motion-mode-enabled 'region)))))

;; Rather dirty hack to enable this mode on non-ivy minibuffer
(defun hydra-motion-mode-on-minibuffer ()
  (when (and (eq major-mode 'minibuffer-inactive-mode)
             (not (memq 'ivy--exhibit post-command-hook))
             (not (eq this-command 'eval-expression)))
    (hydra-motion-mode 1)))

(define-minor-mode hydra-motion-mode
    "Hydra-motion mode replaces Emacs' standard motion commands (e.g., C-n)
with hydra-powered version."
  nil
  ;; TODO change cursor color
  hydra-motion-mode-lighter    ; (:eval (hydra-motion-update-lighter))
  hydra-motion-mode-map
  (if hydra-motion-mode
      (progn
        (unless hydra-motion-mode-is-helpful
          (hydra-set-property 'hydra-motion :verbosity 0))
        (add-hook 'minibuffer-setup-hook #'hydra-motion-mode-on-minibuffer))
    (hydra-set-property 'hydra-motion :verbosity nil)
    (remove-hook 'minibuffer-setup-hook #'hydra-motion-mode-on-minibuffer)))

(defun turn-on-hydra-motion-mode ()
  (unless (or (memq major-mode hydra-motion-exclude-modes)
              (cl-some (lambda (mode) (and (boundp mode) (symbol-value mode)))
                       hydra-motion-conflicting-modes))
    (hydra-motion-mode 1)))

;;;###autoload
(define-global-minor-mode global-hydra-motion-mode
    hydra-motion-mode turn-on-hydra-motion-mode)

(provide 'hydra-motion)
;;; hydra-motion.el ends here
