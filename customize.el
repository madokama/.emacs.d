(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-allowed-packages (quote (all)))
 '(blink-cursor-mode nil)
 '(byte-compile-verbose nil)
 '(checkdoc-force-docstrings-flag nil)
 '(checkdoc-permit-comma-termination-flag t)
 '(checkdoc-verb-check-experimental-flag nil)
 '(company-backends
   (quote
    (company-semantic company-capf company-files
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-dabbrev)))
 '(company-dabbrev-code-everywhere t)
 '(company-global-modes (quote (not shell-mode term-mode)))
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(counsel-find-file-at-point t)
 '(counsel-find-file-ignore-regexp "\\(?:\\.elc\\|~\\)\\'")
 '(counsel-grep-base-command "grep -naiE \"%s\" %s")
 '(counsel-grep-post-action-hook (quote (recenter)))
 '(counsel-mode-override-describe-bindings t)
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows nil)
 '(custom-raised-buttons nil)
 '(custom-safe-themes t)
 '(default-frame-alist
   (quote
    ((internal-border-width . 0)
     (scroll-bar-width . 0)
     (scroll-bar-height . 0)
     (horizontal-scroll-bars)
     (right-divider-width . 0)
     (bottom-divider-width . 0)
     (background-mode . dark)
     (screen-gamma . 2.2)
     (title))))
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-laGh1v --group-directories-first")
 '(display-buffer-alist
   (quote
    (("\\(?:\\*\\(?:\\(?:Async Shell Command\\|compilation\\)\\*\\)\\)" display-buffer-no-window)
     ("\\`\\*\\(?:eww\\)\\*"
      (display-buffer-reuse-window display-buffer-pop-up-window))
     ("\\`\\*\\(?:shell\\)\\*"
      (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-same-window))
     ("\\`\\*\\(?:Compile-Log\\|Help\\|ggtags-global\\|ivy-occur.+?\\)\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames . visible)
      (side . bottom)
      (window-height . 0.3)))))
 '(display-time-default-load-average nil)
 '(display-time-format " %H:%M")
 '(doc-view-continuous t)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-idle-delay 0.8)
 '(elfeed-curl-timeout 60)
 '(elfeed-db-directory "~/Dropbox/emacs/elfeed")
 '(elfeed-enclosure-default-dir "~/Downloads/")
 '(elfeed-show-truncate-long-urls nil)
 '(elfeed-use-curl nil)
 '(elfeed-user-agent "")
 '(elscreen-display-screen-number nil)
 '(elscreen-display-tab nil)
 '(enable-recursive-minibuffers t)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 512)
 '(eww-history-limit 300)
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(explicit-shell-file-name "bash")
 '(extempore-use-pretty-lambdas nil)
 '(fast-but-imprecise-scrolling t)
 '(fit-window-to-buffer-horizontally t)
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(flycheck-global-modes
   (quote
    (not org-mode elfeed-search-mode elfeed-show-mode diff-mode)))
 '(flycheck-keymap-prefix "f")
 '(flycheck-mode-line-prefix "")
 '(flycheck-pos-tip-timeout 5)
 '(gc-cons-threshold 33554432)
 '(global-prettify-symbols-mode t)
 '(gtags-disable-pushy-mouse-mapping t)
 '(history-delete-duplicates t)
 '(idle-update-delay 2)
 '(iedit-toggle-key-default nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-frame-alist nil)
 '(initial-scratch-message nil)
 '(ivy-completing-read-handlers-alist
   (quote
    ((tmm-menubar . completing-read-default)
     (tmm-shortcut . completing-read-default)
     (image-save . completing-read-default)
     (open-junk-file . completing-read-default))))
 '(ivy-extra-directories (quote ("./")))
 '(ivy-fixed-height-minibuffer nil)
 '(ivy-format-function (quote ivy-format-function-arrow))
 '(ivy-ignore-buffers
   (quote
    ("\\` " "company-statistics-cache\\.el" "/elfeed/index" "~\\'")))
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(ivy-wrap nil)
 '(jit-lock-stealth-load 200)
 '(jit-lock-stealth-time 1)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold 536870911)
 '(lisp-indent-function (quote common-lisp-indent-function))
 '(lispy-avy-style-symbol (quote at-full))
 '(lispy-comment-use-single-semicolon t)
 '(lispy-no-space t)
 '(ls-lisp-use-insert-directory-program t)
 '(mailcap-user-mime-data (quote ((pdf-view-mode "application/pdf" nil))))
 '(menu-bar-mode nil)
 '(multi-term-program "bash")
 '(multi-term-program-switches "--login")
 '(next-screen-context-lines 1)
 '(org-agenda-files (quote ("~/Dropbox/emacs/org/notes.org")))
 '(org-babel-clojure-backend (quote cider))
 '(org-babel-hash-show-time t)
 '(org-babel-shell-names (quote ("sh" "bash")))
 '(org-capture-templates
   (quote
    (("l" "Link" entry
          (file+olp+datetree "")
          "* %:description  %^g
%(org-capture--quote)%?Source: %:link
%U")
     ("y" "YouTube" entry
          (file+olp+datetree "vids.org")
          "* %(org-ytdl-capture)  %^g
%(org-ytdl-run-template-hook)
%?
%U")
     ("t" "Todo" entry
          (file+olp+datetree "todo.org")
          "* TODO %?
%i
%a
%U")
     ("n" "Note" entry
          (file+olp+datetree "")
          "* %?
%i
%a
%U"))))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-confirm-babel-evaluate nil)
 '(org-directory "~/Dropbox/emacs/org")
 '(org-ellipsis "…")
 '(org-hide-emphasis-markers nil)
 '(org-image-actual-width nil)
 '(org-protocol-default-template-key "l")
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 6))))
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-window-setup (quote current-window))
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(pdf-tools-enabled-hook (quote (pdf-view-midnight-minor-mode auto-revert-mode)))
 '(pdf-tools-enabled-modes
   (quote
    (pdf-history-minor-mode pdf-isearch-minor-mode pdf-links-minor-mode pdf-outline-minor-mode pdf-cache-prefetch-minor-mode pdf-view-auto-slice-minor-mode pdf-occur-global-minor-mode)))
 '(pdf-tools-handle-upgrades nil)
 '(pdf-view-use-imagemagick t)
 '(persistent-scratch-backup-directory "~/Dropbox/emacs/.persistent-scratch")
 '(persistent-scratch-what-to-save (quote (major-mode point)))
 '(powerline-default-separator (quote chamfer))
 '(proced-auto-update-flag t)
 '(proced-tree-flag t)
 '(recenter-positions (quote (top middle bottom)))
 '(recentf-exclude (quote ("~\\'")))
 '(recentf-max-saved-items 999)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((coding-system-for-read . utf-8)
     (c-default-style . "gnu"))))
 '(save-interprogram-paste-before-kill t)
 '(save-place-forget-unreadable-files t)
 '(save-place-ignore-files-regexp "\\(?:COMMIT_EDITMSG\\|/elfeed/index\\)$")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 2305843009213693951)
 '(sentence-end-double-space nil)
 '(shell-completion-execonly nil)
 '(shell-file-name "sh")
 '(shr-image-animate t)
 '(shr-width 80)
 '(so-long-minor-modes
   (quote
    (font-lock-mode highlight-changes-mode hi-lock-mode hl-line-mode linum-mode nlinum-mode prettify-symbols-mode visual-line-mode whitespace-mode rainbow-delimiters-mode show-paren-mode electric-indent-mode)))
 '(so-long-target-modes (quote (prog-mode css-mode sgml-mode nxml-mode org-mode)))
 '(swiper-action-recenter t)
 '(switch-to-buffer-in-dedicated-window (quote prompt))
 '(symon-refresh-rate 2)
 '(symon-sparkline-ascent (quote center))
 '(symon-sparkline-thickness 1)
 '(symon-sparkline-type (quote plain))
 '(symon-sparkline-width 72)
 '(tab-always-indent (quote complete))
 '(tao-theme-use-height nil)
 '(tool-bar-mode nil)
 '(undo-tree-incompatible-major-modes
   (quote
    (magit-status-mode magit-process-mode magit-diff-mode messages-buffer-mode term-mode shell-mode elfeed-search-mode elfeed-show-mode Custom-mode eww-mode)))
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(url-cache-expire-time 604800)
 '(url-history-file "~/.emacs.d/url/history")
 '(url-mime-language-string "en-US,en")
 '(url-queue-parallel-processes 2)
 '(url-queue-timeout 60)
 '(user-full-name "Madoka Machitani")
 '(windmove-window-distance-delta 2)
 '(windmove-wrap-around t)
 '(window-combination-resize t)
 '(window-resize-pixelwise t)
 '(winner-dont-bind-my-keys t)
 '(yas-use-menu nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo ((t nil)) t)
 '(company-scrollbar-bg ((t nil)))
 '(company-scrollbar-fg ((t nil)))
 '(company-tooltip ((t nil)))
 '(company-tooltip-annotation ((t nil)))
 '(company-tooltip-common ((t)))
 '(eval-sexp-fu-flash ((t (:background "#101010" :foreground "white" :weight normal))))
 '(widget-field ((t (:background "#3C3C3C")))))
