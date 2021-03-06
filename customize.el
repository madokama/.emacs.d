(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-allowed-packages '(all))
 '(auth-source-cache-expiry nil)
 '(aw-keys '(97 115 100 102 103 104 106 107 108))
 '(blink-cursor-mode nil)
 '(byte-compile-verbose nil)
 '(checkdoc-force-docstrings-flag nil)
 '(checkdoc-permit-comma-termination-flag t)
 '(checkdoc-verb-check-experimental-flag nil)
 '(company-backends
   '(company-semantic company-capf company-files
     (company-dabbrev-code company-gtags company-keywords)
     company-dabbrev))
 '(company-dabbrev-code-everywhere t)
 '(company-global-modes '(not shell-mode term-mode))
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(confirm-kill-emacs 'y-or-n-p)
 '(counsel-describe-function-preselect 'ivy-function-called-at-point)
 '(counsel-find-file-at-point t)
 '(counsel-git-grep-skip-counting-lines nil)
 '(counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
 '(counsel-grep-post-action-hook '(recenter))
 '(counsel-mode-override-describe-bindings t)
 '(counsel-yank-pop-preselect-last t)
 '(create-lockfiles nil)
 '(custom-raised-buttons nil)
 '(custom-safe-themes t)
 '(default-frame-alist
   '((internal-border-width . 0)
     (scroll-bar-width . 0)
     (scroll-bar-height . 0)
     (horizontal-scroll-bars)
     (right-divider-width . 0)
     (bottom-divider-width . 0)
     (background-mode . dark)
     (screen-gamma . 2.2)
     (title)))
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-laGh1v --group-directories-first")
 '(display-buffer-alist
   '(("\\(?:\\*\\(?:\\(?:Async Shell Command\\|compilation\\)\\*\\)\\)" display-buffer-no-window)
     ("\\`\\*\\(?:eww\\)\\*"
      (display-buffer-reuse-window display-buffer-pop-up-window))
     ("\\`\\*\\(?:shell\\)\\*"
      (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-same-window))
     ("\\`\\*\\(?:Compile-Log\\|Help\\|ggtags-global\\|ivy-occur.+?\\|define:.+?\\)\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames . visible)
      (side . bottom)
      (window-height . 0.3))))
 '(display-time-default-load-average nil)
 '(display-time-format " %H:%M")
 '(doc-view-continuous t)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-idle-delay 0.8)
 '(elfeed-curl-timeout 60)
 '(elfeed-db-directory "~/.emacs.d/elfeed")
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
 '(explicit-bash-args '("--noediting" "--login" "-i"))
 '(explicit-shell-file-name "bash")
 '(extempore-use-pretty-lambdas nil)
 '(fast-but-imprecise-scrolling t)
 '(fit-window-to-buffer-horizontally t)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes
   '(not org-mode elfeed-search-mode elfeed-show-mode diff-mode))
 '(flycheck-keymap-prefix "f")
 '(flycheck-mode-line-prefix "")
 '(flycheck-pos-tip-timeout 5)
 '(font-lock-global-modes nil)
 '(gc-cons-percentage 0.6)
 '(gc-cons-threshold 33554432)
 '(global-prettify-symbols-mode t)
 '(gtags-disable-pushy-mouse-mapping t)
 '(history-delete-duplicates t)
 '(hydra-lv 'posframe)
 '(idle-update-delay 2)
 '(iedit-toggle-key-default nil t)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-frame-alist '((alpha . 71) (fullscreen . fullboth)))
 '(initial-scratch-message nil)
 '(ivy-auto-select-single-candidate t t)
 '(ivy-completing-read-handlers-alist
   '((tmm-menubar . completing-read-default)
     (tmm-shortcut . completing-read-default)
     (image-save . completing-read-default)
     (open-junk-file . completing-read-default)))
 '(ivy-extra-directories '("./"))
 '(ivy-fixed-height-minibuffer nil)
 '(ivy-format-function 'ivy-format-function-arrow)
 '(ivy-ignore-buffers
   '("\\` " "company-statistics-cache\\.el" "/elfeed/index" "~\\'"))
 '(ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
 '(ivy-posframe-parameters '((alpha . 80)))
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'abbreviate)
 '(ivy-wrap nil)
 '(jit-lock-stealth-load 200)
 '(jit-lock-stealth-time 1)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold 536870911)
 '(lisp-indent-function 'common-lisp-indent-function)
 '(lispy-avy-style-symbol 'at-full)
 '(lispy-comment-use-single-semicolon t)
 '(lispy-no-permanent-semantic t)
 '(lispy-no-space t)
 '(ls-lisp-use-insert-directory-program t)
 '(mailcap-user-mime-data '((pdf-view-mode "application/pdf" nil)))
 '(menu-bar-mode nil)
 '(mpa-options '("--volume=80" "--ytdl-raw-options=no-mark-watched="))
 '(multi-term-program "bash")
 '(multi-term-program-switches "--login")
 '(next-error-recenter '(4))
 '(next-screen-context-lines 1)
 '(org-agenda-files '("~/Dropbox/emacs/org/piano.org"))
 '(org-babel-clojure-backend 'cider)
 '(org-babel-hash-show-time t)
 '(org-babel-shell-names '("sh" "bash" "cmd" "bat"))
 '(org-capture-templates
   '(("l" "Link" entry
      (file+olp+datetree "")
      "* %:description  %^g
%(org-capture--quote)%?Source: %(org-make-link-string \"%:link\")
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
%U")))
 '(org-catch-invisible-edits 'show-and-error)
 '(org-confirm-babel-evaluate nil)
 '(org-directory "~/Dropbox/emacs/org")
 '(org-ellipsis "…")
 '(org-hide-emphasis-markers nil)
 '(org-image-actual-width nil)
 '(org-protocol-default-template-key "l")
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-return-follows-link t)
 '(org-src-preserve-indentation t)
 '(org-src-window-setup 'current-window)
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(pdf-tools-enabled-hook '(pdf-view-midnight-minor-mode auto-revert-mode))
 '(pdf-tools-enabled-modes
   '(pdf-history-minor-mode pdf-isearch-minor-mode pdf-links-minor-mode pdf-outline-minor-mode pdf-cache-prefetch-minor-mode pdf-view-auto-slice-minor-mode pdf-occur-global-minor-mode))
 '(pdf-tools-handle-upgrades nil)
 '(pdf-view-use-imagemagick t)
 '(persistent-scratch-backup-directory "~/Dropbox/emacs/.persistent-scratch")
 '(persistent-scratch-what-to-save '(major-mode point))
 '(powerline-default-separator 'chamfer)
 '(proced-auto-update-flag t)
 '(proced-tree-flag t)
 '(recentb-abema-channels '(shogi shogi-live))
 '(recenter-positions '(top middle bottom))
 '(recenter-redisplay t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude '("~\\'" "\\.elc\\'" "/\\.emacs\\.d/url/cache/"))
 '(recentf-max-saved-items 999)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((coding-system-for-read . utf-8)
     (c-default-style . "gnu")))
 '(save-interprogram-paste-before-kill t)
 '(save-place-forget-unreadable-files t)
 '(save-place-ignore-files-regexp "\\(?:COMMIT_EDITMSG\\|/elfeed/index\\)$")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 2305843009213693951)
 '(scroll-margin 1)
 '(sentence-end-double-space nil)
 '(shell-completion-execonly nil)
 '(shell-dynamic-complete-functions
   '(comint-c-a-p-replace-by-expanded-history shell-environment-variable-completion shell-command-completion shell-c-a-p-replace-by-expanded-directory shell-filename-completion comint-filename-completion))
 '(shell-file-name "sh")
 '(shr-image-animate t)
 '(shr-width 80)
 '(so-long-minor-modes
   '(font-lock-mode highlight-changes-mode hi-lock-mode hl-line-mode linum-mode nlinum-mode prettify-symbols-mode visual-line-mode whitespace-mode rainbow-delimiters-mode show-paren-mode electric-indent-mode))
 '(so-long-target-modes '(prog-mode css-mode sgml-mode nxml-mode org-mode))
 '(swiper-action-recenter t)
 '(switch-to-buffer-in-dedicated-window 'prompt)
 '(symon-refresh-rate 2)
 '(symon-sparkline-ascent 'center)
 '(symon-sparkline-thickness 1)
 '(symon-sparkline-type 'plain)
 '(symon-sparkline-width 72)
 '(tab-always-indent 'complete)
 '(tao-theme-use-height nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(undo-tree-incompatible-major-modes
   '(magit-status-mode magit-process-mode magit-diff-mode messages-buffer-mode term-mode shell-mode elfeed-search-mode elfeed-show-mode Custom-mode eww-mode))
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(url-cache-expire-time 604800)
 '(url-history-file "~/.emacs.d/url/history")
 '(url-mime-language-string "en-US,en")
 '(url-queue-timeout 60)
 '(url-user-agent
   "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:59.0) Gecko/20100101 Firefox/59.0")
 '(user-full-name "Madoka Machitani")
 '(windmove-window-distance-delta 2)
 '(windmove-wrap-around t)
 '(window-combination-resize t)
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
