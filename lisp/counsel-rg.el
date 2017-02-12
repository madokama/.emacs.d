;;; counsel-rg --- search with ripgrep -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'counsel)

(defvar counsel-rg-params nil)

(defun counsel--async-process (cmd args)
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq counsel--async-start (setq counsel--async-time (current-time)))
    (make-process :name counsel--process
                  :buffer counsel--process
                  :command (cons cmd args)
                  :connection-type nil
                  :filter #'counsel--async-filter
                  :sentinel #'counsel--async-sentinel)))

(defun counsel--rg-candidates (regex &optional no-async)
  (let ((args
         `("-n" "--no-heading"
                ,@counsel-rg-params
                "-e" ,(counsel-unquote-regex-parens regex)
                ,(expand-file-name counsel--git-grep-dir))))
    (if no-async
        (split-string (with-output-to-string
                        (with-current-buffer standard-output
                          (apply #'call-process "rg" nil t nil args)))
                      "\n" t)
      (counsel--async-process "rg" args)
      nil)))

(defun counsel-rg-function (input)
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (counsel--rg-candidates (setq ivy--old-re (ivy--regex input)))))

(advice-add 'counsel-grep-function :override #'counsel-rg-function)
(advice-add 'counsel--gg-candidates :override #'counsel--rg-candidates)
(advice-add 'counsel--gg-count :override (lambda (&rest _) 20001))

(defun ad-counsel-rg (&optional initial-input initial-directory)
  "Search for INITIAL-INPUT in INITIAL-DIRECTORY recursively with rg."
  (interactive (list nil
                     (when current-prefix-arg
                       (read-directory-name "rg in directory: "))))
  (setq counsel-grep-last-line nil)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-set-prompt 'counsel-rg #'counsel-prompt-function-default)
  (let ((init-point (point))
        res)
    (unwind-protect
         (setq res (ivy-read "rg" #'counsel-rg-function
                             :initial-input initial-input
                             ;; :matcher #'counsel-git-grep-matcher
                             :dynamic-collection t
                             :keymap counsel-ag-map
                             :history 'counsel-git-grep-history
                             :re-builder #'ivy--regex
                             :action #'counsel-git-grep-action
                             :unwind (lambda ()
                                       (counsel-delete-process)
                                       (swiper--cleanup))
                             :caller 'counsel-rg))
      (unless res
        (goto-char init-point)))))

(advice-add 'counsel-rg :override #'ad-counsel-rg)

;; TODO implement counsel-rg-occur

;; (defun counsel-rg-occur ()
;;   (unless (eq major-mode 'ivy-occur-grep-mode)
;;     (ivy-occur-grep-mode))
;;   (setq default-directory counsel--git-grep-dir)
;;   (let ((cands nil))
;;     (insert
;;      (format "-*- mode:grep; default-directory: %S -*-\n\n\n" default-directory))
;;     (insert (format "%d candidates:\n" (length cands)))
;;     (ivy--occur-insert-lines
;;      (mapcar (apply-partially #'concat "./")
;;              cands))))

;; (defun counsel-rg-occur ()
;;   "Generate a custom occur buffer for `counsel-rg'."
;;   (unless (eq major-mode 'ivy-occur-grep-mode)
;;     (ivy-occur-grep-mode))
;;   (setq default-directory counsel--git-grep-dir)
;;   (let* ((regex
;;           (counsel-unquote-regex-parens
;;            (setq ivy--old-re
;;                  (ivy--regex
;;                   (progn (string-match "\"\\(.*\\)\"" (buffer-name))
;;                          (match-string 1 (buffer-name)))))))
;;          (cands (counsel--rg-candidates regex t)))
;;     ;; Need precise number of header lines for `wgrep' to work.
;;     (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
;;                     default-directory))
;;     (insert (format "%d candidates:\n" (length cands)))
;;     (ivy--occur-insert-lines
;;      (mapcar (apply-partially #'concat "./")
;;              cands))))

;; (counsel-set-async-exit-code 'counsel-rg 1 "No matches found")
(counsel-set-async-exit-code 'counsel-git-grep 1 "No matches found")
;; (ivy-set-occur 'counsel-rg 'counsel-rg-occur)
;; (ivy-set-display-transformer 'counsel-rg #'counsel-git-grep-transformer)

(provide 'counsel-rg)
;;; counsel-rg.el ends here
