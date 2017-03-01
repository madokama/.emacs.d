;;; elshogi-display --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'elshogi-game)
(require 'elshogi-mouse)

(defvar elshogi-display-style 'plain)
(defvar elshogi-display-handler nil)
(defvar elshogi-display-use-frame nil)
(defvar elshogi-display-frame-params nil)
(defvar elshogi-display-frame-hooks nil)

(defun elshogi-display-register (plist)
  (setq elshogi-display-handler plist))

(defsubst elshogi-display-initialize ()
  (unless elshogi-display-handler
    (require (intern (format "elshogi-%s" elshogi-display-style)))))

(defsubst elshogi-display-generic (op &rest args)
  ;; (info "sicp#2-4-3")
  (if-let* (f (plist-get elshogi-display-handler op))
      (apply f args)
    (message "Operation %s on %S not defined for %s"
             op args elshogi-display-style)))

(defun elshogi-display-board (game)
  (elshogi-display-generic :board game)
  (elshogi-display-note game))

(defun elshogi-display-piece-stand (game side)
  (elshogi-display-generic :stand game side))

(defun elshogi-display-update-squares (&rest indices)
  (apply #'elshogi-display-generic :squares indices))

(defun elshogi-display-highlight-selected (index)
  (elshogi-display-generic :hl-sel index))

(defun elshogi-display-highlight-candidates (indices)
  (elshogi-display-generic :hl-cands indices))



(defun elshogi-display-note-bufname (game)
  (format "*kifc:%s*" (elshogi-game/title game)))

(defun elshogi-display-note-buffer (game)
  "Return commentary buffer name of GAME."
  (let ((bufname (elshogi-display-note-bufname game)))
    (or (get-buffer bufname)
        (with-current-buffer (generate-new-buffer bufname)
          (setq mode-line-format nil)
          (current-buffer)))))

(defun elshogi-display-note (game)
  (when (elshogi-game/watch-p game)
    (when-let* ((mrec (elshogi-game-latest-move game)))
      (with-current-buffer (elshogi-display-note-buffer game)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when-let* (note (elshogi-mrec/note mrec))
            (save-excursion
              (insert note)
              (elshogi-display-note-urlify game))
            (display-buffer (current-buffer)
                            '(display-buffer-in-side-window))))))))

(defun elshogi-display-follow-link (ev)
  "Follow link on mouse event EV."
  (interactive "e")
  (seq-let (game link) (elshogi-mouse-read-props ev 'game 'htmlize-link)
    ;; Not to open links in a shogi-dedicated frame.
    (with-selected-frame default-minibuffer-frame
      (browse-url (plist-get link :uri)))
    (elshogi-game-focus game)))

(defvar org-link-parameters)
(autoload 'org-activate-plain-links "org")

(defvar elshogi-display-link-params
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [mouse-2] #'elshogi-display-follow-link)
    (define-key map [follow-link] 'mouse-face)
    `(("http" :keymap ,map) ("https" :keymap ,map))))

(defun elshogi-display-note-urlify (game)
  (save-match-data
    (let ((org-link-parameters elshogi-display-link-params))
      (goto-char (point-min))
      ;; Copied from org-agenda.el
      (while (org-activate-plain-links (point-max))
        (add-text-properties (match-beginning 0) (match-end 0)
                             `(game ,game))))))

(defun elshogi-display-note-scroll (cmd)
  (when-let* (win
              (get-buffer-window
               (elshogi-display-note-bufname elshogi-current-game)))
    (with-selected-window win
      (call-interactively cmd))))

(defun elshogi-display-note-scroll-up ()
  "Scroll Kif commentary window forward."
  (interactive)
  (elshogi-display-note-scroll #'scroll-up-command))

(defun elshogi-display-note-scroll-down ()
  "Scroll Kif commentary window backward."
  (interactive)
  (elshogi-display-note-scroll #'scroll-down-command))

(declare-function elshogi-game-replay-next "elshogi")

(defun elshogi-display-note-scroll-next ()
  "Scroll Kif commentary window forward.
Go to next position if the bottom of the window is visible."
  (interactive)
  (condition-case nil
      (elshogi-display-note-scroll-up)
    (end-of-buffer
     (elshogi-game-replay-next))))



(defvar elshogi-mode-map)
(declare-function elshogi-mode "elshogi")
(declare-function elshogi-quit "elshogi")

(defun elshogi-display-quit-frame ()
  "Quit elshogi game frame."
  (interactive)
  (let ((bufs
         (cl-delete-duplicates (mapcar #'window-buffer (window-list))
                               :test #'eq)))
    (elshogi-quit)
    (mapc #'kill-buffer bufs))
  (delete-frame))

(defun elshogi-display-buffer (game)
  (let ((buf (elshogi-game-buffer game)))
    (with-current-buffer buf
      (elshogi-mode)
      (if elshogi-display-use-frame
          (unless (get-buffer-window buf t)
            (elshogi-display-frame game))
        (pop-to-buffer-same-window buf)))
    buf))

(defun elshogi-display-frame (game)
  (let ((map (make-sparse-keymap))
        (after-make-frame-functions nil) ; Avoid interference by elscreen
        )
    (select-frame
     (make-frame `((tool-bar-lines . 0)
                   (menu-bar-lines . 0)
                   ;; Make new frames minibufferless so they can refer
                   ;; the parent frame with `default-minibuffer-frame'.
                   (minibuffer . ,(not (elshogi-game/watch-p game)))
                   (vertical-scroll-bars . nil)
                   ,@elshogi-display-frame-params)))
    (define-key map [?q] #'elshogi-display-quit-frame)
    (set-keymap-parent map elshogi-mode-map)
    (use-local-map map)
    (run-hooks 'elshogi-display-frame-hooks)))

;; Avoid cyclic require error
(add-hook 'elshogi-mode-hook #'elshogi-display-initialize)

(provide 'elshogi-display)
;;; elshogi-display.el ends here
