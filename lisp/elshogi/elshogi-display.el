;;; elshogi-display --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'elshogi-game)

(defvar elshogi-display-style 'plain)
(defvar elshogi-display-handler nil)
(defvar elshogi-display-use-frame nil)
(defvar elshogi-display-frame-params nil)
(defvar elshogi-display-after-hooks nil)

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

(defun elshogi-display-update-squares (&rest indice)
  (apply #'elshogi-display-generic :squares indice))

(defun elshogi-display-highlight-selected (index)
  (elshogi-display-generic :hl-sel index))

(defun elshogi-display-highlight-candidates (indice)
  (elshogi-display-generic :hl-cands indice))

(defun elshogi-display-note (game)
  (when (elshogi-game/watch-p game)
    (when-let* ((mrec (elshogi-game-latest-move game)))
      (with-current-buffer
          (get-buffer-create (format "*kifc:%s*" (elshogi-game/title game)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when-let* (note (elshogi-mrec/note mrec))
            (save-excursion (insert note))
            (display-buffer (current-buffer)
                            '(display-buffer-in-side-window))))))))

(defvar elshogi-mode-map)
(declare-function elshogi-mode "ext:elshogi")
(declare-function elshogi-quit "ext:elshogi")

(defun elshogi-display-quit-frame ()
  "Quit elshogi game frame."
  (interactive)
  (let ((bufs
         (cl-delete-duplicates (mapcar #'window-buffer (window-list))
                               :test #'eq)))
    (elshogi-quit)
    (mapc #'kill-buffer bufs))
  (delete-frame))

(defun elshogi-display-buffer (buf &optional update-p)
  (with-current-buffer buf
    (elshogi-mode)
    (if (and elshogi-display-use-frame (not update-p))
        (elshogi-display-frame)
      (pop-to-buffer-same-window buf))
    (run-hooks 'elshogi-display-after-hooks))
  buf)

(defun elshogi-display-frame ()
  (let ((map (make-sparse-keymap))
        (after-make-frame-functions nil) ; Avoid interference by elscreen
        )
    (select-frame
     (make-frame `((tool-bar-lines . 0)
                   (menu-bar-lines . 0)
                   ;; Make new frames minibufferless so they can refer
                   ;; the parent frame with `default-minibuffer-frame'.
                   (minibuffer . nil)
                   (vertical-scroll-bars . nil)
                   ,@elshogi-display-frame-params)))
    (define-key map [?q] #'elshogi-display-quit-frame)
    (set-keymap-parent map elshogi-mode-map)
    (use-local-map map)))

;; Avoid cyclic require error
(add-hook 'elshogi-mode-hook #'elshogi-display-initialize)

(provide 'elshogi-display)
;;; elshogi-display.el ends here
