;;; elshogi-watch --- minor mode for watching shogi games -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi)
(require 'elshogi-kif)

(defvar-local elshogi-watch-mode nil)
(defvar-local elshogi-watch-update-interval 300)
(defvar-local elshogi-watch-timer nil)

(defun elshogi-watch-mode (game)
  (setq elshogi-current-game game)
  (unless elshogi-watch-mode
    (let ((map (current-local-map)))
      (define-key map [remap scroll-down-command] #'elshogi-display-note-scroll-down)
      (define-key map [remap scroll-up-command] #'elshogi-display-note-scroll-up)
      (define-key map [? ] #'elshogi-display-note-scroll-next))
    (setq mode-line-format
          (list " " '(:eval (elshogi-watch-move-count))
                " " (propertize (elshogi-game/title game)
                                'face 'mode-line-buffer-id)))
    (setq-local revert-buffer-function #'elshogi-watch-update)
    (setq elshogi-watch-mode t)))

(defsubst elshogi-watch-may-amb-count (n amb-p)
  ;; To avoid revealing the winner.
  (if amb-p
      (replace-regexp-in-string (rx digit eos) "x" (number-to-string n))
    n))

(defun elshogi-watch-move-count ()
  (cl-flet ((count (move)
              (or (elshogi-mrec/count (funcall move elshogi-current-game))
                  0)))
    (format "%d/%s"
            (count #'elshogi-game-latest-move)
            (elshogi-watch-may-amb-count
             (count #'elshogi-game-final-move)
             (not (elshogi-game/live-p elshogi-current-game))))))

(defun elshogi-watch-game (game)
  (with-current-buffer (elshogi-display-buffer game)
    (elshogi-display-board game)
    (elshogi-watch-mode game)
    (elshogi-watch-auto-update game)))

(defun elshogi-watch-auto-update (game)
  (when (and (elshogi-game/live-p game)
             elshogi-watch-update-interval)
    ;; Prevent manual updates firing extra timers.
    (when elshogi-watch-timer
      (cancel-timer elshogi-watch-timer))
    (setq elshogi-watch-timer
          (run-with-timer (max elshogi-watch-update-interval 30) nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (setq elshogi-watch-timer nil)
                                (elshogi-watch-update))))
                          (elshogi-game-buffer game)))))

(defun elshogi-watch-update (&rest _)
  (when (derived-mode-p 'elshogi-mode)
    (when-let* ((game elshogi-current-game)
                (kif (or (elshogi-game/url game) (elshogi-game/kif game))))
      (elshogi-kif-parse
       kif
       (lambda (new-game)
         (with-selected-frame (elshogi-game-frame game)
           (elshogi-watch-game
            (elshogi-replay-seek
             new-game
             (elshogi-mrec/count (elshogi-game-latest-move game))))))))))

(declare-function eww-current-url "eww")

(defun elshogi-watch-candidates ()
  (cl-reduce (lambda (l r)
               (or (and l
                        (let ((url
                               (replace-regexp-in-string (rx (or (and bos "\"")
                                                                 (and "\"" eos)))
                                                         ""
                                                         l)))
                          (and (elshogi-kif-url-p url)
                               (cons url r))))
                   r))
             (list (gui-get-selection 'CLIPBOARD)
                   (car kill-ring)
                   (get-text-property (point) 'shr-url)
                   (thing-at-point 'url)
                   (cond ((derived-mode-p 'eww-mode)
                          (eww-current-url))))
             :initial-value nil :from-end t))

;;;###autoload
(defun elshogi-watch (kif &optional _new-window)
  "Replay shogi game record KIF."
  (interactive (list (completing-read "KIF: " (elshogi-watch-candidates))))
  (elshogi-kif-parse kif #'elshogi-watch-game))

(declare-function org-store-link-props "org")

;;;###autoload
(defun elshogi-watch-org-link ()
  (when (derived-mode-p 'elshogi-mode)
    (let ((url (elshogi-game/url elshogi-current-game)))
      (org-store-link-props
       :type "kif"
       :link (format "kif:%s" url)
       :url url
       :description (elshogi-game/title elshogi-current-game)))))

;;;###autoload(with-eval-after-load 'org (org-link-set-parameters "kif" :follow #'elshogi-watch :store #'elshogi-watch-org-link))

(provide 'elshogi-watch)
;;; elshogi-watch.el ends here
