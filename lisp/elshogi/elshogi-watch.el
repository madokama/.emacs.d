;;; elshogi-watch --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi)
(require 'elshogi-kif)

(defun elshogi-watch-buffer (game)
  (get-buffer-create (format "*elshogi:%s*" (elshogi-game/title game))))

(defun elshogi-watch-minor-mode (game)
  (setq elshogi-current-game game)
  (setq mode-line-format
        (list " " '(:eval (elshogi-watch-move-count))
              " " (propertize (elshogi-game/title game)
                              'face 'mode-line-buffer-id)))
  (setq-local revert-buffer-function #'elshogi-watch-update))

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

(defun elshogi-watch-game (game &optional update-p)
  (with-current-buffer
      (elshogi-display-buffer (elshogi-watch-buffer game) update-p)
    (elshogi-watch-minor-mode game)
    (elshogi-display-board game)))

(defun elshogi-watch-update (&rest _)
  (when (derived-mode-p 'elshogi-mode)
    (when-let* (kif (or (elshogi-game/url elshogi-current-game)
                        (elshogi-game/kif elshogi-current-game)))
      (elshogi-kif-parse kif
                         (lambda (game)
                           (elshogi-watch-game
                            (elshogi-replay-seek
                             game
                             (elshogi-mrec/count
                              (elshogi-game-latest-move elshogi-current-game)))
                            t))))))

;;;###autoload
(defun elshogi-watch (kif &optional _new-window)
  "Replay shogi game record KIF."
  (interactive (list (read-string "KIF: ")))
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
