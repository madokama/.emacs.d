;;; elshogi-watch --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi)
(require 'elshogi-kif)

(defvar-local elshogi-watch-update-interval 300)

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

(defun elshogi-watch-game (game)
  (with-current-buffer
      (elshogi-display-buffer (elshogi-game-buffer game))
    (elshogi-display-board game)
    (elshogi-watch-mode game)
    (elshogi-watch-auto-update game)))

(defun elshogi-watch-auto-update (game)
  (when (and (elshogi-game/live-p game)
             elshogi-watch-update-interval)
    (run-with-timer (max elshogi-watch-update-interval 30) nil
                    (lambda (buf)
                      (with-current-buffer buf
                        (elshogi-watch-update)))
                    (elshogi-game-buffer game))))

(defun elshogi-watch-update (&rest _)
  (when (derived-mode-p 'elshogi-mode)
    (when-let* (kif (or (elshogi-game/url elshogi-current-game)
                        (elshogi-game/kif elshogi-current-game)))
      (elshogi-kif-parse
       kif
       (lambda (game)
         (with-selected-frame (elshogi-game-frame game)
           (elshogi-watch-game
            (elshogi-replay-seek
             game
             (elshogi-mrec/count
              (elshogi-game-latest-move elshogi-current-game))))))))))

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
