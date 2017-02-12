;;; recentb-elshogi --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'elshogi-game))
(require 'recentb)

(recentb-define-mode elshogi-mode
  :var recentb-elshogi
  :history recentb-elshogi-history
  :candidate recentb-elshogi-candidate)

(defun recentb-elshogi-candidate (item)
  (let ((title (format "*elshogi:%s*" (cadr item))))
    (unless (get-buffer title)
      (propertize title
                  'recentb (list 'elshogi-watch (car item))))))

(defvar elshogi-current-game)
(declare-function dlink/content "ext:dlist")
;; (declare-function elshogi-game-cursor "ext:elshogi-game")
(declare-function elshogi-game/url "ext:elshogi-struct")
(declare-function elshogi-game/kif "ext:elshogi-struct")
(declare-function elshogi-game/title "ext:elshogi-struct")
(declare-function elshogi-mrec/count "ext:elshogi-struct")

(defun recentb-elshogi-history ()
  (when (derived-mode-p 'elshogi-mode)
    (when-let* (kif (or (elshogi-game/url elshogi-current-game)
                        (elshogi-game/kif elshogi-current-game)))
      (cl-delete-duplicates
       (cons (list kif
                   (elshogi-game/title elshogi-current-game)
                   (thread-first elshogi-current-game
                     elshogi-game-cursor
                     dlink/content
                     elshogi-mrec/count))
             recentb-elshogi)
       :test (lambda (a b)
               (string= (car a) (car b)))
       :from-end t))))

(provide 'recentb-elshogi)
;;; recentb-elshogi.el ends here
