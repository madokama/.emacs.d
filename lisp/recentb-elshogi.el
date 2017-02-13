;;; recentb-elshogi --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'elshogi-game))
(require 'seq)
(require 'recentb)

(recentb-define-mode elshogi-mode
  :var recentb-elshogi
  :history recentb-elshogi-history
  :candidate recentb-elshogi-candidate)

(defun recentb-elshogi-watch (item)
  (elshogi-watch (seq-let (kif _ count) item
                   (if (and (ffap-url-p kif) count)
                       (let ((urlobj (url-generic-parse-url kif)))
                         (setf (url-filename urlobj)
                               (format "%s?te=%d"
                                       (car (url-path-and-query urlobj))
                                       count))
                         (url-recreate-url urlobj))
                     kif))))

(defun recentb-elshogi-candidate (item)
  (let ((title (format "*elshogi:%s*" (cadr item))))
    (unless (get-buffer title)
      (propertize title
                  'recentb (list 'recentb-elshogi-watch item)))))

(defvar elshogi-current-game)
(declare-function dlink/content "ext:dlist")
(declare-function elshogi-game/url "ext:elshogi-struct")
(declare-function elshogi-game/kif "ext:elshogi-struct")
(declare-function elshogi-game/title "ext:elshogi-struct")
(declare-function elshogi-mrec/count "ext:elshogi-struct")

(defun recentb-elshogi-normalize-url (url)
  "Strip query parameters from URL."
  (if (ffap-url-p url)
      (let ((urlobj (url-generic-parse-url url)))
        (setf (url-filename urlobj)
              (car (url-path-and-query urlobj)))
        (url-recreate-url urlobj))
    url))

(defun recentb-elshogi-history ()
  (when (derived-mode-p 'elshogi-mode)
    (when-let* (url (or (elshogi-game/url elshogi-current-game)
                        (elshogi-game/kif elshogi-current-game)))
      (cl-delete-duplicates
       (cons (list (recentb-elshogi-normalize-url url)
                   (elshogi-game/title elshogi-current-game)
                   (elshogi-mrec/count
                    (elshogi-game-latest-move elshogi-current-game)))
             recentb-elshogi)
       :test (lambda (a b)
               (string= (car a) (car b)))
       :from-end t))))

(provide 'recentb-elshogi)
;;; recentb-elshogi.el ends here
