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

(defun recentb-elshogi-candidate-url (item)
  (seq-let (kif _ count) item
    (if (and (ffap-url-p kif) count)
        (let ((urlobj (url-generic-parse-url kif)))
          (setf (url-filename urlobj)
                (format "%s?te=%d"
                        (car (url-path-and-query urlobj))
                        count))
          (url-recreate-url urlobj))
      kif)))

(defun recentb-elshogi-watch (item)
  (elshogi-watch (recentb-elshogi-candidate-url item)))

(defun recentb-elshogi-candidate (item)
  (let ((title (format "*elshogi:%s*" (cadr item))))
    (unless (get-buffer title)
      (propertize title
                  'recentb (list 'recentb-elshogi-watch item)))))

(defvar elshogi-current-game)
(declare-function elshogi-game/url "ext:elshogi-struct")
(declare-function elshogi-game/kif "ext:elshogi-struct")
(declare-function elshogi-game/title "ext:elshogi-struct")
(declare-function elshogi-mrec/count "ext:elshogi-struct")

(defun recentb-elshogi-normalize-url (url)
  "Strip query parameters from URL."
  (when url
    (cond ((ffap-url-p url)
           (let ((urlobj (url-generic-parse-url url)))
             (setf (url-filename urlobj)
                   (car (url-path-and-query urlobj)))
             (url-recreate-url urlobj)))
          ((file-exists-p url)
           url))))

(defsubst recentb-elshogi-history-item (game)
  (when-let* (url (recentb-elshogi-normalize-url
                   (or (elshogi-game/url game) (elshogi-game/kif game))))
    (list url
          (elshogi-game/title game)
          (elshogi-mrec/count (elshogi-game-latest-move game)))))

(defun recentb-elshogi-history ()
  (when (derived-mode-p 'elshogi-mode)
    (let ((hist
           (cl-delete-if-not (pcase-lambda (`(,kif . _))
                               (or (ffap-url-p kif)
                                   (file-exists-p kif)))
                             recentb-elshogi)))
      (cl-delete-duplicates
       (if-let* (item (recentb-elshogi-history-item elshogi-current-game))
           (cons item hist)
         hist)
       :test (lambda (a b)
               (string= (car a) (car b)))
       :from-end t))))

(provide 'recentb-elshogi)
;;; recentb-elshogi.el ends here
