;;; elshogi-csa --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; http://www.computer-shogi.org/protocol/record_v21.html

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'elshogi-core)
(require 'dash)

(declare-function elshogi-sfen->position "elshogi-sfen")

(defvar elshogi-csa-piece-names
  '((p "FU" "TO")
    (l "KY" "NY")
    (n "KE" "NK")
    (s "GI" "NG")
    (g "KI")
    (b "KA" "UM")
    (r "HI" "RY")
    (k "OU")))

(defun elshogi-csa-side (side)
  (if (elshogi-black-p side) "+" "-"))

(defun elshogi-csa-time (time)
  ;$START_TIME:
  (format-time-string "%Y/%m/%d %H:%M:%S" time))

;; TODO generic functions
;; (defun elshogi-csa-player-name (player)
;;   (if (elshogi-player-p player)
;;       (elshogi-player/name player)
;;     (elshogi-engine/name player)))

;; (defun elshogi-csa-player-side (player)
;;   (if (elshogi-player-p player)
;;       (elshogi-player/side player)
;;     (elshogi-engine/side player)))

(defun elshogi-csa-players (game)
  (mapcar (pcase-lambda (`(,name ,side))
            (format "N%s%s" (elshogi-csa-side side) name))
          (let ((black (elshogi-game/black game))
                (white (elshogi-game/white game)))
            `((,(elshogi-player/name black) ,(elshogi-player/side black))
              (,(elshogi-player/name white) ,(elshogi-player/side white))))))

(defun elshogi-csa-piece-name (piece)
  (let ((name
         (cdr (assq (elshogi-piece/name piece) elshogi-csa-piece-names))))
    (if (elshogi-piece/promoted piece)
        (cadr name)
      (car name))))

(defun elshogi-csa-rank (rank)
  (mapconcat (lambda (piece)
               (if (elshogi-piece-p piece)
                   (format "%s%s"
                           (elshogi-csa-side (elshogi-piece/side piece))
                           (elshogi-csa-piece-name piece))
                 " * "))
             rank
             ""))

(defun elshogi-csa-pieces-in-hand (pieces side)
  (when pieces
    (list
     (format "P%s%s"
             (elshogi-csa-side side)
             (mapconcat (lambda (piece)
                          (format "00%s"
                                  (cadr (assq (elshogi-piece/name piece)
                                              elshogi-csa-piece-names))))
                        pieces
                        "")))))

(defun elshogi-csa-position (position)
  (append
   (-map-indexed (lambda (index rank)
                   (format "P%d%s" (1+ index) (elshogi-csa-rank rank)))
                 (seq-partition (elshogi-position/on-board position) 9))
   (elshogi-csa-pieces-in-hand (elshogi-position/on-black-stand position) 'b)
   (elshogi-csa-pieces-in-hand (elshogi-position/on-white-stand position) 'w)))

(defun elshogi-csa-coord (index)
  (format "%s%s"
          (- 9 (elshogi-index-file index))
          (1+ (elshogi-index-rank index))))

(defun elshogi-csa-move (mrec)
  (format "%s%s%s%s"
          (elshogi-csa-side (elshogi-mrec/side mrec))
          (if (elshogi-mrec-drop-p mrec)
              "00"
            (elshogi-csa-coord (elshogi-mrec/origin mrec)))
          (elshogi-csa-coord (elshogi-mrec/target mrec))
          (elshogi-csa-piece-name (elshogi-mrec/piece mrec))))

(defun elshogi-csa-move-time (prev curr)
  (format-time-string "T%S"
                      (time-subtract (elshogi-mrec/time curr)
                                     (elshogi-mrec/time prev))))

(defun elshogi-csa-moves (grec)
  (cl-loop for move = (dlink/next (dlist/head (elshogi-grec/moves grec)))
             then (dlink/next move)
           while move
           collect
           (format "%s\n%s"
                   (elshogi-csa-move (dlink/content move))
                   (elshogi-csa-move-time (dlink/content (dlink/prev move))
                                          (dlink/content move)))))

;; (defun elshogi-csa-moves (grec)
;;   (let ((move
;;          (dlist/head (elshogi-grec/moves grec)))
;;         (moves nil))
;;     (awhile (dlink/next move)
;;       (push (format "%s\n%s"
;;                     (elshogi-csa-move (dlink/content it))
;;                     (elshogi-csa-move-time (dlink/content it)
;;                                            (dlink/content move)))
;;             moves)
;;       (setq move it))
;;     (nreverse moves)))

(defun elshogi-csa-result (game)
  (cl-case (elshogi-game/result game)
    (draw "%HIKIWAKE")
    (t "%TORYO")))

(defun elshogi-csa-game (game)
  (string-join
   (let* ((grec (elshogi-game/record game))
          (position
           (elshogi-sfen->position (elshogi-grec/startpos grec))))
     `("V2.1"
       ,@(elshogi-csa-players game)
       ,@(elshogi-csa-position position)
       ,(elshogi-csa-side (elshogi-position/side position))
       ,@(elshogi-csa-moves grec)
       ,(elshogi-csa-result game)))
   "\n"))

(provide 'elshogi-csa)
;;; elshogi-csa.el ends here
