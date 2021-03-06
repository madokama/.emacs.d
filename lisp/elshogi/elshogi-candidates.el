;;; elshogi-candidates --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'elshogi-core)
(require 'elshogi-direction)
(require 'dash-functional)

(defun elshogi-candidates-move-pawn (index piece)
  (list (elshogi-direction-upward-index index piece)))

(defun elshogi-candidates-move-directions (index piece directions)
  (mapcar (lambda (direction)
            (funcall direction index piece))
          directions))

(defun elshogi-candidates-move-knight (index piece)
  (elshogi-candidates-move-directions
   (elshogi-direction-upward-index index piece)
   piece
   '(elshogi-direction-upleft-index elshogi-direction-upright-index)))

(defun elshogi-candidates-move-silver (index piece)
  (elshogi-candidates-move-directions
   index piece
   (cons 'elshogi-direction-upward-index elshogi-direction-x)))

(defun elshogi-candidates-move-gold (index piece)
  (elshogi-candidates-move-directions
   index piece
   (append elshogi-direction-+
           '(elshogi-direction-upleft-index elshogi-direction-upright-index))))

(defun elshogi-candidates-move-king (index piece)
  (elshogi-candidates-move-directions
   index piece
   (append elshogi-direction-+ elshogi-direction-x)))

(defun elshogi-candidates-move--ranging (index piece dir)
  (cl-loop for index~ = (funcall dir index piece) then (funcall dir index~ piece)
           while index~
           collect index~ into candidates
           if (elshogi-piece-p (elshogi-piece-at elshogi-current-game index~))
             return candidates
           finally return candidates))

(defun elshogi-candidates-move-lance (index piece)
  (elshogi-candidates-move--ranging index piece #'elshogi-direction-upward-index))

(defun elshogi-candidates-move-rook (index piece)
  (append (cl-mapcan (apply-partially #'elshogi-candidates-move--ranging
                                      index piece)
                     elshogi-direction-+)
          (when (elshogi-piece/promoted piece)
            (elshogi-candidates-move-directions index piece elshogi-direction-x))))

(defun elshogi-candidates-move-bishop (index piece)
  (append (cl-mapcan (apply-partially #'elshogi-candidates-move--ranging
                                      index piece)
                     elshogi-direction-x)
          (when (elshogi-piece/promoted piece)
            (elshogi-candidates-move-directions index piece elshogi-direction-+))))

(defun elshogi-candidates-file-with-pawn-p (file piece)
  (cl-loop for rank below 9
           for piece~ = (elshogi-piece-at elshogi-current-game
                                          (elshogi-calc-index file rank))
           if (and (elshogi-pieces-same-side-p piece piece~)
                   (eq (elshogi-piece/name piece~) 'p)
                   (not (elshogi-piece/promoted piece~)))
             return t))

(defun elshogi-candidates-drop-pawn (piece candidates)
  (seq-remove (lambda (index)
                (elshogi-candidates-file-with-pawn-p (elshogi-index-file index)
                                                     piece))
              (elshogi-candidates-drop-lance piece candidates)))

(defun elshogi-candidates-drop-lance (piece candidates)
  (let ((illegal-rank
         (if (elshogi-piece-black-p piece)
             0
           8)))
    (seq-remove (lambda (index)
                  (= (elshogi-index-rank index) illegal-rank))
                candidates)))

(defun elshogi-candidates-drop-knight (piece candidates)
  (let ((illegal-p
         (if (elshogi-piece-black-p piece)
             (lambda (rank)
               (<= rank 1))
           (lambda (rank)
             (>= rank 7)))))
    (seq-remove (-compose illegal-p #'elshogi-index-rank)
                candidates)))

(defun elshogi-candidates-drop (piece)
  (let ((candidates
         (elshogi-collect-indices (-not
                                   (apply-partially #'elshogi-piece-at
                                                    elshogi-current-game)))))
    (cl-case (elshogi-piece/name piece)
      (p (elshogi-candidates-drop-pawn piece candidates))
      (l (elshogi-candidates-drop-lance piece candidates))
      (n (elshogi-candidates-drop-knight piece candidates))
      (t candidates))))

(defun elshogi-candidates-move--legal (piece candidates)
  (let ((candidates~
         (seq-remove (lambda (index)
                       (or (not index)
                           (elshogi-pieces-same-side-p piece
                                                       (elshogi-piece-at
                                                        elshogi-current-game
                                                        index))))
                     candidates)))
    (if (eq (elshogi-piece/name piece) 'k)
        (seq-difference candidates~
                        (cl-mapcan #'elshogi-candidates-move--raw
                                   (elshogi-indices-on-board
                                    elshogi-current-game
                                    (elshogi-negate-side
                                     (elshogi-piece/side piece)))))
      candidates~)))

(defun elshogi-candidates-move--raw (index)
  (let* ((piece (elshogi-piece-at elshogi-current-game index))
         (name (elshogi-piece/name piece)))
    (funcall (if (and (elshogi-piece/promoted piece)
                      (memq name '(p l n s)))
                 #'elshogi-candidates-move-gold
               (cl-case name
                 (p #'elshogi-candidates-move-pawn)
                 (l #'elshogi-candidates-move-lance)
                 (n #'elshogi-candidates-move-knight)
                 (s #'elshogi-candidates-move-silver)
                 (g #'elshogi-candidates-move-gold)
                 (b #'elshogi-candidates-move-bishop)
                 (r #'elshogi-candidates-move-rook)
                 (k #'elshogi-candidates-move-king)))
             index piece)))

(defun elshogi-candidates--raw-target (index)
  (if (elshogi-piece-p index)
      (elshogi-candidates-drop index)
    (elshogi-candidates-move--legal
     (elshogi-piece-at elshogi-current-game index)
     (elshogi-candidates-move--raw index))))

(defun elshogi-candidates-target (plst)
  (mapcar (apply-partially #'elshogi-pack-index (elshogi-pack:game plst))
          (elshogi-candidates--raw-target (elshogi-pack-unpack plst))))

(defun elshogi-candidates-origin (game file)
  (cl-loop for rank below 9
           for index = (elshogi-calc-index file rank)
           for plst = (elshogi-pack-index game index)
           with side = (elshogi-players-side game)
           when (and (elshogi-piece-of-side-p (elshogi-piece-at game index)
                                              side)
                     (elshogi-candidates-target plst))
             collect plst))

(provide 'elshogi-candidates)
;;; elshogi-candidates.el ends here
