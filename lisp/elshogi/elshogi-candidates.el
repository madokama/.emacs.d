;;; elshogi-candidates --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'elshogi-core)
(require 'elshogi-directions)
(require 'dash-functional)

(defun elshogi-move-candidates-pawn (index piece)
  (list (elshogi-upward-index index piece)))

(defun elshogi-move--directions (index piece directions)
  (mapcar (lambda (direction)
            (funcall direction index piece))
          directions))

(defun elshogi-move-candidates-knight (index piece)
  (elshogi-move--directions (elshogi-upward-index index piece)
                            piece
                            '(elshogi-upleft-index elshogi-upright-index)))

(defun elshogi-move-candidates-silver (index piece)
  (elshogi-move--directions index piece
                            (cons 'elshogi-upward-index elshogi-directions-x)))

(defun elshogi-move-candidates-gold (index piece)
  (elshogi-move--directions index piece
                            (append elshogi-directions-+
                                    '(elshogi-upleft-index
                                      elshogi-upright-index))))

(defun elshogi-move-candidates-king (index piece)
  (elshogi-move--directions index piece
                            (append elshogi-directions-+ elshogi-directions-x)))

(defun elshogi-move-candidates--ranging (index piece dir)
  (cl-loop for index~ = (funcall dir index piece) then (funcall dir index~ piece)
           while index~
           collect index~ into candidates
           if (elshogi-piece-p (elshogi-piece-at elshogi-current-game index~))
             return candidates
           finally return candidates))

(defun elshogi-move-candidates-lance (index piece)
  (elshogi-move-candidates--ranging index piece #'elshogi-upward-index))

(defun elshogi-move-candidates-rook (index piece)
  (append (cl-mapcan (apply-partially #'elshogi-move-candidates--ranging
                                      index piece)
                     elshogi-directions-+)
          (when (elshogi-piece/promoted piece)
            (elshogi-move--directions index piece elshogi-directions-x))))

(defun elshogi-move-candidates-bishop (index piece)
  (append (cl-mapcan (apply-partially #'elshogi-move-candidates--ranging
                                      index piece)
                     elshogi-directions-x)
          (when (elshogi-piece/promoted piece)
            (elshogi-move--directions index piece elshogi-directions-+))))

(defun elshogi-file-with-pawn-p (file piece)
  (cl-loop for rank below 9
           for piece~ = (elshogi-piece-at elshogi-current-game
                                          (elshogi-calc-index file rank))
           if (and (elshogi-pieces-same-side-p piece piece~)
                   (eq (elshogi-piece/name piece~) 'p)
                   (not (elshogi-piece/promoted piece~)))
             return t))

(defun elshogi-drop-candidates-pawn (piece candidates)
  (seq-remove (lambda (index)
                (elshogi-file-with-pawn-p (elshogi-index-file index) piece))
              (elshogi-drop-candidates-lance piece candidates)))

(defun elshogi-drop-candidates-lance (piece candidates)
  (let ((illegal-rank
         (if (elshogi-piece-black-p piece)
             0
           8)))
    (seq-remove (lambda (index)
                  (= (elshogi-index-rank index) illegal-rank))
                candidates)))

(defun elshogi-drop-candidates-knight (piece candidates)
  (let ((illegal-p
         (if (elshogi-piece-black-p piece)
             (lambda (rank)
               (<= rank 1))
           (lambda (rank)
             (>= rank 7)))))
    (seq-remove (-compose illegal-p #'elshogi-index-rank)
                candidates)))

(defun elshogi-drop-candidates (piece)
  (let ((candidates
         (elshogi-collect-indices (-not
                                   (apply-partially #'elshogi-piece-at
                                                    elshogi-current-game)))))
    (cl-case (elshogi-piece/name piece)
      (p (elshogi-drop-candidates-pawn piece candidates))
      (l (elshogi-drop-candidates-lance piece candidates))
      (n (elshogi-drop-candidates-knight piece candidates))
      (t candidates))))

(defun elshogi-move-candidates--legal (piece candidates)
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
                        (cl-mapcan #'elshogi-move-candidates--raw
                                   (elshogi-indices-on-board
                                    elshogi-current-game
                                    (elshogi-negate-side
                                     (elshogi-piece/side piece)))))
      candidates~)))

(defun elshogi-move-candidates--raw (index)
  (let* ((piece (elshogi-piece-at elshogi-current-game index))
         (name (elshogi-piece/name piece)))
    (funcall (if (and (elshogi-piece/promoted piece)
                      (memq name '(p l n s)))
                 #'elshogi-move-candidates-gold
               (cl-case name
                 (p #'elshogi-move-candidates-pawn)
                 (l #'elshogi-move-candidates-lance)
                 (n #'elshogi-move-candidates-knight)
                 (s #'elshogi-move-candidates-silver)
                 (g #'elshogi-move-candidates-gold)
                 (b #'elshogi-move-candidates-bishop)
                 (r #'elshogi-move-candidates-rook)
                 (k #'elshogi-move-candidates-king)))
             index piece)))

(defun elshogi-candidates-target (index)
  (if (consp index)
      (elshogi-drop-candidates (cdr (assq 'piece index)))
    (elshogi-move-candidates--legal (elshogi-piece-at elshogi-current-game
                                                      index)
                                    (elshogi-move-candidates--raw index))))

(defun elshogi-candidates-origin (file)
  (cl-loop for rank below 9
           for index = (elshogi-calc-index file rank)
           when (and (elshogi-piece-of-side-p
                      (elshogi-piece-at elshogi-current-game index)
                      (elshogi-players-side elshogi-current-game))
                     (elshogi-candidates-target index))
             collect index))

(provide 'elshogi-candidates)
;;; elshogi-candidates.el ends here
