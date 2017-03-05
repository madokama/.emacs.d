;;; elshogi-plain --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elshogi-game)
(require 'elshogi-candidates)
(require 'elshogi-display)

(defface elshogi-pov-face
  '((((class color) (background dark)) (:foreground "Cyan"))
    (t (:background "White" :foreground "Black")))
  ""
  :group 'elshogi)

(defvar elshogi-plain-border-chars
  '((top-left  . ?╭)
    (mid-left  . ?├)
    (bot-left  . ?╰)
    (horz-bar  . ?─)
    (top-mid   . ?┬)
    (bot-mid   . ?┴)
    (vert-bar  . ?│)
    (intersect . ?┼)
    (top-right . ?┐)
    (mid-right . ?┤)
    (bot-right . ?╯)))

(defmacro elshogi-plain--sandwitch (center side)
  (declare (indent 0))
  `(progn ,side ,center ,side))

(defmacro elshogi-plain--stand-point (game side)
  `(let ((display (elshogi-game/display ,game)))
     (if (eq ,side elshogi-black)
         (elshogi-display/piece-stand/b display)
       (elshogi-display/piece-stand/w display))))

(defun elshogi-plain-border-type (type)
  (assoc-default type elshogi-plain-border-chars #'eq ? ))

(defun elshogi-plain-draw-board (game &optional hl)
  (let ((horz-bars
         (make-list 9
                    (make-string 2
                                 (elshogi-plain-border-type 'horz-bar))))
        (horz-short
         (make-string 4 (elshogi-plain-border-type 'horz-bar)))
        (pov (elshogi-game-pov game))
        (hl (or hl #'ignore))
        (inhibit-read-only t))
    (erase-buffer)
    (insert ?\n)

    (elshogi-plain--sandwitch
     (insert "    "
             (mapconcat #'string
                        (let ((l (string-to-list "987654321")))
                          (if (not (elshogi-black-p pov))
                              (nreverse l)
                            l))
                        (make-string 2 ? ))
             "  ")
     (insert (elshogi-plain-border-type 'top-left)
             horz-short
             (elshogi-plain-border-type 'top-right)))
    (insert ?\n
            (elshogi-plain-border-type 'vert-bar))

    (setf (elshogi-plain--stand-point game (elshogi-negate-side pov))
          (point))

    (insert "    "
            (elshogi-plain-border-type 'vert-bar)
            "  "
            (elshogi-plain-border-type 'top-left)
            (string-join horz-bars
                         (string (elshogi-plain-border-type 'top-mid)))
            (elshogi-plain-border-type 'top-right)
            " "
            (elshogi-plain-border-type 'vert-bar))

    (setf (elshogi-plain--stand-point game pov) (point))
    (insert "    " (elshogi-plain-border-type 'vert-bar))

    (let ((map-coord (elshogi-pov-coord pov)))
      (dotimes (rank 9)
        (insert ?\n)
        (elshogi-plain--sandwitch
         (progn
           (insert " "
                   (elshogi-rank->char (funcall map-coord rank))
                   (elshogi-plain-border-type 'vert-bar))
           (dotimes (file 9)
             (let* ((index
                     (elshogi-calc-index (funcall map-coord file)
                                         (funcall map-coord rank)))
                    (piece (elshogi-piece-at game index)))
               ;; NOTE: It takes two spaces to draw a single piece
               (insert
                (propertize "  "
                            'display
                            (when piece
                              (format "%2s"
                                      (elshogi-plain-fontify-piece piece game)))
                            'face (funcall hl index)
                            'elshogi-index (elshogi-piece-index game index)
                            'mouse-face 'highlight
                            'keymap elshogi-mouse-map)
                (elshogi-plain-border-type 'vert-bar))))
           (insert ? ))
         (cond ((< rank 7)
                (insert (elshogi-plain-border-type 'vert-bar)
                        "    "
                        (elshogi-plain-border-type 'vert-bar)))
               ((= rank 7)
                (insert (elshogi-plain-border-type 'bot-left)
                        horz-short
                        (elshogi-plain-border-type 'bot-right)))
               (t
                (insert (make-string 6 ? )))))
        (insert ?\n)

        (elshogi-plain--sandwitch
         (insert "  "
                 (if (< rank 8)
                     (elshogi-plain-border-type 'mid-left)
                   (elshogi-plain-border-type 'bot-left))
                 (string-join horz-bars
                              (string (elshogi-plain-border-type
                                       (if (< rank 8) 'intersect 'bot-mid))))
                 (if (< rank 8)
                     (elshogi-plain-border-type 'mid-right)
                   (elshogi-plain-border-type 'bot-right))
                 " ")
         (cond ((< rank 7)
                (insert (elshogi-plain-border-type 'vert-bar)
                        "    "
                        (elshogi-plain-border-type 'vert-bar)))
               (t (insert (make-string 6 ? )))))))))

(defun elshogi-plain-fontify-piece (piece game)
  (propertize (elshogi-piece-text piece)
              'face
              (when (eq (elshogi-piece/side piece)
                        (elshogi-game-pov game))
                'elshogi-pov-face)))

(defun elshogi-plain-draw-stand (game hl side)
  (save-excursion
    (goto-char (elshogi-plain--stand-point game side))
    (let ((pieces
           (seq-group-by #'elshogi-piece/name
                         (elshogi-pieces-on-stand game side)))
          (column (current-column))
          (inhibit-read-only t))
      (dolist (p (mapcar #'car (cdr elshogi-piece-values)))
        (forward-line)
        (goto-char (+ (point) column))
        (add-text-properties
         (point) (+ (point) 3)
         (when-let* (group (cdr (assq p pieces)))
           (list 'display
                 (format "%2d%s"
                         (length group)
                         (elshogi-plain-fontify-piece (car group) game))
                 'face (funcall hl (car group))
                 'elshogi-index (elshogi-piece-index game (car group))
                 'mouse-face 'highlight
                 'keymap elshogi-mouse-map)))
        (forward-line)))))

(defun elshogi-plain-draw-stands (game &optional hl)
  (mapc (apply-partially #'elshogi-plain-draw-stand game (or hl #'ignore))
        '(b w)))

;;; Draw interface

(defun elshogi-plain-face (type)
  (cl-case type
    ((latest sel) '(:inverse-video t))
    (cands 'highlight)))

(defun elshogi-plain-hl-latest (game &optional indices)
  (let ((prev (car indices))
        (latest (or (cadr indices) (car indices)
                    (elshogi-mrec/target (elshogi-game-latest-move game))))
        (face-prev (elshogi-plain-face 'cands))
        (face-latest (elshogi-plain-face 'latest)))
    (elshogi-plain-draw-board game
                              (lambda (index)
                                (cond ((eq index latest) face-latest)
                                      ;; using `eq' since `prev' may be nil
                                      ((eq index prev) face-prev))))
    (elshogi-plain-draw-stands game)))

(defun elshogi-plain-hl-sel (plst)
  (let* ((game (elshogi-plst:game plst))
         (sel (or (elshogi-plst:index plst) (elshogi-plst:piece plst)))
         (cands (elshogi-candidates--raw-target sel))
         (face-sel (elshogi-plain-face 'sel))
         (face-cands (elshogi-plain-face 'cands)))
    (elshogi-plain-draw-board game
                              (if (elshogi-piece-p sel)
                                  (lambda (index)
                                    (when (memq index cands)
                                      face-cands))
                                (lambda (index)
                                  (cond ((= index sel) face-sel)
                                        ((memq index cands) face-cands)))))
    (elshogi-plain-draw-stands game
                               (when (elshogi-piece-p sel)
                                 (lambda (piece)
                                   (when (elshogi-piece= piece sel)
                                     face-sel))))))

(defun elshogi-plain-hl-cands (cands)
  (let ((game (elshogi-plst:game (car cands)))
        (cands (mapcar #'elshogi-plst:index cands))
        (face-cands (elshogi-plain-face 'cands)))
    (elshogi-plain-draw-board game
                              (lambda (index)
                                (when (memq index cands)
                                  face-cands)))
    (elshogi-plain-draw-stands game)))

(defun elshogi-plain-install ()
  "Install textual elshogi display handler."
  (interactive)
  (elshogi-display-register
   '(:board elshogi-plain-hl-latest
     :squares elshogi-plain-hl-latest
     :hl-sel elshogi-plain-hl-sel
     :hl-cands elshogi-plain-hl-cands)))

(elshogi-plain-install)

(provide 'elshogi-plain)
;;; elshogi-plain.el ends here
