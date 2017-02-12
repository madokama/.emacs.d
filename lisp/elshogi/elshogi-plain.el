;;; elshogi-plain --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'elshogi-game)
(require 'elshogi-display)
(require 'elshogi-highlight)

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

(defun elshogi-plain-border-type (type)
  (assoc-default type elshogi-plain-border-chars #'eq ? ))

(defun elshogi-plain-draw-board (game)
  (let ((horz-bars
         (make-list 9
                    (make-string 2
                                 (elshogi-plain-border-type 'horz-bar))))
        (horz-short
         (make-string 4
                      (elshogi-plain-border-type 'horz-bar)))
        (square-points (elshogi-current-position-on-display))
        (pov (elshogi-game-pov game))
        ;; (watch-p (elshogi-game/watch-p game))
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

    (setf (elshogi-point-of-piece-stand
           game
           (elshogi-negate-side pov)
           ;; (if watch-p
           ;;     'w
           ;;   (elshogi-negate-side (elshogi-players-side)))
           )
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

    (setf (elshogi-point-of-piece-stand game
                                        pov
                                        ;; (if watch-p 'b (elshogi-players-side))
                                        )
          (point))
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
             (let ((index
                    (elshogi-calc-index (funcall map-coord file)
                                        (funcall map-coord rank))))
               (aset square-points index (point))
               ;; NOTE: It takes two spaces to draw a single piece
               (insert "  " (elshogi-plain-border-type 'vert-bar))
               (elshogi-plain-draw-piece game index)))
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
               (t (insert (make-string 6 ? ))))))))
  (mapc (apply-partially #'elshogi-plain-draw-stand game) '(b w))
  (elshogi-plain-highlight-latest game))

(defun elshogi-plain-fontify-piece (piece watch-p)
  (let ((str (elshogi-piece-text piece)))
    (propertize str
                'face
                (let ((side (elshogi-piece/side piece)))
                  (if (or (and watch-p (elshogi-black-p side))
                          (elshogi-players-side-p side))
                      'elshogi-pov-face)))))

(defun elshogi-plain-draw-stand (game side)
  (save-excursion
    (goto-char (elshogi-point-of-piece-stand game side))
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
         (let ((group (cdr (assq p pieces))))
           (list 'display
                 (and group
                      (format "%2d%s"
                              (length group)
                              (elshogi-plain-fontify-piece
                               (car group)
                               (elshogi-game/watch-p game))))
                 'elshogi-index
                 (and group
                      `((piece . ,(car group))
                        (point . ,(point)))))))
        (forward-line)))))

(defun elshogi-plain-highlight-latest (game)
  (when-let* ((mrec (elshogi-game-latest-move game))
              (index (elshogi-mrec/target mrec)))
    (elshogi-highlight-move index)))

(defun elshogi-plain-update-squares (game &rest indices)
  (let ((inhibit-read-only t))
    (cl-loop for index in indices
             do (elshogi-plain-draw-piece game index)
             if (elshogi-piece-at game index)
               do (elshogi-highlight-move index))))

(defun elshogi-plain-draw-piece (game index)
  (let ((piece (elshogi-piece-at game index))
        (point (elshogi-index->point index)))
    (add-text-properties
     point (+ point 2)
     (list 'display
           (and (elshogi-piece-p piece)
                (format "%2s"
                        (elshogi-plain-fontify-piece piece
                                                     (elshogi-game/watch-p game))))
           'elshogi-index index
           'mouse-face
           (and (elshogi-piece-p piece)
                (elshogi-players-side-p
                 (elshogi-piece/side piece))
                'highlight)))))

;;;###autoload
(defun elshogi-plain-install ()
  "Install textual elshogi display handler."
  (interactive)
  (elshogi-display-register
   '(:board elshogi-plain-draw-board
     :square elshogi-plain-draw-piece
     :squares elshogi-plain-update-squares
     :stand elshogi-plain-draw-stand
     ;; :hl-move elshogi-highlight-move
     :hl-sel elshogi-highlight-selected
     :hl-cands elshogi-highlight-candidates)))

(elshogi-plain-install)

(provide 'elshogi-plain)
;;; elshogi-plain.el ends here
