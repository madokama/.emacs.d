;;; elshogi-plain --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'elshogi-game)
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

(defun elshogi-plain-draw-board (game hl)
  (let ((horz-bars
         (make-list 9
                    (make-string 2
                                 (elshogi-plain-border-type 'horz-bar))))
        (horz-short
         (make-string 4 (elshogi-plain-border-type 'horz-bar)))
        (pov (elshogi-game-pov game))
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
                            'elshogi-index (elshogi-pack-index game index)
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
         (when-let* (group (alist-get p pieces))
           (list 'display
                 (format "%2d%s"
                         (length group)
                         (elshogi-plain-fontify-piece (car group) game))
                 'face (funcall hl (car group))
                 'elshogi-index (elshogi-pack-index game (car group))
                 'mouse-face 'highlight
                 'keymap elshogi-mouse-map)))
        (forward-line)))))

(defun elshogi-plain-draw-stands (game hl)
  (mapc (apply-partially #'elshogi-plain-draw-stand game hl)
        '(b w)))



(defun elshogi-plain-hl-face (type)
  (cl-case type
    ((latest sel) '(:inverse-video t))
    ((prev cands) 'highlight)))

(defun elshogi-plain-install ()
  "Install textual elshogi display handler."
  (interactive)
  (elshogi-display-register
   '(:board elshogi-plain-draw-board
     :stands elshogi-plain-draw-stands
     :hl elshogi-plain-hl-face)))

(elshogi-plain-install)

(provide 'elshogi-plain)
;;; elshogi-plain.el ends here
