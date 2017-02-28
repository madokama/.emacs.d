;;; elshogi-images --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'elshogi-game)
(require 'elshogi-display)
(require 'url-cache)

(defvar elshogi-images-name-alist
  '((p "fu" "to")
    (l "kyo")
    (n "kei")
    (s "gin")
    (g "kin")
    (b "kaku" "uma")
    (r "hi" "ryu")
    (k "ou")))

(defvar elshogi-images-directory
  (expand-file-name "images/"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar elshogi-images-coord-alphabet-size "18x48")
(defvar elshogi-images-coord-number-size "43x30")
(defvar elshogi-images-coord-char-font nil)

(defsubst elshogi-images-file (name)
  (expand-file-name (format "%s.png" name) elshogi-images-directory))

(defsubst elshogi-images-char-file (char)
  (elshogi-images-file (string char)))

(defun elshogi-images-coord-char-font ()
  "Compute font name acceptable to convert."
  (or elshogi-images-coord-char-font
      (when (executable-find "fc-match")
        (setq elshogi-images-coord-char-font
              (with-temp-buffer
                (call-process "fc-match" nil t nil
                              "-f" "%{fullname}"
                              (face-attribute 'default :family))
                (subst-char-in-string ?  ?- (buffer-string) t))))))

(defun elshogi-images-generate-coord-char (char)
  (cl-flet ((generate (size)
              (apply #'call-process "convert"
                     nil nil nil
                     (list "-background" "transparent"
                           "-fill" (face-attribute 'default :foreground)
                           "-font" (or (elshogi-images-coord-char-font) "Courier")
                           "-size" size
                           "-gravity" "center"
                           (format "label:%c" char)
                           (elshogi-images-char-file char)))))
    (cond ((<= ?1 char ?9)
           (generate elshogi-images-coord-number-size))
          ((<= ?a char ?i)
           (generate elshogi-images-coord-alphabet-size)))))

(defun elshogi-images-prepare-coord-chars ()
  (when (executable-find "convert")
    (cl-flet ((test (c)
                (file-exists-p (elshogi-images-char-file c))))
      (cl-loop for c from ?a to ?i
               for n from ?1 to ?9
               unless (test c)
                 do (elshogi-images-generate-coord-char c)
               end
               unless (test n)
                 do (elshogi-images-generate-coord-char n)))))

(defun elshogi-images-piece (piece pov &optional relief-p hl-p)
  (let ((name
         (cdr (assq (elshogi-piece/name piece) elshogi-images-name-alist))))
    (apply #'create-image
           (elshogi-images-file
            (concat (if (eq pov (elshogi-piece/side piece)) "S" "G")
                    (if (elshogi-piece/promoted piece)
                        (if (memq (elshogi-piece/name piece) '(p b r))
                            (cadr name)
                          (concat "n" (car name)))
                      (car name))))
           nil nil :relief (if relief-p 1 0)
           (when hl-p `(:background ,(face-attribute 'default :foreground))))))

(defun elshogi-images-empty-square (width height &optional relief-p)
  ;; From chess-images.el at https://github.com/jwiegley/emacs-chess.git
  (apply #'create-image
         (with-temp-buffer
           (insert "/* XPM */\n")
           (insert "static char *chessdotel[] = {\n")
           (insert "/* columns rows colors chars-per-pixel */\n")
           (insert (format "\"%d %d 2 1\",\n" width height))
           (insert "\"  c none s void\",\n")
           (insert "\". c none s background\",\n")
           (insert "/* pixels */\n")
           (dotimes (_ height)
             (insert ?\" (make-string width ?.) ?\" ?, ?\n))
           (delete-char -2)
           (insert "\n};\n")
           (buffer-string))
         'xpm t (if relief-p '(:relief 1) '(:margin 1))))

(defun elshogi-images-draw-board (game)
  (let ((pov (elshogi-game-pov game))
        (empty1
         (apply #'elshogi-images-empty-square
                (mapcar #'string-to-number
                        (split-string elshogi-images-coord-alphabet-size "x"))))
        (inhibit-read-only t))
    (erase-buffer)
    (when (elshogi-black-p pov)
      (insert-image empty1)
      (cl-loop for file from ?9 downto ?1
               do (insert-image
                   (create-image (elshogi-images-char-file file)
                                 nil nil :margin 1)))
      (insert ?\n))
    (let ((map-coord (elshogi-pov-coord pov))
          (latest (elshogi-mrec/target (elshogi-game-latest-move game)))
          (empty (elshogi-images-empty-square 43 48 t)))
      (dotimes (rank 9)
        (insert-image
         (create-image
          (elshogi-images-char-file (elshogi-rank->char (funcall map-coord rank)))
          nil nil :margin 1))
        (dotimes (file 9)
          (let* ((index
                  (elshogi-calc-index (funcall map-coord file)
                                      (funcall map-coord rank)))
                 (piece (elshogi-piece-at game index)))
            (insert
             (apply #'propertize " "
                    'display
                    (if piece
                        (elshogi-images-piece piece pov t
                                              (and latest (= index latest)))
                      (cons 'image (cdr empty)))
                    'elshogi-index (elshogi-piece-index game index)
                    ;; Hackish way to support mouse even when
                    ;; `disable-mouse-mode' is enabled.
                    (list 'mouse-face 'highlight
                          'keymap elshogi-mouse-map)))))
        (insert ?\n)))
    (unless (elshogi-black-p pov)
      (insert-image empty1)
      (cl-loop for file from ?1 upto ?9
               do (insert-image
                   (create-image (elshogi-images-char-file file)
                                 nil nil :margin 1)))))
  (elshogi-images-draw-stands game))

(defun elshogi-images-insert-cache (url buf)
  (let ((cache (url-cache-create-filename url)))
    (when (file-readable-p cache)
      (condition-case nil
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (insert-image (create-image cache))
              (insert ?\n))
            t)
        (error (delete-file cache)
               nil)))))

(defun elshogi-images-draw-player (player)
  (when-let* (url (elshogi-player/image player))
    (unless (elshogi-images-insert-cache url (current-buffer))
      (url-retrieve url
                    (lambda (status stand-buf)
                      (let ((url-buf (current-buffer)))
                        (unless (plist-get status :error)
                          (elshogi-images-insert-cache url stand-buf))
                        (kill-buffer url-buf)))
                    (list (current-buffer))))))

(defun elshogi-images-draw-stand (game player)
  (with-current-buffer (elshogi-images-stand-buffer game player)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (elshogi-images-draw-player player)
      (let* ((side (elshogi-player/side player))
             (side-p (elshogi-players-side-p game side))
             (pieces
              (seq-sort-by (pcase-lambda (`(,name . _))
                             (cdr (assq name elshogi-piece-values)))
                           (if (eq (elshogi-game-pov game) side) #'> #'<)
                           (seq-group-by #'elshogi-piece/name
                                         (elshogi-pieces-on-stand game side)))))
        (cl-loop for (_ . pieces) in pieces
                 do
                    (goto-char (point-max))
                    (insert
                     (apply #'propertize " "
                            'display
                            (elshogi-images-piece (car pieces)
                                                  (elshogi-game-pov game))
                            'elshogi-index (elshogi-piece-index game (car pieces))
                            (when side-p
                              (list 'mouse-face 'highlight
                                    'keymap elshogi-mouse-map))))
                    (insert (format "x%d\n" (length pieces)))))
      (current-buffer))))

(defun elshogi-images-stand-buffer (game player)
  (let ((bufname
         (format " *elshogi:%s@%s*"
                 (elshogi-player/name player)
                 (elshogi-game/title game))))
    (or (get-buffer bufname)
        (with-current-buffer (generate-new-buffer bufname)
          (setq mode-line-format
                (list (format " %c"
                              (if (elshogi-black-p (elshogi-player/side player))
                                  #x25b2 #x25b3))
                      (propertize (elshogi-player/name player)
                                  'face 'mode-line-buffer-id)))
          (current-buffer)))))

(defun elshogi-images-draw-stands (game)
  (let ((pov (elshogi-game-pov game))
        (params '((window-width . 16))))
    (display-buffer (elshogi-images-draw-stand game (elshogi-game/black game))
                    `(display-buffer-in-side-window
                      ,@params
                      (side . ,(if (elshogi-black-p pov) 'right 'left))))
    (display-buffer (elshogi-images-draw-stand game (elshogi-game/white game))
                    `(display-buffer-in-side-window
                      ,@params
                      (side . ,(if (elshogi-black-p pov) 'left 'right))))))

(defun elshogi-images-install ()
  (elshogi-images-prepare-coord-chars)
  (elshogi-display-register
   `(:board elshogi-images-draw-board
            :squares ,(lambda (game &rest _) (elshogi-images-draw-board game))
            :stand ignore
            ;; TODO :hl-sel :hl-cands
            )))

(elshogi-images-install)

(provide 'elshogi-images)
;;; elshogi-images.el ends here
