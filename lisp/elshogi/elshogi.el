;;; elshogi --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'dash)
(require 'elshogi-game)
(require 'elshogi-move)
(require 'elshogi-candidates)
(require 'elshogi-replay)
(require 'elshogi-display)
(require 'elshogi-mouse)
(require 'elshogi-sfen)
(require 'elshogi-csa)

;;;

(defgroup elshogi nil
  "A Shogi playing program for Emacs."
  :group 'games)

(defvar-local elshogi-last-selected nil)
(defvar-local elshogi-last-selected-file nil)

;; Allow 角厨 vs 飛車厨
;; See: http://www.nicovideo.jp/watch/sm25567679
(defvar elshogi-allow-double-rook nil)
(defvaralias 'elshogi-allow-double-bishop 'elshogi-allow-double-rook)

(defvar elshogi-default-engine nil)
(defvar elshogi-engines nil)
(defvar elshogi-engine-default-conf
  '((USI_Ponder . nil)
    (USI_Hash . 256)
    (byoyomi . 5)))

(defvar-local elshogi-engine-conf nil)
(defvar-local elshogi-engine-pondering nil)

(defvar elshogi-debug nil)

;;;

;; (defun elshogi-check-piece-num (group limit)
;;   (when (> (length (cdr group)) limit)
;;     (error "The number of %s must not be greater than %d"
;;            (car group) limit)))

;; (defun elshogi-assert-pieces (pieces)
;;   (dolist (group (seq-group-by #'elshogi-piece/name
;;                                (seq-filter #'elshogi-piece-p pieces)))
;;     (let ((type (car group)))
;;       (cond ((eq type 'p)
;;              (elshogi-check-piece-num group 9))
;;             ((memq type '(l n s g))
;;              (elshogi-check-piece-num group 2))
;;             ((memq type '(r b))
;;              (elshogi-check-piece-num group
;;                                       (if elshogi-allow-double-rook 2 1)))
;;             ((eq type 'k)
;;              (elshogi-check-piece-num group 1)))))
;;   t)

;;; Engine Interface

(defun elshogi-engine-active ()
  (when-let ((process
              (and elshogi-game-engine
                   (elshogi-player/engine elshogi-game-engine))))
    (and (eq (process-status process) 'run)
         process)))

(defun elshogi-engine-filter (_process string)
  (mapc #'elshogi-usi-parse-command
        (split-string (string-trim-right string) "\n")))

(defun elshogi-engine-sentinel (_process event)
  (message "USI [exit]: %S" event))

(defun elshogi-engine-setup (conf side)
  (let* ((path (alist-get 'path conf))
         (default-directory (file-name-directory path)))
    (elshogi-make-player :name (file-name-base path)
                         :engine
                         (make-process :name "USI Engine"
                                       :command (list path)
                                       :noquery t
                                       :filter #'elshogi-engine-filter
                                       :sentinel #'elshogi-engine-sentinel)
                         :side side)))

(defun elshogi-usi-parse-option (option type default)
  (message "USI:< %S" (list option type default))
  (elshogi-usi-send-command
   (format "setoption name %s value %s"
           option
           (or (alist-get (intern option) elshogi-engine-conf)
               (if (string-match "^\\(\\sw+\\) " default)
                   (match-string 1 default)
                   default)))))

(defun elshogi-usi-parse-info (info)
  info)

;; Rough USI Protocal specification:
;; http://www.geocities.jp/shogidokoro/usi.html
(defun elshogi-usi-parse-command (command)
  (when elshogi-debug
    (message "USI:< %S" command))
  (cond ((string-match "^id name \\(.+\\)$" command)
         (let ((name (match-string 1 command)))
           ;; (setf (elshogi-engine/name elshogi-game-engine)
           ;;       name)
           (setq mode-line-process name)))
        ((string-match
          "^option name \\(\\sw+\\) type \\(\\sw+\\) default \\(.+\\)"
          command)
         (elshogi-usi-parse-option (match-string 1 command)
                                   (match-string 2 command)
                                   (match-string 3 command)))
        ((string= "usiok" command)
         (elshogi-usi-send-command
          (format "setoption name USI_Ponder value %s"
                  (if (alist-get 'USI_Ponder elshogi-engine-conf)
                      "true" "false")))
         (when-let ((hash (alist-get 'USI_Hash elshogi-engine-conf)))
           (elshogi-usi-send-command
            (format "setoption name USI_Hash value %s" hash)))
         (elshogi-usi-send-command "isready"))
        ((string= "readyok" command)
         (elshogi-usi-start))
        ((string-match "^bestmove \\(.+\\)$" command)
         (let ((move (match-string 1 command)))
           (cond (elshogi-engine-pondering
                  ;; Player didn't choose the move which engine expected
                  (setq elshogi-engine-pondering nil)
                  (elshogi-usi-move))
                 ((string= "resign" move)
                  (elshogi-game-set-result
                   (elshogi-negate-side
                    (elshogi-current-side elshogi-current-game)))
                  (elshogi-game-end))
                 ((string= "win" move)
                  (elshogi-game-set-result
                   (elshogi-current-side elshogi-current-game))
                  (elshogi-game-end))
                 ((string-match "^\\(.+?\\) ponder \\(.+\\)$" move)
                  (elshogi-usi-ponder (match-string 1 move)
                                      (match-string 2 move)))
                 (t
                  (elshogi-usi-interpret-move move)))))
        ((string-match "^illegal move \\(.+\\)$" command)
         (elshogi-game-set-result (elshogi-current-side elshogi-current-game))
         (elshogi-game-end))
        ((string-match "^info \\(.+\\)$" command)
         (elshogi-usi-parse-info (match-string 1 command)))
        ((not elshogi-debug)
         (message "USI:< %S" command))))

(defun elshogi-usi-start ()
  (setf (elshogi-mrec/time
         (elshogi-dlist-head (elshogi-game-moves elshogi-current-game)))
        (current-time))
  (elshogi-usi-send-command "usinewgame")
  (unless (elshogi-players-turn-p elshogi-current-game)
    (elshogi-usi-move)))

(defun elshogi-usi-ready-p ()
  (and (elshogi-engine-active)
       (elshogi-mrec/time
        (elshogi-dlist-head (elshogi-game-moves elshogi-current-game)))
       (elshogi-players-turn-p elshogi-current-game)))

(defun elshogi-usi-interpret-move (move)
  (cond ((string-match "^\\([1-9][a-i]\\)\\([1-9][a-i]\\)\\(\\+\\)?$" move)
         (elshogi-move-piece elshogi-current-game
                             (elshogi-coord->index (match-string 1 move))
                             (elshogi-coord->index (match-string 2 move))
                             (match-string 3 move)))
        ((string-match "^\\([[:upper:]]\\)\\*\\([1-9][a-i]\\)$" move)
         (elshogi-drop-piece elshogi-current-game
                             (elshogi-coord->index (match-string 2 move))
                             (elshogi-pick-piece-on-stand
                              (intern (downcase (match-string 1 move))))))))

(defun elshogi-usi-send-command (command)
  ;;(message "USI:> %S" command)
  (when-let ((engine (elshogi-engine-active)))
    (process-send-string engine (concat command "\n"))))

(defun elshogi-usi-ponder (move ponder)
  (setq elshogi-engine-pondering ponder)
  (elshogi-usi-interpret-move move)
  (elshogi-usi-move))

(defun elshogi-usi-move ()
  (let ((grec (elshogi-game/record elshogi-current-game))
        (ponder elshogi-engine-pondering))
    (elshogi-usi-send-command
     (format "position %s%s"
             (elshogi-grec/startpos grec)
             ;; NOTE: Car of dlist-elements is always a dummy move
             (let ((moves (cdr (dlist-elements (elshogi-grec/moves grec)))))
               (if moves
                   (format " moves %s%s"
                           (mapconcat #'elshogi-mrec->sfen moves " ")
                           (if ponder
                               (format " %s" ponder)
                             ""))
                 ""))))
    (elshogi-usi-send-command
     (format "go %s"
             (if ponder
                 "ponder"
               (format "btime 0 wtime 0 byoyomi %d"
                       (* (alist-get 'byoyomi elshogi-engine-conf 5)
                          1000)))))))

;;; Game

(defun elshogi-game-setup (conf black-p &optional sfen)
  (let* ((player
          (elshogi-make-player :name user-login-name
                               :side (if black-p elshogi-black elshogi-white)))
         (engine (elshogi-engine-setup conf (if black-p elshogi-white elshogi-black)))
         (game
          (elshogi-game-initialize
           :position (if sfen
                         (elshogi-sfen->position sfen)
                       (elshogi-new-position))
           :black (if black-p player engine)
           :white (if black-p engine player)
           :record (elshogi-make-grec :startpos (or sfen 'startpos))
           :display (elshogi-make-display :pov (if black-p elshogi-black elshogi-white)))))
    (with-current-buffer (elshogi-display-buffer game)
      (setq mode-line-format
            (list " " (propertize (elshogi-game/title game)
                                  'face 'mode-line-buffer-id)))
      (setq elshogi-current-game game
            elshogi-game-engine engine
            elshogi-engine-conf conf)
      (elshogi-display-board elshogi-current-game)
      (elshogi-usi-send-command "usi"))))

(defun elshogi-game-start (conf)
  (elshogi-game-setup conf
                      (or (alist-get 'white-p conf)
                          (zerop (random 2)))))

(defun elshogi-game-resume ()
  "Resume the game."
  (interactive)
  (elshogi-game-setup elshogi-engine-conf
                      (elshogi-players-side-p elshogi-current-game elshogi-black)
                      (elshogi-position->sfen
                       (elshogi-current-position elshogi-current-game))))

(defun elshogi-game-end ()
  (let ((result (elshogi-game/result elshogi-current-game)))
    (elshogi-usi-send-command
     (format "gameover %s" (elshogi-game-usi-result result)))
    (elshogi-usi-send-command "quit")
    (delete-process (elshogi-player/engine elshogi-game-engine))
    (message (if (elshogi-game-draw-p result)
                 "Draw"
               (format "%s wins" result)))))

(defun elshogi-game-export ()
  "Export the game to the clipboard as CSA format."
  (interactive)
  (w32-set-clipboard-data (elshogi-csa-game elshogi-current-game)))

(defun elshogi-game-set-result (result)
  (setf (elshogi-game/result elshogi-current-game) result))

(defun elshogi-game-usi-result (result)
  (cond ((elshogi-game-draw-p result) "draw")
        ((elshogi-players-side-p elshogi-current-game result) "lose")
        (t "win")))

(defun elshogi-game-draw-p (result)
  ;; `g' for gray
  (eq result 'g))

(defun elshogi-game-resign ()
  "Resign the game."
  (interactive)
  (elshogi-game-set-result
   (elshogi-negate-side (elshogi-current-side elshogi-current-game)))
  (elshogi-game-end))

(defun elshogi-game-turn (game mrec)
  (catch 'ponder
    (when (elshogi-players-turn-p game)
      (when elshogi-engine-pondering
        (if (not (elshogi-move-equals-sfen mrec elshogi-engine-pondering))
            (elshogi-usi-send-command "stop")
          (elshogi-usi-send-command "ponderhit")
          (setq elshogi-engine-pondering nil))
        (throw 'ponder nil))
      (elshogi-usi-move)))
  (elshogi-toggle-side game))

(defun elshogi-game-replay-prev ()
  "Replay the previous position."
  (interactive)
  (unless (elshogi-engine-active)
    (elshogi-display-board (elshogi-replay-prev-internal elshogi-current-game))))

(defun elshogi-game-replay-next ()
  "Replay the next position."
  (interactive)
  (unless (elshogi-engine-active)
    (elshogi-display-board (elshogi-replay-next-internal elshogi-current-game))))

(defun elshogi-game-rewind ()
  "Go to the initial position."
  (interactive)
  (unless (elshogi-engine-active)
    (elshogi-display-board (elshogi-replay-rewind elshogi-current-game))))

(defvar elshogi-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [mouse-1] #'elshogi-mouse-select-square)
    (define-key map [?q] #'elshogi-quit)
    (define-key map [remap backward-char] #'elshogi-game-replay-prev)
    (define-key map [remap forward-char] #'elshogi-game-replay-next)
    (define-key map [remap move-beginning-of-line] #'elshogi-game-rewind)
    (define-key map [?N] #'elshogi)
    (define-key map [?R] #'elshogi-game-resign)
    (define-key map [?G] #'elshogi-game-resume)
    ;; (define-key map "\C-c" #'elshogi-game-export)
    (cl-loop for i below 9
             do (define-key map (vector (+ ?1 i)) #'elshogi-key-move-piece))
    (define-key map [?*] #'elshogi-key-drop-piece)
    (define-key map [?x] #'elshogi-key-capture-back)
    map))

(define-derived-mode elshogi-mode special-mode "ElShogi"
  "Play/Watch Shogi games."
  (buffer-disable-undo)
  (setq cursor-type nil
        truncate-lines t))

;;;

(defun elshogi-piece-promotable-p (piece)
  (not (memq (elshogi-piece/name piece) '(k g))))

(defun elshogi-may-promote-p (game origin target)
  (let ((piece (elshogi-piece-at game origin)))
    (and (elshogi-piece-promotable-p piece)
         (not (elshogi-piece/promoted piece))
         (seq-some (if (elshogi-piece-black-p piece)
                       (lambda (rank) (< rank 3))
                     (lambda (rank) (> rank 5)))
                   (mapcar #'elshogi-index-rank (list origin target)))
         (y-or-n-p "Promote? "))))

(defun elshogi-pick-piece-on-stand (name)
  (seq-find (lambda (piece)
              (eq (elshogi-piece/name piece) name))
            (elshogi-pieces-on-stand elshogi-current-game
                                     (elshogi-current-side elshogi-current-game))))

(defun elshogi-move-piece (game origin target promoted)
  (let ((mrec
         (elshogi-move-make-move game origin target promoted)))
    (elshogi-display-update-squares game origin target)
    (elshogi-game-turn game mrec)))

(defun elshogi-drop-piece (game index piece)
  (let ((mrec (elshogi-move-make-drop game index piece)))
    (elshogi-display-update-squares game index)
    (elshogi-game-turn game mrec)))

;;; User Interface

;;;###autoload
(defun elshogi (conf)
  "Start playing shogi.
Specify the engine settings with CONF."
  (interactive (list
                (alist-get
                 (or elshogi-default-engine
                     (intern
                      (completing-read "Choose engine: "
                                       (mapcar #'car elshogi-engines))))
                 elshogi-engines)))
  (unless conf
    (user-error "Engine not specified.  Exiting"))
  (elshogi-game-start (append conf elshogi-engine-default-conf)))

(defun elshogi-clear-last-selected (game)
  (elshogi-display-board game)
  (setq elshogi-last-selected nil))

(defun elshogi-set-last-selected (plst)
  (elshogi-display-highlight-selected plst)
  (setq elshogi-last-selected plst))

(defun elshogi-selectable-origin-p (plst)
  (when-let* ((game (elshogi-pack:game plst))
              (piece (or (elshogi-pack:piece plst)
                         (elshogi-piece-at game (elshogi-pack:index plst)))))
    (eq (elshogi-piece/side piece)
        (elshogi-current-side game))))

(defun elshogi-selectable-target-p (plst)
  (not (elshogi-selectable-origin-p plst)))

(defmacro elshogi-with-game-buffer (game &rest body)
  (declare (indent 1))
  `(with-current-buffer (elshogi-game-buffer ,game)
     ,@body))

(defun elshogi-select-square (plst &optional mouse-p)
  (let ((game (elshogi-pack:game plst)))
    (elshogi-with-game-buffer game
      (when (elshogi-usi-ready-p)
        (let ((use-dialog-box mouse-p))
          (cond (elshogi-last-selected
                 (cond ((elshogi-selectable-target-p plst)
                        (let ((origin (elshogi-pack:index elshogi-last-selected))
                              (target (elshogi-pack:index plst)))
                          ;; Prevent simple mis-clicks, in which case
                          ;; do not clear the last selection.
                          (when (elshogi-move-legal-p elshogi-last-selected target)
                            (if-let ((piece
                                      (elshogi-pack:piece elshogi-last-selected)))
                                (elshogi-drop-piece game target piece)
                              (elshogi-move-piece
                               game origin target
                               (elshogi-may-promote-p game origin target)))
                            (elshogi-clear-last-selected game))))
                       ;; Cancel the previous selection
                       ((not (elshogi-pack= plst elshogi-last-selected))
                        (elshogi-set-last-selected plst))
                       (t
                        (elshogi-clear-last-selected game))))
                ((elshogi-selectable-origin-p plst)
                 (elshogi-set-last-selected plst))))))))

(defun elshogi-mouse-select-square (ev)
  "Select the square on mouse event EV."
  (interactive "e")
  (when-let ((plst (elshogi-mouse-read-prop ev 'elshogi-index)))
    (elshogi-select-square plst t)))

(defun elshogi-piece-algebraic (plst)
  (if-let ((piece (elshogi-pack:piece plst)))
      (format "%s*" (elshogi-piece-text piece))
    (let* ((index (elshogi-pack:index plst))
           (piece (elshogi-piece-at (elshogi-pack:game plst) index)))
      (format "%s%s"
              (if piece
                  (elshogi-piece-text piece)
                "")
              (elshogi-index->coord index)))))

(defun elshogi-query-selection (cands)
  (let ((cands
         (mapcar (lambda (plst)
                   (cons (elshogi-piece-algebraic plst) plst))
                 cands)))
    (assoc-default (completing-read (format "Which %s? "
                                            (if elshogi-last-selected
                                                "square"
                                              "piece"))
                                    cands)
                   cands)))

(defun elshogi-select-target (plst)
  (thread-first plst
    elshogi-candidates-target
    elshogi-query-selection
    elshogi-select-square))

(defun elshogi-key-move-piece ()
  "Select the piece to move with keyboard."
  (interactive)
  (when-let ((cands (elshogi-candidates-origin
                     elshogi-current-game
                     (elshogi-char->file last-command-event))))
    (elshogi-display-highlight-candidates cands)
    (let ((plst (elshogi-query-selection cands)))
      (when (elshogi-select-square plst)
        (elshogi-select-target plst)))))

(defun elshogi-key-drop-candidates (game)
  (mapcar (apply-partially #'elshogi-pack-index game)
          (seq-sort-by (lambda (p)
                         (alist-get (elshogi-piece/name p) elshogi-piece-values))
                       #'>
                       (cl-delete-duplicates
                        (elshogi-pieces-on-stand game (elshogi-current-side game))
                        :test (lambda (a b)
                                (eq (elshogi-piece/name a)
                                    (elshogi-piece/name b)))))))

(defun elshogi-key-drop-piece ()
  "Select the piece to drop with keyboard."
  (interactive)
  (when-let* ((pieces (elshogi-key-drop-candidates elshogi-current-game))
              (plst (elshogi-query-selection pieces)))
    (when (elshogi-select-square plst)
      (elshogi-select-target plst))))

(defun elshogi-key-capture-back ()
  "Capture back the last opponent piece."
  (interactive)
  (when-let* ((game elshogi-current-game)
              (target (elshogi-mrec/target (elshogi-game-final-move game)))
              (cands
               (mapcar (apply-partially #'elshogi-pack-index game)
                       (seq-filter (lambda (index)
                                     (memq target
                                           (elshogi-candidates--raw-target index)))
                                   (elshogi-indices-on-board
                                    game
                                    (elshogi-current-side game))))))
    (when (elshogi-select-square (elshogi-query-selection cands))
      (elshogi-select-square (elshogi-pack-index game target)))))

(defun elshogi-quit ()
  "Quit the game."
  (interactive)
  (unless (elshogi-game/watch-p elshogi-current-game)
    (elshogi-game-end))
  (bury-buffer))

(provide 'elshogi)
;;; elshogi.el ends here
