;;; pml --- my powerline settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'powerline)
(require 'w32-symon nil t)

(defface powerline-face-outside-modified
    `((t (:foreground ,(face-attribute 'default :background)
                      :background ,(face-attribute 'error :foreground)
                      :weight bold)))
  "outside-modified face" :group 'powerline)

(defvar pml-orig-format mode-line-format)

(defvar pml-use-icon-font-p t)

(defvar pml-major-mode-alist nil)

(defun pml--safe-char (char &optional alt)
  (if (char-displayable-p char) (char-to-string char) alt))

;;;###autoload
(defun pml-diminish-major-mode (mode str)
  (when (seq-every-p #'char-displayable-p str)
    (add-to-list 'pml-major-mode-alist (cons mode str))))

(when pml-use-icon-font-p
  (mapc (apply-partially #'apply #'pml-diminish-major-mode)
        '((fundamental-mode "")
          (shell-mode "")
          (term-mode "")
          (messages-buffer-mode "")
          (help-mode "")
          (Info-mode "")
          (Custom-mode "")
          (eww-mode "")
          (pdf-view-mode "")
          (text-mode "")
          (elfeed-search-mode "")
          (elfeed-show-mode ""))))

(defun pml-widep ()
  (>= (window-body-width) 120))

(defun pml-buffer-id (active)
  (let ((id (format-mode-line "%b"))
        (maxlen (if (pml-widep) 40 20)))
    (save-match-data
      (string-match "^\\(.*?\\)\\(.\\{,3\\}\\)$" id)
      (propertize
       (format "%s%s"
               (truncate-string-to-width (match-string 1 id) (- maxlen 2) 0 nil "…")
               (match-string 2 id))
       'face (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive)))))

(defun pml-buffer-modified ()
  (cond (buffer-read-only
         (pml--safe-char 61475 "RO"))
        ((and buffer-file-name (buffer-modified-p))
         (pml--safe-char 61639 "*"))))

(defun pml-major-mode ()
  (format-mode-line
   (or (alist-get major-mode pml-major-mode-alist)
       mode-name)))

(defun pml-minor-modes ()
  (mapconcat #'identity
             (split-string (format-mode-line minor-mode-alist))
             "|"))

(defun pml-process ()
  (when mode-line-process
    (let ((str (format-mode-line mode-line-process)))
      (mapconcat (lambda (part)
                   (if (file-name-absolute-p part)
                       (file-name-base part)
                     part))
                 (split-string str " ")
                 " "))))

(defsubst pml--narrow ()
  (when (buffer-narrowed-p)
    (pml--safe-char 61542 "Narrowed")))

(defun pml--region-info ()
  (when mark-active
    (format "%s(%s, %s)"
            (pml--safe-char 62022 "S")
            (count-lines (region-beginning) (region-end))
            (- (region-end) (region-beginning)))))

(defun pml-region-state (face1 face2 sepl sepr)
  (when-let* (state
              (delq nil (list (pml--narrow) (pml--region-info))))
    (list (powerline-raw " " face1)
          (funcall sepl face1 face2)
          (powerline-raw " " face2)
          (powerline-raw (mapconcat #'identity state " ") face2)
          (powerline-raw " " face2)
          (funcall sepr face2 face1))))

(defun pml-eol ()
  (cl-case (coding-system-eol-type buffer-file-coding-system)
    (0 "")
    (1 "")
    (2 "")
    (t "")))

(eval-when-compile
  (require 'pdf-view nil t))

;; (declare-function pdf-view-current-page "ext:pdf-view")
(declare-function pdf-cache-number-of-pages "ext:pdf-cache")

(defun pml-pdf-hud (face1 face2 &optional width)
  (let ((page (pdf-view-current-page)))
    (pl/percent-xpm (or powerline-height (frame-char-height))
                    (pdf-cache-number-of-pages)
                    1
                    (1+ page) page
                    (* (frame-char-width) (or width 2))
                    (if face1 (face-background face1) "None")
                    (if face2 (face-background face2) "None"))))

(defun pml-left (active line-face face1 face2 sepl sepr)
  `(,(funcall (if (derived-mode-p 'pdf-view-mode)
                  #'pml-pdf-hud
                #'powerline-hud)
              face1 'default)
     ,(funcall sepr 'default face1)
     ,@(when active
         (list (powerline-raw mode-line-mule-info face1 'l)
               (powerline-raw (pml-buffer-modified) face1)
               (powerline-raw " " face1)))
     ,(funcall sepr face1 line-face)
     ,(powerline-raw (pml-buffer-id active) line-face 'l)
     ;; ,(if (derived-mode-p 'pdf-view-mode)
     ;;      (powerline-raw mode-line-position line-face)
     ;;    (powerline-raw ":%l" line-face))
     ,(powerline-raw " " line-face)
     ,(funcall sepl line-face face1)
     ;;,(funcall sepl face1 line-face)
     ;;,(powerline-raw mode-line-modes face1 'l)
     ,(powerline-raw (pml-major-mode) face1 'l)
     ,@(if active
           `(,@(pml-region-state face1 line-face sepl sepr)
               ,(powerline-raw (pml-minor-modes) face1 'l)
               ,(powerline-raw " " face1)
               ,(funcall sepr face1 face2)
               ,(powerline-vc face2 'r)
               ,(powerline-raw (pml-process) face2 'r))
         `(,(powerline-raw " " face1)
            ,(funcall sepr face1 face2)))))

(defvar which-func-format)

(defun pml-right (active _line-face face1 face2 sepl _sepr)
  (when active
    `(,@(when (bound-and-true-p which-function-mode)
          (list (powerline-raw which-func-format face2 'r)))
        ,(powerline-raw global-mode-string face2 'r)
        ,(funcall sepl face2 face1)
        ,@(when (bound-and-true-p w32-symon-mode)
            `(,(powerline-symon 'disk face1)
               ,(funcall sepl face1 face2)
               ,(powerline-raw " " face2)
               ,(powerline-symon 'cpu face2)
               ,(powerline-chamfer-left face2 'default)))
        ,(powerline-raw " " 'default))))

(defun pml-render ()
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (face1 (cond ((and buffer-file-name
                            (not (verify-visited-file-modtime)))
                       'powerline-face-outside-modified)
                      (active 'powerline-active1)
                      (t 'powerline-inactive1)))
         (face2 'default ; (if active 'powerline-active2 'powerline-inactive2)
           )
         (sepl (intern
                (format "powerline-%s-%s"
                        powerline-default-separator
                        (car powerline-default-separator-dir))))
         (sepr (intern
                (format "powerline-%s-%s"
                        powerline-default-separator
                        (cdr powerline-default-separator-dir))))
         (lhs (pml-left active line-face face1 face2 sepl sepr))
         (rhs (pml-right active line-face face1 face2 sepl sepr)))
    (concat (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))

(defun pml ()
  ;; (setq mode-line-position " %l:%2c")
  (setq-default mode-line-format '("%e" (:eval (pml-render)))))

(advice-add 'mode-line-eol-desc :override #'pml-eol)

(provide 'pml)
;;; pml.el ends here
