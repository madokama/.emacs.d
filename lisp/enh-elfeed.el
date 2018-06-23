;;; enh-elfeed --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'bind-key)
  (require 'youtube-dl))
(require 'elfeed)
(require 'seq)
(require 'cl-seq)
(require 'dash)
(require 'hydra)

(defmacro elfeed--define-tag-toggle (&rest args)
  `(progn
     ,@(mapcar (lambda (datum)
                 (seq-let (key tag) datum
                   (let ((fn-search (intern (format "elfeed--search-toggle-%s" tag)))
                         (fn-show (intern (format "elfeed--show-toggle-%s" tag))))
                     `(progn
                        (defun ,fn-search ()
                          (interactive)
                          (elfeed-search-toggle-all ',tag))
                        (defun ,fn-show ()
                          (interactive)
                          (elfeed--show-toggle-tag ',tag))
                        (bind-key ,key #',fn-search elfeed-search-mode-map)
                        (bind-key ,key #',fn-show elfeed-show-mode-map)))))
               args)))


(defmacro elfeed--define-filter-toggle (&rest args)
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (datum)
                 (seq-let (key filter) datum
                   `(bind-key ,key
                              (lambda ()
                                (interactive)
                                (elfeed--toggle-filter ,filter))
                              elfeed-search-mode-map)))
               args)))

(defun elfeed--entry-toggle-tag (entry tag)
  (if (elfeed-tagged-p tag entry)
      (elfeed-untag entry tag)
    (elfeed-tag entry tag)))

(defun elfeed--show-toggle-tag (tag)
  (let ((entry elfeed-show-entry))
    (elfeed--entry-toggle-tag entry tag)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry)))
  (elfeed-show-refresh))

(elfeed--define-tag-toggle
 ("m" unread)
 ("s" star)
 ("j" junk)
 ;; ("d" disclosure)
 ;; ("h" hello)
 )

(defun elfeed--negate-sign (sign)
  (if (eq sign ?-) ?+ ?-))

(defun elfeed--negate-tag (tag)
  (concat (char-to-string
           (elfeed--negate-sign (elt tag 0)))
          (substring tag 1)))

(defun elfeed--toggle-filter (the-tag)
  (elfeed-search-set-filter
   (seq-let (tags nontags)
       (-separate (lambda (x)
                    (memq (elt x 0) '(?- ?+)))
                  (split-string elfeed-search-filter nil))
     (concat
      (string-join
       (append nontags
               (seq-let (the-tags misc-tags)
                   (-separate (lambda (tag)
                                (string= (substring tag 1) the-tag))
                              tags)
                 (cons (if (null the-tags)
                           (concat "+" the-tag)
                         (elfeed--negate-tag (car the-tags)))
                       (cl-remove-if-not (lambda (tag)
                                           (or (string= tag "+unread")
                                               (string= tag "-junk")))
                                         misc-tags))))
       " ")
      " "))))

(elfeed--define-filter-toggle
  ("B" "blog")
  ("D" "disclosure")
  ;; ("E" "emacs")
  ("H" "hello")
  ("N" "news")
  ;; ("P" "pocket")
  ("T" "tech")
  ("U" "unread")
  ("V" "video"))

(defun elfeed--entries-equal (a b)
  (and (equal (elfeed-entry-title a)
              (elfeed-entry-title b))
       (equal (elfeed-entry-content a)
              (elfeed-entry-content b))))

(defun elfeed--entry-merge-force-unread (orig-fun old new)
  "Mark the entry NEW unread."
  (unless (or (elfeed-tagged-p 'junk old)
              (elfeed--entries-equal old new))
    (funcall orig-fun old new)
    (setf (elfeed-entry-tags old)
          (elfeed-normalize-tags
           (elfeed-entry-tags old) elfeed-initial-tags))
    t))

(advice-add 'elfeed-entry-merge :around #'elfeed--entry-merge-force-unread)

(advice-add 'elfeed-db-save :before
            (lambda ()
              (with-elfeed-db-visit (entry _)
                  (when (and (elfeed-tagged-p 'junk entry)
                             (elfeed-ref-p (elfeed-entry-content entry)))
                    (setf (elfeed-entry-content entry) nil)))))

;; (advice-add 'elfeed-search-update--force :after
;;             (lambda ()
;;               (run-with-idle-timer 1 nil #'elfeed-db-save)))

(defun elfeed--reload-entry ()
  (interactive)
  (when-let ((entry elfeed-show-entry))
    (cl-loop for hook in elfeed-new-entry-hook
             do (funcall hook entry))
    (elfeed-deref-entry entry)
    (elfeed-show-refresh)))

;;

(defun elfeed-search--operate (op arg)
  (let ((entry (elfeed-search-selected :ignore-region)))
    (funcall op (elfeed-entry-link entry) arg)
    ;;(elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry))
  (forward-line))

(declare-function youtube-dl/dl "youtube-dl")
(declare-function youtube-dl/play "youtube-dl")
(declare-function youtube-dl-url "youtube-dl")

(defun elfeed-search-youtube-dl/dl (&optional arg)
  (interactive "P")
  (elfeed-search--operate #'youtube-dl/dl arg))

(defun elfeed-search-youtube-dl/play (&optional arg)
  (interactive "P")
  (elfeed-search--operate #'youtube-dl/play arg))

(defun elfeed-show--link ()
  (youtube-dl-url (elfeed-entry-link elfeed-show-entry)))

(defun elfeed-show-youtube-dl/dl (&optional query-fmt)
  (interactive)
  (youtube-dl/dl (elfeed-show--link) query-fmt))

(defun elfeed-show-youtube-dl/play (&optional query-fmt)
  (interactive)
  (youtube-dl/play (elfeed-show--link) query-fmt))

(defun elfeed-search-play-video-maybe (&optional arg)
  (interactive "P")
  (if (elfeed-tagged-p 'video (elfeed-search-selected :ignore-region))
      (elfeed-search-youtube-dl/play)
    (elfeed-search-browse-url arg)))

(defun elfeed-show-play-video-maybe (&optional arg)
  (interactive "P")
  (let ((entry elfeed-show-entry))
    (if (elfeed-tagged-p 'video entry)
        (youtube-dl/play (elfeed-entry-link entry) arg)
      (elfeed-show-visit))))

;;

(defun elfeed-show--header ()
  (format "[%s] %s"
          (propertize (mapconcat #'symbol-name
                                 (elfeed-entry-tags elfeed-show-entry)
                                 " ")
                      'face 'message-header-other)
          (propertize (elfeed-entry-title elfeed-show-entry)
                      'face 'message-header-subject)))

(defun enh-elfeed-entry-switch (buf)
  (switch-to-buffer buf)
  (when (> (point-max) (window-end (get-buffer-window buf) t))
    (setq header-line-format
          '(:eval (elfeed-show--header)))))

(setq elfeed-show-entry-switch #'enh-elfeed-entry-switch)

(declare-function el-pocket-add "ext:el-pocket")

(defun elfeed-save-pocket (entry)
  ;; Skip if it's already starred
  (when (and (not (memq 'star (elfeed-entry-tags entry)))
             (require 'el-pocket nil t))
    (el-pocket-add (elfeed-entry-link entry)
                   (elfeed-entry-title entry)
                   (remq 'unread (elfeed-entry-tags entry)))))

(add-hook 'elfeed-tag-hooks
          (lambda (entries tags)
            (cond ((memq 'star tags)
                   (mapc #'elfeed-save-pocket entries))
                  ((memq 'junk tags)
                   (dolist (entry entries)
                     (when (elfeed-tagged-p 'unread entry)
                       (elfeed-untag entry 'unread)))))))

(declare-function ffedit-url "ffedit")

(bind-keys :map elfeed-search-mode-map
           ("F" . (lambda ()
                    (interactive)
                    (ffedit-url
                     (elfeed-entry-link (car (elfeed-search-selected))))))
           ("G" . (lambda ()
                    (interactive)
                    (elfeed-search-set-filter nil)))
           ("R" . elfeed-search-fetch))

(bind-keys :map elfeed-search-mode-map
           ("b" . nil)
           ("r" . nil)
           ("u" . nil)
           ("f" . elfeed-search-live-filter)
           ("v" . elfeed-search-play-video-maybe))

(declare-function ace-link-eww "ext:ace-link")
(bind-keys :map elfeed-show-mode-map
           ("b" . nil)
           ("r" . nil)
           ("u" . nil)
           ;; ("f" . elfeed-show-new-live-search)
           ("F" . (lambda ()
                    (interactive)
                    (ffedit-url
                     (elfeed-entry-link elfeed-show-entry))))
           ("o" . ace-link-eww)
           ("v" . elfeed-show-play-video-maybe)
           ("q" . (lambda ()
                    (interactive)
                    (elfeed-kill-buffer)
                    (run-with-idle-timer 1 nil #'elfeed-db-save)
                    (switch-to-buffer (elfeed-search-buffer))))
           ("R" . elfeed--reload-entry))

(bind-key ","
          (defhydra hydra-elfeed-search (:exit t)
            "
_p_: play video  _d_: dl video
_s_: save db
"
                                        ;_f_: feed only
            ("d" elfeed-search-youtube-dl/dl nil)
            ("p" elfeed-search-youtube-dl/play nil)
                                        ;("f" (elfeed--filter-by-feed (elfeed-search-selected :ignore-region)) nil)
            ("s" (elfeed-db-save) nil)
            ("z" nil))
          elfeed-search-mode-map)

(bind-key ","
          (defhydra hydra-elfeed-show (:exit t)
            "
_p_: play video  _d_: dl video
_s_: save db
"
            ("d" elfeed-show-youtube-dl/dl nil)
            ("p" elfeed-show-youtube-dl/play nil)
            ("s" (elfeed-db-save) nil)
            ;;("h" (set-transient-map 'helm-command-prefix) nil)
            ("z" nil))
          elfeed-show-mode-map)

(defun elfeed-show--keep-position ()
  (interactive)
  (let* ((orig-line (line-number-at-pos))
         (rel-pos (- (point) (line-beginning-position))))
    (elfeed-show-refresh--mail-style)
    (goto-char (point-min))
    (forward-line (1- orig-line))
    (goto-char (min (+ (line-beginning-position) rel-pos)
                    (line-end-position)))))

(setq elfeed-show-refresh-function #'elfeed-show--keep-position)

;; FIXME: seems not working
(advice-add 'elfeed-search-update-line
            :around (lambda (orig-fun &rest args)
                      "Ensure the jobs is done on the search buffer"
                      (with-current-buffer (elfeed-search-buffer)
                        (apply orig-fun args))))

;; (declare-function hydra-motion/body "hydra-motion")

;; (defun elfeed-show--hydra-motion (key)
;;   (setq last-input-event (aref (kbd key) 0))
;;   (call-interactively #'hydra-motion/body))

;; (defun elfeed-show--enh-motion-keys ()
;;   (bind-key "n"
;;             (elfeed-expose #'elfeed-show--hydra-motion "C-n")
;;             elfeed-show-mode-map)
;;   (bind-key "p"
;;             (elfeed-expose #'elfeed-show--hydra-motion "C-p")
;;             elfeed-show-mode-map)
;;   (add-hook 'hydra-motion-next-line-action #'elfeed-show-next)
;;   (add-hook 'hydra-motion-prev-line-action #'elfeed-show-prev))

;; (add-hook 'elfeed-show-mode-hook #'elfeed-show--enh-motion-keys)
(add-hook 'elfeed-show-mode-hook #'hl-line-mode)

(provide 'enh-elfeed)
;;; enh-elfeed.el ends here
