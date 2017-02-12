;;; counsel-mark-ring --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'ivy)

(defvar counsel-browse-mark-ring-ignore-regexp
  (rx bos " *Minibuf"))

;;;###autoload
(defun counsel-browse-mark-ring ()
  "Browse mark ring with ivy."
  (interactive)
  (ivy-read "Mark ring: "
            (cl-delete-duplicates
             (mapcar (lambda (mark)
                       (let ((buf (marker-buffer mark)))
                         (with-current-buffer buf
                           (cons (format "%s:%s %s"
                                         (buffer-name buf)
                                         (line-number-at-pos
                                          (marker-position mark))
                                         (ivy-cleanup-string
                                          (save-excursion
                                            (goto-char mark)
                                            (buffer-substring
                                             (line-beginning-position)
                                             (line-end-position)))))
                                 mark))))
                     (pcase (-separate (lambda (mark)
                                         (eq (current-buffer)
                                             (marker-buffer mark)))
                                       (cl-remove-if
                                        (lambda (mark)
                                          (or (null (marker-buffer mark))
                                              (string-match-p
                                               counsel-browse-mark-ring-ignore-regexp
                                               (buffer-name (marker-buffer mark)))))
                                        global-mark-ring))
                       (`(,local ,global)
                         (append (append mark-ring local)
                                 global))))
             :test (lambda (a b)
                     (string= (car a) (car b))))
            :action #'counsel--browse-mark-ring-action
            :caller 'counsel-browse-mark-ring))

(defun counsel--browse-mark-ring-action (datum)
  (let ((mark (cdr datum)))
    (switch-to-buffer (marker-buffer mark))
    (goto-char mark)
    (recenter)))

;;;###autoload(define-key goto-map "m" #'counsel-browse-mark-ring)

(provide 'counsel-mark-ring)
;;; counsel-mark-ring.el ends here
