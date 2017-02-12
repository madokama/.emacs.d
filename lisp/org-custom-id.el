;;; org-custom-id --- Add custom id automatically. -*- lexical-binding: t; -*-

;;; Commentary:

;; Taken from https://writequit.org/articles/emacs-org-mode-generate-ids.html

;;; Code:

(require 'subr-x)
(require 'org-id)

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun org-cid-get (&optional pom)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM."
  ;; (interactive)
  (let ((id (org-entry-get pom "CUSTOM_ID")))
    (when (and id (not (string-empty-p id)))
      id)))

;;;###autoload
(defun org-cid-set (&optional pom prefix)
  "Add a CUSTOM_ID property to the entry at POM.
PREFIX will be passed through to `org-id-new'"
  (interactive (list (point) nil))
  (unless (org-cid-get pom)
    (let ((id (org-id-new (concat prefix "h"))))
      (org-entry-put pom "CUSTOM_ID" id)
      (org-id-add-location id (buffer-file-name (buffer-base-buffer))))))

;; (defun eos/org-custom-id-get (&optional pom create prefix)
;;   "Get the CUSTOM_ID property of the entry at point-or-marker POM.
;; If POM is nil, refer to the entry at point. If the entry does
;; not have an CUSTOM_ID, the function returns nil. However, when
;; CREATE is non nil, create a CUSTOM_ID if none is present
;; already. PREFIX will be passed through to `org-id-new'. In any
;; case, the CUSTOM_ID of the entry is returned."
;;   (interactive)
;;   (org-with-point-at pom
;;     (let ((id (org-entry-get nil "CUSTOM_ID")))
;;       (cond
;;         ((and id (stringp id) (string-match-p "\\S-" id))
;;          id)
;;         (create
;;          (setq id (org-id-new (concat prefix "h")))
;;          (org-entry-put pom "CUSTOM_ID" id)
;;          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
;;          id)))))

(defun org-cid-add-ids-all ()
  "Add CUSTOM_ID properties to all headlines in the current file."
  (interactive)
  (org-map-entries #'org-cid-set))

(defun org-cid-auto-id-p ()
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward "^#\\+OPTIONS:\\s *auto-id:\\s *t\\_>" nil t)))))

;;;###autoload
(defun org-cid-add-ids-maybe ()
  (when (org-cid-auto-id-p)
    (org-cid-add-ids-all)))

;; automatically add ids to captured headlines
;; automatically add ids to saved org-mode headlines
;;;###autoload(with-eval-after-load 'org (add-hook 'before-save-hook #'org-cid-add-ids-maybe) (add-hook 'org-capture-prepare-finalize-hook #'org-cid-set))

(provide 'org-custom-id)
;;; org-custom-id.el ends here
