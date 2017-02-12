;;; org-elfeed.el --- org-mode link integration with elfeed -*- lexical-binding: t; -*-
;; Copyright (C) 2013
;; Danie Roux <danie@danieroux.com>

;; Authors: Danie Roux <danie@danieroux.com>
;; Keywords: orgmode, elfeed

;; This file is NOT part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'org)
(require 'elfeed)

;;;###autoload
(defun org-elfeed-open (path)
  (cond
   ((string-match "^entry-id:\\(.+\\)" path)
    (let* ((entry-id-str (substring-no-properties (match-string 1 path)))
	   (parts (split-string entry-id-str "|"))
	   (feed-id-str (car parts))
	   (entry-part-str (cadr parts))
	   (entry-id (cons feed-id-str entry-part-str))
	   (entry (elfeed-db-get-entry entry-id)))
      (elfeed-show-entry entry)))
   (t (error "%s %s" "elfeed: Unrecognised link type - " path))))

(defun org-elfeed-create-link (entry)
  (let* ((title (elfeed-entry-title entry))
         (url (elfeed-entry-link entry))
         (entry-id (elfeed-entry-id entry))
         (entry-id-str (concat (car entry-id)
                               "|"
                               (cdr entry-id)
                               "|"
                               url))
         (org-link (concat "elfeed:entry-id:" entry-id-str)))
    (org-store-link-props :description title
                          :type "elfeed"
                          :link org-link
                          :url url
                          :entry-id entry-id)
    org-link))

;;;###autoload
(defun org-elfeed-store-link ()
  "Store a link to an elfeed entry."
  (interactive)
  (cond ((eq major-mode 'elfeed-show-mode)
         (org-elfeed-create-link elfeed-show-entry))
        ((eq major-mode 'elfeed-search-mode)
         (org-elfeed-create-link (elfeed-search-selected :ignore-region)))
        (t nil)))

;;;###autoload(with-eval-after-load 'org (org-link-set-parameters "elfeed" :follow #'org-elfeed-open :store #'org-elfeed-store-link))

(provide 'org-elfeed)
;;; org-elfeed.el ends here
