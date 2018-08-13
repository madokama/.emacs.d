;;; page2feed-ig --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Instagram

(require 'instagram)

(defun page2feed-instagram-entry-html (json)
  (cl-flet ((tag (medium)
              (format "<img src=%S />"
                      (plist-get medium :image))))
    (let ((media (instagram-entry-media json)))
      (concat (format "<pre>%s</pre>" (instagram-entry-type json))
              (if (eq (instagram-entry-type json) 'GraphSidecar)
                  (mapconcat #'tag media "<br />")
                (tag media))))))

(defun page2feed-instagram-entry (json)
  (let* ((caption (instagram-entry-caption json))
         (caption (and caption (split-string caption "\n"))))
    (list 'title (car caption)
          'link (instagram-entry-page json)
          'updated (instagram-entry-timestamp json)
          'content
          (concat (page2feed-instagram-entry-html json)
                  (mapconcat (lambda (line)
                               (format "<div>%s</div>" line))
                             (cdr caption) "")))))

(defun page2feed-instagram-scrape ()
  (when (re-search-forward (rx "<title>" (+? anything) "Instagram" (+? anything) "</title>")
                           nil t)
    (let ((json (instagram-shared-data)))
      (cl-case (instagram-json-type json)
        (feed)
        (user
         (let ((json (instagram-user-json json)))
           (list 'link (instagram-user-page json)
                 'author (instagram-user-name json)
                 'entries
                 (nconc (mapcar #'page2feed-ig-story-entry
                                (instagram-graphql-user-story
                                 (instagram-user-id json)
                                 (instagram-user-page json)))
                        (mapcar #'page2feed-instagram-entry
                                (instagram-user-entries json))))))))))

(defun page2feed-instagram-story-html (json)
  (let ((medium (instagram-story-medium json)))
    (format "<pre>%s</pre><a href=%S><img src=%S /></a>"
            (instagram-entry-type json)
            (or (plist-get medium :video) (plist-get medium :image))
            (plist-get medium :image))))

(defun page2feed-ig-story-entry (json)
  (let ((time (instagram-entry-timestamp json))
        (medium (instagram-story-medium json)))
    (list 'title (format-time-string "%F %T" time)
          'link (or (plist-get medium :video) (plist-get medium :image))
          'updated time
          'content (page2feed-instagram-story-html json))))

(add-hook 'page2feed-scrapers #'page2feed-instagram-scrape)

(provide 'page2feed-ig)
;;; page2feed-ig.el ends here
