;;; ig-eww --- instagram support for eww -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'shr)
(require 'instagram)

(defvar ig-eww-video-player shr-external-browser)

(defvar-local ig-eww-cursor nil)

(defvar eww-data)

(defun ig-eww--shared-data ()
  (let ((script
         (thread-first
             (plist-get eww-data :dom)
           (dom-by-tag 'body)
           (dom-by-tag 'script)
           car
           dom-strings
           car)))
    (with-temp-buffer
      (insert script)
      (instagram-shared-data))))

;; Forward declarations
(defvar ig-eww-story-map)
(defvar ig-eww-map)

(defun ig-eww--image-props (json)
  (if (memq (instagram-entry-type json) '(GraphStoryImage GraphStoryVideo))
      (let-hash json
        (list 'keymap ig-eww-story-map
              'shr-url .display_url
              'face
              `((:box
                 (:line-width 2
                  :color
                  ,(cond ((eq (instagram-entry-type json) 'GraphStoryVideo)
                          "magenta")
                         (.story_cta_url "cyan")
                         (t (face-background 'default))))))
              'shr-alt .story_cta_url
              'help-echo .story_cta_url))
    (let ((alt (instagram-entry-caption json)))
      (list 'keymap ig-eww-map
            'face
            `((:box
               (:line-width 2
                :color
                ,(cl-case (instagram-entry-type json)
                   (GraphSidecar "cyan")
                   (GraphVideo "magenta")
                   (t (face-background 'default))))))
            'shr-url (instagram-entry-page json)
            'shr-alt alt
            'help-echo alt))))

(defun ig-eww-insert-image (url json start end)
  (let ((spec (shr-get-image-data url))
        (inhibit-read-only t))
    (set-text-properties
     start end
     (nconc (list 'display
                  (create-image (car spec) 'imagemagick t
                                :max-width 150
                                :margin 2
                                :format (cadr spec))
                  'json json)
            (ig-eww--image-props json)))))

(defun ig-eww--newline ()
  (let ((inhibit-read-only t))
    (insert "\n")))

(defun ig-eww--space ()
  (let ((inhibit-read-only t))
    (insert " ")))

(defun ig-eww--thumb-url (entry)
  (let-hash entry
    ;; Silently fail (e.g. return nil) if none found.
    (or (let-hash (or (cl-find-if (lambda (node)
                                    (let-hash node
                                      ;; Choose 150
                                      (<= 150 .config_width (1- 240))))
                                  .thumbnail_resources)
                      (cl-find-if (lambda (node)
                                    (let-hash node
                                      (<= .config_width 640)))
                                  .display_resources))
          .src)
        .thumbnail_src)))

(defun ig-eww--render-timeline (entries)
  (mapc (lambda (entry)
          (let-hash entry
            (when-let ((img-url (ig-eww--thumb-url entry))
                       (start (point)))
              (ig-eww--space)
              (let ((end (point)))
                (if (url-is-cached img-url)
                    (ig-eww-insert-image img-url entry start end)
                  (url-queue-retrieve
                   img-url
                   (lambda (_ buf)
                     (unwind-protect
                          (with-current-buffer buf
                            (ig-eww-insert-image img-url entry start end))
                       (kill-buffer)))
                   (list (current-buffer))
                   t t))))))
        entries))

(defun ig-eww-render-feed (json)
  (ig-eww--render-timeline (instagram-feed-entries json))
  (setq ig-eww-cursor (instagram-feed-end-cursor json)))

(defun ig-eww-render-feed-stories ()
  (mapc (lambda (user)
          (let-hash user
            (ig-eww-render-user-story .id)))
        (instagram-graphql-feed-stories)))

(defun ig-eww-render-user-story (user-id)
  (when-let ((story
              (instagram-graphql-user-story user-id
                                            (plist-get eww-data :url))))
    (ig-eww--render-timeline story)
    (ig-eww--newline)))

(defun ig-eww-render-user (json)
  (ig-eww--render-timeline (instagram-user-entries json))
  (setq ig-eww-cursor (instagram-user-end-cursor json)))

(defun ig-eww-render-saved (json)
  (ig-eww--render-timeline (instagram-saved-entries json))
  (setq ig-eww-cursor (instagram-saved-end-cursor json)))

(defun ig-eww--saved-page-p ()
  (string-match-p (rx "/saved/" eos) (plist-get eww-data :url)))

(defun ig-eww-next-page ()
  (when-let ((end-cursor ig-eww-cursor)
             (json (ig-eww--shared-data)))
    (ig-eww--newline)
    (save-excursion
      (cl-case (instagram-json-type json)
        (feed
         (ig-eww-render-feed (instagram-graphql-feed-page end-cursor)))
        (user
         (let ((json (instagram-user-json json)))
           (if (ig-eww--saved-page-p)
               (ig-eww-render-saved
                (instagram-graphql-user-saved (instagram-user-id json)
                                              (concat (instagram-user-page json)
                                                      "saved/")
                                              end-cursor))
             (ig-eww-render-user
              (instagram-graphql-user-page (instagram-user-id json)
                                           (instagram-user-page json)
                                           end-cursor)))))))))

(defun ig-eww--make-dom (media json)
  `(body nil
         ,@(mapcar (lambda (medium)
                     (when-let ((video (plist-get medium :video)))
                       (funcall ig-eww-video-player video))
                     `(img ((src . ,(plist-get medium :image)))))
                   media)
         (pre nil ,(instagram-entry-caption json))))

(defun ig-eww-display-images (images json)
  (let ((dom (ig-eww--make-dom images json))
        (inhibit-read-only t))
    (save-excursion (shr-insert-document dom))))

(defun ig-eww-render-entry (json)
  (let ((media (instagram-entry-media json)))
    ;; Media may be plist or list of plists.
    (ig-eww-display-images (if (consp (car media))
                               media
                             (list media))
                           json)))


;; Mode commands

(defun ig-eww-forward-char (arg)
  "Move cursor ARG chars forward."
  (interactive "p")
  (condition-case nil
      (forward-char arg)
    (end-of-buffer
     (ig-eww-next-page))))

(defun ig-eww-browse ()
  "Browse instagram image/video under point."
  (interactive)
  (when-let ((json (get-text-property (point) 'json)))
    (eww (instagram-entry-page json))))

(defun ig-eww-like ()
  "Like the instagram post."
  (interactive)
  (when-let ((json (instagram-entry-json (ig-eww--shared-data))))
    (let-hash json
      (unless .viewer_has_liked
        'like))))

(defvar ig-eww-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'ig-eww-forward-char)
    (define-key map [remap forward-char] #'ig-eww-forward-char)
    (define-key map "p" #'backward-char)
    (define-key map "v" #'ig-eww-browse)
    (make-composed-keymap map shr-map)))

(defun ig-eww-story-browse ()
  "Browse instagram story."
  (interactive)
  (when-let* ((json (get-text-property (point) 'json))
              (media (instagram-story-medium json)))
    (if-let ((video (plist-get media :video)))
        (funcall ig-eww-video-player video)
      (browse-url (plist-get media :image)))))

(defvar ig-eww-story-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" #'ig-eww-story-browse)
    (make-composed-keymap map ig-eww-map)))



;;;###autoload
(defun ig-eww-hook ()
  (when (string-match-p (rx ".instagram.com/") (plist-get eww-data :url))
    (when-let ((json (ig-eww--shared-data)))
      (save-excursion
        (cl-case (instagram-json-type json)
          (feed
           (let ((json (instagram-feed-json json)))
             ;; json may be empty
             (ig-eww-render-feed-stories)
             (ig-eww-render-feed (or json (instagram-graphql-feed-page)))))
          (user
           (let ((json (instagram-user-json json)))
             (if (ig-eww--saved-page-p)
                 (ig-eww-render-saved json)
               (ig-eww-render-user-story (instagram-user-id json))
               (ig-eww-render-user json))))
          (entry
           (ig-eww-render-entry (instagram-entry-json json))))))))

;;;###autoload(add-hook 'eww-after-render-hook #'ig-eww-hook)

(provide 'ig-eww)
;;; ig-eww.el ends here
