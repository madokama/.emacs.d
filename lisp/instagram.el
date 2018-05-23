;;; instagram --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'url-http)

(defvar instagram-home "https://www.instagram.com/")


;;; Shared data

(defun instagram-shared-data ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (when (search-forward "window._sharedData = " nil t)
        (json-parse-buffer)))))

(defun instagram-json-type (json)
  (let-hash json
    (let-hash .entry_data
      (cond (.FeedPage 'feed)
            (.ProfilePage 'user)
            (.PostPage 'entry)))))

(defun instagram-feed-json (json)
  (and (eq (instagram-json-type json) 'feed)
       (let-hash json
         (let-hash (aref .entry_data.FeedPage 0)
           .graphql))))

(defun instagram-user-json (json)
  (and (eq (instagram-json-type json) 'user)
       (let-hash json
         (let-hash (aref .entry_data.ProfilePage 0)
           .graphql))))

(defun instagram-entry-json (json)
  (and (eq (instagram-json-type json) 'entry)
       (let-hash json
         (let-hash (aref .entry_data.PostPage 0)
           .graphql.shortcode_media))))

;; Accessors

(defun instagram-entry-type (json)
  (intern (let-hash json .__typename)))

(defun instagram-entry-media (json &optional force)
  ;; JSON may or may not contain media data depending on its origin.
  (or (instagram-shortcode-media json)
      (let-hash json
        (let ((p (delay
                  (instagram-shortcode-media
                   (instagram-shortcode-json .shortcode)))))
          (if force
              (force p)
            p)))))

(defun instagram-entry-id (json)
  (let-hash json .id))

(defun instagram-entry-page (json)
  (url-expand-file-name (format "p/%s/" (let-hash json .shortcode))
                        instagram-home))

(defun instagram-entry-caption (json)
  (let-hash json
    (unless (zerop (length .edge_media_to_caption.edges))
      (let-hash (aref .edge_media_to_caption.edges 0)
        .node.text))))

(defun instagram-entry-timestamp (json)
  (let-hash json
    .taken_at_timestamp))

(defun instagram--entries (json)
  (let-hash json
    (mapcar (lambda (entry)
              (let-hash entry .node))
            .edges)))

(defun instagram--with-feed (json fn)
  (let-hash json
    (funcall fn .user.edge_web_feed_timeline)))

(defun instagram--with-user (json fn)
  (let-hash json
    (funcall fn .user.edge_owner_to_timeline_media)))

(defun instagram--with-saved (json fn)
  (let-hash json
    (funcall fn .user.edge_saved_media)))

(defun instagram-feed-entries (json)
  (instagram--with-feed json #'instagram--entries))

(defun instagram-user-entries (json)
  (instagram--with-user json #'instagram--entries))

(defun instagram-saved-entries (json)
  (instagram--with-saved json #'instagram--entries))

(defun instagram--end-cursor (json)
  (let-hash json
    (when .page_info.has_next_page
      .page_info.end_cursor)))

(defun instagram-feed-end-cursor (json)
  (instagram--with-feed json #'instagram--end-cursor))

(defun instagram-user-end-cursor (json)
  (instagram--with-user json  #'instagram--end-cursor))

(defun instagram-saved-end-cursor (json)
  (instagram--with-saved json  #'instagram--end-cursor))

(defun instagram-user-id (json)
  (let-hash json .user.id))

(defun instagram-user-name (json)
  (let-hash json .user.username))

(defun instagram-user-page (json)
  (url-expand-file-name (concat (instagram-user-name json) "/") instagram-home))

(defun instagram-story-medium (json)
  (let-hash json
    (pcase .__typename
      ("GraphStoryImage"
       (list :image .display_url))
      ("GraphStoryVideo"
       (list :image .display_url
             :video
             (let-hash (aref (seq-sort-by (lambda (json)
                                            (let-hash json
                                              .config_width))
                                          #'>
                                          .video_resources)
                             0)
               .src))))))

(defun instagram-story-media (json)
  (mapcar #'instagram-story-medium json))


;; Graphql API

(defun instagram-graphql-api (query)
  (url-expand-file-name (format "graphql/query/?%s"
                                (url-build-query-string query))
                        instagram-home))

(defun instagram-graphql-variables (vars)
  (list 'variables
        (concat "{"
                (mapconcat (pcase-lambda (`(,k . ,v))
                             (format "%S:%S"
                                     (symbol-name k)
                                     (cond ((null v) 'false)
                                           ((eq v t) 'true)
                                           (t v))))
                           vars
                           ",")
                "}")))

(defun instagram-graphql-query (url &optional referer)
  (with-temp-buffer
    (call-process "curl" nil t nil
                  "-s" "--compressed"
                  "-b" (url-curl-cookie)
                  "-e" (or referer instagram-home)
                  "-H" (string-trim-right (url-http-user-agent-string))
                  "-H" "X-Requested-With: XMLHttpRequest"
                  "-H" "DNT: 1"
                  url)
    (goto-char (point-min))
    (let-hash (json-parse-buffer)
      .data)))

(defun instagram-graphql-user-page (id referer &optional end-cursor)
  (cl-assert (stringp id))
  (instagram-graphql-query
   (instagram-graphql-api
    `((query_hash "42323d64886122307be10013ad2dcc44")
      (id ,id)
      (first 12)
      ,@(when end-cursor
          `((after ,end-cursor)))))
   referer))

(defun instagram-graphql-user-saved (id referer &optional end-cursor)
  (cl-assert (stringp id))
  (instagram-graphql-query
   (instagram-graphql-api
    `((query_hash "f883d95537fbcd400f466f63d42bd8a1")
      (id ,id)
      (first 12)
      ,@(when end-cursor
          `((after ,end-cursor)))))
   referer))

(defun instagram-graphql-feed-stories ()
  (let-hash (instagram-graphql-query
             (instagram-graphql-api
              `((query_hash "60b755363b5c230111347a7a4e242001")
                ,(instagram-graphql-variables
                  '((only_stories . t))))))
    (instagram--entries
     .user.feed_reels_tray.edge_reels_tray_to_reel)))

(defun instagram-graphql-feed-page (&optional end-cursor)
  (instagram-graphql-query
   (instagram-graphql-api
    `((query_hash "485c25657308f08317c1e4b967356828")
      ,(instagram-graphql-variables
        `((fetch_media_item_count . 12)
          ,@(when end-cursor
              `((fetch_media_item_cursor . ,end-cursor)))
          (fetch_comment_count . 0)
          (fetch_like . 0)
          (has_stories . nil)))))))

(defun instagram-graphql-user-has-story (id referer)
  (let-hash (instagram-graphql-query
             (instagram-graphql-api
              `((query_hash "7e1e0c68bbe459cf48cbd5533ddee9d4")
                ,(instagram-graphql-variables
                  `((user_id . ,id)
                    (include_chaining . nil)
                    (include_reel . t)
                    (include_suggested_users . nil)
                    (include_logged_out_extras . t)))))
             referer)
    .user.has_public_story))

(defun instagram-graphql-user-story (id referer)
  (when (instagram-graphql-user-has-story id referer)
    (let-hash (instagram-graphql-query
               (instagram-graphql-api
                `((query_hash "bf41e22b1c4ba4c9f31b844ebb7d9056")
                  ,(instagram-graphql-variables
                    `((reel_ids . [,id])
                      (precomposed_overlay . nil)))))
               referer)
      (let-hash (aref .reels_media 0)
        .items))))


;;; IG Public APIs

(defun instagram-shortcode-json (code)
  (with-temp-buffer
    (apply #'call-process "curl" nil t nil
           (url-expand-file-name (format "p/%s/?__a=1&__b=1" code) instagram-home)
           "-s" "--compressed"
           "-e" instagram-home
           (mapcan (apply-partially #'list "-H")
                   '("Accept: */*"
                     "Accept-Language: en-US"
                     "Connection: close"
                     "x-requested-with: XMLHttpRequest")))
    (goto-char (point-min))
    (let-hash (json-parse-buffer)
      .graphql.shortcode_media)))

(defun instagram-shortcode-media (json)
  (let-hash json
    (pcase .__typename
      ("GraphImage"
       (list :image .display_url))
      ("GraphVideo"
       (and .video_url
            (list :image .display_url :video .video_url)))
      ("GraphSidecar"
       ;; May contain videos.
       (mapcar (lambda (node)
                 (let-hash node
                   (instagram-shortcode-media .node)))
               .edge_sidecar_to_children.edges)))))

(provide 'instagram)
;;; instagram.el ends here
