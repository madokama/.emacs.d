;;; instagram --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'xml)

(autoload 'url-curl-extract-cookies "url-curl")

(defun instagram-credentials ()
  (url-curl-extract-cookies
   "www.instagram.com"
   '("csrftoken" "ds_user_id" "sessionid")))

(defun instagram-user-id ()
  (goto-char (point-min))
  (when (search-forward "window._sharedData = " nil t)
    (let-hash (aref (let-hash (json-parse-buffer)
                       .entry_data.ProfilePage)
                     0)
      .user.id)))

(defun instagram-api/user-feed (endpoint user-id)
  (format "https://i.instagram.com/api/v1/feed/user/%s/%s"
          user-id (or endpoint "")))

(defvar instagram-user-agent
  "Instagram 10.26.0 (iPhone7,2; iOS 10_1_1; en_US; en-US; scale=2.00; gamut=normal; 750x1334) AppleWebKit/420+")

(defun instagram-json-api (url credentials)
  (with-temp-buffer
    (apply #'call-process "curl" nil t nil
           url
           "-s" "--compressed"
           "-A" instagram-user-agent
           (mapcan (lambda (header)
                     (list "-H" header))
                   (list "dnt: 1"
                         "accept-encoding: gzip, deflate, br"
                         "x-ig-capabilities: 36oD"
                         "accept-language: en-US,en;q=0.8"
                         "accept: */*"
                         "authority: i.instagram.com"
                         (concat "cookie: " credentials))))
    (goto-char (point-min))
    (json-parse-buffer)))

;;; IG Private APIs

(defun instagram-user-feed (user-id credentials)
  (let-hash (instagram-json-api (instagram-api/user-feed nil user-id)
                                credentials)
    .items))

(defun instagram-user-story (user-id credentials)
  (let-hash (instagram-json-api (instagram-api/user-feed "story/" user-id)
                                credentials)
    .reel.items))

;;; IG Public APIs

(defun instagram-shortcode-json (code)
  (with-temp-buffer
    (apply #'call-process "curl" nil t nil
           (format "https://www.instagram.com/p/%s/?__a=1&__b=1" code)
           "-s" "--compressed"
           "-A" instagram-user-agent
           (mapcan (apply-partially #'list "-H")
                   '("Accept: */*"
                     "Accept-Language: en-US"
                     "Accept-Encoding: gzip, deflate"
                     "Connection: close"
                     "Referer: https://www.instagram.com"
                     "x-requested-with: XMLHttpRequest")))
    (goto-char (point-min))
    (let-hash (json-parse-buffer)
      .graphql.shortcode_media)))

(defun instagram-shortcode-media (json)
  (let-hash json
    (pcase .__typename
      ("GraphImage" .display_url)
      ("GraphSidecar"
       (mapcar (lambda (node)
                 (let-hash node
                   .node.display_url))
               .edge_sidecar_to_children.edges))
      ("GraphVideo"
       (list .video_url .display_url)))))

(defun instagram-media-source (code)
  ;; "shortcode" media info
  (instagram-shortcode-media (instagram-shortcode-json code)))

;;; Media item APIs

(defsubst instagram--media-type (item)
  (gethash "media_type" item))

(defun instagram-item-video-p (item)
  (= (instagram--media-type item) 2))

(defun instagram-item-carousel-p (item)
  (= (instagram--media-type item) 8))

(defsubst instagram--first-url (vec)
  (gethash "url" (aref vec 0)))

(defun instagram-item-image (item)
  (instagram--first-url
   (let-hash item
     .image_versions2.candidates)))

(defun instagram-item-carousel (item)
  (gethash "carousel_media" item))

(defun instagram-item-video (item)
  (when (instagram-item-video-p item)
    (instagram--first-url (gethash "video_versions" item))))

(defun instagram-item-caption (item)
  (let-hash item
    (when .caption.text
      (xml-escape-string .caption.text))))

(defun instagram-item-time (item)
  (gethash "taken_at" item))

(defun instagram-item-link (item)
  (format "https://www.instagram.com/p/%s/" (gethash "code" item)))

(provide 'instagram)
;;; instagram.el ends here
