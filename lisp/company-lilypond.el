;; -*- byte-compile-dynamic: t; -*-

(require 'company)
;; (require 'cl-lib)
(require 'lilypond-mode)

(defun company-lilypond-grab-symbol ()
  (and (looking-at-p "\\_>")
       (let ((limit (line-beginning-position)))
         (or (looking-back "\\\\\\sw+" limit)
             (looking-back "\\sw+" limit)))
       (match-string-no-properties 0)))

(defun company-lilypond-candidates (prefix)
  (mapcan (lambda (candidates)
            (seq-filter (lambda (candidate)
                          (string-prefix-p prefix candidate))
                        candidates))
          (if (= (elt prefix 0) ?\\)
              (list LilyPond-identifiers
                    LilyPond-keywords)
            (list LilyPond-Capitalized-Reserved-Words
                  LilyPond-non-capitalized-reserved-words))))

(defun company-lilypond (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-lilypond))
    (prefix (and (eq major-mode 'LilyPond-mode)
                 (not (company-in-string-or-comment))
                 (company-lilypond-grab-symbol)))
    (candidates (company-lilypond-candidates arg))))


(add-to-list 'company-backends 'company-lilypond)

(provide 'company-lilypond)
