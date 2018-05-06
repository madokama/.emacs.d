;;; scorely --- lilypond to csound converter -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'dash)

(defvar scorely-default-inst 1)

;; Music Types

;; (defvar scorely-types nil)

(defmacro scorely-deftype (name &rest slots)
  (declare (indent 1))
  `(progn
     (cl-defstruct (,(intern (format "scorely-%s" name))
                     (:conc-name ,(intern (format "scorely-%s:" name))))
       ,@slots)))

(scorely-deftype note
  pc                                    ; pitch class
  acc                                   ; accidental
  oct                                   ; octave shift
  dur
  pn                                    ; pitch naturale
  pitch
  dyn
  attr)

(scorely-deftype rest
  dur attr)

(scorely-deftype tuplet
  frac notes)

(scorely-deftype chord
  dur notes attr)

(scorely-deftype voice
  inst notes)

(scorely-deftype voices
  voices)

(defun scorely:dur (mexp)
  (cond ((scorely-note-p mexp) (scorely-note:dur mexp))
        ((scorely-rest-p mexp) (scorely-rest:dur mexp))
        ((scorely-tuplet-p mexp)
         (pcase (scorely-tuplet:frac mexp)
           (`(,num . ,den)
             (* (scorely:dur (scorely-tuplet:notes mexp))
                den (expt num -1)))))
        ((listp mexp)
         (cl-reduce (lambda (acc note)
                      (+ (scorely:dur note) acc))
                    mexp
                    :initial-value 0))
        (t 0)))

(defun scorely:attr (ly)
  (cond ((scorely-note-p ly) (scorely-note:attr ly))
        ((scorely-chord-p ly) (scorely-chord:attr ly))
        ((scorely-rest-p ly) (scorely-rest:attr ly))))

;;

(defvar scorely-map-pc
  (cl-mapcar #'cons
             '("c" "d" "e" "f" "g" "a" "b")
             '(0 2 4 5 7 9 11)))

(defvar scorely-map-acc
  (cl-mapcar #'cons '("is" "es") '(1 -1)))

(defvar scorely-map-oct
  (cl-mapcar #'cons '("'" ",") '(1 -1)))

(defconst scorely-default-note
  (make-scorely-note :pc 0 :dur 1.0 :dyn 80 :pn 48))

(defun scorely-calc-pitch (base-pitch note)
  (let* ((pitch (+ (* (/ base-pitch 12) 12)
                   (scorely-note:pc note)))
         (diff (abs (- base-pitch pitch))))
    (cond ((< diff 7) pitch)
          ((> pitch base-pitch) (- pitch 12))
          (t (+ pitch 12)))))

(defun scorely-relative-pitch (base note)
  (when (scorely-note-p note)
    (let ((pitch (+ (scorely-calc-pitch (scorely-note:pn base) note)
                    (* (scorely-note:oct note) 12))))
      (setf (scorely-note:pn note) pitch)
      (setf (scorely-note:pitch note) (+ pitch (scorely-note:acc note)))))
  note)

(defun scorely-relative-note (base note)
  (let ((note (scorely-relative-pitch base note)))
    (unless (scorely-note:dur note)
      (setf (scorely-note:dur note) (scorely-note:dur base)))
    (unless (scorely-note:dyn note)
      (setf (scorely-note:dyn note) (scorely-note:dyn base)))
    note))

(defun scorely-in-beats (dur)
  (* 4 (expt dur -1)))

;; Parse

(require 'parsec)

(defmacro scorely-tok (parser)
  `(parsec-between (scorely-spaces) (scorely-spaces) ,parser))

(defsubst scorely-spaces ()
  (parsec-many (parsec-re "[[:space:]\r\n]")))

(defun scorely-number ()
  (string-to-number (parsec-many1-s (parsec-digit))))

(defun scorely-string ()
  (parsec-between (parsec-ch ?\")
                  (parsec-ch ?\")
                  (parsec-many-s (parsec-none-of ?\"))))

(defun scorely-numeric-fold (map lst)
  (cl-reduce (lambda (acc x)
               (+ acc
                  (cdr (assoc x map))))
             lst
             :initial-value 0))

(defun scorely-parse-note-acc ()
  (scorely-numeric-fold scorely-map-acc
                        (parsec-many1
                         (parsec-or (parsec-str "is") (parsec-str "es")))))

(defun scorely-parse-note-oct ()
  (scorely-numeric-fold scorely-map-oct
                        (parsec-many1 (parsec-one-of ?, ?\'))))

(defun scorely-parse-note-dur ()
  (let ((dur (scorely-in-beats (scorely-number)))
        (dot (parsec-many (parsec-ch ?.)))
        (scale (parsec-optional
                (parsec-and (parsec-ch ?*) (scorely-number)))))
    (when dot
      (setq dur (cl-reduce (lambda (acc n)
                             (+ (* dur (expt 2 (- (1+ n))))
                                acc))
                           (number-sequence 0 (1- (length dot)))
                           :initial-value dur)))
    (when scale
      (setq dur (* dur scale)))
    dur))

(defun scorely-parse-note-sattr ()
  ;; -> (accent) etc.
  (parsec-many
   (parsec-and (parsec-ch ?-)
               (cons 'short (parsec-any-ch)))))

(defun scorely-parse-note-lattr ()
  ;; \mf \fermata etc.
  (parsec-many
   (parsec-and (parsec-ch ?\\)
               (parsec-or (cons 'art (parsec-many1-s (parsec-letter)))
                          (cons 'hairpin (parsec-one-of ?< ?> ?!))))))

(defun scorely-parse-note-attr ()
  (append (scorely-parse-note-sattr)
          (scorely-parse-note-lattr)
          (parsec-optional (scorely-parse-tie))))

(defun scorely-parse-note ()
  (scorely-tok
   (make-scorely-note :pc (cdr
                           (assoc (parsec-one-of ?a ?b ?c ?d ?e ?f ?g)
                                  scorely-map-pc))
                      :acc (parsec-or (scorely-parse-note-acc) 0)
                      :oct (parsec-or (scorely-parse-note-oct) 0)
                      :dur (parsec-optional (scorely-parse-note-dur))
                      :attr (scorely-parse-note-attr))))

(defun scorely-parse-rest ()
  (scorely-tok
   (parsec-and (parsec-ch ?r)
               (make-scorely-rest
                :dur (parsec-optional (scorely-parse-note-dur))
                ;; fermata may follow
                :attr (scorely-parse-note-lattr)))))

(defun scorely-parse-chord ()
  (scorely-tok
   (cl-list* 'Chord
             (parsec-between (parsec-ch ?<)
                             (parsec-ch ?>)
                             (parsec-many (scorely-parse-note)))
             (scorely-parse-note-dur)
             (scorely-parse-note-attr))))

(defun scorely-parse-tuplet ()
  (parsec-and (scorely-tok (parsec-str "\\tuplet"))
              (make-scorely-tuplet
               :frac (scorely-tok (cons (scorely-number)
                                        (parsec-and (parsec-ch ?/)
                                                    (scorely-number))))
               :notes (scorely-parse-mexp-braced))))

(defun scorely-parse-tie ()
  (parsec-try
   (parsec-and (scorely-spaces)
               (parsec-ch ?~)
               '((tie . t)))))

(defun scorely-parse-bar ()
  (and (scorely-tok (parsec-ch ?|))
       'bar))

;; (defun scorely-parse-cmd ()
;;   (scorely-tok (parsec-or )))

(defun scorely-parse-mexp1 ()
  (parsec-or (scorely-parse-note)
             (scorely-parse-rest)
             (scorely-parse-chord)
             (scorely-parse-tuplet)
             (scorely-parse-bar)
             (scorely-parse-voice)
             (scorely-parse-mexp)))

(defun scorely-parse-mexp-braced ()
  (parsec-between (scorely-tok (parsec-ch ?\{))
                  (scorely-tok (parsec-ch ?\}))
                  (parsec-many (scorely-parse-mexp1))))

(defun scorely-parse-relative ()
  (parsec-and (scorely-tok (parsec-str "\\relative"))
              (scorely-reify-mexp
               (scorely-tok
                (scorely-relative-note scorely-default-note
                                       (scorely-parse-note)))
               (scorely-parse-mexp-braced))))

(defun scorely-parse-assign ()
  (parsec-and (scorely-tok (parsec-ch ?=))
              (parsec-or (scorely-string)
                         (scorely-number))))

(defun scorely-parse-voice ()
  ;; \new Voice {}
  ;; \new Voice = "xxx" {}
  ;; \context Voice = "xxx" {}
  (parsec-and (scorely-tok
               (parsec-or (parsec-str "\\new")
                          (parsec-str "\\context")))
              (scorely-tok (parsec-str "Voice"))
              (make-scorely-voice :inst (parsec-optional (scorely-parse-assign))
                                  :notes (scorely-parse-mexp-braced))))

(defun scorely-parse-mexp-angled ()
  (parsec-between (scorely-tok (parsec-str "<<"))
                  (scorely-tok (parsec-str ">>"))
                  (parsec-many (scorely-parse-mexp))))

(defun scorely-parse-mexp ()
  (parsec-or (scorely-parse-relative)
             (scorely-parse-mexp-angled)
             (scorely-parse-mexp-braced)))

(defun scorely-tie-p (ly)
  (assq 'tie (scorely:attr ly)))

(defun scorely-reify-note (base note)
  (cond ((scorely-voice-p note)
         (setf (scorely-voice:notes note)
               (scorely-reify-mexp base (scorely-voice:notes note)))
         note)
        ((scorely-tuplet-p note)
         (setf (scorely-tuplet:notes note)
               (scorely-reify-mexp base (scorely-tuplet:notes note)))
         note)
        ((scorely-note-p note)
         (scorely-relative-note base note))
        ((scorely-rest-p note)
         (unless (scorely-rest:dur note)
           (setf (scorely-rest:dur note) (scorely:dur base)))
         note)
        (t note)))

(defun scorely-tie-notes (tied)
  (let ((note (copy-scorely-note (car tied))))
    (setf (scorely-note:dur note) (scorely:dur tied))
    note))

(defun scorely-reify-tie (base mexp)
  (let* ((next (scorely-reify-note base (car mexp)))
         (tied (list base next)))
    (while (scorely-tie-p next)
      (setq mexp (cdr mexp)
            next (scorely-reify-note next (car mexp)))
      (push next tied))
    (list (scorely-tie-notes tied) (copy-scorely-note next) (cdr mexp))))

(defun scorely-reify-mexp (base mexp)
  (let ((acc nil))
    (while mexp
      (let ((note (scorely-reify-note base (car mexp))))
        (cond ((scorely-tie-p note)
               (seq-let (note~ base~ mexp~) (scorely-reify-tie note (cdr mexp))
                 (push note~ acc)
                 (setq base base~
                       mexp mexp~)))
              (t
               (push note acc)
               (setq base (scorely-last-note base note)
                     mexp (cdr mexp))))))
    (nreverse acc)))

(defun scorely-last-note (l r)
  (cond ((or (scorely-note-p r))
         r)
        ((scorely-tuplet-p r)
         (cl-reduce #'scorely-last-note (scorely-tuplet:notes r)))
        ((scorely-tuplet-p l)
         (cl-reduce #'scorely-last-note (scorely-tuplet:notes l)))
        ((scorely-rest-p r)
         (let ((note (copy-scorely-note l)))
           (setf (scorely-note:dur note) (scorely:dur r))
           note))
        (t l)))

(defun scorely-parse ()
  (parsec-many (scorely-parse-mexp)))

(defun scorely-parse-file (file)
  "Parse simultaneous voices in Lilypond FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (parsec-parse (scorely-parse))))



;; Convert

(defun scorely-apply-frac (frac note)
  (pcase frac
    (`(,num . ,den)
      (cond ((scorely-note-p note)
             (setf (scorely-note:dur note)
                   (* (scorely-note:dur note) den (expt num -1))))
            ((scorely-rest-p note)
             (setf (scorely-rest:dur note)
                   (* (scorely-rest:dur note) den (expt num -1))))
            ((scorely-tuplet-p note)
             (setf (scorely-tuplet:frac note)
                   (pcase (scorely-tuplet:frac note)
                     (`(,num2 . ,den2)
                       (cons (* num num2) (* den den2)))))))
      note)))

(defun scorely-score1 (notes inst time)
  (let ((inst (or inst scorely-default-inst)))
    (while notes
      (let* ((note (car notes))
             (dur (scorely:dur note)))
        (cond ((scorely-note-p note)
               (insert (format "i %S %S %S %S %S\n"
                               inst time dur
                               (scorely-note:pitch note)
                               (scorely-note:dyn note))))
              ((scorely-tuplet-p note)
               (scorely-score1 (mapcar
                                (apply-partially #'scorely-apply-frac
                                                 (scorely-tuplet:frac note))
                                (scorely-tuplet:notes note))
                               inst time)))
        (setq notes (cdr notes)
              time (+ time dur))))))

(defun scorely-mainline (voices)
  (let ((voice (car voices)))
    (if (scorely-voice-p (car voice))
        (scorely-voice:notes (car voice))
      voice)))

(defun scorely-beats/measure (inst beats time)
  (dotimes (i beats)
    (insert (format "i %S %S 1 %d\n" inst (+ time i) (if (zerop i) 1 0))))
  (+ beats time))

(defun scorely-beat-score (inst beats measures)
  (let ((time (scorely-beats/measure inst beats 0)))
    (while measures
      (setq time (scorely-beats/measure inst
                                        (scorely:dur (car measures))
                                        time)
            measures (cdr measures)))))

(defun scorely-tempo-score ()
  )

(defun scorely-effect-score (inst dur)
  (insert (format "i %S 0 %S\n" inst dur)))

(defun scorely-score (time voices)
  (mapc (lambda (voice)
          (let ((voicep (scorely-voice-p (car voice))))
            (scorely-score1 (if voicep
                                (scorely-voice:notes (car voice))
                              voice)
                            (and voicep (scorely-voice:inst (car voice)))
                            time)))
        voices))

(defun scorely-score/metro (voices)
  (let* ((measures
          (-split-when (apply-partially #'eq 'bar) (scorely-mainline voices)))
         ;; For the initial "count in" clicks
         (beats (scorely:dur (car measures))))
    (scorely-effect-score "effect"
                          (+ (* 2 beats) (scorely:dur (car voices))))
    (scorely-beat-score "metro" beats measures)
    (scorely-score beats voices)))

;;;###autoload
(defun scorely-convert (file)
  "Convert Lilypond score FILE to CSound score."
  (interactive (list (read-file-name ".ly file: " nil buffer-file-name t)))
  (if-let ((voices (scorely-parse-file file)))
      (with-temp-buffer
        (scorely-score/metro voices)
        (write-region nil nil (concat (file-name-sans-extension file) ".sco")))
    (message "Parse failed.")))

(when nil
  (require 'scorely)
  (scorely-score/metro
   (parsec-with-input (or  "\\relative c' { c4~ c8~ c16  }"
                           "\\relative c' { c4 c c d | e2 \\tuplet 3/2 { c8 d e } f4 }")
     (scorely-parse))))

(provide 'scorely)
;;; scorely.el ends here
