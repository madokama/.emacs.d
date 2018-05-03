;;; 9stars --- Implement Chinese cosmological theory -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))
(require 'cl-lib)
(require 'cl-seq)
(require 'solmath)

;;; Utility functions

(defun 9stars--rotate (lst seekfn n)
  (seq-take (funcall seekfn (setcdr (last lst) lst))
            n))

(defsubst 9stars--plist-animal (branch)
  (plist-get branch :animal))

(defsubst 9stars--plist-kanji (plst)
  (plist-get plst :kanji))

(defsubst 9stars--plist-phase (plst)
  (plist-get plst :phase))

(defvar 9stars-phase-kanji-alist)

(defun 9stars--phase-kanji (plst)
  (alist-get (9stars--plist-phase plst) 9stars-phase-kanji-alist))

(defsubst 9stars--plist-yomi (stem)
  (plist-get stem :yomi))

(defun 9stars-stem= (a b)
  (eq (9stars--plist-yomi a) (9stars--plist-yomi b)))

(defvar 9stars-heavenly-stems)

(defun 9stars--stem-position (stem)
  (cl-position-if (apply-partially #'9stars-stem= stem)
                  9stars-heavenly-stems))

(defsubst 9stars--stem-ref (pos)
  (aref 9stars-heavenly-stems (% pos 10)))

(defvar 9stars-earthly-branches)

(defsubst 9stars--branch-ref (pos)
  (aref 9stars-earthly-branches (% pos 12)))

(defun 9stars-branch= (a b)
  (eq (9stars--plist-animal a) (9stars--plist-animal b)))

(defvar 9stars-animal-aliases)

(defun 9stars--normalize-branch (animal)
  (or (alist-get animal 9stars-animal-aliases)
      animal))

(defun 9stars--branch-position (branch)
  (cl-position-if (apply-partially #'9stars-branch= branch)
                  9stars-earthly-branches))


;;; Constants

(defvar 9stars-phases
  '#1=(wood fire earth metal water . #1#)
  "Circular list of Chinese five elements/phases, represented in
the generative order.")

(defvar 9stars-phase-star-alist
  (cl-mapcar #'cons
             (seq-take 9stars-phases 5)
             '(((3 :color blue :kanji 三碧木星) (4 :color green :kanji 四緑木星))
               ((9 :color purple :kanji 九紫火星))
               ((2 :color black :kanji 二黒土星)
                (5 :color yellow :kanji 五黄土星)
                (8 :color white :kanji 八白土星))
               ((6 :color white :kanji 六白金星) (7 :color red :kanji 七赤金星))
               ((1 :color white :kanji 一白水星)))))

(defvar 9stars-phase-kanji-alist
  (cl-mapcar #'cons
             (seq-take 9stars-phases 5)
             '(:木 :火 :土 :金 :水)))

(defvar 9stars-months
  (9stars--rotate (number-sequence 1 12) #'cdr 12))

(defvar 9stars-heavenly-stems
  [(:kanji 甲 :yomi kinoe :phase wood :yang t)
   (:kanji 乙 :yomi kinoto :phase wood :yang nil)
   (:kanji 丙 :yomi hinoe :phase fire :yang t)
   (:kanji 丁 :yomi hinoto :phase fire :yang nil)
   (:kanji 戊 :yomi tsuchinoe :phase earth :yang t)
   (:kanji 己 :yomi tsuchinoto :phase earth :yang nil)
   (:kanji 庚 :yomi kanoe :phase metal :yang t)
   (:kanji 辛 :yomi kanoto :phase metal :yang nil)
   (:kanji 壬 :yomi mizunoe :phase water :yang t)
   (:kanji 癸 :yomi mizunoto :phase water :yang nil)])

(defvar 9stars-earthly-branches
  [(:kanji 子 :animal rat :phase water :yang t)
   (:kanji 丑 :animal ox :phase earth :yang nil)
   (:kanji 寅 :animal tiger :phase wood :yang t)
   (:kanji 卯 :animal rabbit :phase wood :yang nil)
   (:kanji 辰 :animal dragon :phase earth :yang t)
   (:kanji 巳 :animal snake :phase fire :yang nil)
   (:kanji 午 :animal horse :phase fire :yang t)
   (:kanji 未 :animal goat :phase earth :yang nil)
   (:kanji 申 :animal monkey :phase metal :yang t)
   (:kanji 酉 :animal rooster :phase metal :yang nil)
   (:kanji 戌 :animal dog :phase earth :yang t)
   (:kanji 亥 :animal pig :phase water :yang nil)])

(defvar 9stars-animal-aliases
  '((sheep . goat)
    (boar . pig)))

(defvar 9stars-yeargroup-alist
  (cl-loop for i from 0 to 11
           with 1st = '(:1st)
           and 2nd = '(:2nd)
           and 3rd = '(:3rd)
           do (nconc (cl-case (% i 3)
                       (0 1st)
                       (1 2nd)
                       (2 3rd))
                     (list (9stars--plist-animal (9stars--branch-ref i))))
           finally return (list 1st 2nd 3rd)))

(defvar 9stars-yeargroup-maps
  (cl-mapcar (lambda (phases yeargroup)
               (cons (car yeargroup)
                     (cl-loop for m in 9stars-months
                              for p in phases
                              collect (cons m p))))
             (seq-partition (9stars--rotate (nreverse (number-sequence 1 9))
                                            #'cdr
                                            (* 3 12))
                            12)
             9stars-yeargroup-alist))

(defvar 9stars-solar-longitudes
  (cl-mapcar #'cons
             9stars-months
             (9stars--rotate (cl-loop for i from 0 to 11
                                      collect (+ 15 (* i 30)))
                             (apply-partially #'memq 315)
                             12)))


;;; Solar term functions

(defun 9stars-prev-year-p (year month day)
  ;; Check whether the given date is earlier than lichun of the year.
  (let ((term
         (solmath-solar-term year (alist-get 2 9stars-solar-longitudes))))
    (or (< month (plist-get term :month))
        (and (= month (plist-get term :month))
             (< day (plist-get term :day))))))

(defun 9stars-prev-month-p (year month day)
  (let ((term
         (solmath-solar-term year (alist-get month 9stars-solar-longitudes))))
    (< day (plist-get term :day))))

(defun 9stars-solar-year (year month day)
  (if (9stars-prev-year-p year month day)
      (1- year)
    year))

(defun 9stars-solar-month (year month day)
  (list :year (9stars-solar-year year month day)
        :month (if (9stars-prev-month-p year month day)
                   (if (= month 1)
                       12
                     (1- month))
                 month)))

;;; Basic wuxing functions

(defun 9stars-number-star (n)
  (cl-loop for (phase . alst) in 9stars-phase-star-alist
           if (assq n alst)
             return (cl-list* :phase phase
                              :number (assq n alst))))

;; TODO handle year overlaps

(defun 9stars-year-star (year)
  (9stars-number-star
   (- 11 (pcase (% year 9)
           (0 9)
           (1 10)
           (n n)))))

(defun 9stars-month-star (animal month)
  (when-let* ((animal (9stars--normalize-branch animal))
              (group
               (car (cl-find-if (pcase-lambda (`(,_group . ,animals))
                                  (memq animal animals))
                                9stars-yeargroup-alist)))
              (number
               (alist-get month (alist-get group 9stars-yeargroup-maps))))
    (9stars-number-star number)))

(defun 9stars-year-stem (year)
  (9stars--stem-ref (+ year 6)))

(defun 9stars-year-branch (year)
  (9stars--branch-ref (+ year 8)))

(defun 9stars-month-stem (year month)
  (9stars--stem-ref
   (+ month
      (cl-case (% year 10)
        ((4 9) 0)
        ((5 0) 2)
        ((6 1) 4)
        ((7 2) 6)
        ((8 3) 8)))))

(defun 9stars-month-branch (month)
  (9stars--branch-ref month))

(defun 9stars-day-stem (year month day)
  (9stars--stem-ref (solmath-ymd2jd year month day)))

(defun 9stars-day-branch (year month day)
  (9stars--branch-ref (+ (solmath-ymd2jd year month day) 2)))

;;; Relation functions

(defun 9stars-relative-phases (phase)
  (seq-let (_self c gc gp p) (memq phase 9stars-phases)
    (list :child c :parent p            ; generative cycle
          :grandchild gc :grandparent gp ; overcoming cycle
          )))

(defun 9stars-phase-generative-p (a b)
  (let ((p (9stars-relative-phases a)))
    (or (and (eq b (plist-get p :child))
             '->)
        (and (eq b (plist-get p :parent))
             '<-))))

(defun 9stars-phase-overcoming-p (a b)
  (let ((p (9stars-relative-phases a)))
    (or (and (eq b (plist-get p :grandchild))
             '->)
        (and (eq b (plist-get p :grandparent))
             '<-))))

(defun 9stars-phase-resonant-p (a b)
  (eq a b))

(defun 9stars-star-generative-p (a b)
  (when-let ((arrow
              (9stars-phase-generative-p (9stars--plist-phase a)
                                         (9stars--plist-phase b))))
    (list :相生 (list arrow (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-star-overcoming-p (a b)
  (when-let ((arrow
              (9stars-phase-overcoming-p (9stars--plist-phase a)
                                         (9stars--plist-phase b))))
    (list :相剋 (list arrow (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-star-resonant-p (a b)
  (when (9stars-phase-resonant-p (9stars--plist-phase a)
                                 (9stars--plist-phase b))
    (list :比和 (list '= (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-star-relation (a b)
  (or (9stars-star-generative-p a b)
      (9stars-star-overcoming-p a b)
      (9stars-star-resonant-p a b)))

(defun 9stars-stem-combination (stem)
  ;; http://www.piano.or.jp/report/04ess/kyusei/2018/01/15_24151.html
  (9stars--stem-ref (+ (9stars--stem-position stem) 5)))

(defun 9stars-stem-combination-p (a b)
  (when (9stars-stem= (9stars-stem-combination a) b)
    (list :干合 (list (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-branch-combination/parallel (branch)
  ;; Compute BRANCH's 支合 (parallel combination):
  ;; http://www.piano.or.jp/report/04ess/kyusei/2018/01/19_24152.html
  (9stars--branch-ref (- 13 (9stars--branch-position branch))))

(defun 9stars-branch-combination/parallel-p (a b)
  (when (9stars-branch= (9stars-branch-combination/parallel a) b)
    (list :支合 (list (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-branch-combination/triangle (branch)
  (let* ((pos (9stars--branch-position branch))
         (peers (list (+ pos 4) (+ pos 8))))
    (list :phase (9stars--branch-ref
                  (cl-find-if (lambda (p)
                                (zerop (% p 3)))
                              (cons pos peers)))
          :peers (mapcar #'9stars--branch-ref peers))))

(defun 9stars-branch-combination/triangle-p (a b)
  (let ((triangle (9stars-branch-combination/triangle a)))
    (when (cl-find-if (apply-partially #'9stars-branch= b)
                      (plist-get triangle :peers))
      (list :三合
            (list (9stars--phase-kanji (9stars--plist-phase triangle))
                  (9stars--plist-kanji a)
                  (9stars--plist-kanji b))))))

(defun 9stars-branch-clash/opposite (branch)
  (9stars--branch-ref (+ (9stars--branch-position branch) 6)))

(defun 9stars-branch-clash/opposite-p (a b)
  ;; http://www.piano.or.jp/report/04ess/kyusei/2018/02/10_24154.html
  (when (9stars-branch= (9stars-branch-clash/opposite a) b)
    (list :冲 (list (9stars--plist-kanji a) (9stars--plist-kanji b)))))

(defun 9stars-branch-relations (a b)
  (nconc (9stars-branch-combination/parallel-p a b)
         (9stars-branch-combination/triangle-p a b)
         (9stars-branch-clash/opposite-p a b)))

;;; Database

(defun 9stars--parse-month (month)
  (pcase month
    ("January" 1)
    ("February" 2)
    ("March" 3)
    ("April" 4)
    ("May" 5)
    ("June" 6)
    ("July" 7)
    ("August" 8)
    ("September" 9)
    ("October" 10)
    ("November" 11)
    ("December" 12)))

(defun 9stars--parse-date (date)
  (save-match-data
    (cond ((string-match (rx bos
                             (group (+ digit)) space
                             (group (+ alpha)) (opt ?,) space
                             (group (+ digit)) eos)
                         date)
           (list (string-to-number (match-string 3 date))
                 (9stars--parse-month (match-string 2 date))
                 (string-to-number (match-string 1 date))))
          ((string-match (rx bos
                             (group (+ alpha)) space
                             (group (+ digit)) (opt ?,) space
                             (group (+ digit))
                             eos)
                         date)
           (list (string-to-number (match-string 3 date))
                 (9stars--parse-month (match-string 1 date))
                 (string-to-number (match-string 2 date))))
          ((string-match (rx bos
                             (group (+ digit)) (or ?年 ?.)
                             (group (+ digit)) (or ?月 ?.)
                             (group (+ digit)) (or ?日 (opt ?.)))
                         date)
           (mapcar (lambda (n)
                     (string-to-number (match-string n date)))
                   (list 1 2 3))))))

(defun 9stars--parse-entry (data)
  (pcase data
    (`(,name ,y ,m ,d)
      (list (string-trim name) y m d))
    (`(,name ,date)
      (cons (string-trim name)
            (9stars--parse-date (string-trim date))))
    (data (message "[9s] Parse failed: %S" data)
          nil)))

(defun 9stars--read-db1 ()
  (condition-case nil
      (read (current-buffer))
    (end-of-file nil)))

(defun 9stars-read-db (db)
  (with-current-buffer (find-file-noselect db)
    (save-excursion
      (goto-char (point-min))
      (delq nil
            (cl-loop until (eobp)
                     collect (9stars--read-db1))))))

;;; Entry point

;;;###autoload
(defun 9stars-birtyday-relations (a b)
  (nconc (list (format "%s - %s" (plist-get a :name) (plist-get b :name)))
         (9stars-star-relation
          (plist-get a :year-star) (plist-get b :year-star))
         (9stars-stem-combination-p
          (plist-get a :day-stem) (plist-get b :day-stem))
         (9stars-branch-relations
          (plist-get a :year-branch) (plist-get b :year-branch))))

;;;###autoload
(defun 9stars-birthday-profile (data)
  (pcase (9stars--parse-entry data)
    (`(,name ,year ,month ,day)
      (let* ((solar (9stars-solar-month year month day))
             (solar-year (plist-get solar :year))
             (solar-month (plist-get solar :month))
             (year-branch (9stars-year-branch solar-year)))
        (list :name name
              :year-star (9stars-year-star solar-year)
              :month-star
              (9stars-month-star (9stars--plist-animal year-branch) solar-month)
              :year-stem (9stars-year-stem solar-year)
              :year-branch year-branch
              :month-stem (9stars-month-stem solar-year solar-month)
              :month-branch (9stars-month-branch solar-month)
              ;; NOTE Use normal gregorian year and month for the following
              :day-stem (9stars-day-stem year month day)
              :day-branch (9stars-day-branch year month day))))))

(provide '9stars)
;;; 9stars.el ends here
