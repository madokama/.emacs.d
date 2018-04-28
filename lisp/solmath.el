;;; solmath --- calculate misc. solar events -*- lexical-binding: t; -*-

;;; Commentary:

;; Taken from http://koyomi.vis.ne.jp/24sekki.htm

;;; Code:

(require 'seq)

(defvar solmath-julian-year 365.25)

(defvar solmath-julian-century (* solmath-julian-year 100))

(defvar solmath-circle-deg 360.0)

(defun solmath--delta-t (year)
  (+ 58 (* 0.44 (- year 1990))))

(defun solmath-days-february (year)
  (if (zerop (% year 4))
      (cond ((<= year 1582) 29)
            ((zerop (% year 400)) 29)
            ((zerop (% year 100)) 28)
            (t 29))
    28))

(defun solmath-list-month-days (year)
  (vector 31 (solmath-days-february year) 31 30 31 30 31 31 30 31 30 31))

;; jd for julian days
(defun solmath-ymd2jd (yy mm dd)
  (let ((yy-1 (1- yy))
        (days 1721422))
    (cl-incf days
             (+ (floor (+ (* solmath-julian-year yy-1) 0.1))
                (solmath-yeardays yy mm dd)))
    (when (>= days 2299160)
      (cl-decf days 10))
    (when (>= yy-1 1600)
      (cl-decf days (floor (/ (+ (- yy-1 1600) 0.1) 100)))
      (cl-incf days (floor (/ (+ (- yy-1 1600) 0.1) 400))))
    days))

(defvar solmath-sla
  [36000.7695 280.4659 1.9147 0.02 -0.0048 0.002 0.0018 0.0018 0.0015 0.0013 0.0007 0.0007 0.0007 0.0006 0.0005 0.0005 0.0004 0.0004])

(defvar solmath-slb
  [0 0 35999.05 71998.1 35999 32964 19 445267 45038 22519 65929 3035 9038 33718 155 2281 29930 31557])

(defvar solmath-slc
  [0 0 267.52 265.1 268 158 159 208 254 352 45 110 64 316 118 221 48 161])

(defun solmath-sunmlong (T)
  (let* ((T (/ T solmath-julian-century))
         (ans
          (cl-loop for i from 17 downto 0
                   for rad = (degrees-to-radians
                              (+ (* (aref solmath-slb i) T)
                                 (aref solmath-slc i)))
                   if (or (zerop i) (= i 4))
                     sum (* (aref solmath-sla i) T (cos rad))
                   else
                     sum (* (aref solmath-sla i) (cos rad)))))
    (- ans
       (* (floor (/ ans solmath-circle-deg))
          solmath-circle-deg))))


(defun solmath-sunlong (T)
  (let* ((dans (+ -0.0057
                  (* 0.0048
                     (cos (degrees-to-radians
                           (+ (/ (* 1934 T)
                                 solmath-julian-century)
                              145))))))
         (ans (+ (solmath-sunmlong T) dans)))
    (while (< ans 0.0)
      (cl-incf ans solmath-circle-deg))
    (while (>= ans solmath-circle-deg)
      (cl-decf ans solmath-circle-deg))
    ans))

;; long for longitude
(defun solmath-longdays (year deg)
  (let* ((t0 (- (solmath-ymd2jd year 1 0)
                (solmath-ymd2jd 2000 1 1.5)))
         (bsl (solmath-sunlong t0))
         (ofs (if (< deg bsl)
                  (* -1 solmath-circle-deg)
                0.0))
         (T (floor (* (- deg bsl ofs) 0.9))))
    (cl-loop for sl = (solmath-sunlong (+ T t0))
             if (< sl bsl)
               do (cl-incf bsl ofs)
             if (and (>= sl deg) (< bsl deg))
               do (cl-incf T (/ (- deg sl) (- sl bsl)))
                  (cl-incf T (/ (- deg (solmath-sunlong (+ T t0)))
                                (- sl bsl)))
                  (cl-return T)
             else
               do (setq bsl sl)
                  (cl-incf T)
             finally return T)))

(defun solmath-yeardays (yy mm dd)
  (+ (cl-reduce #'+
                (seq-take (solmath-list-month-days yy)
                          (1- mm)))
     dd))

(defun solmath-days2md (year days)
  (let (d)
    (cons (cl-loop for m downfrom 12 to 1
                   do (setq d (solmath-yeardays year m 1))
                   if (<= d days)
                     return m
                   finally return m)
          (1+ (- days d)))))

(defun solmath-solar-term (year deg)
  (let* ((dt (/ (solmath--delta-t year) 86400.0))
         (h (solmath-longdays year deg))
         (h (- (+ h (/ 9.0 24)) dt))
         (d (floor h))
         (h (floor (+ 0.5 (* 24 (- h d)))))
         (md (solmath-days2md year d)))
    (list :month (car md)
          :day (cdr md)
          :hour h)))

(provide 'solmath)
;;; solmath.el ends here
