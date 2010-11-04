;;================================================
;; base.lisp
;;
;; all of the basic operators and constructors
;; are defined for real, integer(fixnum), float(single-float)
;; and double(double-float).
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 1)
		   (space 0)))

(in-package :nurarihyon)

;; constant
(alexandria:define-constant +e+ (exp 1.0d0) :test #'=
                            :documentation "Euler's number")
(alexandria:define-constant +pi+ pi :test #'=
                            :documentation "ratio of any circles :math:`\pi`")
(alexandria:define-constant +-pi+ (- pi) :test #'=
                            :documentation "negative value of pi")
(alexandria:define-constant +2pi+ (* 2.0d0 +pi+) :test #'=
                            :documentation "the value of 2 * pi")
(alexandria:define-constant +-2pi+ (- +2pi+) :test #'=)
(alexandria:define-constant +pi/2+ (/ +pi+ 2.0d0) :test #'=)
(alexandria:define-constant +-pi/2+ (/ +-pi+ 2.0d0) :test #'=)
(alexandria:define-constant +pi/4+ (/ +pi+ 4.0d0) :test #'=)
(alexandria:define-constant +-pi/4+ (/ +-pi+ 4.0d0) :test #'=)
(alexandria:define-constant +eps+ 0.0001d0 :test #'=)

(eval-when (:compile-toplevel)
  (enable-nurarihyon-reader-syntax))

;; operator utility
(defmacro with-array-dimension-check ((a b) &rest args)
  ;;  check dimensions of vecs are equal or not
  `(if (equal (array-dimensions ,a) (array-dimensions ,b))
       (progn ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimensions-bind-and-check ((dims a b) &rest args)
  ;;  check dimensions of vecs are equal or not
  `(let ((,dims (array-dimensions ,a)))
     (if (equal ,dims (array-dimensions ,b))
         (progn ,@args)
         (error "vector dimension mismatch"))))

(defmacro with-array-dimension-check* ((vecs dim) &rest args)
  ;;  check dimensions of vecs are equal to dim or not
  `(if (and (equal (array-dimensions ,(car vecs)) ,dim)
            (equal (array-dimensions ,(cadr vecs)) ,dim))
       (progn ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans ((a b) &rest args)
  ;;  The number of vecs must be two.
  `(if (= (cadr (array-dimensions ,a))
	  (car (array-dimensions ,b)))
       (progn ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans* ((a b) dim &rest args)
  ;;  The number of vecs must be two.
  `(if (and (equal (array-dimensions ,a) ,dim)
            (equal (reverse (array-dimensions ,b)) ,dim))
       (progn ,@args)
       (error "vector dimension mismatch")))

;; for utility functions
(defmacro -== (a b)
  `(setf ,a (- ,a ,b)))

(defmacro +== (a b)
  `(setf ,a (+ ,a ,b)))

(defmacro *== (a b)
  `(setf ,a (* ,a ,b)))

(defmacro /== (a b)
  `(setf ,a (/ ,a ,b)))

;; utility
(defun rad2deg (rad)
  "convert from radian to degree"
  (declare (type double-float rad))
  (the double-float (* rad (/ 360.0d0 +2pi+))))

(defun deg2rad (deg)
  "convert from degree to radian"
  (declare (type double-float deg))
  (the double-float (* deg (/ +2pi+ 360.d0))))

;; (defun list->vector (list)
;;   "convert list to float vector"
;;   (coerce list '(array single-float 1)))

;; (defun vector->list (vec)
;;   "convert vector to list"
;;   (coerce vec 'cons))

;; ;; あほい
;; (defun list->matrix (list)
;;   "convert list to matrix."
;;   (let ((mat (make-float-matrix (length list) (length (car list)))))
;;     (dotimes (i (length list))
;;       (dotimes (j (length (car list)))
;;         (setf (aref mat i j) (coerce (elt (elt list i) j) 'single-float))))
;;     mat))

;; ;; あほい
;; (defun matrix->list (mat)
;;   (let* ((dims (array-dimensions mat))
;;          (row (car dims))
;;          (col (cadr dims)))
;;     (let ((ret nil))
;;     (dotimes (i row)
;;       (push (coerce (matrix-row mat i) 'cons) ret))
;;     (nreverse ret))))
    
(defun random-range (min max)
  "return random value between min and max."
  (declare (type number min max))
  (let ((d (- max min)))
    (declare (type real d))
    (+ (random d) min)))

(declaim (inline ->double-float))
(defun ->double-float (val)
  "convert VAL to double-float"
  (coerce val 'double-float))

(declaim (inline ->single-float))
(defun ->single-float (val)
  "convert VAL to single-float"
  (coerce val 'single-float))

;; eps=
(declaim (inline eps=))
(defun eps= (a b &optional (diff +eps+))
  (declare (type double-float a b diff))
  (the symbol (< (abs (- a b)) diff)))

(defun mean (list)
  (declare (type list list))
  (/ (apply #'+ list) (length list)))

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
