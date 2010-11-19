;;================================================
;; base.lisp
;;
;; all of the basic operators and constructors
;; are defined for real, integer(fixnum), float(single-float)
;; and double(double-float).
;;
;; written by R.Ueda (garaemon)
;;================================================

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

;; for utility functions, not exported
(defmacro -== (a b)
  `(setf ,a (- ,a ,b)))

(defmacro +== (a b)
  `(setf ,a (+ ,a ,b)))

(defmacro *== (a b)
  `(setf ,a (* ,a ,b)))

(defmacro /== (a b)
  `(setf ,a (/ ,a ,b)))

;; utility
(declaim-inline-nhfun rad2deg)
(define-nhfun rad2deg (rad)
  "convert from radian to degree"
  (declare (type double-float rad))
  (the double-float (* rad (/ 360.0d0 +2pi+))))

(declaim-inline-nhfun deg2rad)
(define-nhfun deg2rad (deg)
  "convert from degree to radian"
  (declare (type double-float deg))
  (the double-float (* deg (/ +2pi+ 360.d0))))

(declaim-inline-nhfun random-range)
(define-nhfun random-range (min max)
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
(declaim-inline-nhfun eps=)
(define-nhfun eps= (a b &optional (diff +eps+))
  "compare two double-float values , A and B, and if the difference between
the two values within DIFF, return T.

+eps+ is used as the default DIFF value."
  (declare (type double-float a b diff))
  (the symbol (< (abs (- a b)) diff)))

(declaim-inline-nhfun mean)
(define-nhfun mean (args)
  "calculate the mean of ARGS."
  (declare (type list args))
  (/ (apply #'+ args) (length args)))

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
