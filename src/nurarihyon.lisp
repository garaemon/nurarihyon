;;================================================
;; nurarihyon.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (debug 0)
		   (safety 0)
		   (compilation-speed 0)
		   (space 0)))

(defpackage :nurarihyon
  (:use
   #:common-lisp)
  (:export
   #:make-integer-vector #:make-float-vector
   #:make-float-matrix
   #:make-identity-matrix
   #:float-vector
   #:v+ #:v- #:v* #:v.
   #:norm #:distance
   #:copy-vector
   #:m+ #:m- #:m* #:m-1
   #:copy-matrix
   #:flip
   #:mv*
   #:eps= #:eps-vector=
   #:eps-matrix=
   #:rad2deg
   #:deg2rad
   #:+pi+ #:+pi/2+ #:+pi/4+ #:+2pi+
   #:+e+
   #:list->vector #:list->matrix
   #:print-matrix
   #:rotation-matrix #:rotate-matrix
   #:euler-matrix #:rpy-matrix
   #:+x-axis+ #:+y-axis+ #:+z-axis+
   #:axis->vec
   )
  )
