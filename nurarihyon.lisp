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
  (:use #:common-lisp)
  (:shadow #:norm)
  (:nicknames :nh)
  (:export
   ;; syntax.lisp
   #:enable-nurarihyon-reader-syntax #:disable-nurarihyon-reader-syntax
   ;; base.lisp
   #:+e+
   #:+pi+ #:+2pi+ #:+pi/2+ #:+pi/4+
   #:+-pi+ #:+-2pi+ #:+-pi/2+ #:+-pi/4+
   #:+eps+
   #:rad2deg #:deg2rad
   #:random-range
   #:->double-float #:->single-float
   #:eps= #:mean
   ;; vector.lisp
   #:vector-dimension
   #:make-vector #:double-vector
   #:make-vector3
   #:real-vector
   #:copy-vector
   #:v+ #:v- #:v. #:v*
   #:scale
   #:norm
   #:distance
   #:vector-mean
   #:eps-vector=
   #:make-random-vector
   ;; matrix.lisp
   #:make-matrix #:double-matrix
   #:make-matrix33 #:make-identity-matrix3
   #:make-identity-matrix
   #:copy-matrix
   #:m+ #:m- #:m* #:mv*
   #:transpose
   #:lu-decompose
   #:inverse-matrix #:m-1
   #:matrix-determinant
   #:matrix-column #:matrix-row #:matrix-diagonal
   #:eps-matrix=
   ;; geometry.lisp
   #:+x-axis+ #:+y-axis+ #:+z-axis+
   #:rotation-matrix
   #:rotate-matrix
   #:euler-matrix
   #:rpy-matrix
   #:rpy-angle
   #:axis->vec
   )
  )
