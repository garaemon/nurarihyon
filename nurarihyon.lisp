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
   #:make-vector3 #:make-vector4
   #:copy-vector #:copy-vector*
   #:v+ #:v- #:v. #:v*
   #:vscale
   #:norm
   #:distance
   #:normalize-vector
   #:eps-vector=
   #:vector-sum
   #:make-random-vector
   #:with-vector-dimension-bind-and-check
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
   ;; quaternion.lisp
   #:matrix33->quaternion
   #:quaternion->matrix33
   #:quaternion-axis
   #:quaternion-angle
   #:quaternion-conjugate
   #:identity-quaternion
   ;; geometry.lisp
   #:+x-axis+ #:+y-axis+ #:+z-axis+
   #:rotation-matrix
   ;;#:rotate-matrix
   #:rotate-matrix-local #:rotate-matrix-world
   #:euler-matrix #:euler-angle
   #:rpy-matrix #:rpy-angle
   #:axis->vec
   )
  )
