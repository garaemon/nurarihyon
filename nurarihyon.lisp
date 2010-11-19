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
  (:documentation
   "nurarihyon is a mathematical computation library fully written
on CommonLisp. ")
  (:export
   ;; conditions.lisp
   #:vector-dimension-mismatch
   #:matrix-dimensions-mismatch
   ;; syntax.lisp
   #:enable-nurarihyon-reader-syntax #:disable-nurarihyon-reader-syntax
   ;; base.lisp
   ;; constants
   #:+e+
   #:+pi+ #:+2pi+ #:+pi/2+ #:+pi/4+
   #:+-pi+ #:+-2pi+ #:+-pi/2+ #:+-pi/4+
   #:+eps+
   ;; functions
   #:->double-float #:->single-float
   #:eps= #:mean
   ;; in vector.lisp, only export macros here, because 
   ;; functions are exported by define-nhfun macro.
   #:with-ensure-2vectors-dimension #:with-ensure-2vectors-dimension*
   #:with-2vector-dimension-bind-and-check #:with-ensure-vector-dimension
   ;; matrix.lisp
   #:make-matrix #:double-matrix
   #:make-matrix33 #:make-identity-matrix3
   #:make-matrix44 #:make-identity-matrix4
   #:make-identity-matrix
   #:matrix-dimensions #:matrix-row-dimension #:matrix-column-dimension
   #:copy-matrix #:copy-matrix*
   #:m+ #:m- #:m* #:mv*
   #:transpose
   #:lu-decompose
   #:inverse-matrix #:m-1
   #:matrix-determinant #:matrix22-determinant
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
