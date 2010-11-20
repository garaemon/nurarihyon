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
   ;; util.lisp
   #:define-nhfun #:define-nhfun-setf #:optimize-function-name
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
   ;; in matrix.lisp, only export macros and fea functions here, because 
   ;; functions are exported by define-nhfun macro.
   #:eps-matrix=
   #:with-ensure-matrix-row-smaller-than
   #:with-ensure-matrix-column-smaller-than
   #:with-ensure-2matrices-transpose-dimension
   #:with-ensure-and-bind-square-matrix
   #:with-ensure-and-bind-2matrices-dimension
   #:with-ensure-and-bind-2matrices-multipable
   #:with-ensure-2matrices-dimensions
   #:with-ensure-matrix-dimensions
   ;; quaternion.lisp
   #:qx #:qy #:qz #:qw
   )
  )
