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
  (:export
   ;; base.lisp
   #:make-vector #:make-integer-vector #:make-float-vector #:make-double-vector
   #:make-matrix #:make-integer-matrix #:make-float-matrix #:make-double-matrix
   #:make-identity-matrix #:make-integer-identity-matrix
   #:make-float-identity-matrix #:make-double-identity-matrix
   #:real-vector #:integer-vector #:float-vector #:double-vector
   #:copy-vector #:copy-integer-vector #:copy-float-vector #:copy-double-vector

   #:v+ #:iv+ #:fv+ #:dv+
   #:v- #:iv- #:fv- #:dv-
   #:eps-vector= #:feps-vector= #:deps-vector= #:ieps-vector=
   )
  )
