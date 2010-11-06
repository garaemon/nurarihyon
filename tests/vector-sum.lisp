;;================================================
;; vector-sum.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test vector-sum-test
    (lisp-unit:assert-float-equal
     6.0d0
     (nh:vector-sum #d(1 2 3)))
    (lisp-unit:assert-float-equal
     0.0d0
     (nh:vector-sum #d(1 -1 3 -3 60 -60)))
    nil)

(nh:disable-nurarihyon-reader-syntax)
