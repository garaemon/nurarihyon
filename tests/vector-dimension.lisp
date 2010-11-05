;;================================================
;; vector-dimension.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test vector-dimension-test
  (lisp-unit:assert-eq (nh:vector-dimension #d(1 2))
                       2)
  (lisp-unit:assert-eq (nh:vector-dimension #d(1 2 3 4))
                       4)
  (lisp-unit:assert-eq (nh:vector-dimension #d(1))
                       1))

(nh:disable-nurarihyon-reader-syntax)