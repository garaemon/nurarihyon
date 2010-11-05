;;================================================
;; eps-vector.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)
(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test eps-vector=-test
  (lisp-unit:assert-true (nh:eps-vector= #d(1 2 3) #d(1 2 3)))
  (lisp-unit:assert-false (nh:eps-vector= #d(1 2 3) #d(1 2 4)))
  (lisp-unit:assert-true (nh:eps-vector= #d(1.1 2 3) #d(1 2 3) 0.11d0))
  (lisp-unit:assert-false (nh:eps-vector= #d(1.1 2 3) #d(1 2 4) 0.01d0))
  )

(nh:disable-nurarihyon-reader-syntax)
