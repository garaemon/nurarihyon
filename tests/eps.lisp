;;================================================
;; eps.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)

(lisp-unit:define-test eps=-test
  (lisp-unit:assert-true (nurarihyon:eps= 10.0d0 10.0d0))
  (lisp-unit:assert-false (nurarihyon:eps= 10.0d0 9.9d0))
  (lisp-unit:assert-true (nurarihyon:eps= 10.0d0 9.1d0 1.0d0))
  (lisp-unit:assert-true (nurarihyon:eps= 10.0d0 10.9d0 1.0d0))
  (lisp-unit:assert-false (nurarihyon:eps= 10.0d0 11.1d0 1.0d0)))

