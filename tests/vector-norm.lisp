;;================================================
;; vector-norm.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test norm-test
  (let ((a (nurarihyon:double-vector 1 0 0)))
    (lisp-unit:assert-float-equal (nurarihyon:norm a) 1.0))
  (let ((a (nurarihyon:double-vector 2 0 0)))
    (lisp-unit:assert-float-equal (nurarihyon:norm a) 2.0))
  (let ((a (nurarihyon:double-vector -2 0 0)))
    (lisp-unit:assert-float-equal (nurarihyon:norm a) 2.0))
  )

(lisp-unit:run-tests norm-test)

