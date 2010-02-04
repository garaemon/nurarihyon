;;================================================
;; vector-cross.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test v*-test
  ;; check cross product by using dot product
  (let ((a (nurarihyon:double-vector 1 2 3))
	(b (nurarihyon:double-vector 4 5 6)))
    (let ((c (nurarihyon:v* a b)))
      (lisp-unit:assert-float-equal (nurarihyon:v. a c) 0.0)
      (lisp-unit:assert-float-equal (nurarihyon:v. b c) 0.0)
      ))
  (let ((a (nurarihyon:double-vector -1 2 -3))
	(b (nurarihyon:double-vector 4 -5 6)))
    (let ((c (nurarihyon:v* a b)))
      (lisp-unit:assert-float-equal (nurarihyon:v. a c) 0.0)
      (lisp-unit:assert-float-equal (nurarihyon:v. b c) 0.0)
      ))
  (let ((a (nurarihyon:double-vector 1 2 3)))
    (lisp-unit:assert-true
     (nurarihyon:eps-vector=
      (nurarihyon:v* a a) (nurarihyon:double-vector 0.0 0.0 0.0))))
  (let ((a (nurarihyon:double-vector 4 5 6)))
    (lisp-unit:assert-true
     (nurarihyon:eps-vector=
      (nurarihyon:v* a a) (nurarihyon:double-vector 0.0 0.0 0.0))))
  )

(lisp-unit:run-tests v*-test)
