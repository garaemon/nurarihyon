;;================================================
;; vector-distance.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test distance-test
  (let ((a (nurarihyon:double-vector 1 0 0)))
    (lisp-unit:assert-float-equal (nurarihyon:distance a a) 0.0))
  (dotimes (i 10)
    (let ((a (nurarihyon:double-vector
              (nurarihyon:random-range -100.0 100.0)
              (nurarihyon:random-range -100.0 100.0)
              (nurarihyon:random-range -100.0 100.0))))
      (lisp-unit:assert-float-equal (nurarihyon:distance a a) 0.0)))
  (dotimes (i 10)
    (let ((c (nurarihyon:double-vector 0 0 0))
	  (a (nurarihyon:double-vector
              (nurarihyon:random-range -100.0 100.0)
              (nurarihyon:random-range -100.0 100.0)
              (nurarihyon:random-range -100.0 100.0))))
      (lisp-unit:assert-float-equal
       (nurarihyon:distance a c) (nurarihyon:norm a))))
  )

(lisp-unit:run-tests distance-test)
