;;================================================
;; vector-copy.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)
(nh:enable-nurarihyon-reader-syntax)
(lisp-unit:define-test copy-vector-test
  (let ((a #d(1 2 3 4 5)))
    (let ((b (nh:copy-vector a)))
      (dotimes (i (nh:vector-dimension a))
        (lisp-unit:assert-float-equal [a i] [b i]))
      (lisp-unit:assert-false (eq a b))))
  (let ((a #d(1 2 3 4 5))
        (b (nh:make-vector 5)))
    (let ((c (nh:copy-vector a b)))
      (dotimes (i (nh:vector-dimension a))
        (lisp-unit:assert-float-equal [a i] [c i]))
      (lisp-unit:assert-false (eq a b))
      (lisp-unit:assert-eq b c)))
  )

(lisp-unit:define-test copy-vector*-test
  (let ((a #d(-1 2 -3 -1 2))
        (b (nh:make-vector 5)))
    (let ((c (nh:copy-vector* a b)))
      (lisp-unit:assert-eq b c)
      (lisp-unit:assert-false (eq a b))
      (dotimes (i (nh:vector-dimension a))
        (lisp-unit:assert-float-equal [a i] [b i]))
      ))
  )

(nh:disable-nurarihyon-reader-syntax)
