;;================================================
;; vector-scale.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test vscale-test
  (let ((a #d(1 2 3 4 5))
        (scale 2.0d0))
    (let ((b (nh:vscale scale a)))
      (lisp-unit:assert-float-equal (* scale [a 0]) [b 0])
      (lisp-unit:assert-float-equal (* scale [a 1]) [b 1])
      (lisp-unit:assert-float-equal (* scale [a 2]) [b 2])
      (lisp-unit:assert-float-equal (* scale [a 3]) [b 3])
      (lisp-unit:assert-float-equal (* scale [a 4]) [b 4])
      (lisp-unit:assert-false (eq a b)) ;a and b is not the same instance
      )))

(lisp-unit:define-test vscale-buffer-test
  (let ((a #d(-1 2 -3 4 -5))
        (buf (nh:make-vector 5))
        (scale 4.0d0))
    (let ((b (nh:vscale scale a buf)))
      (lisp-unit:assert-float-equal (* scale [a 0]) [b 0])
      (lisp-unit:assert-float-equal (* scale [a 1]) [b 1])
      (lisp-unit:assert-float-equal (* scale [a 2]) [b 2])
      (lisp-unit:assert-float-equal (* scale [a 3]) [b 3])
      (lisp-unit:assert-float-equal (* scale [a 4]) [b 4])
      (lisp-unit:assert-false (eq a b)) ;a and b is not the same instance
      (lisp-unit:assert-eq b buf) ;b and buf is the same instance
      )))

(lisp-unit:define-test vscale-same-buffer-test
  (let ((a #d(-1 2 -3 4 -5))
        (scale 4.0d0))
    (let ((a-buf (nh:copy-vector a)))
      (let ((b (nh:vscale scale a-buf a-buf)))
        (lisp-unit:assert-float-equal (* scale [a 0]) [b 0])
        (lisp-unit:assert-float-equal (* scale [a 1]) [b 1])
        (lisp-unit:assert-float-equal (* scale [a 2]) [b 2])
        (lisp-unit:assert-float-equal (* scale [a 3]) [b 3])
        (lisp-unit:assert-float-equal (* scale [a 4]) [b 4])
        (lisp-unit:assert-eq a-buf b) ;b and buf is the same instance
        ))))

(nh:disable-nurarihyon-reader-syntax)
