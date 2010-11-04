;;================================================
;; vector-create.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test make-vector-test
  (let ((dim 10)
	(init-element 10.0d0))
    (let ((vec (nurarihyon:make-vector dim :initial-element init-element)))
      ;; check dimension
      (lisp-unit:assert-equal (list dim) (array-dimensions vec))
      (dotimes (i dim)
        ;; check elements
	(lisp-unit:assert-float-equal init-element (aref vec i))))))

(lisp-unit:define-test double-vector-test
  (dotimes (i 10)
    (let ((a (nurarihyon:random-range -100.0d0 100.0d0))
          (b (nurarihyon:random-range -100.0d0 100.0d0))
          (c (nurarihyon:random-range -100.0d0 100.0d0)))
      (let ((vec (nurarihyon:double-vector a b c)))
        (lisp-unit:assert-float-equal a (aref vec 0))
        (lisp-unit:assert-float-equal b (aref vec 1))
        (lisp-unit:assert-float-equal c (aref vec 2))))
    ))



