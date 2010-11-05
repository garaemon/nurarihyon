;;================================================
;; vector-create.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)
(nh:enable-nurarihyon-reader-syntax)
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

(lisp-unit:define-test make-vector3-test
  (let ((vec (nh:make-vector3)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 3)
    (lisp-unit:assert-float-equal [vec 0] 0.0d0)
    (lisp-unit:assert-float-equal [vec 1] 0.0d0)
    (lisp-unit:assert-float-equal [vec 2] 0.0d0))
  (let ((vec (nh:make-vector3 :initial-element 2.0d0)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 3)
    (lisp-unit:assert-float-equal [vec 0] 2.0d0)
    (lisp-unit:assert-float-equal [vec 1] 2.0d0)
    (lisp-unit:assert-float-equal [vec 2] 2.0d0))
  (let ((vec (nh:make-vector3 :initial-element -2.0d0)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 3)
    (lisp-unit:assert-float-equal [vec 0] -2.0d0)
    (lisp-unit:assert-float-equal [vec 1] -2.0d0)
    (lisp-unit:assert-float-equal [vec 2] -2.0d0)))

(lisp-unit:define-test make-vector4-test
  (let ((vec (nh:make-vector4)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 4)
    (lisp-unit:assert-float-equal [vec 0] 0.0d0)
    (lisp-unit:assert-float-equal [vec 1] 0.0d0)
    (lisp-unit:assert-float-equal [vec 2] 0.0d0)
    (lisp-unit:assert-float-equal [vec 3] 0.0d0))
  (let ((vec (nh:make-vector4 :initial-element 2.0d0)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 4)
    (lisp-unit:assert-float-equal [vec 0] 2.0d0)
    (lisp-unit:assert-float-equal [vec 1] 2.0d0)
    (lisp-unit:assert-float-equal [vec 2] 2.0d0)
    (lisp-unit:assert-float-equal [vec 3] 2.0d0))
  (let ((vec (nh:make-vector4 :initial-element -2.0d0)))
    (lisp-unit:assert-eq (nh:vector-dimension vec) 4)
    (lisp-unit:assert-float-equal [vec 0] -2.0d0)
    (lisp-unit:assert-float-equal [vec 1] -2.0d0)
    (lisp-unit:assert-float-equal [vec 2] -2.0d0)
    (lisp-unit:assert-float-equal [vec 3] -2.0d0)))
                                  
(nh:disable-nurarihyon-reader-syntax)
