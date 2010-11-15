;;================================================
;; matrix-create.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test make-matrix-test
  (let ((dim 10)
	(init-element 10.0d0))
    (let ((mat (nurarihyon:make-matrix dim dim :initial-element init-element)))
      (lisp-unit:assert-equal (list dim dim) (array-dimensions mat))
      (lisp-unit:assert-equal (list dim dim) (nh:matrix-dimensions mat))
      (lisp-unit:assert-eq dim (nh:matrix-row-dimension mat))
      (lisp-unit:assert-eq dim (nh:matrix-column-dimension mat))
      (dotimes (i dim)
	(dotimes (j dim)
	  (lisp-unit:assert-float-equal init-element (aref mat i j))))))
  (let ((r 2)
        (c 5)
        (init-element 30.0d0))
    (let ((mat (nh:make-matrix r c :initial-element init-element)))
      (lisp-unit:assert-equal (list r c) (array-dimensions mat))
      (lisp-unit:assert-equal (list r c) (nh:matrix-dimensions mat))
      (lisp-unit:assert-eq r (nh:matrix-row-dimension mat))
      (lisp-unit:assert-eq c (nh:matrix-column-dimension mat))
      (dotimes (i r)
        (dotimes (j c)
          (lisp-unit:assert-float-equal init-element (aref mat i j))))
      )))




