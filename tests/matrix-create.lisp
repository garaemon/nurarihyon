;;================================================
;; matrix-create.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test make-matrix-test
  (let ((dim 10)
	(init-element 10.0d0))
    (let ((vec (nurarihyon:make-matrix dim dim :initial-element init-element)))
      (lisp-unit:assert-equal (list dim dim) (array-dimensions vec))
      (dotimes (i dim)
	(dotimes (j dim)
	  (lisp-unit:assert-float-equal init-element (aref vec i j)))))))


