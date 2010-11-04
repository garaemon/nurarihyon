;;================================================
;; random-range.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test random-range-test
  ;; float
  (dotimes (i 100)
    (let ((min -100.0d0)
	  (max 100.0d0))
      (lisp-unit:assert-true (<= min (nurarihyon:random-range min max) max))))
  ;; integer
  (dotimes (i 100)
    (let ((min -100)
	  (max 100))
      (lisp-unit:assert-true (<= min (nurarihyon:random-range min max) max)))))


