;;================================================
;; copy-vector.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test copy-vector-test
  (dotimes (i 3)
    (let ((vec (nurarihyon:double-vector
                (nurarihyon:random-range -100.0d0 100.0d0)
                (nurarihyon:random-range -100.0d0 100.0d0)
                (nurarihyon:random-range -100.0d0 100.0d0)))
	  (buf (nurarihyon:make-vector 3)))
      (nurarihyon:copy-vector vec buf)
      (lisp-unit:assert-true (nurarihyon:eps-vector= vec buf)))))


