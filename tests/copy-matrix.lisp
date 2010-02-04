;;================================================
;; copy-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test copy-matrix-test
  (dotimes (i 3)
    (let ((dim 10))
      (let ((mat (nurarihyon:make-matrix
                  dim dim
                  :initial-element (nurarihyon:random-range -100.0d0 100.0d0)))
	    (tmp (nurarihyon:make-matrix dim dim)))
	(nurarihyon:copy-matrix mat tmp)
	(lisp-unit:assert-true (nurarihyon:eps-matrix= mat tmp))))))

(lisp-unit:run-tests copy-matrix-test)
