;;================================================
;; matrix-add.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)
(nh:enable-nurarihyon-reader-syntax)
(lisp-unit:define-test m+-test
  (let ((a #d((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #d((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #d((5.0 7.0 9.0) (5.0 7.0 9.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m+ a b) c)))
  (let ((a #d((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #d((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #d((-5.0 -7.0 -9.0) (-5.0 -7.0 -9.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m+ a b) c)))
  )

(nh:disable-nurarihyon-reader-syntax)