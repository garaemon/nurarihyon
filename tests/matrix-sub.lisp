;;================================================
;; matrix-sub.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(nurarihyon:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test m--test
  (let ((a #d((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #d((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #d((3.0 3.0 3.0) (3.0 3.0 3.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m- b a) c)))
  (let ((a #d((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #d((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #d((-3.0 -3.0 -3.0) (-3.0 -3.0 -3.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m- b a) c)))
  )

(lisp-unit:run-tests m--test)

