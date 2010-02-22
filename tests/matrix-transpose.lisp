;;================================================
;; matrix-transpose.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(nurarihyon:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test transpose-test
  ;; 2x2
  (dotimes (i 100)
    (let* ((a (nurarihyon:random-range -100.0d0 100.0d0))
	   (b (nurarihyon:random-range -100.0d0 100.0d0))
	   (mat (nurarihyon:double-matrix (list a b)
                                          (list b a))))
      (lisp-unit:assert-true (nurarihyon:eps-matrix=
                              (nurarihyon:transpose mat) mat))
      ))
  ;; 3x3
  (dotimes (i 100)
    (let* ((a (nurarihyon:random-range -100.0d0 100.0d0))
	   (b (nurarihyon:random-range -100.0d0 100.0d0))
	   (c (nurarihyon:random-range -100.0d0 100.0d0))
	   (d (nurarihyon:random-range -100.0d0 100.0d0))
	   (e (nurarihyon:random-range -100.0d0 100.0d0))
	   (f (nurarihyon:random-range -100.0d0 100.0d0))
	   (mat (nurarihyon:double-matrix (list a b c)
                                          (list b d e)
                                          (list c e f))))
      (lisp-unit:assert-true (nurarihyon:eps-matrix=
                              (nurarihyon:transpose mat) mat))))
  )

(lisp-unit:run-tests transpose-test)

