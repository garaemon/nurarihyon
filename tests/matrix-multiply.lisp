;;================================================
;; matrix-multiply.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)
(nurarihyon:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test m*-test
  ;; 2x2 x 2x2
  (let ((a #d((1.0 2.0) (3.0 4.0)))
	(b #d((5.0 6.0) (7.0 8.0)))
	(c #d((19.0 22.0) (43.0 50.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m* a b) c)))
  ;; 3x2 x 2x3 = 3x3
  ;; 2x3 x 3x2 = 2x2
  (let ((a #d((1.0 2.0) (3.0 4.0) (5.0 6.0)))
	(b #d((-7.0 -8.0 -9.0) (-10.0 -11.0 -12.0)))
	(c #d((-27.0 -30.0 -33.0) (-61.0 -68.0 -75.0) (-95.0 -106.0 -117.0)))
	(d #d((-76.0 -100.0) (-103.0 -136.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m* a b) c))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m* b a) d)))
  ;; 3x3 x 3x3
  (let ((a #d((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)))
	(b #d((4.0 5.0 6.0) (1.0 2.0 3.0) (7.0 8.0 9.0)))
	(c #d((27.0 33.0 39.0) (63.0 78.0 93.0) (99.0 123.0 147.0))))
    (lisp-unit:assert-true (nurarihyon:eps-matrix= (nurarihyon:m* a b) c)))
  ;; use identity matrix
  ;; identity-matrix x any-matrix = any-matrix
  ;; any-matrix x identity-matrix= any-matrix
  ;; 2x2
  (let ((ident (nurarihyon:make-identity-matrix 2)))
    (dotimes (i 20)
      (let ((m (nurarihyon:double-matrix
                (list (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0))
                (list (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0)))))
	(lisp-unit:assert-true (nurarihyon:eps-matrix=
                                (nurarihyon:m* m ident m) m))
	(lisp-unit:assert-true (nurarihyon:eps-matrix=
                                (nurarihyon:m* ident m) m))
	)))
  ;; 3x3
  (let ((ident (nurarihyon:make-identity-matrix 3)))
    (dotimes (i 20)
      (let ((m (nurarihyon:double-matrix
                (list (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0))
                (list (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0))
                (list (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0)
                      (nurarihyon:random-range -100.0 100.0)))))
	(lisp-unit:assert-true (nurarihyon:eps-matrix=
                                (nurarihyon:m* m ident) m))
	(lisp-unit:assert-true (nurarihyon:eps-matrix=
                                (nurarihyon:m* ident m) m))
	)))
  )

(nurarihyon:disable-nurarihyon-reader-syntax)
