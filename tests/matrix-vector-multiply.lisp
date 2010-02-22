;;================================================
;; matrix-vector-multiply.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(nurarihyon:enable-nurarihyon-reader-syntax)

;; mv*
(lisp-unit:define-test mv*-test
  ;; 2x2 x 2x1
  (let ((a #d((1.0 2.0) (3.0 4.0)))
	(b #d(5.0 6.0))
	(c #d(17.0 39.0)))
    (lisp-unit:assert-true (nurarihyon:eps-vector= (nurarihyon:mv* a b) c)))
  ;; 3x3 x 3x1
  (let ((a #d((1.0 2.0 3.0) (3.0 4.0 5.0) (6.0 7.0 8.0)))
	(b #d(9.0 10.0 11.0))
	(c #d(62.0 122.0 212.0)))
    (lisp-unit:assert-true (nurarihyon:eps-vector= (nurarihyon:mv* a b) c)))
  (dotimes (i 100)
    (let ((mat (nurarihyon:make-identity-matrix 2))
	  (vec (nurarihyon:double-vector
                (nurarihyon:random-range -100.0d0 100.0d0)
                (nurarihyon:random-range -100.0d0 100.0d0))))
      (lisp-unit:assert-true
       (nurarihyon:eps-vector= (nurarihyon:mv* mat vec) vec))))
  (dotimes (i 100)
    (let ((mat (nurarihyon:make-identity-matrix 3))
	  (vec (nurarihyon:double-vector
                (nurarihyon:random-range -100.0d0 100.0d0)
                (nurarihyon:random-range -100.0d0 100.0d0)
                (nurarihyon:random-range -100.0d0 100.0d0))))
      (lisp-unit:assert-true
       (nurarihyon:eps-vector= (nurarihyon:mv* mat vec) vec)))
    ))

(lisp-unit:run-tests mv*-test)
