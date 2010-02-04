;;================================================
;; inverse-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(nurarihyon:enable-aref-reader-syntax)

(lisp-unit:define-test m-1-test
  ;; 2x2
  (let ((identity (nurarihyon:make-identity-matrix 2)))
    (dotimes (i 100)
      (let ((mat (nurarihyon:double-matrix
                  (list (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0))
                  (list (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0)))))
	(let ((inv-mat (nurarihyon:m-1 mat)))
	  (when inv-mat
	    (lisp-unit:assert-true (nurarihyon:eps-matrix=
                                    (nurarihyon:m* inv-mat mat) identity))
	    (lisp-unit:assert-true (nurarihyon:eps-matrix=
                                    (nurarihyon:m* mat inv-mat) identity)))
	  ))))
  ;; 3x3
  (let ((identity (nurarihyon:make-identity-matrix 3)))
    (dotimes (i 100)
      (let ((mat (nurarihyon:double-matrix
                  (list (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0))
                  (list (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0))
                  (list (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0)
                        (nurarihyon:random-range -100.0d0 100.0d0)))))
	(let ((inv-mat (nurarihyon:m-1 mat)))
	  (when inv-mat
	    (lisp-unit:assert-true
             (nurarihyon:eps-matrix= (nurarihyon:m* inv-mat mat) identity))
	    (lisp-unit:assert-true
             (nurarihyon:eps-matrix= (nurarihyon:m* mat inv-mat) identity)))
	  ))))
  )

(lisp-unit:run-tests m-1-test)