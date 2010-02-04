;;================================================
;; test-base.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'lisp-unit)
(asdf:operate 'asdf:load-op 'nurarihyon)

(use-package :lisp-unit)
(use-package :nurarihyon)

(lisp-unit:define-test m+-test
  (let ((a #2A((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #2A((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #2A((5.0 7.0 9.0) (5.0 7.0 9.0))))
    (lisp-unit:assert-true (eps-matrix= (m+ a b) c)))
  (let ((a #2A((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #2A((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #2A((-5.0 -7.0 -9.0) (-5.0 -7.0 -9.0))))
    (lisp-unit:assert-true (eps-matrix= (m+ a b) c)))
  )

(lisp-unit:define-test m--test
  (let ((a #2A((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #2A((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #2A((3.0 3.0 3.0) (3.0 3.0 3.0))))
    (lisp-unit:assert-true (eps-matrix= (m- b a) c)))
  (let ((a #2A((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #2A((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #2A((-3.0 -3.0 -3.0) (-3.0 -3.0 -3.0))))
    (lisp-unit:assert-true (eps-matrix= (m- b a) c)))
  )

(lisp-unit:define-test matrix-addsub-test
  (dotimes (i 10)
    (let ((a (list->matrix (list (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0))
				 (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0))
				 (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0)))))
	  (b (list->matrix (list (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0))
				 (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0))
				 (list (random-range -100.0 100.0)
				       (random-range -100.0 100.0)
				       (random-range -100.0 100.0))))))
      (lisp-unit:assert-true (eps-matrix= (m- (m+ a b) b) a))
      (lisp-unit:assert-true (eps-matrix= (m- (m+ a b) a) b))
      )))

(lisp-unit:define-test m*-test
  ;; 2x2 x 2x2
  (let ((a #2A((1.0 2.0) (3.0 4.0)))
	(b #2A((5.0 6.0) (7.0 8.0)))
	(c #2A((19.0 22.0) (43.0 50.0))))
    (lisp-unit:assert-true (eps-matrix= (m* a b) c)))
  ;; 3x2 x 2x3 = 3x3
  ;; 2x3 x 3x2 = 2x2
  (let ((a #2A((1.0 2.0) (3.0 4.0) (5.0 6.0)))
	(b #2A((-7.0 -8.0 -9.0) (-10.0 -11.0 -12.0)))
	(c #2A((-27.0 -30.0 -33.0) (-61.0 -68.0 -75.0) (-95.0 -106.0 -117.0)))
	(d #2A((-76.0 -100.0) (-103.0 -136.0))))
    (lisp-unit:assert-true (eps-matrix= (m* a b) c))
    (lisp-unit:assert-true (eps-matrix= (m* b a) d)))
  ;; 3x3 x 3x3
  (let ((a #2A((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)))
	(b #2A((4.0 5.0 6.0) (1.0 2.0 3.0) (7.0 8.0 9.0)))
	(c #2A((27.0 33.0 39.0) (63.0 78.0 93.0) (99.0 123.0 147.0))))
    (lisp-unit:assert-true (eps-matrix= (m* a b) c))
    )
  ;; use identity matrix
  ;; identity-matrix x any-matrix = any-matrix
  ;; any-matrix x identity-matrix= any-matrix
  ;; 2x2
  (let ((ident (make-identity-matrix 2)))
    (dotimes (i 20)
      (let ((m (list->matrix (list (list (random-range -100.0 100.0)
					 (random-range -100.0 100.0))
				   (list (random-range -100.0 100.0)
					 (random-range -100.0 100.0))))))
	(lisp-unit:assert-true (eps-matrix= (m* m ident m) m))
	(lisp-unit:assert-true (eps-matrix= (m* ident m) m))
	)))
  ;; 3x3
  (let ((ident (make-identity-matrix 3)))
    (dotimes (i 20)
      (let ((m (list->matrix (list (list (random-range -100.0 100.0)
					 (random-range -100.0 100.0)
					 (random-range -100.0 100.0))
				   (list (random-range -100.0 100.0)
					 (random-range -100.0 100.0)
					 (random-range -100.0 100.0))
				   (list (random-range -100.0 100.0)
					 (random-range -100.0 100.0)
					 (random-range -100.0 100.0))))))
	(lisp-unit:assert-true (eps-matrix= (m* m ident) m))
	(lisp-unit:assert-true (eps-matrix= (m* ident m) m))
	)))
  )

(lisp-unit:define-test m-1-test
  ;; 2x2
  (let ((identity (make-identity-matrix 2)))
    (dotimes (i 100)
      (let ((mat (list->matrix (list (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0))
				     (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0))))))
	(let ((inv-mat (m-1 mat)))
	  (when inv-mat
	    (lisp-unit:assert-true (eps-matrix= (m* inv-mat mat) identity))
	    (lisp-unit:assert-true (eps-matrix= (m* mat inv-mat) identity)))
	  ))))
  ;; 3x3
  (let ((identity (make-identity-matrix 3)))
    (dotimes (i 100)
      (let ((mat (list->matrix (list (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0)
					   (random-range -100.0 100.0))
				     (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0)
					   (random-range -100.0 100.0))
				     (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0)
					   (random-range -100.0 100.0))))))
	(let ((inv-mat (m-1 mat)))
	  (when inv-mat
	    (lisp-unit:assert-true (eps-matrix= (m* inv-mat mat) identity))
	    (lisp-unit:assert-true (eps-matrix= (m* mat inv-mat) identity)))
	  ))))
  )

(lisp-unit:define-test flip-test
  ;; 2x2
  (dotimes (i 100)
    (let* ((a (random-range -100.0 100.0))
	   (b (random-range -100.0 100.0))
	   (mat (list->matrix (list (list a b)
				    (list b a)))))
      (lisp-unit:assert-true (eps-matrix= (flip mat) mat))
      ))
  ;; 3x3
  (dotimes (i 100)
    (let* ((a (random-range -100.0 100.0))
	   (b (random-range -100.0 100.0))
	   (c (random-range -100.0 100.0))
	   (d (random-range -100.0 100.0))
	   (e (random-range -100.0 100.0))
	   (f (random-range -100.0 100.0))
	   (mat (list->matrix (list (list a b c)
				    (list b d e)
				    (list c e f)))))
      (lisp-unit:assert-true (eps-matrix= (flip mat) mat))))
  )

;; mv*
(lisp-unit:define-test mv*-test
  ;; 2x2 x 2x1
  (let ((a #2A((1.0 2.0) (3.0 4.0)))
	(b #(5.0 6.0))
	(c #(17.0 39.0)))
    (lisp-unit:assert-true (eps-vector= (mv* a b) c)))
  ;; 3x3 x 3x1
  (let ((a #2A((1.0 2.0 3.0) (3.0 4.0 5.0) (6.0 7.0 8.0)))
	(b #(9.0 10.0 11.0))
	(c #(62.0 122.0 212.0)))
    (lisp-unit:assert-true (eps-vector= (mv* a b) c)))
  (dotimes (i 100)
    (let ((mat (make-identity-matrix 2))
	  (vec (list->vector (list (random-range -100.0 100.0)
				   (random-range -100.0 100.0)))))
      (lisp-unit:assert-true (eps-vector= (mv* mat vec) vec))))
  (dotimes (i 100)
    (let ((mat (make-identity-matrix 3))
	  (vec (list->vector (list (random-range -100.0 100.0)
				   (random-range -100.0 100.0)
				   (random-range -100.0 100.0)))))
      (lisp-unit:assert-true (eps-vector= (mv* mat vec) vec))))
  )

