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

(define-test make-integer-vector-test
  (let ((dim 10)
	(init-element 10))
    (let ((vec (make-integer-vector dim :initial-element init-element)))
      (assert-equal (list dim) (array-dimensions vec))
      (dotimes (i dim)
	(assert-equal init-element (aref vec i))))))

(define-test make-float-vector-test
  (let ((dim 10)
	(init-element 10.0))
    (let ((vec (make-float-vector dim :initial-element init-element)))
      (assert-equal (list dim) (array-dimensions vec))
      (dotimes (i dim)
	(assert-float-equal init-element (aref vec i))))))

(define-test make-float-matrix-test
  (let ((dim 10)
	(init-element 10.0))
    (let ((vec (make-float-matrix dim dim :initial-element init-element)))
      (assert-equal (list dim dim) (array-dimensions vec))
      (dotimes (i dim)
	(dotimes (j dim)
	  (assert-float-equal init-element (aref vec i j)))))))

(define-test make-identity-matrix-test
    (let ((dim 10))
      (let ((mat (make-identity-matrix dim)))
	(assert-equal (list dim dim) (array-dimensions mat))
	(dotimes (i dim)
	  (dotimes (j dim)
	    (if (= i j)
		(assert-float-equal 1.0 (aref mat i j))
		(assert-float-equal 0.0 (aref mat i j))))))))

(define-test float-vector-test
  (let ((vec (float-vector 1 2 3)))
    (assert-true (eps-vector= vec #(1.0 2.0 3.0))))
  (let ((vec (float-vector 3 2 1)))
    (assert-true (eps-vector= vec #(3.0 2.0 1.0))))
  (let ((vec (float-vector 1.1 1.1 1.1)))
    (assert-true (eps-vector= vec #(1.1 1.1 1.1)))))

(define-test copy-vector-test
  (dotimes (i 3)
    (let ((vec (float-vector (random-range -100.0 100.0)
			     (random-range -100.0 100.0)
			     (random-range -100.0 100.0)))
	  (buf (make-float-vector 3)))
      (copy-vector vec buf)
      (assert-true (eps-vector= vec buf)))))

(define-test copy-matrix-test
  (dotimes (i 3)
    (let ((dim 10))
      (let ((mat (make-float-matrix dim dim :initial-element (random-range -100.0 100.0)))
	    (tmp (make-float-matrix dim dim)))
	(copy-matrix mat tmp)
	(assert-true (eps-matrix= mat tmp))))))

(define-test v+-test
  (let ((a (float-vector 1 2 3))
	(b (float-vector 4 5 6)))
    ;; #(1 2 3) + #(4 5 6) = #(5 7 9)
    (assert-true (eps-vector= (v+ a b) (float-vector 5 7 9)))
    ;; #(1 2 3) + #(5 7 9) = #(6 9 12)
    (assert-true (eps-vector= (v+ a (v+ a b)) (float-vector 6 9 12)))
    ;; #(4 5 6) + #(6 9 12) = #(10 14 18)
    (assert-true (eps-vector= (v+ b (v+ a (v+ a b))) (float-vector 10 14 18)))
    ))

(define-test v--test
  (let ((a (float-vector 1 2 3))
	(b (float-vector 4 5 6)))
    ;; #(4 5 6) - #(1 2 3) = #(3 3 3)
    (assert-true (eps-vector= (v- b a) (float-vector 3 3 3)))
    ;; #(3 3 3) - #(1 2 3) = #(2 1 0)
    (assert-true (eps-vector= (v- (v- b a) a) (float-vector 2 1 0)))
    ;; #(2 1 0) - #(4 5 6) = #(-2 -4 -6)
    (assert-true (eps-vector= (v- (v- (v- b a) a) b) (float-vector -2 -4 -6)))
    ))

(define-test v.-test
  ;; norm of unit vector
  (let ((a (float-vector 1 0 0)))
    (assert-float-equal (v. a a) 1.0))
  (let ((a (float-vector 0 1 0)))
    (assert-float-equal (v. a a) 1.0))
  (let ((a (float-vector 0 0 -1)))
    (assert-float-equal (v. a a) 1.0))
  ;; check vertical vector
  (let ((a (float-vector 1 0 0))
	(b (float-vector 0 1 0)))
    (assert-float-equal (v. a b) 0.0))
  (let* ((theta 30.0)
	 (a (float-vector (cos (deg2rad theta)) (sin (deg2rad theta))))
	 (b (float-vector (cos (deg2rad (- theta 90))) (sin (deg2rad (- theta 90))))))
    (assert-float-equal (v. a b) 0.0))
  )

(define-test v*-test
  ;; check cross product by using dot product
  (let ((a (float-vector 1 2 3))
	(b (float-vector 4 5 6)))
    (let ((c (v* a b)))
      (assert-float-equal (v. a c) 0.0)
      (assert-float-equal (v. b c) 0.0)
      ))
  (let ((a (float-vector -1 2 -3))
	(b (float-vector 4 -5 6)))
    (let ((c (v* a b)))
      (assert-float-equal (v. a c) 0.0)
      (assert-float-equal (v. b c) 0.0)
      ))
  (let ((a (float-vector 1 2 3)))
    (assert-true (eps-vector= (v* a a) (float-vector 0.0 0.0 0.0))))
  (let ((a (float-vector 4 5 6)))
    (assert-true (eps-vector= (v* a a) (float-vector 0.0 0.0 0.0))))
  )

(define-test norm-test
  (let ((a (float-vector 1 0 0)))
    (assert-float-equal (norm a) 1.0))
  (let ((a (float-vector 2 0 0)))
    (assert-float-equal (norm a) 2.0))
  (let ((a (float-vector -2 0 0)))
    (assert-float-equal (norm a) 2.0))
  )

(define-test distance-test
  (let ((a (float-vector 1 0 0)))
    (assert-float-equal (distance a a) 0.0))
  (dotimes (i 10)
    (let ((a (float-vector (random-range -100.0 100.0)
			   (random-range -100.0 100.0)
			   (random-range -100.0 100.0))))
      (assert-float-equal (distance a a) 0.0)))
  (dotimes (i 10)
    (let ((c (float-vector 0 0 0))
	  (a (float-vector (random-range -100.0 100.0)
			   (random-range -100.0 100.0)
			   (random-range -100.0 100.0))))
      (assert-float-equal (distance a c) (norm a))))
  )

(define-test list->vector-test
  (let ((a (list 1.0 2.0 3.0))
	(b (float-vector 1 2 3)))
    (assert-true (eps-vector= (list->vector a) b)))
  (let ((a (list 2.0 3.0 4.0))
	(b (float-vector 2 3 4)))
    (assert-true (eps-vector= (list->vector a) b))))

(define-test vector->list-test
  (let ((a (list 1.0 2.0 3.0))
	(b (float-vector 1 2 3)))
    ;;(assert-true ((vector->list a) b))
    (mapcar #'(lambda (aa bb)
		(assert-float-equal aa bb))
	    a (vector->list b))
    )
  (let ((a (list 2.0 3.0 4.0))
	(b (float-vector 2 3 4)))
    (mapcar #'(lambda (aa bb)
		(assert-float-equal aa bb))
	    a (vector->list b))))

(define-test list->matrix-test
  (let ((a (list->matrix '((1.0 2.0 3.0) (1.0 2.0 3.0))))
	(b #2A((1.0 2.0 3.0) (1.0 2.0 3.0))))
    (assert-true (eps-matrix= a b)))
  (let ((a (list->matrix '((0.0 2.0 0.0) (0.0 2.0 -2.0))))
	(b #2A((0.0 2.0 0.0) (0.0 2.0 -2.0))))
    (assert-true (eps-matrix= a b)))
  )

(define-test m+-test
  (let ((a #2A((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #2A((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #2A((5.0 7.0 9.0) (5.0 7.0 9.0))))
    (assert-true (eps-matrix= (m+ a b) c)))
  (let ((a #2A((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #2A((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #2A((-5.0 -7.0 -9.0) (-5.0 -7.0 -9.0))))
    (assert-true (eps-matrix= (m+ a b) c)))
  )

(define-test m--test
  (let ((a #2A((1.0 2.0 3.0) (1.0 2.0 3.0)))
	(b #2A((4.0 5.0 6.0) (4.0 5.0 6.0)))
	(c #2A((3.0 3.0 3.0) (3.0 3.0 3.0))))
    (assert-true (eps-matrix= (m- b a) c)))
  (let ((a #2A((-1.0 -2.0 -3.0) (-1.0 -2.0 -3.0)))
	(b #2A((-4.0 -5.0 -6.0) (-4.0 -5.0 -6.0)))
	(c #2A((-3.0 -3.0 -3.0) (-3.0 -3.0 -3.0))))
    (assert-true (eps-matrix= (m- b a) c)))
  )

(define-test matrix-addsub-test
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
      (assert-true (eps-matrix= (m- (m+ a b) b) a))
      (assert-true (eps-matrix= (m- (m+ a b) a) b))
      )))

(define-test m*-test
  ;; 2x2 x 2x2
  (let ((a #2A((1.0 2.0) (3.0 4.0)))
	(b #2A((5.0 6.0) (7.0 8.0)))
	(c #2A((19.0 22.0) (43.0 50.0))))
    (assert-true (eps-matrix= (m* a b) c)))
  ;; 3x2 x 2x3 = 3x3
  ;; 2x3 x 3x2 = 2x2
  (let ((a #2A((1.0 2.0) (3.0 4.0) (5.0 6.0)))
	(b #2A((-7.0 -8.0 -9.0) (-10.0 -11.0 -12.0)))
	(c #2A((-27.0 -30.0 -33.0) (-61.0 -68.0 -75.0) (-95.0 -106.0 -117.0)))
	(d #2A((-76.0 -100.0) (-103.0 -136.0))))
    (assert-true (eps-matrix= (m* a b) c))
    (assert-true (eps-matrix= (m* b a) d)))
  ;; 3x3 x 3x3
  (let ((a #2A((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)))
	(b #2A((4.0 5.0 6.0) (1.0 2.0 3.0) (7.0 8.0 9.0)))
	(c #2A((27.0 33.0 39.0) (63.0 78.0 93.0) (99.0 123.0 147.0))))
    (assert-true (eps-matrix= (m* a b) c))
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
	(assert-true (eps-matrix= (m* m ident m) m))
	(assert-true (eps-matrix= (m* ident m) m))
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
	(assert-true (eps-matrix= (m* m ident) m))
	(assert-true (eps-matrix= (m* ident m) m))
	)))
  )

(define-test m-1-test
  ;; 2x2
  (let ((identity (make-identity-matrix 2)))
    (dotimes (i 100)
      (let ((mat (list->matrix (list (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0))
				     (list (random-range -100.0 100.0)
					   (random-range -100.0 100.0))))))
	(let ((inv-mat (m-1 mat)))
	  (when inv-mat
	    (assert-true (eps-matrix= (m* inv-mat mat) identity))
	    (assert-true (eps-matrix= (m* mat inv-mat) identity)))
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
	    (assert-true (eps-matrix= (m* inv-mat mat) identity))
	    (assert-true (eps-matrix= (m* mat inv-mat) identity)))
	  ))))
  )

(define-test flip-test
  ;; 2x2
  (dotimes (i 100)
    (let* ((a (random-range -100.0 100.0))
	   (b (random-range -100.0 100.0))
	   (mat (list->matrix (list (list a b)
				    (list b a)))))
      (assert-true (eps-matrix= (flip mat) mat))
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
      (assert-true (eps-matrix= (flip mat) mat))))
  )

;; mv*
(define-test mv*-test
  ;; 2x2 x 2x1
  (let ((a #2A((1.0 2.0) (3.0 4.0)))
	(b #(5.0 6.0))
	(c #(17.0 39.0)))
    (assert-true (eps-vector= (mv* a b) c)))
  ;; 3x3 x 3x1
  (let ((a #2A((1.0 2.0 3.0) (3.0 4.0 5.0) (6.0 7.0 8.0)))
	(b #(9.0 10.0 11.0))
	(c #(62.0 122.0 212.0)))
    (assert-true (eps-vector= (mv* a b) c)))
  (dotimes (i 100)
    (let ((mat (make-identity-matrix 2))
	  (vec (list->vector (list (random-range -100.0 100.0)
				   (random-range -100.0 100.0)))))
      (assert-true (eps-vector= (mv* mat vec) vec))))
  (dotimes (i 100)
    (let ((mat (make-identity-matrix 3))
	  (vec (list->vector (list (random-range -100.0 100.0)
				   (random-range -100.0 100.0)
				   (random-range -100.0 100.0)))))
      (assert-true (eps-vector= (mv* mat vec) vec))))
  )

(format t "<<<<< test-base.lisp >>>>>~%")
(format t "-----------------------------------------------~%")
(run-tests constants-test
	   eps=-test
	   random-range-test
	   deg2rad-test
	   rad2deg-test
	   deg2rad-and-rad2deg-test
	   make-integer-vector-test
	   make-float-vector-test
	   make-float-matrix-test
	   make-identity-matrix-test
	   float-vector-test
	   copy-vector-test
	   copy-matrix-test
	   v+-test
	   v--test
	   v.-test
	   v*-test
	   norm-test
	   distance-test
	   list->vector-test
	   vector->list-test
	   list->matrix-test
	   m+-test
	   m--test
	   matrix-addsub-test
	   m*-test
	   m-1-test
	   flip-test
	   mv*-test)
(format t "~%-----------------------------------------------~%~%")
