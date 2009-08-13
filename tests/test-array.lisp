;;================================================
;; test-array.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'lisp-unit)
(asdf:operate 'asdf:load-op 'nurarihyon)

(use-package :lisp-unit)
(use-package :nurarihyon)

(define-test axis->vec-test
  (assert-true (eps-vector= (axis->vec :x) +x-axis+))
  (assert-true (eps-vector= (axis->vec :y) +y-axis+))
  (assert-true (eps-vector= (axis->vec :y) +y-axis+))
  )

(define-test rotation-matrix-test
  ;; rotate 0 rad matrix = identity matrix
  (let ((identity (make-identity-matrix 3)))
    (assert-true (eps-matrix= (rotation-matrix 0.0 (axis->vec :x)) identity))
    (assert-true (eps-matrix= (rotation-matrix 0.0 (axis->vec :y)) identity))
    (assert-true (eps-matrix= (rotation-matrix 0.0 (axis->vec :z)) identity))
    )
  (let ((identity (make-identity-matrix 3)))
    (dotimes (i 200)
      (let ((theta (random-range -360.0 360.0))
	    (axis (random-select '(:x :y :z))))
	(let ((mat+ (rotation-matrix (deg2rad theta) (axis->vec axis)))
	      (mat- (rotation-matrix (deg2rad (- theta)) (axis->vec axis))))
	  (assert-true (eps-matrix= (m* mat+ mat-) identity))
	  (assert-true (eps-matrix= (m* mat- mat+) identity))))))
  )

(define-test rotate-matrix-test
  (let ((identity (make-identity-matrix 3)))
    (dotimes (i 100)
      (let* ((axis (random-select '(:x :y :z)))
	     (theta (random-range -360.0 360.0))
	     (inv-axis (case axis
			 (:x :-x)
			 (:z :-z)
			 (:y :-y))))
	(let ((mat (rotation-matrix (deg2rad theta) (axis->vec axis))))
	  (assert-true (eps-matrix= (rotate-matrix mat (- theta) axis) identity))
	  (assert-true (eps-matrix= (rotate-matrix mat theta inv-axis) identity))
	  )
	))
    ;; worldpについてのものがほしい
    ))
  


(run-tests axis->vec-test
	   rotation-matrix-test
	   rotate-matrix-test)

(force-output)
;;(sb-ext:quit)
