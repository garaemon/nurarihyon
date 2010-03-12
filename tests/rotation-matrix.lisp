;;================================================
;; rotation-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(use-package :nurarihyon)
(enable-nurarihyon-reader-syntax)

;; +-
(lisp-unit:define-test rotation-matrix-test
  ;; rotate 0 rad matrix = identity matrix
  (let ((identity (make-identity-matrix 3)))
    (lisp-unit:assert-true
     (eps-matrix= (rotation-matrix 0.0d0 (axis->vec :x)) identity))
    (lisp-unit:assert-true
     (eps-matrix= (rotation-matrix 0.0d0 (axis->vec :y)) identity))
    (lisp-unit:assert-true
     (eps-matrix= (rotation-matrix 0.0d0 (axis->vec :z)) identity))
    )
  (let ((identity (make-identity-matrix 3)))
    (dotimes (i 200)
      (let ((theta (random-range -360.0d0 360.0d0))
	    (axis (chimi:random-select '(:x :y :z))))
	(let ((mat+ (rotation-matrix (deg2rad theta) (axis->vec axis)))
	      (mat- (rotation-matrix (deg2rad (- theta)) (axis->vec axis))))
	  (lisp-unit:assert-true (eps-matrix= (m* mat+ mat-) identity))
	  (lisp-unit:assert-true (eps-matrix= (m* mat- mat+) identity))))))
  )

(lisp-unit:run-tests rotation-matrix-test)
