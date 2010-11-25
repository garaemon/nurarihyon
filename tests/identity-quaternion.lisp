;;================================================
;; identity-quaternion.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test identity-quaternion-test
  (let ((q (nh:identity-quaternion)))
    (lisp-unit:assert-eq 4 (nh:vector-dimension q))
    (lisp-unit:assert-float-equal 0.0d0 (nh:qx q))
    (lisp-unit:assert-float-equal 0.0d0 (nh:qy q))
    (lisp-unit:assert-float-equal 0.0d0 (nh:qz q))
    (lisp-unit:assert-float-equal 1.0d0 (nh:qw q))
    (lisp-unit:assert-float-equal 1.0d0 (nh:norm q))))


  
