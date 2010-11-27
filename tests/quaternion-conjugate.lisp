;;================================================
;; quaternion-conjugate.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test quaternion-conjugate-test
  (dotimes (i 100)
    (let ((q (nh:normalize-vector
              (nh:make-random-vector 4 :min -1.0d0 :max 1.0d0))))
      (let ((q-dash (nh:quaternion-conjugate q)))
        (lisp-unit:assert-float-equal (- (nh:qx q)) (nh:qx q-dash))
        (lisp-unit:assert-float-equal (- (nh:qy q)) (nh:qy q-dash))
        (lisp-unit:assert-float-equal (- (nh:qz q)) (nh:qz q-dash))
        (lisp-unit:assert-float-equal (nh:qw q) (nh:qw q-dash))))))

(nh:disable-nurarihyon-reader-syntax)
