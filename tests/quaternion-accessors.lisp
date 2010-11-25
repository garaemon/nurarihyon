;;================================================
;; quaternion-accessors.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test quaternion-accessors-test
  (lisp-unit:assert-equal '(aref a 0) (macroexpand '(nh:qw a)))
  (lisp-unit:assert-equal '(aref a 1) (macroexpand '(nh:qx a)))
  (lisp-unit:assert-equal '(aref a 2) (macroexpand '(nh:qy a)))
  (lisp-unit:assert-equal '(aref a 3) (macroexpand '(nh:qz a)))
  )

