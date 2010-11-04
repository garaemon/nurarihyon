;;================================================
;; vector-dot.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)

(defmacro deffloat-v.-test (name type dot-func vector-constructor)
  `(lisp-unit:define-test ,name
       ;; norm of unit vector
     (let ((a (,vector-constructor 1 0 0)))
       (lisp-unit:assert-float-equal (,dot-func a a) 1.0))
     (let ((a (,vector-constructor 0 1 0)))
       (lisp-unit:assert-float-equal (,dot-func a a) 1.0))
     (let ((a (,vector-constructor 0 0 -1)))
       (lisp-unit:assert-float-equal (,dot-func a a) 1.0))
     ;; check vertical vector
     (let ((a (,vector-constructor 1 0 0))
           (b (,vector-constructor 0 1 0)))
       (lisp-unit:assert-float-equal (,dot-func a b) 0.0))
     (let* ((theta 30.0d0)
            (a (,vector-constructor (coerce (cos (nh:deg2rad theta)) ',type)
                                    (coerce (sin (nh:deg2rad theta)) ',type)))
            (b (,vector-constructor (coerce (cos (nh:deg2rad (- theta 90)))
                                            ',type)
                                    (coerce (sin (nh:deg2rad (- theta 90)))
                                            ',type))))
       (lisp-unit:assert-float-equal (,dot-func a b) 0.0))
     )
  )

(deffloat-v.-test v.-test double-float nh:v. nh:double-vector)

