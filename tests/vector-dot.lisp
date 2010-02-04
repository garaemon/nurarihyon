;;================================================
;; vector-dot.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'lisp-unit)
(asdf:operate 'asdf:load-op 'nurarihyon)

(use-package :lisp-unit)
(use-package :nurarihyon)

(defmacro deffloat-v.-test (name type dot-func vector-constructor)
  `(define-test ,name
       ;; norm of unit vector
     (let ((a (,vector-constructor 1 0 0)))
       (assert-float-equal (,dot-func a a) 1.0))
     (let ((a (,vector-constructor 0 1 0)))
       (assert-float-equal (,dot-func a a) 1.0))
     (let ((a (,vector-constructor 0 0 -1)))
       (assert-float-equal (,dot-func a a) 1.0))
     ;; check vertical vector
     (let ((a (,vector-constructor 1 0 0))
           (b (,vector-constructor 0 1 0)))
       (assert-float-equal (,dot-func a b) 0.0))
     (let* ((theta 30.0)
            (a (,vector-constructor (coerce (cos (deg2rad theta)) ',type)
                                    (coerce (sin (deg2rad theta)) ',type)))
            (b (,vector-constructor (coerce (cos (deg2rad (- theta 90)))
                                            ',type)
                                    (coerce (sin (deg2rad (- theta 90)))
                                            ',type))))
       (assert-float-equal (,dot-func a b) 0.0))
     )
  )

(deffloat-v.-test v.-test real v. real-vector)
(deffloat-v.-test fv.-test single-float fv. float-vector)
(deffloat-v.-test dv.-test double-float dv. double-vector)

(run-tests v.-test fv.-test dv.-test)

