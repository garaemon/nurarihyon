;;================================================
;; vector-add.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

;; template
(defmacro defv+-test (name v+-func vector-constructor eps-func)
  `(lisp-unit:define-test ,name
     (let ((a (,vector-constructor 1 2 3))
           (b (,vector-constructor 4 5 6)))
       ;; #(1 2 3) + #(4 5 6) = #(5 7 9)
       (lisp-unit:assert-true (,eps-func (,v+-func a b)
                               (,vector-constructor 5 7 9)))
       ;; #(1 2 3) + #(5 7 9) = #(6 9 12)
       (lisp-unit:assert-true (,eps-func (,v+-func a (,v+-func a b))
                                         (,vector-constructor 6 9 12)))
       ;; #(4 5 6) + #(6 9 12) = #(10 14 18)
       (lisp-unit:assert-true (,eps-func (,v+-func b
                                                   (,v+-func a (,v+-func a b)))
                                         (,vector-constructor 10 14 18)))
       ))
  )

;; define
(defv+-test v+-test nh:v+ nh:double-vector nh:eps-vector=)
