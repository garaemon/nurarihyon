;;================================================
;; vector-sub.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

;; template
(defmacro defv--test (name v-func vector-constructor eps-func)
  `(lisp-unit:define-test ,name
     (let ((a (,vector-constructor 1 2 3))
           (b (,vector-constructor 4 5 6)))
       ;; #(4 5 6) - #(1 2 3) = #(3 3 3)
       (lisp-unit:assert-true (,eps-func (,v-func b a)
                                         (,vector-constructor 3 3 3)))
       ;; #(3 3 3) - #(1 2 3) = #(2 1 0)
       (lisp-unit:assert-true (,eps-func (,v-func (,v-func b a) a)
                                         (,vector-constructor 2 1 0)))
       ;; #(2 1 0) - #(4 5 6) = #(-2 -4 -6)
       (lisp-unit:assert-true (,eps-func (,v-func (,v-func (,v-func b a) a) b)
                                         (,vector-constructor -2 -4 -6)))
       )))

;; define
(defv--test v--test nh:v- nh:double-vector nh:eps-vector=)

