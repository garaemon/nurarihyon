;;================================================
;; vector-sub.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'lisp-unit)
(asdf:operate 'asdf:load-op 'nurarihyon)

(use-package :lisp-unit)
(use-package :nurarihyon)

;; template
(defmacro defv--test (name v-func vector-constructor eps-func)
  `(define-test ,name
     (let ((a (,vector-constructor 1 2 3))
           (b (,vector-constructor 4 5 6)))
       ;; #(4 5 6) - #(1 2 3) = #(3 3 3)
       (assert-true (,eps-func (,v-func b a) (,vector-constructor 3 3 3)))
       ;; #(3 3 3) - #(1 2 3) = #(2 1 0)
       (assert-true (,eps-func (,v-func (,v-func b a) a)
                               (,vector-constructor 2 1 0)))
       ;; #(2 1 0) - #(4 5 6) = #(-2 -4 -6)
       (assert-true (,eps-func (,v-func (,v-func (,v-func b a) a) b)
                               (,vector-constructor -2 -4 -6)))
       )))

;; define
(defv--test v--test v- real-vector eps-vector=)
(defv--test iv--test iv- integer-vector ieps-vector=)
(defv--test fv--test fv- float-vector feps-vector=)
(defv--test dv--test dv- double-vector deps-vector=)

;; run
(run-tests v--test iv--test fv--test dv--test)
