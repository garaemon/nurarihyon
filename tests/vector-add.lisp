;;================================================
;; vector-add.lisp
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
(defmacro defv+-test (name v+-func vector-constructor eps-func)
  `(define-test ,name
     (let ((a (,vector-constructor 1 2 3))
           (b (,vector-constructor 4 5 6)))
       ;; #(1 2 3) + #(4 5 6) = #(5 7 9)
       (assert-true (,eps-func (,v+-func a b)
                               (,vector-constructor 5 7 9)))
       ;; #(1 2 3) + #(5 7 9) = #(6 9 12)
       (assert-true (,eps-func (,v+-func a (,v+-func a b))
                               (,vector-constructor 6 9 12)))
       ;; #(4 5 6) + #(6 9 12) = #(10 14 18)
       (assert-true (,eps-func (,v+-func b (,v+-func a (,v+-func a b)))
                               (,vector-constructor 10 14 18)))
       ))
  )

;; define
(defv+-test v+-test v+ real-vector eps-vector=)
(defv+-test iv+-test iv+ integer-vector ieps-vector=)
(defv+-test fv+-test fv+ float-vector feps-vector=)
(defv+-test dv+-test dv+ double-vector deps-vector=)

;; run
(run-tests v+-test iv+-test fv+-test dv+-test)
