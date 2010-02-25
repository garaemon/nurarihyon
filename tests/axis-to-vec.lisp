;;================================================
;; axis-to-vec.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)
(use-package :nurarihyon)
(enable-nurarihyon-reader-syntax)

(lisp-unit:define-test axis->vec-test
  (lisp-unit:assert-true (eps-vector= (axis->vec :x) +x-axis+))
  (lisp-unit:assert-true (eps-vector= (axis->vec :y) +y-axis+))
  (lisp-unit:assert-true (eps-vector= (axis->vec :z) +z-axis+))
  )

(lisp-unit:run-tests axis->vec-test)
