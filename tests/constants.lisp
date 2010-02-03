;;================================================
;; constants.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test constants-test
  (lisp-unit:assert-float-equal nurarihyon:+e+ (exp 1.0))
  (lisp-unit:assert-float-equal nurarihyon:+pi+ pi)
  (lisp-unit:assert-float-equal nurarihyon:+-pi+ (- pi))
  (lisp-unit:assert-float-equal nurarihyon:+2pi+ (* 2.0d0 pi))
  (lisp-unit:assert-float-equal nurarihyon:+-2pi+ (* -2.0d0 pi))
  (lisp-unit:assert-float-equal nurarihyon:+pi/2+ (/ pi 2.0d0))
  (lisp-unit:assert-float-equal nurarihyon:+-pi/2+ (/ pi -2.0d0))
  (lisp-unit:assert-float-equal nurarihyon:+pi/4+ (/ pi 4.0d0))
  (lisp-unit:assert-float-equal nurarihyon:+-pi/4+ (/ pi -4.0d0)))

(lisp-unit:run-tests constants-test)
