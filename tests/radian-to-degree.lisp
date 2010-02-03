;;================================================
;; radian-to-degree.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test rad2deg-test
  ;; 0[rad] -> 0[deg]
  (lisp-unit:assert-float-equal (nurarihyon:rad2deg 0.0d0) 0.0d0)
  ;; pi/2[rad] -> 90[deg]
  (lisp-unit:assert-float-equal (nurarihyon:rad2deg nurarihyon:+pi/2+) 90.0d0)
  ;; 180[deg] -> pi[rad]
  (lisp-unit:assert-float-equal (nurarihyon:rad2deg nurarihyon:+pi+) 180.0d0)
  ;; 270[deg] -> 3pi/2[rad]
  (lisp-unit:assert-float-equal
   (nurarihyon:rad2deg (+ nurarihyon:+pi/2+ nurarihyon:+pi+)) 270.0d0)
  ;; 360[deg] -> 2pi[rad]
  (lisp-unit:assert-float-equal (nurarihyon:rad2deg nurarihyon:+2pi+) 360.0d0)
  )

(lisp-unit:run-tests rad2deg-test)

