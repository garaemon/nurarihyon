;;================================================
;; deg-to-radian.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test deg2rad-test
  ;; 0[deg] -> 0[rad]
  (lisp-unit:assert-float-equal (nurarihyon:deg2rad 0.0d0) 0.0d0)
  ;; 90[deg] -> pi/2[rad]
  (lisp-unit:assert-float-equal (nurarihyon:deg2rad 90.0d0)
                                nurarihyon:+pi/2+)
  ;; 180[deg] -> pi[rad]
  (lisp-unit:assert-float-equal (nurarihyon:deg2rad 180.0d0)
                                nurarihyon:+pi+)
  ;; 270[deg] -> 3pi/2[rad]
  (lisp-unit:assert-float-equal (nurarihyon:deg2rad 270.0d0)
                                (+ nurarihyon:+pi/2+ nurarihyon:+pi+))
  ;; 360[deg] -> 2pi[rad]
  (lisp-unit:assert-float-equal (nurarihyon:deg2rad 360.0d0)
                                nurarihyon:+2pi+)
  )

(lisp-unit:run-tests deg2rad-test)
