;;================================================
;; degree-to-radian-to-degree.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test deg2rad-and-rad2deg-test
  (dotimes (i 10)
    (let ((theta (nurarihyon:random-range -350.0d0 350.0d0)))
      (lisp-unit:assert-true
       (nurarihyon:eps= (nurarihyon:rad2deg (nurarihyon:deg2rad theta))
                        theta)))))

(lisp-unit:run-tests deg2rad-and-rad2deg-test)
 