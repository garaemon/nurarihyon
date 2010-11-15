;;================================================
;; matrix-row.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test matrix-row-read-test
  (let ((mat #d((1 2 3) (4 5 6))))
    (lisp-unit:assert-true (nh:eps-vector= #d(1 4)
                                           (nh:matrix-row mat 0)))
    (lisp-unit:assert-true (nh:eps-vector= #d(2 5)
                                           (nh:matrix-row mat 1)))
    (lisp-unit:assert-true (nh:eps-vector= #d(3 6)
                                           (nh:matrix-row mat 2)))
    ))

(lisp-unit:define-test matrix-row-write-test
  (let ((mat (nh:make-matrix 2 3))
        (row0 #d(1 2))
        (row1 #d(3 4))
        (row2 #d(5 6)))
    (setf (nh:matrix-row mat 0) row0)
    (setf (nh:matrix-row mat 1) row1)
    (setf (nh:matrix-row mat 2) row2)
    (lisp-unit:assert-true (nh:eps-vector= row0
                                           (nh:matrix-row mat 0)))
    (lisp-unit:assert-true (nh:eps-vector= row1
                                           (nh:matrix-row mat 1)))
    (lisp-unit:assert-true (nh:eps-vector= row2
                                           (nh:matrix-row mat 2)))
    ))

(nh:disable-nurarihyon-reader-syntax)
