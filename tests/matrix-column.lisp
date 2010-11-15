;;================================================
;; matrix-row.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test matrix-column-read-test
  (let ((mat #d((1 2 3) (4 5 6))))
    (lisp-unit:assert-true (nh:eps-vector= #d(1 2 3)
                                           (nh:matrix-column mat 0)))
    (lisp-unit:assert-true (nh:eps-vector= #d(4 5 6)
                                           (nh:matrix-column mat 1)))))

(lisp-unit:define-test matrix-column-write-test
  (let ((mat (nh:make-matrix 3 2))
        (column0 #d(1 2))
        (column1 #d(3 4))
        (column2 #d(5 6)))
    (setf (nh:matrix-column mat 0) column0)
    (setf (nh:matrix-column mat 1) column1)
    (setf (nh:matrix-column mat 2) column2)
    (lisp-unit:assert-true (nh:eps-vector= column0
                                           (nh:matrix-column mat 0)))
    (lisp-unit:assert-true (nh:eps-vector= column1
                                           (nh:matrix-column mat 1)))
    (lisp-unit:assert-true (nh:eps-vector= column2
                                           (nh:matrix-column mat 2)))
    ))

(nh:disable-nurarihyon-reader-syntax)
