;;================================================
;; identity-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test make-identity-matrix-test
  (let* ((dim 10)
         (mat (nurarihyon:make-identity-matrix dim)))
    (dotimes (j dim)
      (dotimes (i dim)
        (if (= i j)
            (lisp-unit:assert-float-equal (aref mat i j) 1.0d0)
            (lisp-unit:assert-float-equal (aref mat i j) 0.0d0))))
    ))

(lisp-unit:define-test make-identity-matrix3-test
  (let ((mat (nh:make-identity-matrix3)))
    (dotimes (i 3)
      (dotimes (j 3)
        (if (= i j)
            (lisp-unit:assert-float-equal (aref mat i j) 1.0d0)
            (lisp-unit:assert-float-equal (aref mat i j) 0.0d0))))))

(lisp-unit:define-test make-identity-matrix4-test
  (let ((mat (nh:make-identity-matrix4)))
    (dotimes (i 4)
      (dotimes (j 4)
        (if (= i j)
            (lisp-unit:assert-float-equal (aref mat i j) 1.0d0)
            (lisp-unit:assert-float-equal (aref mat i j) 0.0d0))))))

(lisp-unit:run-tests make-identity-test)
