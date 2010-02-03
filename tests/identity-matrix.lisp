;;================================================
;; identity-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(require :asdf)
(require :nurarihyon)
(require :lisp-unit)

(lisp-unit:define-test make-identity-test
  (let* ((dim 10)
         (mat (nurarihyon:make-identity-matrix dim)))
    (dotimes (j dim)
      (dotimes (i dim)
        (if (= i j)
            (lisp-unit:assert-float-equal (aref mat i j) 1.0d0)
            (lisp-unit:assert-float-equal (aref mat i j) 0.0d0))))
    ))

(lisp-unit:run-tests make-identity-test)
