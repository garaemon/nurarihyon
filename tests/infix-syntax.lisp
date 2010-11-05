;;================================================
;; infix-syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(lisp-unit:define-test infix-syntax-test
    (nh:enable-nurarihyon-reader-syntax)
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(1 + 2)"))
     '(+ 1 2))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(1 + 2 + 3 + 4)"))
     '(+ 1 (+ 2 (+ 3 4))))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(a @ 1)"))
     '(aref a 1))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(a @ (1 2))"))
     '(aref a 1 2))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(sin(a))"))
     '(sin a))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(atan(a $ b))"))
     '(atan a b))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(a <- 10)"))
     '(setf a 10))
    (lisp-unit:assert-equal
     (nth-value 0 (read-from-string "#%(a = 10)"))
     '(setf a 10))
    (nh:disable-nurarihyon-reader-syntax)
    )
