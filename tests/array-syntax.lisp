;;================================================
;; array-syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)

(lisp-unit:define-test double-vector-syntax-test
  (nh:enable-nurarihyon-reader-syntax)
  (lisp-unit:assert-equal
   (nth-value 0 (read-from-string "#d(a b c)"))
   '(nh:double-vector a b c))
  (nh:disable-nurarihyon-reader-syntax))

(lisp-unit:define-test double-matrix-syntax-test
  (nh:enable-nurarihyon-reader-syntax)
  (lisp-unit:assert-equal
   (nth-value 0 (read-from-string "#d((a b c) (d e f))"))
   '(nh:double-matrix (list a b c) (list d e f)))
  (nh:disable-nurarihyon-reader-syntax))

(lisp-unit:define-test aref-syntax-test
    (nh:enable-nurarihyon-reader-syntax)
  (lisp-unit::assert-equal
   (nth-value 0 (read-from-string "[a 0]"))
   '(aref a 0))
  (lisp-unit::assert-equal
   (nth-value 0 (read-from-string "[a 0 1 2]"))
   '(aref a 0 1 2))
  (nh:disable-nurarihyon-reader-syntax))
