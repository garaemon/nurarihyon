;;================================================
;; array-syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon-test)

(lisp-unit:define-test double-vector-syntax-test
  (nh:enable-nurarihyon-reader-syntax)
  (lisp-unit:assert-equal
   (let ((r (read-from-string "#d(a b c)")))
     r)                                 ;remove multi-values
   '(nh:double-vector a b c))
  (nh:disable-nurarihyon-reader-syntax))

(lisp-unit:define-test double-matrix-syntax-test
  (nh:enable-nurarihyon-reader-syntax)
  (lisp-unit:assert-equal
   (let ((r (read-from-string "#d((a b c) (d e f))")))
     r)
   '(nh:double-matrix (list a b c) (list d e f)))
  (nh:disable-nurarihyon-reader-syntax))
  