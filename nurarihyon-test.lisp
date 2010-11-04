(defpackage #:nurarihyon-test
  (:use #:common-lisp)
  (:export #:run-all-tests)
  )

(in-package :nurarihyon-test)

(defun run-all-tests ()
  (lisp-unit:run-all-tests :nurarihyon-test))

