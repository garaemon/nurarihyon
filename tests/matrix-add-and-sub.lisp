;;================================================
;; matrix-add-and-sub.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)
(nurarihyon:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test matrix-addsub-test
  (dotimes (i 10)
    (let ((a (nurarihyon:double-matrix
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0))
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0))
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0))))
          (b (nurarihyon:double-matrix
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0))
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0))
              (list (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)
                    (nurarihyon:random-range -100.0d0 100.0d0)))))
      (lisp-unit:assert-true (nurarihyon:eps-matrix=
                              (nurarihyon:m- (nurarihyon:m+ a b) b) a))
      (lisp-unit:assert-true (nurarihyon:eps-matrix=
                              (nurarihyon:m- (nurarihyon:m+ a b) a) b))
      )))

(nurarihyon:disable-nurarihyon-reader-syntax)

