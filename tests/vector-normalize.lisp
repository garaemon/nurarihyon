;;================================================
;; vector-normalize.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

(lisp-unit:define-test vector-normalize-test
  (let ((org #d(1 2 3 4 5 6)))
    (let ((n (nh:normalize-vector org)))
      ;; same length
      (lisp-unit:assert-eq (nh:vector-dimension org) (nh:vector-dimension n))
      ;; same direction
      (lisp-unit:assert-float-equal (nh:v. org n) (nh:norm org))))
  ;; buffer test
  (let ((org #d(-1 2 -3 4 -5 6))
        (buf (nh:make-vector 6)))
    (let ((n (nh:normalize-vector org buf)))
      ;; same length
      (lisp-unit:assert-eq (nh:vector-dimension org) (nh:vector-dimension n))
      ;; same direction
      (lisp-unit:assert-float-equal (nh:v. org n) (nh:norm org))
      (lisp-unit:assert-eq n buf)))
  (let* ((org #d(1 -2 3 -4 5 -6))
         (org-copy (nh:copy-vector org)))
    (let ((n (nh:normalize-vector org org)))
      ;; same length
      (lisp-unit:assert-eq (nh:vector-dimension org) (nh:vector-dimension n))
      ;; same direction
      (lisp-unit:assert-float-equal (nh:v. org-copy n) (nh:norm org-copy))
      (lisp-unit:assert-eq n org)))
  )

(nh:disable-nurarihyon-reader-syntax)
