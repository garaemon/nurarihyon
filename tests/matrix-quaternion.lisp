;;================================================
;; matrix-quaternion.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

;; identity
(lisp-unit:define-test identity-matrix-quaternion-conversion
  (let ((m (nh:make-identity-matrix3)))
    (let ((q (nh:matrix33->quaternion m)))
      (lisp-unit:assert-true (nh:eps-vector= q (nh:identity-quaternion)))))
  ;; 3rd argument
  (let ((m (nh:make-identity-matrix3))
        (buf (nh:make-vector4)))
    (let ((q (nh:matrix33->quaternion m buf)))
      (lisp-unit:assert-true (nh:eps-vector= q (nh:identity-quaternion)))
      (lisp-unit:assert-eq q buf)))
  )

(lisp-unit:define-test matrix-quaternion-conversion1
  (let ((m #d((0.990033 -0.089418 0.108805)
              (0.099335 0.991028 -0.089418)
              (-0.099833 0.099335 0.990033)))
        (q-result #d(0.99638 0.04736 0.052349 0.04736)))
    (let ((q-compute (nh:matrix33->quaternion m)))
      (lisp-unit:assert-true (nh:eps-vector= q-result q-compute)))))

(lisp-unit:define-test matrix-quaternion-conversion2
  (let ((m #d((0.97517 -0.153792 0.159345)
              (0.097843 0.944703 0.312992)
              (-0.198669 -0.289629 0.936293)))
        (q-result #d(0.981856 -0.153439 0.091158 0.064071)))
    (let ((q-compute (nh:matrix33->quaternion m)))
      (lisp-unit:assert-true (nh:eps-vector= q-result q-compute)))))

(lisp-unit:define-test matrix-quaternion-conversion3
  (let ((m #d((0.808307 0.072066 -0.584334)
              (-0.341747 0.865602 -0.365982)
              (0.479426 0.49552 0.7243)))
        (q-result #d(0.921711 0.233669 -0.288528 -0.11224)))
    (let ((q-compute (nh:matrix33->quaternion m)))
      (lisp-unit:assert-true (nh:eps-vector= q-result q-compute)))))

(nh:disable-nurarihyon-reader-syntax)
