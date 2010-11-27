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
  ;; 2nd argument
  (let ((m (nh:make-identity-matrix3))
        (buf (nh:make-vector4)))
    (let ((q (nh:matrix33->quaternion m buf)))
      (lisp-unit:assert-true (nh:eps-vector= q (nh:identity-quaternion)))
      (lisp-unit:assert-eq q buf)))
  )

(defmacro define-matrix33->quaternion-test (name matrix quaternion)
  `(lisp-unit:define-test ,name
     (let ((m ,matrix)
           (q-result ,quaternion))
       (let ((q-computed (nh:matrix33->quaternion m)))
         (lisp-unit:assert-true (nh:eps-vector= q-result q-computed))))))

(define-matrix33->quaternion-test matrix-quaternion-conversion1
    #d((0.990033 -0.089418 0.108805)
       (0.099335 0.991028 -0.089418)
       (-0.099833 0.099335 0.990033))
    #d(0.99638 0.04736 0.052349 0.04736))

(define-matrix33->quaternion-test matrix-quaternion-conversion2
    #d((0.97517 -0.153792 0.159345)
       (0.097843 0.944703 0.312992)
       (-0.198669 -0.289629 0.936293))
    #d(0.981856 -0.153439 0.091158 0.064071))

(define-matrix33->quaternion-test matrix-quaternion-conversion3
    #d((0.808307 0.072066 -0.584334)
       (-0.341747 0.865602 -0.365982)
       (0.479426 0.49552 0.7243))
    #d(0.921711 0.233669 -0.288528 -0.11224))

(nh:disable-nurarihyon-reader-syntax)
