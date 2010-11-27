;;================================================
;; quaternion-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon-test)

(nh:enable-nurarihyon-reader-syntax)

;; identity
(lisp-unit:define-test identity-quaternion-matrix-conversion
  (let ((q (nh:identity-quaternion)))
    (let ((m (nh:quaternion->matrix33 q))
          (e (nh:make-identity-matrix 3)))
      (lisp-unit:assert-true (nh:eps-matrix= m e))))
  ;; 2nd argument
  (let ((q (nh:identity-quaternion))
        (buf (nh:make-matrix 3 3)))
    (let ((m (nh:quaternion->matrix33 q buf))
          (e (nh:make-identity-matrix 3)))
      (lisp-unit:assert-true (nh:eps-matrix= m e))
      (lisp-unit:assert-eq buf m)))
  )

(defmacro define-quatermion->matrix33-test (name quaternion matrix)
  `(lisp-unit:define-test ,name
     (let ((q ,quaternion)
           (mat-result ,matrix))
       (let ((mat-computed (nh:quaternion->matrix33 q)))
         (lisp-unit:assert-true (nh:eps-matrix= mat-result mat-computed))))))

(define-quatermion->matrix33-test quaternion-matrix-conversion1
    #d(0.99638 0.04736 0.052349 0.04736)
    #d((0.990033 -0.089418 0.108805)
       (0.099335 0.991028 -0.089418)
       (-0.099833 0.099335 0.990033)))

(define-quatermion->matrix33-test quaternion-matrix-conversion2
    #d(0.981856 -0.153439 0.091158 0.064071)
    #d((0.97517 -0.153792 0.159345)
       (0.097843 0.944703 0.312992)
       (-0.198669 -0.289629 0.936293)))

(define-quatermion->matrix33-test quaternion-matrix-conversion3
    #d(0.921711 0.233669 -0.288528 -0.11224)
    #d((0.808307 0.072066 -0.584334)
       (-0.341747 0.865602 -0.365982)
       (0.479426 0.49552 0.7243)))

(nh:disable-nurarihyon-reader-syntax)
