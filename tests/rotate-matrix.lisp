;;================================================
;; rotate-matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :nurarihyon)
(require :lisp-unit)

(nh:enable-nurarihyon-reader-syntax)

;; random test
(defmacro define-+--test (name func)
  `(lisp-unit:define-test ,name
       (dotimes (i 100)
         (let ((mat (nh:make-identity-matrix3)))
           (let ((ang (nh:random-range nh:+-pi+ nh:+pi+))
                 (vec (nh:normalize-vector (nh:make-random-vector 3))))
             (lisp-unit:assert-true
              (nh:eps-matrix= mat
                              (,func (,func mat ang vec) (- ang) vec)))
             (lisp-unit:assert-true
              (nh:eps-matrix= mat
                              (,func (,func mat (- ang) vec) ang vec)))
             (lisp-unit:assert-true
              (nh:eps-matrix= mat
                              (,func (,func mat ang vec) ang
                                     (nh:scale -1.0d0 vec))))
             (lisp-unit:assert-true
              (nh:eps-matrix= mat
                              (,func (,func mat ang (nh:scale -1.0d0 vec))
                                     ang vec)))
             ))))
  )

(define-+--test rotate-matrix-local-+- nh:rotate-matrix-local)
(define-+--test rotate-matrix-world-+- nh:rotate-matrix-world)

;; fix test set
(lisp-unit:define-test rotate-matrix-identity-test
  (labels ((test-func (ang)
             (let ((sin (sin ang))
                   (cos (cos ang)))
               (lisp-unit:assert-true
                (nh:eps-matrix=
                 (nh:rotate-matrix-local
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(1 0 0))
                 (nh:double-matrix `(1 0 0)
                                   `(0 ,cos ,(- sin))
                                   `(0 ,sin ,cos))))
               (lisp-unit:assert-true
                (nh:eps-matrix= 
                 (nh:rotate-matrix-local
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(0 1 0))
                 (nh:double-matrix `(,cos     0 ,sin)
                                   `(0        1 0)
                                   `(,(- sin) 0 ,cos))))
               (lisp-unit:assert-true
                (nh:eps-matrix= 
                 (nh:rotate-matrix-local
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(0 0 1))
                 (nh:double-matrix `(,cos ,(- sin)   0)
                                   `(,sin  ,cos      0)
                                   `(0      0        1))))
               (lisp-unit:assert-true
                (nh:eps-matrix= 
                 (nh:rotate-matrix-world
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(1 0 0))
                 (nh:double-matrix `(1 0 0)
                                   `(0 ,cos ,(- sin))
                                   `(0 ,sin ,cos))))
               (lisp-unit:assert-true
                (nh:eps-matrix= 
                 (nh:rotate-matrix-world
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(0 1 0))
                 (nh:double-matrix `(,cos     0 ,sin)
                                   `(0        1 0)
                                   `(,(- sin) 0 ,cos))))
               (lisp-unit:assert-true
                (nh:eps-matrix= 
                 (nh:rotate-matrix-world
                  #d((1 0 0) (0 1 0) (0 0 1)) ang #d(0 0 1))
                 (nh:double-matrix `(,cos ,(- sin)   0)
                                   `(,sin  ,cos      0)
                                   `(0      0        1)))))))
    (test-func (nh:deg2rad 30.0d0))
    (test-func (nh:deg2rad 60.0d0))
    (test-func (nh:deg2rad 90.0d0))
    (test-func (nh:deg2rad 120.0d0))
    (test-func (nh:deg2rad 150.0d0))
    (test-func (nh:deg2rad 180.0d0))
    (test-func (nh:deg2rad 210.0d0))
    (test-func (nh:deg2rad 240.0d0))
    (test-func (nh:deg2rad 270.0d0))
    (test-func (nh:deg2rad 300.0d0))
    (test-func (nh:deg2rad 330.0d0))
    (test-func (nh:deg2rad 360.0d0))))
  

;; (lisp-unit:define-test rotate-matrix-world-fix-test
;;   )

(lisp-unit:run-tests rotate-matrix-local-+- rotate-matrix-world-+-
                     rotate-matrix-identity-test)
