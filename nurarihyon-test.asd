(defsystem nurarihyon-test
    :depends-on (nurarihyon lisp-unit)
    :components
    ((:file "nurarihyon-test")
     (:module "tests"
              :depends-on ("nurarihyon-test")
              :components
              ((:file "constants")
               (:file "random-range")
               (:file "degree-to-radian")
               (:file "degree-to-radian-to-degree")
               (:file "radian-to-degree")
               (:file "eps")
               ;; syntax
               (:file "array-syntax")
               (:file "infix-syntax")
               ;; vector operations
               (:file "eps-vector")
               (:file "vector-dimension")
               (:file "vector-scale")
               (:file "vector-copy")
               (:file "vector-normalize")
               (:file "vector-create")
               (:file "copy-vector")
               (:file "vector-add")
               (:file "vector-sub")
               (:file "vector-dot")
               (:file "vector-cross")
               (:file "vector-distance")
               (:file "vector-norm")
               (:file "vector-sum")
               ;; matrix operations
               (:file "matrix-create")
               (:file "matrix-row")
               (:file "matrix-column")
               (:file "matrix-add")
               (:file "matrix-sub")
               (:file "matrix-add-and-sub")
               (:file "matrix-transpose")
               (:file "matrix-multiply")
               (:file "matrix-vector-multiply")
               ;; quaternion
               (:file "quaternion-accessors")
               (:file "identity-quaternion")
               (:file "matrix-quaternion")
               (:file "quaternion-matrix")
               (:file "quaternion-conjugate")
               ))))
