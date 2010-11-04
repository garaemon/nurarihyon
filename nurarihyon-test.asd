(defsystem nurarihyon-test
    :depends-on (nurarihyon lisp-unit)
    :components
    ((:file "nurarihyon-test")
     (:module "tests"
              :depends-on ("nurarihyon-test")
              :components
              ((:file "constants")
               (:file "degree-to-radian")
               (:file "degree-to-radian-to-degree")
               (:file "radian-to-degree")
               (:file "eps")
               ;; vector operations
               (:file "vector-create")
               (:file "copy-vector")
               (:file "vector-add")
               (:file "vector-sub")
               (:file "vector-dot")
               ))))



