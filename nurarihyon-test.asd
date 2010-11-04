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
               (:file "copy-vector")
               ))))



