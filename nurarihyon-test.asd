(defsystem nurarihyon-test
    :depends-on (nurarihyon lisp-unit)
    :components
    ((:module "tests"
              :components
              ((:file "nurarihyon-test")
               (:file "constants" :depends-on ("nurarihyon-test"))
               (:file "degree-to-radian" :depends-on ("nurarihyon-test"))
               (:file "copy-vector" :depends-on ("nurarihyon-test"))
               ))))



