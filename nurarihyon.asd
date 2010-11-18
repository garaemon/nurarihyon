(defsystem nurarihyon
    :version "0.0.1"
    :author "R. Ueda (garaemon) <garaemon@gmail.com>"
    :licence "New BSD"
    :depends-on (alexandria)
    :components
    ((:file "nurarihyon")
     (:file "conditions" :depends-on ("nurarihyon"))
     (:file "util" :depends-on ("nurarihyon" "conditions"))
     (:file "syntax" :depends-on ("nurarihyon" "util" "conditions"))
     (:file "base" :depends-on ("nurarihyon" "syntax" "conditions"))
     (:file "vector" :depends-on ("nurarihyon" "syntax" "base"))
     (:file "matrix" :depends-on ("nurarihyon" "syntax"
                                  "base" "vector"))
     (:file "quaternion" :depends-on ("nurarihyon" "syntax"
                                      "base" "vector"
                                      "matrix"))
     ;;(:file "geometry" :depends-on ("nurarihyon" "syntax"
     ;;                               "base" "vector"
     ;;                               "matrix"))
     ))
