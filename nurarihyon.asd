(defsystem nurarihyon
    :version "0.0.1"
    :author "R. Ueda (garaemon) <garaemon@gmail.com>"
    :licence "New BSD"
    :depends-on (chimi alexandria)
    :components ((:file "nurarihyon")
                 (:file "syntax" :depends-on ("nurarihyon"))
		 (:file "base" :depends-on ("nurarihyon" "syntax"))
                 (:file "vector" :depends-on ("nurarihyon" "syntax" "base"))
                 (:file "matrix" :depends-on ("nurarihyon" "syntax"
                                              "base" "vector"))
                 (:file "geometry" :depends-on ("nurarihyon" "syntax"
                                                "base" "vector"
                                                "matrix"))
		 ))
