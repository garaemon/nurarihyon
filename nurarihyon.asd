(defsystem nurarihyon
    :version "0.0.0"
    :author "R. Ueda (garaemon) <garaemon@gmail.com>"
    :licence "New BSD"
    :depends-on (chimi alexandria)
    :components ((:file "nurarihyon")
		 (:file "base" :depends-on ("nurarihyon"))
		 (:file "array" :depends-on ("nurarihyon" "base"))
		 ;;(:file "base" :depends-on ("math"))
                 ;;(:file "array" :depends-on ("base" "math"))
		 ))
