(asdf:defsystem cl-openal
  :version "1.0"
  :description "CFFI bindings for OpenAL sound system."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "public domain"
  :depends-on (cffi alexandria)
  :components
  ((:module al
            :components
            ((:file "packages")
             (:file "bindings" :depends-on ("packages"))
             (:file "al" :depends-on ("packages" "bindings"))))))
