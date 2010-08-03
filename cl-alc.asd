(asdf:defsystem cl-alc
  :version "1.0"
  :description "CFFI bindings for OpenAL's ALC API."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "public domain"
  :depends-on (cffi cl-openal)
  :components
  ((:module alc
            :components
            ((:file "packages")
             (:file "bindings" :depends-on ("packages"))
             (:file "alc" :depends-on ("packages" "bindings"))))))