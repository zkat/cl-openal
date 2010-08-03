(asdf:defsystem cl-alc
  :version "1.0"
  :description "CFFI bindings for OpenAL's ALC API."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "public domain"
  :depends-on (cffi cl-openal)
  :components
  ((:module alc
            :components
            ((:file "packages")
             (:file "bindings" :depends-on ("packages"))
             (:file "alc" :depends-on ("packages" "bindings"))))))