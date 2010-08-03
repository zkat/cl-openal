(asdf:defsystem cl-alut
  :version "1.0"
  :description "CFFI bindings for OpenAL's ALUT API."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "public domain"
  :depends-on (cffi cl-openal)
  :components
  ((:module alut
            :components
            ((:file "packages")
             (:file "bindings" :depends-on ("packages"))
             (:file "alut" :depends-on ("packages" "bindings"))))))
