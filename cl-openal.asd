(asdf:defsystem cl-openal
  :version "0"
  :description "OpenAL bindings"
  :maintainer "Josh <sykopomp@sykosomatic.org>"
  :author "Josh <sykopomp@sykosomatic.org>"
  :licence "public domain"
  :depends-on (cffi)
  :components
  ((:file "packages")
   (:module al
            :depends-on ("packages")
            :components
            ((:file "bindings")
             (:file "al" :depends-on ("bindings"))))
   (:module alc
            :depends-on ("packages")
            :components
            ((:file "bindings")
             (:file "alc" :depends-on ("bindings"))))
   (:module alut
            :depends-on ("packages")
            :components
            ((:file "bindings")
             (:file "alut" :depends-on ("bindings"))))))



