(asdf:defsystem cl-openal
  :version "0"
  :description "OpenAL bindings"
  :maintainer "Josh <sykopomp@sykosomatic.org>"
  :author "Josh <sykopomp@sykosomatic.org>"
  :licence "Public Domain"
  :depends-on (cffi)
  :serial t
  :components
  ((:file "packages")
   (:module al
            :serial t
            :components
            ((:file "bindings")
             (:file "al")))
   (:module alc
            :serial t
            :components
            ((:file "bindings")
             (:file "alc")))
   (:module alut
            :serial t
            :components
            ((:file "bindings")
             (:file "alut")))))



