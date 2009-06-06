(asdf:defsystem cl-openal
  :version "0"
  :description "OpenAL bindings"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
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



