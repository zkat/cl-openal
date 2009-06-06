(asdf:defsystem cl-openal
  :version "0"
  :description "OpenAL bindings"
  :maintainer "Josh <sykopomp@sykosomatic.org>"
  :author "Josh <sykopomp@sykosomatic.org>"
  :licence "Public Domain"
  :depends-on (cffi)
  :serial t
  :components ((:file "packages")
               (:file "al")
               (:file "alc")
               (:file "alut")))


