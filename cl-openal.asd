(asdf:defsystem cl-openal
  :version "0"
  :description "OpenAL bindings"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "Public Domain"
  :depends-on (cffi)
  :serial t
  :components ((:file "packages")
               (:file "al")
               (:file "alc")
               (:file "alut")))


