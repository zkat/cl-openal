;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; cl-openal-examples.asd --- ASDF system definition for various examples
;;;

(defsystem cl-openal-examples
  :description "Examples using cl-openal."
  :depends-on (cffi cl-openal cl-alc cl-alut)
  :components
  ((:module "examples"
            :serial t
            :components
            ((:file "packages")
             (:file "alut-hello-world")
             (:file "getting-started")
             (:file "lesson2")))))
