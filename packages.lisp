(defpackage #:cl-openal
  (:use :cl)
  (:nicknames :al))
(defpackage #:openal-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%al))

(defpackage #:cl-openal-alc
  (:use :cl)
  (:nicknames :alc))
(defpackage #:alc-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alc))

(defpackage #:cl-openal-alut
  (:use :cl)
  (:nicknames :alut))
(defpackage #:alut-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alut))