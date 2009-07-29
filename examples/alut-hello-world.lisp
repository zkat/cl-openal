;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package #:cl-openal-examples)

;;;
;;; This is the traditional 'Hello, world!' alut style.
;;;
;;; For C version consult
;;; http://connect.creativelabs.com/openal/Documentation/The%20OpenAL%20Utility%20Toolkit.htm
;;;

(defun alut-hello-world ()
  (alut:init)
  (let ((buffer (alut:create-buffer-hello-world))
	(source (gen-source)))
    (al:source source :buffer buffer)
    (al:source-play source)
    (alut:sleep 1)
    (alut:exit)))

(defun alut-hello-world ()
  (alut:with-init
    (al:with-source (source)
      (let ((buffer (alut:create-buffer-hello-world)))
	(al:source source :buffer buffer)
	(al:source-play source)
	(alut:sleep 1)))))
