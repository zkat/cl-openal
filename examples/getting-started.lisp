(in-package #:cl-openal-examples)

;;;
;;; http://www.devmaster.net/articles/openal/
;;;

(defun getting-started ()
  (let ((data nil))
    (alut:with-init
	;; Init alut so LOAD-MEMORY-HELLO-WORLD works.
	(setf data (alut:load-memory-hello-world)))
    (alc:with-device (device)
      ;; Here it is appropriate to check so
      ;; device is actually opened. GET-ERROR
      ;; for example.
      (alc:with-context (context device)
	(alc:make-context-current context)
	;; Again: GET-ERROR.
	(al:with-buffer (buffer)
	  (al:with-source (source)
	    (al:buffer-data buffer #x1101 data 16356 11025)
	    ;; How do I know format, size, freq?
	    ;; LOAD-MEMORY-HELLO-WORLD told me.
	    ;; The appropirate thing to do if you don't
	    ;; know these values is m-v-b.
	    ;; Link source to buffer, and place
	    ;; source at (1 1 1).
	    (al:source source :buffer buffer)
	    (al:source source :position (list 1 1 1))
	    (al:source source :velocity (list 5 5 0))
	    ;; Place listener at (1 1 1), and have it
	    ;; face (0 0 0).
	    (al:listener :position (list 1 1 1))
	    (al:listener :orientation (list 0 0 0))
	    ;; Let the music play...
	    (al:source source :looping :true)
	    (al:source-play source)
	    (sleep 10)
	    (al:source-stop source)))))))
