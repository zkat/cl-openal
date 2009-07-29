;;;
;;; Lesson 2 - Looping and fadeaway
;;;
;;; Common Lisp adaptation of
;;; http://www.devmaster.net/articles/openal-tutorials/lesson2.php
;;;
;;; (LESSON2) will use about 25 seconds of your time.
;;;

(defun init-source-data (sourcepos sourcevel)
  (let ((buffer (al:gen-buffer))
	(source (al:gen-source))
	(data   (alut:load-memory-hello-world)))
    ;; AL:GET-ERROR somewhere about here
    ;; is generally a good idea.
    (al:buffer-data buffer :mono16 data 16356 11025)
    (al:source source :buffer buffer)
    (al:source source :pitch    1.0)
    (al:source source :gain     1.0)
    (al:source source :position sourcepos)
    (al:source source :velocity sourcevel)
    (al:source source :looping  t)
    ;; GET-ERROR to see this all went smooth.
    (values buffer source data)))

(defun init-listener ()
  (al:listener :position    #(0 0 0))
  (al:listener :velocity    #(0 0 0))
  (al:listener :orientation #(0.0  0.0 -1.0
			      0.0  1.0  0.0)))

(defun lesson2 ()
    (let ((sourcepos #(0.0 0.0 0.0))
	  (sourcevel #(0.0 0.0 0.1)))
      (alut:with-init
	  (multiple-value-bind (buffer source data)
	      ;; m-v-b? I'm worried SBCL will forget about
	      ;; BUFFER and DATA. They're in C space.
	      (init-source-data sourcepos sourcevel)
	    (init-listener)
	    (al:source-play source)
	    (dotimes (i 250)
	      (sleep 0.1)
	      (setf sourcepos (map 'vector #'+ sourcepos sourcevel))
	      (al:source source :position sourcepos)))))) ; Move source.

