(in-package :alut)

(defun load-libraries ()
  (cffi:define-foreign-library alut
      (:windows "alut.dll" :calling-convention :stdcall)
    (:unix (:or "libalut.so" "libalut.so.0" "libalut.so.0.1.0"))
    (t (:default ("libalut"))))
  (cffi:use-foreign-library alut))

(defun init ()
  (%alut:init (cffi:null-pointer) (cffi:null-pointer)))
(defun init-without-context ()
  (%alut:init-without-context (cffi:null-pointer) (cffi:null-pointer)))
(defun exit ()
  (%alut:exit))

(defun get-error ()
  (%alut:get-error))
(defun get-error-string (error-name)
  (%alut:get-error-string error-name))

;;;
;;; Creating buffers
;;;
(defun create-buffer-from-file (filename)
  (%alut:create-buffer-from-file filename))

(defun create-buffer-from-file-image (data)
  (let ((length (length data)))
    (cffi:with-foreign-object (data-array :int length)
      (loop for i below length
         do (setf (cffi:mem-aref data-array :int i)
                  (elt data i)))
      (%alut:create-buffer-from-file-image data-array length))))

(defun create-buffer-hello-world ()
  (%alut:create-buffer-hello-world))

(defun create-buffer-waveform (waveshape frequency phase duration)
  (%alut:create-buffer-waveform waveshape frequency phase duration))

;;;
;;; Loading memory
;;;
(defun load-memory-from-file (filename)
  (let ((format (cffi:foreign-alloc '%al:enum))
        (size (cffi:foreign-alloc :int))
        (frequency (cffi:foreign-alloc '%al:ensure-float)))
    (values-list
     (cons
      (%alut:load-memory-from-file filename format size frequency)
      (handler-case
	  (list (cffi:mem-ref format '%al:enum)
		(cffi:mem-ref size :int)
		(cffi:mem-ref frequency '%al:ensure-float))
	(error ()
	  (error "There was an error loading ~A" filename)))))))

(defun load-memory-from-file-image (data)
  (let ((length (length data))
        (format (cffi:foreign-alloc '%al:enum))
        (size (cffi:foreign-alloc :int))
        (frequency (cffi:foreign-alloc '%al::ensure-float)))
    (cffi:with-foreign-object (data-array :int length)
      (loop for i below length
         do (setf (cffi:mem-aref data-array :int i)
                  (elt data i)))
      (values-list
       (cons
	(%alut:load-memory-from-file-image data-array length format size frequency)
	(handler-case
	    (list (cffi:mem-ref format '%al:enum)
		  (cffi:mem-ref size :int)
		  (cffi:mem-ref frequency '%al::ensure-float))
	  (error ()
	    (error "There was an error loading data"))))))))

(defun load-memory-hello-world ()
  (let ((format (cffi:foreign-alloc '%al:enum))
        (size (cffi:foreign-alloc :int))
        (frequency (cffi:foreign-alloc '%al::ensure-float)))
    (values-list
     (cons
      (%alut:load-memory-hello-world format size frequency)
      (handler-case
	  (list (cffi:mem-ref format '%al:enum)
		(cffi:mem-ref size :int)
		(cffi:mem-ref frequency '%al::ensure-float))
	(error ()
	  (error "There was an error loading memory!")))))))

(defun load-memory-waveform (waveshape frequency phase duration)
  (let ((format (cffi:foreign-alloc '%al:enum))
        (size (cffi:foreign-alloc :int))
        (freq (cffi:foreign-alloc '%al::ensure-float)))
    (values-list
     (cons
      (%alut:load-memory-waveform waveshape frequency phase duration format size freq)
      (handler-case
	  (list (cffi:mem-ref format '%al:enum)
		(cffi:mem-ref size :int)
		(cffi:mem-ref freq '%al::ensure-float))
	(error ()
	  (error "There was an error loading this waveform")))))))

;;;
;;; Misc
;;;
(defun get-mime-types (loader)
  (%alut:get-mime-types loader))

(defun get-major-version ()
  (%alut:get-major-version))
(defun get-minor-version ()
  (%alut:get-minor-version))

(defun sleep (duration)
  (%alut:sleep duration))

;;;
;;; Helper macros to keep the world clean.
;;;

(defmacro with-init (&body body)
  `(unwind-protect
	(init)
	(progn
	  ,@body)
     (alut:exit)))
