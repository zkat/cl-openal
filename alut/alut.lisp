(in-package :alut)

(defun load-libraries ()
  (cffi:define-foreign-library alut
    (:windows "alut.dll" :calling-convention :stdcall)
    (:darwin (:or (:default "libalut") (:framework "alut")))
    (:unix (:or "libalut.so" "libalut.so.0" "libalut.so.0.1.0"))
    (t (:default ("libalut"))))
  (cffi:use-foreign-library alut))

;; NOTE: ALUT-ERROR is defined in %ALUT, but we re-export it

(defvar *previous-error* nil
  "Internal: if non-NIL, stores the error code recorded by ALUT-ERROR handler established
  inside DEFUN-ALUT. In that case GET-ERROR will clear and return this value instead of
  actually calling `alutGetError'. Only relevant when AL:*PROPAGATE-ERRORS* is NIL")

(defmacro defun-alut (name (&rest args) &body body)
  "Like DEFUN, but will automatically handle errors in any ALUT bindings being
  called. This is different from DEFUN-AL, because ALUT error reporting is different from
  OpenAL error reporting. Because (save for a handful of exceptions) ALUT functions always
  report a boolean indicating whether an operation succeed, it's possible to make the
  error handling entirely automatic.

  BODY is executed inside a HANDLER-BIND, set up to handle ALUT-ERROR. If one is
  signalled, and AL:*PROPAGATE-ERRORS* is set, the handler will simply decline to handle
  it and let it propagate further. If it's NIL, the handler will record the code in
  *PREVIOUS-ERROR*, so that GET-ERROR can return it to the user when called"
  `(defun ,name (,@args)
     (handler-bind
         ((alut-error (lambda (err)
                        (unless *propagate-errors*
                          (setf *previous-error* (errcode err))
                          (return-from ,name)))))
       (clear-error)
       ,@body)))

(defun-alut init ()
  (%alut:init (cffi:null-pointer) (cffi:null-pointer)))
(defun-alut init-without-context ()
  (%alut:init-without-context (cffi:null-pointer) (cffi:null-pointer)))
(defun-alut exit ()
  (%alut:exit))

(defun clear-error ()
  (setf *previous-error* nil)
  (%alut:get-error))
(defun get-error ()
  (if *previous-error*
      (prog1 *previous-error*
        (setf *previous-error* nil))
      (%alut:get-error)))
(defun get-error-string (error-name)
  (%alut:get-error-string error-name))

;;;
;;; Creating buffers
;;;
(defun-alut create-buffer-from-file (filename)
  (%alut:create-buffer-from-file filename))

(defun-alut create-buffer-from-file-image (data)
  (let ((length (length data)))
    (cffi:with-foreign-object (data-array :int length)
      (loop for i below length
         do (setf (cffi:mem-aref data-array :int i)
                  (elt data i)))
      (%alut:create-buffer-from-file-image data-array length))))

(defun-alut create-buffer-hello-world ()
  (%alut:create-buffer-hello-world))

(defun-alut create-buffer-waveform (waveshape frequency phase duration)
  (%alut:create-buffer-waveform waveshape frequency phase duration))

;;;
;;; Loading memory
;;;
(defun-alut load-memory-from-file (filename)
  (cffi:with-foreign-objects ((format '%al:enum)
                              (size :int)
                              (frequency '%al:ensure-float))
    (values
     (%alut:load-memory-from-file filename format size frequency)
     (cffi:mem-ref format '%al:enum)
     (cffi:mem-ref size :int)
     (cffi:mem-ref frequency '%al:ensure-float))))

(defun-alut load-memory-from-file-image (data)
  (let ((length (length data)))
    (cffi:with-foreign-objects ((format '%al:enum)
                                (size :int)
                                (frequency '%al:ensure-float)
                                (data-array :int length))
      (loop for i below length
            do (setf (cffi:mem-aref data-array :int i)
                     (elt data i)))
      (values
       (%alut:load-memory-from-file-image data-array length format size
                                          frequency)
       (cffi:mem-ref format '%al:enum)
       (cffi:mem-ref size :int)
       (cffi:mem-ref frequency '%al::ensure-float)))))

(defun-alut load-memory-hello-world ()
  (cffi:with-foreign-objects ((format '%al:enum)
                              (size :int)
                              (frequency '%al:ensure-float))
    (values
     (%alut:load-memory-hello-world format size frequency)
     (cffi:mem-ref format '%al:enum)
     (cffi:mem-ref size :int)
     (cffi:mem-ref frequency '%al::ensure-float))))

(defun-alut load-memory-waveform (waveshape frequency phase duration)
  (cffi:with-foreign-objects ((format '%al:enum)
                              (size :int)
                              (freq '%al:ensure-float))
    (values
     (%alut:load-memory-waveform waveshape frequency phase duration format
                                 size freq)
     (cffi:mem-ref format '%al:enum)
     (cffi:mem-ref size :int)
     (cffi:mem-ref freq '%al::ensure-float))))

;;;
;;; Misc
;;;
(defun-alut get-mime-types (loader)
  (%alut:get-mime-types loader))

(defun get-major-version ()
  (%alut:get-major-version))
(defun get-minor-version ()
  (%alut:get-minor-version))

(defun-alut sleep (duration)
  (%alut:sleep duration))

;;;
;;; Helper macros to keep the world clean.
;;;

(defmacro with-init (&body body)
  `(unwind-protect
        (progn
          (init)
          ,@body)
     (alut:exit)))
