(in-package :alc)

(defun open-device (&optional device-name)
  (let ((foreign-dev (%alc:open-device (or device-name
                                           (cffi:null-pointer)))))
    (if (cffi:null-pointer-p foreign-dev)
        nil
        foreign-dev)))

(defun close-device (device)
  (%alc:close-device device))

(defun create-context (device &rest attributes)
  (let ((foreign-ctx
         (%alc:create-context device
                              (if attributes
                                  (let ((n (length attributes)))
                                    (cffi:with-foreign-object (attrlist '%alc:attribute n)
                                      (loop for i below n
                                         do (setf (cffi:mem-aref attrlist '%alc:attribute i)
                                                  (elt attributes i)))
                                      attrlist))
                                  (cffi:null-pointer)))))
    (if (cffi:null-pointer-p foreign-ctx)
        nil
        foreign-ctx)))

(defun make-context-current (context)
  (%alc:make-context-current context))

(defun process-context (context)
  (%alc:process-context context))
(defun suspend-context (context)
  (%alc:suspend-context context))
(defun destroy-context (context)
  (%alc:destroy-context context))

(defun get-current-context ()
  (%alc:get-current-context))
(defun get-contexts-device (context)
  (%alc:get-contexts-device context))

(defun get-error (device)
  (%alc:get-error device))

(defun extension-present-p (device extname)
  (%alc:is-extension-present device extname))
(defun get-proc-address (funcname)
  (%alc:get-proc-address funcname))

(defun get-enum-value (device enum-name)
  (%alc:get-enum-value device enum-name))

(defun get-string (device param)
  (%alc:get-string device param))
(defun get-integer (device param)
  (cffi:with-foreign-object (size-arr :int 1)
    (%alc:get-integer-v device :attributes-size 1 size-arr)
    (let ((size (cffi:mem-aref size-arr :int 0)))
     (cffi:with-foreign-object (int-list :int size)
       (%alc:get-integer-v device param size int-list)
       (loop for i below size
            collect (cffi:mem-aref int-list :int i))))))

(defun capture-open-device (device-name frequency format buffer-size)
  (%alc:capture-open-device device-name (coerce frequency 'int) format (coerce buffer-size 'int)))
(defun capture-close-device (device)
  (%alc:capture-close-device device))
(defun capture-start (device)
  (%alc:capture-start device))
(defun capture-stop (device)
  (%alc:capture-stop device))
(defun capture-samples (device samples)
  (let ((n-samples (length samples)))
    (cffi:with-foreign-object (buffer :pointer n-samples)
      (loop for i below n-samples
           do (setf (cffi:mem-aref buffer i)
                    (elt samples i)))
      (%alc:capture-samples device buffer n-samples))))

;;;
;;; Helper macros to keep the world tidy.
;;;

(defmacro with-capture-device ((var device-name frequency
				    format buffer-size) &body body)
  `(let ((,var (capture-open-device ,device-name ,frequency
				    ,format ,buffer-size)))
     (unwind-protect
	  (progn
	    ,@body)
       (when ,var (capture-close-device ,var)))))

(defmacro with-device ((var &optional (device-name nil)) &body body)
  `(let ((,var (open-device ,device-name)))
     (unwind-protect
	  (progn
	    ,@body)
       (when ,var (close-device ,var)))))

