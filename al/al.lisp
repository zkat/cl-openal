(in-package :al)

;; Renderer State management
(defun enable (capability)
  (%al:enable capability))
(defun disable (capability)
  (%al:disable capability))
(defun enabledp (capability)
  (%al:is-enabled capability))

;; State retrieval
;; TODO: do I actually need all of these?...
(defun get-string (param)
  (%al:get-string param))
(defun get-boolean (param)
  (%al:get-boolean param))
(defun get-integer (param)
  (%al:get-integer param))

;; Errors
(defun get-error ()
  (%al:get-error))

;; Extensions
(defun extension-present-p (extension-string)
  (%al:is-extension-present extension-string))
(defun get-proc-address (fname)
  (%al:get-proc-address fname))
(defun get-enum-value (enum-name)
  (%al:get-enum-value enum-name))

;;;
;;; Listener
;;;
(defun listener (param value1 value2 value3)
  (%al:listener-3f param value1 value2 value3))

(defun get-listener (param)
  (cffi:with-foreign-object (listener-array :float 3)
    (%al:get-listener-fv param listener-array)
    (loop for i below 3
         collect (cffi:mem-aref listener-array :float i))))

;;;
;;; Sources
;;;
(defun gen-sources (n)
  (cffi:with-foreign-object (source-array :uint n)
    (%al:gen-sources n source-array)
    (loop for i below n
         collect (cffi:mem-aref source-array :uint))))
(defun delete-sources (sources)
  (let ((n (length sources)))
   (cffi:with-foreign-object (source-array :uint n)
     (loop for i below n
        do (setf 
            (cffi:mem-aref source-array :uint i)
            (elt sources i)))
     (%al:delete-sources n source-array))))
(defun gen-source ()
  (car (gen-sources 1)))
(defun delete-source (sid)
  (delete-sources (list sid)))

(defun sourcep (sid)
  (%al:is-source sid))

(defun source (sid param value)
  (cond ((and (or (listp value)
                  (vectorp value))
              (= 3 (length value))
              (every #'numberp value))
         (%al:source-3f sid param (float (elt value 0) 1.0)
                        (float (elt value 1) 1.0) (float (elt value 2) 1.0)))
        ((integerp value)
         (%al:source-i sid param value))
        ((floatp value)
         (%al:source-f sid param value))
        (t
         (%al:source-i sid param (if value
                                     1 0)))))

(defun get-source (sid param)
  (cffi:with-foreign-object (source-array :float 3)
    (%al:get-source-fv sid param source-array)
    (loop for i below 3
         collect (cffi:mem-aref source-array :float i))))

;; Playback
(defun source-play (sid)
  (%al:source-play sid))
(defun source-stop (sid)
  (%al:source-stop sid))
(defun source-rewind (sid)
  (%al:source-rewind sid))
(defun source-pause (sid)
  (%al:source-pause sid))

;; queueing
(defun source-queue-buffers (sid buffers)
  (let ((n (length buffers)))
    (cffi:with-foreign-object (buffer-array :uint n)
      (loop for i below n
         do (setf (cffi:mem-aref buffer-array :uint i)
                  (elt buffers i)))
      (%al:source-queue-buffers sid n buffer-array))))
(defun source-unqueue-buffers (sid buffers)
  (let ((n (length buffers)))
    (cffi:with-foreign-object (buffer-array :uint n)
      (loop for i below n
         do (setf (cffi:mem-aref buffer-array :uint i)
                  (elt buffers i)))
      (%al:source-unqueue-buffers sid n buffer-array))))

;;;
;;; Buffers
;;;
(defun gen-buffers (n)
  (cffi:with-foreign-object (buffer-array :uint n)
    (%al:gen-buffers n buffer-array)
    (loop for i below n
         collect (cffi:mem-aref buffer-array :uint))))
(defun delete-buffers (buffers)
  (let ((n (length buffers)))
   (cffi:with-foreign-object (buffer-array :uint n)
     (loop for i below n
        do (setf 
            (cffi:mem-aref buffer-array :uint i)
            (elt buffers i)))
     (%al:delete-buffers n buffer-array))))
(defun gen-buffer ()
  (car (gen-buffers 1)))
(defun delete-buffer (bid)
  (delete-buffers (list bid)))

(defun bufferp (buffer-id)
  (%al:is-buffer buffer-id))

(defun buffer (bid param value)
  (cond ((and (or (listp value)
                  (vectorp value))
              (= 3 (length value))
              (every #'numberp value))
         (%al:buffer-3f bid param (float (elt value 0) 1.0)
                        (float (elt value 1) 1.0) (float (elt value 2) 1.0)))
        ((integerp value)
         (%al:buffer-i bid param value))
        ((floatp value)
         (%al:buffer-f bid param value))
        (t
         (%al:buffer-i bid param (if value
                                     1 0)))))

(defun get-buffer (bid param)
  (cffi:with-foreign-object (buffer-array :float 3)
    (%al:get-buffer-fv bid param buffer-array)
    (loop for i below 3
         collect (cffi:mem-aref buffer-array :float i))))

;;;
;;; Global parameters
;;;
(defun doppler-factor (value)
  (%al:doppler-factor value))
(defun doppler-velocity (value)
  (%al:doppler-velocity value))
(defun speed-of-sound (value)
  (%al:speed-of-sound value))
(defun distance-model (model-param)
  (%al:distance-model model-param))