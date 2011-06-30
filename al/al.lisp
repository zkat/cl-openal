(in-package :al)

;;;; misc.
(defun load-libraries ()
  (cffi:define-foreign-library al
    (:windows "OpenAL32.dll" :calling-convention :stdcall)
    (:darwin (:or (:default "libopenal") (:framework "openal")))    
    (:unix (:or "libopenal.so" "libopenal.so.1"))
    (t (:default "libopenal")))
  (cffi:use-foreign-library al))

;; Renderer State management
(defun enable (capability)
  (%al:enable capability))
(defun disable (capability)
  (%al:disable capability))
(defun enabledp (capability)
  (%al:is-enabled capability))

;; State retrieval
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
(defun listener (param value)
  (ecase param
    ((:position :velocity)
     (assert (= 3 (length value)))
     (%al:listener-3f param (elt value 0) (elt value 1) (elt value 2)))
    ((:orientation)
     (assert (= 6 (length value)))
     (cffi:with-foreign-object (array :float 6)
       (loop for i below 6
          doing (setf (cffi:mem-aref array :float i)
                      (coerce (elt value i) 'float))
          finally (%al:listener-fv param array))))
    ((:gain)
     (%al:listener-f param value))))

(defun get-listener (param)
  (ecase param
    ((:gain)
     (let* ((ptr (cffi:foreign-alloc :float))
            (val (progn
                   (%al:get-listener-f param ptr)
                   (cffi:mem-ref ptr :float))))
       val))
    ((:orientation)
     (cffi:with-foreign-object (listener-array :float 6)
       (%al:get-listener-fv param listener-array)
       (loop for i below 6
          collecting (cffi:mem-aref listener-array :float i))))
    ((:position :velocity)
     (cffi:with-foreign-object (listener-array :float 3)
       (%al:get-listener-fv param listener-array)
       (loop for i below 3
          collect (cffi:mem-aref listener-array :float i))))))

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
  (ecase param
    ((:gain :pitch :min-gain :max-gain :reference-distance :rolloff-factor :max-distance
            :sec-offset :sample-offset :byte-offset :cone-inner-angle :cone-outer-angle :cone-outer-gain)
     (%al:source-f sid param value))
    ((:looping :source-relative)
     (%al:source-i sid param (if value 1 0)))
    ((:source-type :buffer)
     (%al:source-i sid param value))
    ((:position :velocity :direction)
     (assert (= 3 (length value)))
     (%al:source-3f sid param (elt value 0) (elt value 1) (elt value 2)))))

(defun get-source (sid param)
  (ecase param
    ((:gain :pitch :min-gain :max-gain :reference-distance
            :sec-offset :rolloff-factor :max-distance :cone-inner-angle :cone-outer-angle :cone-outer-gain
            :sample-offset :byte-offset)
     (let* ((ptr (cffi:foreign-alloc :float))
            (val (progn
                   (%al:get-source-f sid param ptr)
                   (cffi:mem-ref ptr :float))))
       val))
    ((:looping :source-relative)
     (let* ((ptr (cffi:foreign-alloc :int))
            (val (progn
                   (%al:get-source-i sid param ptr)
                   (cffi:mem-ref ptr :int))))
       (if (> val 0)
           t nil)))
    ((:source-type :buffer :buffers-queued :buffers-processed)
     (let* ((ptr (cffi:foreign-alloc :int))
            (val (progn
                   (%al:get-source-i sid param ptr)
                   (cffi:mem-ref ptr :int))))
       val))
    (:source-state
     (let* ((ptr (cffi:foreign-alloc :int))
            (val (progn
                   (%al:get-source-i sid param ptr)
                   (cffi:mem-ref ptr :int))))
       (cffi:foreign-enum-keyword '%al:enum val)))
    ((:position :velocity :direction)
     (cffi:with-foreign-object (source-array :float 3)
       (%al:get-source-fv sid param source-array)
       (loop for i below 3
          collect (cffi:mem-aref source-array :float i))))))

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

(defun source-unqueue-buffers (sid &optional (num-buffers 1))
  (cffi:with-foreign-object (buffer-array :uint)
    (setf (cffi:mem-ref buffer-array :uint) 0)
    (%al:source-unqueue-buffers sid num-buffers buffer-array)
    (unless (zerop (cffi:mem-ref buffer-array :uint))
      (loop for i below num-buffers
         collect (cffi:mem-aref buffer-array :uint i)))))

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
  (%al:buffer-i bid param value))

(defun buffer-data (bid format data size freq)
  (%al:buffer-data bid format data size freq))

(defun get-buffer (bid param)
  (let* ((ptr (cffi:foreign-alloc :int))
         (val (progn
                (%al:get-buffer-i bid param ptr)
                (cffi:mem-ref ptr :int))))
    val))

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


;;;
;;; Helper macros to keep the world tidy.
;;;

(defmacro with-sources ((n var) &body body)
  `(let ((,var (gen-sources ,n)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-sources ,var)))))

(defmacro with-source ((var) &body body)
  `(let ((,var (gen-source)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-source ,var)))))

(defmacro with-buffers ((n var) &body body)
  `(let ((,var (gen-buffers ,n)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-buffers ,var)))))

(defmacro with-buffer ((var) &body body)
  `(let ((,var (gen-buffer)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-buffer ,var)))))
