(in-package :al)

;; Errors
(defvar *propagate-errors* t
  "If set, PROPAGATE-ERROR will automatically be called after every operation. If NIL, it
  is the user's responsibility to call GET-ERROR or PROPAGATE-ERROR")

;; FIXME: What is OpenAL's contract when it comes to multithreaded code? The spec only
;; seems to mention that ALC's contexts are thread-safe, but makes absolutely no mention
;; of how errors when multiple threads are in use. As far as I can tell, it might be
;; actually not specified, but in practice the error context is probably shared between
;; threads: https://camlorn.net/posts/april2014/shortcomings-of-openal/
;;
;; This means that a DEFVAR will not reproduce the same behaviour, as in most implementations
;; dynamic bindings are thread-local, but also it's probably not the best idea to be accessing
;; OpenAL from multiple threads.
(defvar *previous-error* nil
  "Internal: If non-NIL, stores the previous error reported by `alGetError'. Needed to
  implement PEEK-ERROR functionality")

(define-condition al-error ()
  ((%errcode :initarg :errcode :reader errcode))
  (:report (lambda (c stream)
             (format stream "OpenAL error ~A" (errcode c)))))

(defun peek-error ()
  "Peek the current error. If no peek has been performed since last time the error state
was cleared, this call `alGetError' internally, but store it in a location inspected by
GET-ERROR, so that it isn't cleared until the user has a chance to inspect it. If an error
is already stored, return it immediately without touching the OpenAL error state.

This function should be used by library code to avoid irretrevably clearing the AL error
state if operating in *PROPAGATE-ERRORS* NIL mode. End-user code should not be using it
unless implementing custom wrappers; see GET-ERROR and CLEAR-ERROR instead"
  (if *previous-error*
      *previous-error*
      (setf *previous-error* (%al:get-error))))

(defun get-error ()
  "Get the last AL error. If PEEK-ERROR has been called previously, the stored value will
be cleared and returned. Otherwise, `alGetError' will be called. If called between every
AL operation, this will have the same effect of clearing the error state as the underlying
`alGetError' does. However, to clear the error state no matter what previous operations
were performed, even if the user code did not properly inspect it, CLEAR-ERROR is
preferable"
  (if *previous-error*
      (prog1
          *previous-error*
        (setf *previous-error* nil))
      (%al:get-error)))

(defun clear-error ()
  "Unconditionally clear the previous error state. This guarantees that the effects of
previous PEEK-ERROR calls are undone, and that `alGetError' has been called. Returns the
result of calling `alGetError'"
  (setf *previous-error* nil)
  (%al:get-error))

(defun propagate-error ()
  "Call GET-ERROR, and if an error is detected, signal it wrapped in AL-ERROR. If
  OPERATION is provided, it will be included as context in the reported condition"
  (let ((err (get-error)))
    (unless (eq err :no-error)
      (error 'al-error :errcode err))))

(defmacro defun-al (name (&rest args) &body body)
  "Helper macro to define a function wrapping an OpenAL operation. Functions just like
  regular DEFUN, except for OpenAL error management:

  * Before BODY, (CLEAR-ERROR) is called, so BODY is guaranteed to execute in a fresh
    error context
  * Inside BODY, a local macro CHECKPOINT is defined. Calling it will perform error
    handling, the precise nature of which depends on the value of *PROPAGATE-ERRORS*:
    - If *PROPAGATE-ERRORS* is set (default), (PROPAGATE-ERROR) will be called, and signal
      if an error was detected
    - If *PROPAGATE-ERRORS* is NIL (legacy behaviour), (PEEK-ERROR) will be called, and if
      an error was detected, the function will immediately abort and return NIL
  * (CHECKPOINT) should be inserted after every call to underlying OpenAL C functions, and
    before any results are consumed. This ensures that no unsafe memory accesses can
    happen if the operation failed. A call to CHECKPOINT will be appended at the end of
    BODY automatically, so it's not necessary to add it manually"
  (with-gensyms (error)
   `(defun ,name (,@args)
      (clear-error)
      (macrolet ((checkpoint ()
                   `(if *propagate-errors*
                        (propagate-error)
                        (let ((,',error (peek-error)))
                          (unless (eq ,',error :no-error)
                            (return-from ,',name))))))
        (prog1
            (progn
              ,@body)
          (checkpoint))))))

;;;; misc.
(defun load-libraries ()
  (cffi:define-foreign-library al
    (:windows "OpenAL32.dll" :calling-convention :stdcall)
    (:darwin (:or (:default "libopenal") (:framework "openal")))    
    (:unix (:or "libopenal.so" "libopenal.so.1"))
    (t (:default "libopenal")))
  (cffi:use-foreign-library al))

;; Renderer State management
(defun-al enable (capability)
  (%al:enable capability))
(defun-al disable (capability)
  (%al:disable capability))
(defun-al enabledp (capability)
  (%al:is-enabled capability))

;; State retrieval
(defun-al get-string (param)
  (%al:get-string param))
(defun-al get-boolean (param)
  (%al:get-boolean param))
(defun-al get-integer (param)
  (%al:get-integer param))

;; Extensions
(defun-al extension-present-p (extension-string)
  (%al:is-extension-present extension-string))
(defun-al get-proc-address (fname)
  (%al:get-proc-address fname))
(defun-al get-enum-value (enum-name)
  (%al:get-enum-value enum-name))

;;;
;;; Listener
;;;
(defun-al listener (param value)
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

(defun-al get-listener (param)
  (ecase param
    ((:gain)
     (cffi:with-foreign-object (ptr :float)
       (%al:get-listener-f param ptr)
       (cffi:mem-ref ptr :float)))
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
(defun-al gen-sources (n)
  (cffi:with-foreign-object (source-array :uint n)
    (%al:gen-sources n source-array)
    (checkpoint)
    (loop for i below n
       collect (cffi:mem-aref source-array :uint i))))

(defun-al delete-sources (sources)
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

(defun-al sourcep (sid)
  (%al:is-source sid))

(defun-al source (sid param value)
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

(defun-al get-source (sid param)
  (ecase param
    ((:gain :pitch :min-gain :max-gain :reference-distance
            :sec-offset :rolloff-factor :max-distance :cone-inner-angle :cone-outer-angle :cone-outer-gain
            :sample-offset :byte-offset)
     (cffi:with-foreign-object (ptr :float)
       (%al:get-source-f sid param ptr)
       (cffi:mem-ref ptr :float)))
    ((:looping :source-relative)
     (cffi:with-foreign-object (ptr :int)
       (%al:get-source-i sid param ptr)
       (cffi:mem-ref ptr :boolean)))
    ((:source-type :buffer :buffers-queued :buffers-processed)
     (cffi:with-foreign-object (ptr :int)
       (%al:get-source-i sid param ptr)
       (cffi:mem-ref ptr :int)))
    (:source-state
     (cffi:with-foreign-object (ptr :int)
       (%al:get-source-i sid param ptr)
       (cffi:foreign-enum-keyword '%al:enum (cffi:mem-ref ptr :int))))
    ((:position :velocity :direction)
     (cffi:with-foreign-object (source-array :float 3)
       (%al:get-source-fv sid param source-array)
       (loop for i below 3
             collect (cffi:mem-aref source-array :float i))))))

;; Playback
(defun-al source-play (sid)
  (%al:source-play sid))
(defun-al source-stop (sid)
  (%al:source-stop sid))
(defun-al source-rewind (sid)
  (%al:source-rewind sid))
(defun-al source-pause (sid)
  (%al:source-pause sid))

;; queueing
(defun-al source-queue-buffers (sid buffers)
  (let ((n (length buffers)))
    (cffi:with-foreign-object (buffer-array :uint n)
      (loop for i below n
         do (setf (cffi:mem-aref buffer-array :uint i)
                  (elt buffers i)))
      (%al:source-queue-buffers sid n buffer-array))))

(defun-al source-unqueue-buffers (sid &optional (num-buffers 1))
  (cffi:with-foreign-object (buffer-array :uint)
    (setf (cffi:mem-ref buffer-array :uint) 0)
    (%al:source-unqueue-buffers sid num-buffers buffer-array)
    (checkpoint)
    (unless (zerop (cffi:mem-ref buffer-array :uint))
      (loop for i below num-buffers
         collect (cffi:mem-aref buffer-array :uint i)))))

;;;
;;; Buffers
;;;
(defun-al gen-buffers (n)
  (cffi:with-foreign-object (buffer-array :uint n)
    (%al:gen-buffers n buffer-array)
    (checkpoint)
    (loop for i below n
       collect (cffi:mem-aref buffer-array :uint i))))

(defun-al delete-buffers (buffers)
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

(defun-al bufferp (buffer-id)
  (%al:is-buffer buffer-id))

(defun-al buffer (bid param value)
  (%al:buffer-i bid param value))

(defun-al buffer-data (bid format data size freq)
  (%al:buffer-data bid format data size freq))

(defun-al get-buffer (bid param)
  (cffi:with-foreign-object (ptr :int)
    (%al:get-buffer-i bid param ptr)
    (checkpoint)
    (cffi:mem-ref ptr :int)))

;;;
;;; Global parameters
;;;
(defun-al doppler-factor (value)
  (%al:doppler-factor value))
(defun-al doppler-velocity (value)
  (%al:doppler-velocity value))
(defun-al speed-of-sound (value)
  (%al:speed-of-sound value))
(defun-al distance-model (model-param)
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
