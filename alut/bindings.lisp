;; TODO: enums. As usual.
(in-package :%alut)

(define-foreign-library alut
  (:windows "alut.dll" :calling-convention :stdcall)
  (:darwin (:or (:default "libalut") (:framework "alut")))
  (:unix (:or "libalut.so" "libalut.so.0" "libalut.so.0.1.0"))
  (t (:default ("libalut"))))
(use-foreign-library alut)

(define-foreign-type ensure-integer ()
  ()
  (:actual-type :int)
  (:simple-parser ensure-integer))

(defmethod translate-to-foreign (value (type ensure-integer))
  (truncate value))

(defmethod expand-to-foreign (value (type ensure-integer))
  (if (constantp value)
      (truncate (eval value))
      `(truncate ,value)))

(define-foreign-type ensure-float ()
  ()
  (:actual-type :float)
  (:simple-parser ensure-float))

(defmethod translate-to-foreign (value (type ensure-float))
  (cl:float value 1.0))

(defmethod expand-to-foreign (value (type ensure-float))
  (if (constantp value)
      (cl:float (eval value) 1.0)
      `(cl:float ,value 1.0)))

(define-foreign-type ensure-double ()
  ()
  (:actual-type :double)
  (:simple-parser ensure-double))

(defmethod translate-to-foreign (value (type ensure-double))
  (cl:float value 1.0d0))

(defmethod expand-to-foreign (value (type ensure-double))
  (if (constantp value)
      (cl:float (eval value) 1.0d0)
      `(cl:float ,value 1.0d0)))

(defcenum api
  (:major-version 1)
  (:minor-version 1))

(defcenum error
  (:no-error 0)
  (:out-of-memory #x200)
  (:invalid-enum #x201)
  (:invalid-value #x202)
  (:invalid-operation #x203)
  (:no-current-context #x204)
  (:al-error-on-entry #x205)
  (:alc-error-on-entry #x206)
  (:open-device #x207)
  (:close-device #x208)
  (:create-context #x209)
  (:make-context-current #x20A)
  (:destroy-context #x20B)
  (:gen-buffers #x20C)
  (:buffer-data #x20D)
  (:io-error #x20E)
  (:unsupported-file-type #x20F)
  (:unsupported-file-subtype #x210)
  (:corrupt-or-truncated-data #x211))

(defcenum waveform
  (:sine #x100)
  (:square #x101)
  (:sawtooth #x102)
  (:whitenoise #x103)
  (:impulse #x104))

(defcenum loader
  (:buffer #x300)
  (:memory #x301))

(define-foreign-type pathname-string-type ()
  ()
  (:actual-type :string)
  (:simple-parser pathname-string))
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod expand-to-foreign-dyn (value var body (type pathname-string-type))
    `(with-foreign-string (,var (if (pathnamep ,value) (namestring ,value) ,value))
       ,@body)))

(defcfun ("alutInit" init) :boolean (argcp :pointer) (argv :pointer))
(defcfun ("alutInitWithoutContext" init-without-context) :boolean
  (argcp :pointer) (argv :pointer))
(defcfun ("alutExit" exit) :boolean)

(defcfun ("alutGetError" get-error) error)
(defcfun ("alutGetErrorString" get-error-string) :string (err error))

(defcfun ("alutCreateBufferFromFile" create-buffer-from-file) :uint
  (filename pathname-string))
(defcfun ("alutCreateBufferFromFileImage" create-buffer-from-file-image) :uint
  (data (:pointer :void)) (length :int))
(defcfun ("alutCreateBufferHelloWorld" create-buffer-hello-world) :uint)
(defcfun ("alutCreateBufferWaveform" create-buffer-waveform) :uint
  (waveshape waveform) (frequency ensure-float) (phase ensure-float) (duration ensure-float))

(defcfun ("alutLoadMemoryFromFile" load-memory-from-file) :pointer
  (filename pathname-string) (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryFromFileImage" load-memory-from-file-image) :pointer
  (data (:pointer :void)) (length :int) (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryHelloWorld" load-memory-hello-world) :pointer
  (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryWaveform" load-memory-waveform) :pointer
  (waveshape waveform) (frequency :float) (phase :float) (duration :float)
  (format :pointer) (size :pointer) (freq :pointer))

(defcfun ("alutGetMIMETypes" get-mime-types) :string
  (loader loader))

(defcfun ("alutGetMajorVersion" get-major-version) :int)
(defcfun ("alutGetMinorVersion" get-minor-version) :int)

(defcfun ("alutSleep" sleep) :boolean (duration ensure-float))
