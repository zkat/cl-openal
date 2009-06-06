;; TODO: enums. As usual.
(in-package :%alut)

(define-foreign-library alut
  (:unix (:or "libalut" "libalut.so.0" "libalut.so.0.1.0"))
  (t (:default ("libalut"))))
(use-foreign-library alut)
(defcfun ("alutInit" init) :boolean (argcp :pointer) (argv :pointer))
(defcfun ("alutInitWithoutContext" init-without-context) :boolean
  (argcp :pointer) (argv :pointer))
(defcfun ("alutExit" exit) :boolean)

(defcfun ("alutGetError" get-error) enum)
(defcfun ("alutGetErrorString" get-error-string) :string (err enum))

(defcfun ("alutCreateBufferFromFile" create-buffer-from-file) :uint
  (filename :string))
(defcfun ("alutCreateBufferFromFileImage" create-buffer-from-file-image) :uint
  (data :pointer) (length :int))
(defcfun ("alutCreateBufferHelloWorld" create-buffer-hello-world) :uint)
(defcfun ("alutCreateBufferWaveform" create-buffer-waveform) :uint
  (waveshape enum) (frequency :float) (phase :float) (duration :float))

(defcfun ("alutLoadMemoryFromFile" load-memory-from-file) :void
  (filename :string) (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryFromFileImage" load-memory-from-file-image) :void
  (data :pointer) (length :int) (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryHelloWorld" load-memory-hello-world) :void
  (format :pointer) (size :pointer) (frequency :pointer))
(defcfun ("alutLoadMemoryWaveform" load-memory-waveform) :void
  (waveshape enum) (frequency :float) (phase :float) (duration :float)
  (format :pointer) (size :pointer) (freq :pointer))

(defcfun ("alutGetMIMETypes" get-mime-types) :string
  (loader enum))

(defcfun ("alutGetMajorVersion" get-major-version) :int)
(defcfun ("alutGetMinorVersion" get-minor-version) :int)

(defcfun ("alutSleep" sleep) :boolean (duration :float))
