;; TODO: do enums
(in-package :%alc)

(define-foreign-library al
  (:unix (:or "libopenal" "libopenal.so.1"))
  (t (:default "libopenal")))
(use-foreign-library al)
;; Context Management
(defcfun ("alcCreateContext" create-context) :pointer (device :pointer) (attrlist :pointer))
(defcfun ("alcMakeContextCurrent" make-context-current) :boolean (context :pointer))
(defcfun ("alcProcessContext" process-context) :void (context :pointer))
(defcfun ("alcSuspendContext" suspend-context) :void (context :pointer))
(defcfun ("alcDestroyContext" destroy-context) :void (context :pointer))
(defcfun ("alcGetCurrentContext" get-current-context) :pointer)
(defcfun ("alcGetContextsDevice" get-contexts-device) :pointer (context :pointer))

;; Device Management
(defcfun ("alcOpenDevice" open-device) :pointer (device-name :pointer))
(defcfun ("alcCloseDevice" close-device) :pointer (device :pointer))

;; Error support
(defcfun ("alcGetError" get-error) enum (device :pointer))

;; Extension support
(defcfun ("alcIsExtensionPresent" is-extension-present) :boolean (device :pointer) (extname :string))
(defcfun ("alcGetProcAddress" get-proc-address) :pointer (funcname :string))
(defcfun ("alcGetEnumValue" get-enum-value) enum (device :pointer) (enumname :string))

;; Query functions
(defcfun ("alcGetString" get-string) :string (device :pointer) (param enum))
(defcfun ("alcGetIntegerv" get-integer-v) :void
  (device :pointer) (param enum) (size :int) (data :pointer))

;; Capture functions
(defcfun ("alcCaptureOpenDevice" capture-open-device) :pointer 
  (device-name :string) (frequency :uint) (format enum) (buffer-size :int))
(defcfun ("alcCaptureCloseDevice" capture-close-device) :boolean (device :pointer))
(defcfun ("alcCaptureStart" capture-start) :void (device :pointer))
(defcfun ("alcCaptureStop" capture-stop) :void (device :pointer))
(defcfun ("alcCaptureSamples" capture-samples) :void
  (device :pointer) (buffer :pointer) (samples :int))
