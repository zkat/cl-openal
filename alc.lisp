;; TODO: do enums
(in-package :%alc)

(define-foreign-library al
  (:unix (:or "libopenal" "libopenal.so.1"))
  (t (:default "libopenal")))
(use-foreign-library al)

(defctype boolean (:boolean :unsigned-char))

(defcenum enum
  (:false #x0000)
  (:true #x0001)
  (:frequency #x1007)
  (:refresh #x1008)
  (:sync #x1009)
  (:mono-sources #x1010)
  (:stereo-sources #x1011)

  ;; errors
  (:no-error #x0000)
  (:invalid-device #xA001)
  (:invalid-context #xA002)
  (:invalid-enum #xA003)
  (:invalid-value #xA004)
  (:out-of-memory #xA005)
  
  (:default-device-specifier #x1004)
  (:device-specifier #x1005)
  (:extensions #x1006)
  (:major-version #x1000)
  (:minor-version #x1001)
  (:attributes-size #x1002)
  (:all-attributes #x1003)
  (:default-all-devices-specifier #x1012)
  (:all-devices-specifier #x1013)
  
  (:capture-device-specifier #x310)
  (:capture-default-device-specifier #x311)
  (:capture-samples #x312))

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
