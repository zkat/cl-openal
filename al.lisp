;; TODO: get enums in here
(in-package :%al)

(define-foreign-library al
  (:unix (:or "libopenal" "libopenal.so.1"))
  (t (:default "libopenal")))
(use-foreign-library al)

(defctype boolean :char)
(defctype char :char)
(defctype byte :char)
(defctype ubyte :unsigned-char)
(defctype short :short)
(defctype ushort :unsigned-short)
(defctype int :int)
(defctype uint :unsigned-int)
(defctype enum :int)
(defctype float :float)
(defctype double :double)
(defctype void :void)

;; Renderer State management
(defcfun ("alEnable" enable) :void (target enum))
(defcfun ("alDisable" disable) :void (target enum))
(defcfun ("alIsEnabled" is-enabled) :boolean (target enum))

;; State retrieval
(defcfun ("alGetString" get-string) :string (param enum))
(defcfun ("alGetBooleanv" get-boolean-v) :void (param enum) (data :pointer))
(defcfun ("alGetIntegerv" get-integer-v) :void (param enum) (data :pointer))
(defcfun ("alGetFloatv" get-float-v) :void (param enum) (data :pointer))
(defcfun ("alGetDoublev" get-double-v) :void (param enum) (data :pointer))
(defcfun ("alGetBoolean" get-boolean) :void (param enum))
(defcfun ("alGetInteger" get-integer) :void (param enum))
(defcfun ("alGetFloat" get-float) :void (param enum))
(defcfun ("alGetDouble" get-double) :void (param enum))

;; Error support
(defcfun ("alGetError" get-error) :enum)

;; Extension support
(defcfun ("alIsExtensionPresent" is-extension-present) :boolean (extname :string))
(defcfun ("alGetProcAddress" get-proc-address) :void (fname :string))
(defcfun ("alGetEnumValue" get-enum-value) enum (ename :string))

;;;
;;; Listener
;;;

;; Set Listener parameters
(defcfun ("alListenerf" listener-f) :void (param enum) (value :float))
(defcfun ("alListener3f" listener-3f) :void
  (param enum) (value1 :float) (value2 :float) (value3 :float))
(defcfun ("alListenerfv" listener-fv) :void (param enum) (values :pointer))
(defcfun ("alListeneri" listener-i) :void (param enum) (value :int))
(defcfun ("alListener3i" listener-3i) :void 
  (param enum) (value1 :int) (value2 :int) (value3 :int))
(defcfun ("alListeneriv" listener-iv) :void (param enum) (values :pointer))

;; Get Listener parameters
(defcfun ("alGetListenerf" get-listener-f) :void (param enum) (value :pointer))
(defcfun ("alGetListener3f" get-listener-3f) :void
  (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetListenerfv" get-listener-fv) :void (param enum) (values :pointer))
(defcfun ("alGetListeneri" get-listener-i) :void (param enum) (value :pointer))
(defcfun ("alGetListener3i" get-listener-3i) :void 
  (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetListeneriv" get-listener-iv) :void (param enum) (values :pointer))

;;;
;;; Sources
;;;

;; Source objects
(defcfun ("alGenSources" get-sources) :void (n :int) (sources :pointer))
(defcfun ("alDeleteSources" delete-sources) :void (n :int) (sources :pointer))
(defcfun ("alIsSource" is-source) :boolean (sid :uint))

;; Set Source parameters
(defcfun ("alSourcef" source-f) :void (sid :uint) (param enum) (value :float))
(defcfun ("alSource3f" source-3f) :void
  (sid :uint) (param enum) (value1 :float) (value2 :float) (value3 :float))
(defcfun ("alSourcefv" source-fv) :void (sid :uint) (param enum) (values :pointer))
(defcfun ("alSourcei" source-i) :void (sid :uint) (param enum) (value :int))
(defcfun ("alSource3i" source-3i) :void 
  (sid :uint) (param enum) (value1 :int) (value2 :int) (value3 :int))
(defcfun ("alSourceiv" source-iv) :void (sid :uint) (param enum) (values :pointer))

;; Get Source parameters
(defcfun ("alGetSourcef" get-source-f) :void (sid :uint) (param enum) (value :pointer))
(defcfun ("alGetSource3f" get-source-3f) :void
  (sid :uint) (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetSourcefv" get-source-fv) :void (sid :uint) (param enum) (values :pointer))
(defcfun ("alGetSourcei" get-source-i) :void (sid :uint) (param enum) (value :pointer))
(defcfun ("alGetSource3i" get-source-3i) :void 
  (sid :uint) (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetSourceiv" get-source-iv) :void (sid :uint) (param enum) (values :pointer))

;;; Playback

;; Source vector based
(defcfun ("alSourcePlayv" source-play-v) :void (ns :i) (sids :pointer))
(defcfun ("alSourceStopv" source-stop-v) :void (ns :i) (sids :pointer))
(defcfun ("alSourceRewindv" source-rewind-v) :void (ns :i) (sids :pointer))
(defcfun ("alSourcePausev" source-pause-v) :void (ns :i) (sids :pointer))

;; Source based
(defcfun ("alSourcePlay" source-play) :void (sid :uint))
(defcfun ("alSourceStop" source-stop) :void (sid :uint))
(defcfun ("alSourceRewind" source-rewind) :void (sid :uint))
(defcfun ("alSourcePause" source-pause) :void (sid :uint))

;; Source Queueing
(defcfun ("alSourceQueueBuffers" source-queue-buffers) :void
  (sid :uint) (num-entries :int) (bids :pointer))
(defcfun ("alSourceUnqueueBuffers" source-unqueue-buffers) :void
  (sid :uint) (num-entries :int) (bids :pointer))


;;;
;;; Buffer
;;;

;; Buffer objects
(defcfun ("alGenBuffers" gen-buffers) :void (n :int) (buffer-names :pointer))
(defcfun ("alDeleteBuffers" delete-buffers) :void (n :int) (buffer-names :pointer))
(defcfun ("alIsBuffer" is-buffer) :boolean (bid :uint))

(defcfun ("alBufferData" buffer-data) :void 
  (bid :uint) (format enum) (data :pointer) (size :int) (freq :int))

;; Set Buffer parameters
(defcfun ("alBufferf" buffer-f) :void (bid :uint) (param enum) (value :float))
(defcfun ("alBuffer3f" buffer-3f) :void
  (bid :uint) (param enum) (value1 :float) (value2 :float) (value3 :float))
(defcfun ("alBufferfv" buffer-fv) :void (bid :uint) (param enum) (values :pointer))
(defcfun ("alBufferi" buffer-i) :void (bid :uint) (param enum) (value :int))
(defcfun ("alBuffer3i" buffer-3i) :void 
  (bid :uint) (param enum) (value1 :int) (value2 :int) (value3 :int))
(defcfun ("alBufferiv" buffer-iv) :void (bid :uint) (param enum) (values :pointer))

;; Get Buffer parameters
(defcfun ("alGetBufferf" get-buffer-f) :void (bid :uint) (param enum) (value :pointer))
(defcfun ("alGetBuffer3f" get-buffer-3f) :void
  (bid :uint) (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetBufferfv" get-buffer-fv) :void (bid :uint) (param enum) (values :pointer))
(defcfun ("alGetBufferi" get-buffer-i) :void (bid :uint) (param enum) (value :pointer))
(defcfun ("alGetBuffer3i" get-buffer-3i) :void 
  (bid :uint) (param enum) (value1 :pointer) (value2 :pointer) (value3 :pointer))
(defcfun ("alGetBufferiv" get-buffer-iv) :void (bid :uint) (param enum) (values :pointer))

;; Global Parameters
(defcfun ("alDopplerFactor" doppler-factor) :void (value :float))
(defcfun ("alDopplerVelocity" doppler-velocity) :void (value :float))
(defcfun ("alSpeedOfSound" speed-of-sound) :void (value :float))
(defcfun ("alDistanceModel" distance-model) :void (distance-model :enum))

