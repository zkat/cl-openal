(in-package :%al)

(define-foreign-library al
  (:unix (:or "libopenal" "libopenal.so.1"))
  (t (:default "libopenal")))
(use-foreign-library al)

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

(defctype boolean (:boolean :char))
(defctype byte (:char))
(defctype ubyte (:unsigned-char))
(defcenum enum
  (:none #x0000)
  (:false #x0000)
  (:true #x0001)
  (:source-relative #x202)
  (:cone-inner-angle #x1001)
  (:cone-outer-angle #x1002)
  (:pitch #x1003)

  ;; Location
  (:position #x1004)
  (:direction #x1005)
  (:velocity #x1006)

  (:looping #x1007)
  (:buffer #x1009)
  (:gain #x100A)
  (:min-gain #x100D)
  (:max-gain #x100E)
  (:orientation #x100F)
  (:source-state #x1010)
  (:initial #x1011)
  (:playing #x1012)
  (:paused #x1013)
  (:stopped #x1014)
  (:buffers-queued #x1015)
  (:buffers-processed #x1016)
  (:sec-offset #x1024)
  (:sample-offset #x1025)
  (:byte-offset #x1026)

  ;; Source types
  (:source-type #x1027)
  (:static #x1028)
  (:streaming #x1029)
  (:undetermined #x1030)

  ;; sound sample formats
  (:mono8 #x1100)
  (:mono16 #x1101)
  (:stereo8 #x1102)
  (:stereo16 #x1103)

  ;; more sound sample stuff?
  (:reference-distance #x1020)
  (:rolloff-factor #x1021)
  (:cone-outer-gain #x1022)
  (:max-distance #x1023)

  ;; Sound sample frequency
  (:frequency #x2001)
  (:bits #x2002)
  (:channels #x2003)
  (:size #x2004)

  ;; Buffer state
  (:unused #x2010)
  (:pending #x2011)
  (:processed #x2012)

  ;; errors
  (:no-error #x0000)
  (:invalid-name #xA001)
  (:invalid-enum #xA002)
  (:invalid-value #xA003)
  (:invalid-operation #xA004)
  (:out-of-memory #xA005)

  ;; Context strings: vendor name
  (:vendor #xB001)
  (:version #xB002)
  (:renderer #xB003)
  (:extensions #xB004)

  ;; Global tweakage
  (:doppler-factor #xC000)
  (:doppler-velocity #xC001)
  (:speed-of-sound #xC003)

  ;; Distance model
  (:distance-model #xD000)
  (:inverse-distance #xD001)
  (:inverse-distance-clamped #xD002)
  (:linear-distance #xD003)
  (:linear-distance-clamped #xD004)
  (:exponent-distance #xD005)
  (:exponent-distance-clamped #xD006))


;; Renderer State management
(defcfun ("alEnable" enable) :void (capability enum))
(defcfun ("alDisable" disable) :void (capability enum))
(defcfun ("alIsEnabled" is-enabled) :boolean (capability enum))

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
(defcfun ("alGetError" get-error) enum)

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
  (param enum) (value1 ensure-float) (value2 ensure-float) (value3 ensure-float))
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
(defcfun ("alGenSources" gen-sources) :void (n :int) (sources :pointer))
(defcfun ("alDeleteSources" delete-sources) :void (n :int) (sources :pointer))
(defcfun ("alIsSource" is-source) :boolean (sid :uint))

;; Set Source parameters
(defcfun ("alSourcef" source-f) :void (sid :uint) (param enum) (value :float))
(defcfun ("alSource3f" source-3f) :void
  (sid :uint) (param enum) (value1 ensure-float) (value2 ensure-float) (value3 ensure-float))
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
(defcfun ("alSourcePlayv" source-play-v) :void (ns :int) (sids :pointer))
(defcfun ("alSourceStopv" source-stop-v) :void (ns :int) (sids :pointer))
(defcfun ("alSourceRewindv" source-rewind-v) :void (ns :int) (sids :pointer))
(defcfun ("alSourcePausev" source-pause-v) :void (ns :int) (sids :pointer))

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
  (bid :uint) (param enum) (value1 ensure-float) (value2 ensure-float) (value3 ensure-float))
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
(defcfun ("alDopplerFactor" doppler-factor) :void (value ensure-float))
(defcfun ("alDopplerVelocity" doppler-velocity) :void (value ensure-float))
(defcfun ("alSpeedOfSound" speed-of-sound) :void (value ensure-float))
(defcfun ("alDistanceModel" distance-model) :void (distance-model enum))

