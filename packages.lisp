;;;
;;; Main bindings.
;;;
(defpackage #:cl-openal
  (:use :cl)
  (:nicknames :al)
  (:export
   :enable :disable :enabledp :get-string :get-boolean :get-integer :get-error :extension-present-p
   :get-proc-address :get-enum-value :listener :get-listener :gen-sources :gen-source :delete-sources
   :delete-source :sourcep :source :get-source :source-play :source-stop :source-rewind :source-pause
   :source-queue-buffers :source-unqueue-buffers :gen-buffers :gen-buffer :delete-buffers :delete-buffer
   :bufferp :buffer :get-buffer :doppler-factor :doppler-velocity :speed-of-sound :distance-model))

(defpackage #:cl-openal-alc
  (:use :cl)
  (:nicknames :alc))


(defpackage #:cl-openal-alut
  (:use :cl :al)
  (:nicknames :alut)
  (:shadow :sleep)
  (:export
   :init :init-without-context :exit :get-error :get-error-string :create-buffer-from-file
   :create-buffer-from-file-image :create-buffer-hello-world :create-buffer-waveform
   :load-memory-from-file :load-memory-from-file-image :load-memory-hello-world :load-memory-waveform
   :get-mime-types :get-major-version :get-minor-version :sleep))


;;;
;;; CFFI bindings
;;;
(defpackage #:openal-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%al)
  (:export
   :enum :boolean :byte :ubyte :ensure-float :ensure-integer :ensure-double
   ;; renderer state management
   :enable :disable :is-enabled
   ;; state retrieval
   :get-string :get-boolean-v :get-integer-v :get-float-v :get-double-v
   :get-boolean :get-integer :get-float :get-double
   ;; errors
   :get-error
   ;; extensions
   :is-extension-present
   :get-proc-address
   :get-enum-value
   ;;; listener stuff
   ;; set
   :listener-f :listener-3f :listener-fv :listener-i :listener-3i :listener-iv
   ;; get
   :get-listener-f :get-listener-3f :get-listener-fv :get-listener-i :get-listener-3i :get-listener-iv
   ;;; sources stuff
   ;; objects
   :gen-sources :delete-sources :is-source
   ;; set
   :source-f :source-3f :source-fv :source-i :source-3i :source-iv
   ;; get
   :get-source-f :get-source-3f :get-source-fv :get-source-i :get-source-3i :get-source-iv
   ;; playback - vector based
   :source-play-v :source-stop-v :source-rewind-v :source-pause-v
   ;; playback - source based
   :source-play :source-stop :source-rewind :source-pause
   ;; queueing
   :source-queue-buffers :source-unqueue-buffers
   ;;; buffer stuff
   ;; objects
   :gen-buffers :delete-buffers :is-buffer :buffer-data
   ;; set
   :buffer-f :buffer-3f :buffer-fv :buffer-i :buffer-3i :buffer-iv
   ;; get
   :get-buffer-f :get-buffer-3f :get-buffer-fv :get-buffer-i :get-buffer-3i :get-buffer-iv
   ;;; global parameters
   :doppler-factor :doppler-velocity :speed-of-sound :distance-model))

(defpackage #:alc-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alc)
  (:export
   :enum :device :context :boolean
   ;; Context management
   :create-context :make-context-current :process-context :suspend-context :destroy-context
   :get-current-context :get-current-contexts-device
   ;; device management
   :open-device :close-device
   ;; errors
   :get-error
   ;; extensions
   :is-extension-present :get-proc-address :get-enum-value
   ;; query functions
   :get-string  :get-integer-v
   ;; capture functions
   :capture-open-device :capture-close-device :capture-start :capture-stop :capture-samples))

(defpackage #:alut-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alut)
  (:shadow :sleep)
  (:export
   ;; enums
   :api :error :waveform :loader
   ;; funcs
   :init :init-without-context :exit
   :get-error :get-error-string
   :create-buffer-from-file :create-buffer-from-file-image 
   :create-buffer-hello-world :create-buffer-waveform
   :load-memory-from-file :load-memory-from-file-image
   :load-memory-hello-world :load-memory-waveform
   :get-mime-types :get-major-version :get-minor-version :sleep))
