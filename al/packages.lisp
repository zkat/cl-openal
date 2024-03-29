(defpackage #:openal-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%al)
  (:shadow :boolean :byte)
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

(defpackage #:cl-openal
  (:use :cl)
  (:import-from :alexandria :with-gensyms)
  (:nicknames :al)
  (:export
   :*propagate-errors* :al-error :peek-error :get-error :propagate-error
   :load-libraries
   :enable :disable :enabledp :get-string :get-boolean :get-integer :extension-present-p
   :get-proc-address :get-enum-value :listener :get-listener :gen-sources :gen-source :delete-sources
   :delete-source :sourcep :source :get-source :source-play :source-stop :source-rewind :source-pause
   :source-queue-buffers :source-unqueue-buffers :gen-buffers :gen-buffer :delete-buffers
   :delete-buffer :bufferp :buffer :get-buffer :doppler-factor :doppler-velocity :speed-of-sound
   :distance-model :buffer-data
   :defun-al :checkpoint
   :with-source :with-sources :with-buffer :with-buffers))
