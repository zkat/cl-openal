(defpackage #:alut-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alut)
  (:shadow :sleep)
  (:export
   ;; error signalling
   :alut-error
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

(defpackage #:cl-openal-alut
  (:use :cl :al)
  (:import-from :%alut :alut-error)
  (:nicknames :alut)
  (:shadow :sleep :get-error :clear-error)
  (:export
   :alut-error
   :load-libraries
   :init :init-without-context :exit :get-error :get-error-string :create-buffer-from-file
   :create-buffer-from-file-image :create-buffer-hello-world :create-buffer-waveform
   :load-memory-from-file :load-memory-from-file-image :load-memory-hello-world :load-memory-waveform
   :get-mime-types :get-major-version :get-minor-version :sleep
   :with-init))


