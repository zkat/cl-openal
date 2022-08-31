(defpackage #:alc-cffi-bindings
  (:use :cl :cffi)
  (:nicknames :%alc)
  (:shadow :boolean)
  (:export
   :attribute :error :enum :device :context :boolean 
   ;; Context management
   :create-context :make-context-current :process-context :suspend-context :destroy-context
   :get-current-context :get-contexts-device
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

(defpackage #:cl-openal-alc
  (:use :cl)
  (:import-from :al :defun-al :checkpoint)
  (:nicknames :alc)
  (:export
   #:load-libraries
   :open-device :close-device :create-context :make-context-current :process-context :suspend-context
   :destroy-context :get-current-context :get-contexts-device :get-error :extension-present-p
   :get-proc-address :get-enum-value :get-string :get-integer :capture-open-device 
   :capture-close-device :capture-start :capture-stop :capture-samples
   :with-device :with-capture-device :with-context))
