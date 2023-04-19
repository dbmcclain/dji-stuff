;;;; libusb-ffi1.asd

(asdf:defsystem #:libusb1
  :name "libusb1"
  :description "Common Lisp FFI bindings to libusb-1.x"
  :author "David McClain dbm@refined-audiometrics.com"
  :serial t
  :components ((:file "package")
               (:file "libusb-dff")
               (:file "libusb-dff-inl")
               (:file "libusb-support")
               (:file "libusb-ffi"))
  :depends-on ("com.ral.cps"))
