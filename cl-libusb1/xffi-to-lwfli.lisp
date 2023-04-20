;; xffi-to-lwfli.lisp
;;
;; DM/RAL  2023/04/20 05:49:40
;; ----------------------------------

(defpackage #:xffi-to-lwfli
  (:use #:common-lisp #:fli)
  (:nicknames #:xffi)
  (:export
   #:define-c-enum
   #:define-c-struct
   #:define-c-union
   #:define-c-typedef
   #:define-foreign-function
   #:define-foreign-callable
   #:define-foreign-variable

   #:enum-symbol-value
   #:foreign-aref
   #:foreign-slot-pointer
   #:foreign-slot-value
   #:size-of
   #:make-pointer
   #:null-pointer-p
   #:incf-pointer
   #:dereference
   #:with-dynamic-foreign-objects
   #:convert-from-foreign-string
   #:with-foreign-slots

   #:disconnect-module
   #:register-module
   ))

;; ----------------------------------

