;; xffi-to-cffi.lisp
;;
;; DM/RAL  2023/04/20 05:06:44
;; ----------------------------------

(defpackage #:xffi-to-cffi
  (:use #:common-lisp)
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

(in-package #:xffi-to-cffi)

;; ----------------------------------

(defmacro define-c-enum ((name &rest args) &rest enums)
  (declare (ignore args))
  `(cffi:defcenum ,name ,@enums))

(defmacro define-c-struct ((name &rest args) &rest slots)
  (declare (ignore args))
  `(cffi:defcstruct ,name ,@slots))

(defmacro define-c-union ((name &rest args) &rest slots)
  (declare (ignore args))
  `(cffi:defcunion ,name ,@slots))

(defmacro define-c-typedef ((name &rest args) decl)
  (declare (ignore args))
  `(cffi:defctype ,name ,decl))

(defmacro define-foreign-function ((name c-name &rest args) fn-args &key result-type language)
  (declare (ignore args language))
  `(cffi:defcfun (,name ,c-name) ,result-type
     ,fn-args))

(defmacro enum-symbol-value (enum-type enum-sym)
  `(cffi:foreign-enum-value ,enum-type ,enum-sym))

