
(defpackage :com.ral.useful-macros.safe-read-patch
  (:use :common-lisp))

(in-package :com.ral.useful-macros.safe-read-patch)

(let (pkg sym)
  (when (and (setf pkg (find-package :safe-read))
             (setf sym (find-symbol (string 'add-stream-tracking) pkg))
             (fboundp sym))
    (setf (symbol-function sym) #'add-stream-tracking)))


