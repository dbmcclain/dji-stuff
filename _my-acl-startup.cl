;; -*- Mode: Lisp; coding: UTF-8 -*-

(in-package "USER")

#+nil
(compile-file-if-needed "startup/advice-trace")
#+nil
(load "startup/advice-trace")

#+nil
(compile-file-if-needed "startup/trace-find-package")
#+nil
(load "startup/trace-find-package")

(compile-file-if-needed "startup/project-packages-acl")
(load "startup/project-packages-acl")
(load "startup/project-mappings")

(load "startup/_my_bare-startup")

(setf *print-pretty*  t)
(setf *print-length* 100)
(setf *print-level*   6)
;; (setf *print-circle* t)

;; (asdf :named-readtables)

;; -------------------------------------------------------------------------------

#+nil
(named-readtables:defreadtable :ral-syntax
                               (:merge :current))

;; ---------------------------------------------------------------

(asdf :illogical-pathnames)

(asdf :com.ral.actors)
(pushnew :actors *features*)

#+nil
(progn
  (asdf :com.ral.useful-macros/ext)
  (asdf :plotter)
  (asdf :com.ral.actors.extra) ;; pick up KVDB
  (asdf :com.ral.actors.secure-channel)
  (asdf :refstore))


