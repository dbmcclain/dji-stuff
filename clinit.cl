;; -*- Mode: Lisp; coding: UTF-8 -*-

;; (load-logical-pathname-translations "PROJECTS")
(setf (logical-pathname-translations "PROJECTS")
      #+:MSWINDOWS
      `(("DYLIB;**;*.*"        "z:/usr/local/lib/**/*.*")
        ("LIB;**;*.*"          "z:/usr/local/lib/**/*.*")
        ("LISPLIB;**;*.*"      "z:/usr/local/Lisp/Source/**/*.*")
        ("LISP;**;*.*"         "y:/projects/Lispworks/**/*.*")
        ("**;*.*"              "y:/projects/**/*.*"))
      #-:MSWINDOWS
      `(("DYLIB;**;*.*"        "/usr/local/lib/**/*.*")
        ("LIB;**;*.*"          "/usr/local/lib/**/*.*")
        ("LISPLIB;**;*.*"      "/usr/local/Lisp/Source/**/*.*")
        ("LISP;**;*.*"         "~/projects/Lispworks/**/*.*")
        ("**;*.*"              "~/projects/**/*.*"))
      )

(defvar *my-path*)
(setf *my-path* (translate-logical-pathname "PROJECTS:LISP;"))
(chdir *my-path*)
(setf *default-pathname-defaults* *my-path*)

;; (cd (translate-logical-pathname "PROJECTS:LISP;"))
;; (cd #P"~/projects/Lispworks/")

;; (lw:set-default-character-element-type 'cl:character)

(with-open-file (*standard-output*
                 (merge-pathnames #P"loadup.txt" *my-path*)
                 :if-exists         :rename
                 :if-does-not-exist :create
                 :external-format   :UTF-8
                 :direction         :output)
  (let ((*background-output* *standard-output*))
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (finish-output *background-output*))
                          ))
      (load (merge-pathnames #P"_my-acl-startup" *my-path*))
      )))
