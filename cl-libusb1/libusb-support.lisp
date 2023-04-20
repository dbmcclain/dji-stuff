;; libusb-support.lisp
;;
;; DM/RAL  2023/04/09 04:27:01
;; ----------------------------------

(in-package #:libusb1)

;; -----------------------------------------------------------------
;; We aim to provide parallel concurrent safety for running arbitrary
;; user code, said code not written in Actors style. We should be
;; transparent to the user, yet be fully live for parallel SMP
;; concurrent code. No need for Locks and Threads.
;;
;; -----------------------------------------------------------------
;; For unified USB Error reporting

(define-condition usb-error (error)
    ((errno  :reader usb-error-errno
             :initarg :errno
             :initform 0))
    (:report (lambda (cx stream)
               (format stream "USB Error ~A"
                       (usb-error-errno cx)))))

;; -------------------------------------------------------------
;; Actors Interface for user Lisp code. User's code is executed, via
;; ASK, on arbitrary Actors thread, sending back to their ASK as the
;; customer.

(defvar *enabled-gates* nil) ;; list of gates already held
(defvar *lib-context*   nil) ;; current libusb1 context ptr

(defun user-exec (fn &rest args)
  ;; Used to execute user Lisp code (non-Actor-centric).  Bundle up
  ;; multiple return vals as a list for use in user's VALUES-LIST in
  ;; their ASK. Multiple return values will be seen at output of ASK
  ;; as multiple return vals.
  (multiple-value-list
   (allow-recursive-ask
     ;; muffle warnings in case user performs ASK
     ;; somewhere else
     (apply fn args))))

(def-ser-beh usb-exec-beh ()
  ;; Perform user code, with permissions indicated by gates.  User
  ;; code has been wrapped with =FUT, re-establishing dynamic
  ;; environment prior to fn exec, provided you take care to use the
  ;; CPS-style dynamic mechanisms like =HANDLER-CAEE, =HANDLER-BIND,
  ;; =UNWIND-PROTECT, =RESTART-CASE, etc, where appropriate.
  ((cust fn pctxt gates)
   (let ((*enabled-gates* gates)
         (*lib-context*   pctxt))
     ;; Further recursion, in this same thread, via DO-WITH-EXCL, will
     ;; bypass Serializers held in GATES. Other threads will still
     ;; serialize.
     (send* cust (user-exec fn pctxt))
     )) )

(deflex unprot-gate
  ;; UNPROT-GATE allows unrestricted parallel concurrent execution of
  ;; user Lisp code.
  (create (usb-exec-beh)))

;; ----------------------------------------------
;; Ensure that libusb-init gets called before user
;; code is executed, and libusb-exit performed afterward.
;;
;; The libusb-init and libusb-exit are serialized to prevent race
;; conditions in libusb. But user code runs in parallel concurrent
;; manner with code.
;;
;; This enables one thread to run the libusb event handler, while
;; other threads run USB I/O tasks.


(def-ser-beh usb-mon-beh (mon)
  ;; DEF-SER-BEH - special BEHAVIOR wrapping to intercept errors and
  ;; report back to cust. This is mandatory for Actors behind a
  ;; Serializer interface. Also useful when the target of an ASK.
  ;; Normal Actor behavior would simply never respond to waiting
  ;; customers in event of errors.
  ;;
  ;; Arg mon is a reference to our Serializer wrapper Actor.
  ;;
  ;; This code ensures that user code exec is wrapped by libusb-init
  ;; and libusb-exit. A FUTURE is returned to cust representing the
  ;; eventual outcome of the user code exec. User can ASK again of
  ;; FUTURE to get results. By returning a FUTURE, we do not block
  ;; other threads wanting to use libusb while user code execs.
  ;;
  ((cust :exec fn ppctxt perms)
   ;; fn is user Lisp code
   (let ((r (libusb-init ppctxt)))
     (cond ((minusp r) ;; can't open libusb
            (error 'usb-error :errno r))
           
           (t
            (let ((pctxt  (when ppctxt
                            (xffi:dereference ppctxt))))
              ;; return a FUTURE that represents the outcome of the
              ;; function exec
              (flet ((exec-beh (cust)
                       (β ans
                           (send unprot-gate β fn pctxt perms)
                         (send mon sink :close pctxt) ;; needs to go thru serializer
                         (send* cust ans))))
                (send cust (future (create #'exec-beh)))
                )))
           )))
  
  ((cust :close pctxt)
   (libusb-exit pctxt)
   (send cust :ok)))

(deflex usb-mon
  (let ((mon (serializer
              (create (lambda (cust mon)
                        (become (usb-mon-beh mon))
                        (send cust :ok))))
             ))
    (send mon sink mon)
    mon))

;; ----------------------------------------------

(defun do-with-libusb1 (fn needs-new-context-p)
  ;; Functional interface to underlying Actors system.  Inner ASK
  ;; returns a FUTURE for fn outcome. Outer ASK requests that outcome.
  ;;
  ;; If we have not already init libusb1 then:
  ;;    WITH-LIBUSB1 ()            use null context
  ;;    WITH-LIBUSB1 (name)        use fresh context
  ;;    WITH-LIBUSB1 (name pctxt)  if (NULL pctxt) use null context
  ;;                               else use fresh context
  ;;
  ;; If we have already init libusb11 then:
  ;;    WITH-LIBUSB1 ()           use the context already in force = *LIB-CONTEXT*
  ;;    WITH-LIBUSB1 (name)       use fresh context
  ;;    WITH-LIBUSB1 (name pctxt) if (NULL pctxt) or (EQL pctxt *LIB-CONTEXT*)
  ;;                              then use *LIB-CONTEXT* already in force
  ;;                              else use fresh context
  ;;
  (if (and (member usb-mon *enabled-gates*)
           (or (null needs-new-context-p)
               (eql needs-new-context-p *lib-context*)))
      (funcall fn *lib-context*)
    (flet ((doit (ppctxt)
             (ask (ask usb-mon :exec (=fut fn) ppctxt (cons usb-mon *enabled-gates*)))
             ))
      (if needs-new-context-p
          (xffi:with-dynamic-foreign-objects
              ((ppctxt (:pointer libusb-context)))
            (doit ppctxt))
        (doit nil))
      )))

(defmacro with-libusb1 ((&optional (pctxt-name nil pctxt-name-supplied-p)
                                   (needs-new-context-p t needs-supplied-p))
                        &body body)
  (let ((needs-new-context-p (if needs-supplied-p
                                 needs-new-context-p
                               pctxt-name-supplied-p))
        (pctxt (or pctxt-name
                   (gensym))))
    (check-type pctxt symbol)
    `(do-with-libusb1 #'(lambda (,pctxt)
                          (declare (ignorable ,pctxt))
                          ,@body)
                      ,needs-new-context-p)))

(defmacro with-context (pctxt &body body)
  ;; Use in callback functions that receive a context pointer.  This
  ;; macro ensures that any code that gets directed to run on another
  ;; thread will carry with them the correct contect pointer.
  `(let ((*lib-context* ,pctxt))
     ,@body))

;; ---------------------------------------------------------------
;; Helper Macros

(defmacro call-libusb (bindings libfn-form err-form &body body)
  ;; Use this macro to call libusb when it will return an error code.
  ;; We check that error code and perform the err-form if negative,
  ;; else we perform the body code. The initial bindings can set up an
  ;; object that may get filled in by-ref from the lib function
  ;; called.
  ;;
  ;; In the err-form, you can refer to the error code using
  ;; last-errno, last-error-name, and last-error-string.
  `(xffi:with-dynamic-foreign-objects ,bindings
     (do-call-libusb
      (lambda ()
        ,libfn-form)
      (lambda (rc)
        (declare (ignorable rc))
        (symbol-macrolet ((last-errno rc)
                          (last-error-name (libusb-get-last-error-name rc))
                          (last-error-string (libusb-get-last-error-string rc)))
          (declare (ignorable last-errno last-error-name last-error-string))
          ,err-form))
      (lambda ()
        ,@body))) )

(defun do-call-libusb (fn-libusb fn-err fn-okay)
  (let ((rc  (funcall fn-libusb)))
    (if (minusp rc)
        (funcall fn-err rc)
      (funcall fn-okay))))

(defun libusb-get-last-error-name (errno)
  (xffi:convert-from-foreign-string (libusb-error-name errno)))
    
(defun libusb-get-last-error-string (errno)
  (xffi:convert-from-foreign-string (libusb-strerror errno)))

;; ----------------------------------------------
;; Thread-Exclusive Code Management - Take care to access successive
;; gates in constant deterministic order to avoid logical deadlock.

(defun make-gate ()
  ;; Construct a Serializer around the USB-EXEC Actor.
  (serializer unprot-gate :timeout *timeout*))


(defun do-with-excl-gate (gate fn)
  (if (member gate *enabled-gates*)
      (funcall fn *lib-context*)
    (ask gate (=fut fn) *lib-context* (cons gate *enabled-gates*))))

(defmacro with-excl-gate (gate &body body)
  (lw:with-unique-names (ctxt)
    `(do-with-excl-gate ,gate (lambda (,ctxt)
                                (declare (ignore ,ctxt))
                                ,@body))))

;; ----------------------------
;; Various Gates...

(defvar *default-ask-timeout* 5)

(deflex usb-excl-polling
  ;; This Actor manages exclusive for polling purposes
  (with-timeout nil
    (make-gate)))

(deflex usb-excl-io
  ;; This Actor manages exclusive for I/O
  (with-timeout *default-ask-timeout*
    (make-gate)))

(deflex usb-excl-alloc
  ;; This Actor manages exclusive for alloc
  (with-timeout *default-ask-timeout*
    (make-gate)))

(defmacro with-excl-polling (&body body)
  ;; use for libusb polling stoff
  `(with-excl-gate usb-excl-polling ,@body))
 
(defmacro with-excl-io (&body body)
  ;; use for libusb exclusive I/O stuff
  `(with-excl-gate usb-excl-io ,@body))

(defmacro with-excl-alloc (&body body)
  ;; use when you are going to cause libusb to alloc/dealloc
  `(with-excl-gate usb-excl-alloc ,@body))

;; -----------------------------------------------------------
;; Device Management - allows for excl exec against devs. Any one
;; particular dev can only be banged on by one thread in excl mode,
;; but different devices can be banged on by separate threads, all in
;; parallel concurrent fashion.

(defun incref-gate (hand lst)
  (let ((pair (assoc hand lst)))
    (if pair
        (destructuring-bind (_ count gate) pair
          (declare (ignore _))
          (become (device-list-beh (cons (list hand (1+ count) gate)
                                         (remove pair lst))))
          gate)
      (let ((gate (with-timeout *default-ask-timeout* ;; <-- This could be a problem!
                    (make-gate))))
        (become (device-list-beh (cons (list hand 1 gate) lst)))
        gate))
    ))

(defun decref-gate (hand lst)
  (let ((pair (assoc hand lst)))
    (when pair
      (destructuring-bind (_ count gate) pair
        (declare (ignore _))
        (let ((newlst (remove pair lst))
              (newcnt (1- count)))
          (become (device-list-beh
                   (if (zerop newcnt)
                       newlst
                     (cons (list hand newcnt gate) newlst))))
          )))
    ))

(def-ser-beh device-list-beh (&optional lst)
  ;; Device list is a shared association list between device handles
  ;; and their excl I/O managers.
  ((cust :add-dev hand)
   ;; add excl I/O manager if not already present.
   ;; incr ref count if present
   ;; sends gate to cust
   (incref-gate hand lst)
   (send cust :ok))
  
  ((cust :remove-dev hand)
   ;; remove a device excl manager - harmless if not present
   ;; decr ref count if present, remove if count becomes zero.
   (decref-gate hand lst)
   (send cust :ok))

  ((cust :access-dev hand fn pctxt perms)
   (let* ((me      self)
          (augm-fn (lambda (pctxt)
                     (declare (ignore pctxt))
                     (unwind-protect
                         (funcall fn)
                       (send-to-pool me sink :remove-dev hand))
                     ))
          (gate    (incref-gate hand lst)))
     (if (member gate perms)
         (send unprot-gate cust augm-fn pctxt perms)
       (send gate cust augm-fn pctxt (cons gate perms)))
     )) )
                
(deflex device-list
  (create (device-list-beh)))

;; --------------------------------------------------------------

(defun add-device (hand)
  ;; using ASK here to enforce completion before returning to caller
  (ask device-list :add-dev hand))

(defun remove-device (hand)
  ;; using ASK here to enforce completion before returning to caller
  (ask device-list :remove-dev hand))

(defun do-with-device (hand fn)
  (ask device-list :access-dev hand (=fut fn) *lib-context* *enabled-gates*))
       
(defmacro with-device (hand &body body)
  ;; execute excl body code while owning the device
  `(do-with-device ,hand (lambda () ,@body)))

;; ------------------------------------------------------------
;; Individual open/close/use - try not to use these, prefer
;; WITH-OPEN-DEVICE

(defun open-dev (dev phand)
  ;; Opening a device returns its handle, but does not confer
  ;; ownership. Use WITH-DEVICE around the handle and code.
  (call-libusb ()
      (libusb-open dev phand)
      (error "Can't open device: ~A" last-error-name)
    (let ((hand (xffi:dereference phand)))
      (unless (xffi:null-pointer-p hand)
        (add-device hand)
        hand))))

(defun close-dev (hand)
  (with-device hand
    ;; We have to own device to close it
    (call-libusb ()
        (libusb-close hand)
        (error "Can't close devicee: ~A" last-error-name)
      (remove-device hand))))

;; --------------------------------------------------
;; WITH-OPEN-DEVICE - cleaner preferred interface toward using devices

(defmacro with-open-device ((hand dev) &body body)
  `(do-with-open-device ,dev (lambda (,hand) ,@body)))

(defun do-with-open-device (dev fn)
  (call-libusb
      ((phand (:pointer libusb-device-handle)
              :initial-element nil))
      (libusb-open dev phand)
      (error "Can't open device: ~A, error: ~A" dev last-error-name)
    (let ((hand (xffi:dereference phand)))
      (with-device hand
        (unwind-protect
            (funcall fn hand)
          (libusb-close hand)))
      )))

;; -------------------------------------------------------
#|
(unwind-protect
    (sleep 1)
  (send writeln :ok))

(with-device 15
  ;; (sleep 6)
  (error "What!?"))

(with-excl-io
  (error "What!?"))

(defvar my-gate (make-gate))

(defmacro with-my-gate (&body body)
  `(with-excl-gate my-gate ,@body))

(with-my-gate
  15)

(with-my-gate
  (error "What!?"))

(handler-case
    (with-my-gate
       (sleep 6)
      (error "What!?"))
  (error (c)
    (format t "Caught the error: ~A" c)
    15
    ;; (values nil c)
    ))

(=handler-case
     (with-device 15
      (sleep 6)
      (error "What!?"))
  (error (c)
    (format t "Caught the error: ~A" c)
    c))
|#
