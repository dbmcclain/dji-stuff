;;;; libusb-ffi.lisp

(in-package #:libusb1)

;; --------------------------------------------------------------
;; GET-VER - return a list of version items for this libusb

(defun get-ver ()
  (WITH-LIBUSB1 ()
    (let ((pver (libusb-get-version)))
      (fli:with-foreign-slots (major minor micro nano rc describe) pver
        (list
         :major    major
         :minor    minor
         :micro    micro
         :nano     nano
         :rc       (fli:convert-from-foreign-string rc)
         :describe (fli:convert-from-foreign-string describe))
        ))))
    
;; -------------------------------------------------------------
;; TEST-HOTPLUG - given a vendor-id and product-id to watch for,
;; test out the plug-in and unplug behaviors of libusb

(let ((phand  nil)
      (done   0))

  (fli:define-foreign-callable ("hotplug_callback" :result-type :int)
      ((ctxt (:pointer libusb-context))
       (dev  (:pointer libusb-device))
       (evt  (:pointer libusb-hotplug-event))
       (user-data :pointer))
    (declare (ignore evt user-data))
    (WITH-CONTEXT ctxt
      (call-libusb
          ((desc libusb-device-descriptor))
          (libusb-get-device-descriptor dev desc)
          (print "Error getting device descriptor")
        (fli:with-foreign-slots (idVendor idProduct) desc
          (format t "~%Device attached: ~4,'0x:~4,'0x" idVendor idProduct))
        (let ((hand (fli:dereference phand)))
          (unless (fli:null-pointer-p hand)
            (close-dev hand) ;; <-- !!
            (setf (fli:dereference phand) nil) ))
        (open-dev dev phand) ;; <-- !!
        (incf done)
        0)))

  (fli:define-foreign-callable ("hotplug_callback_detach" :result-type :int)
      ((ctxt (:pointer libusb-context))
       (dev  (:pointer libusb-device))
       (evt  (:pointer libusb-hotplug-event))
       (user-data :pointer))
    (declare (ignore dev evt user-data))
    (WITH-CONTEXT ctxt
      (print "Device detached")
      (let ((hand (fli:dereference phand)))
        (unless (fli:null-pointer-p hand)
          (close-dev hand) ;; <-- !!
          (setf (fli:dereference phand) nil)))
      (incf done)
      0))

  (defun test-hotplug (vendor-id product-id &optional (class-id +LIBUSB-HOTPLUG-MATCH-ANY+))
    (WITH-LIBUSB1 (pctxt)
      (when (zerop (libusb-has-capability
                    (fli:enum-symbol-value 'libusb-capability
                                           'LIBUSB-CAP-HAS-HOTPLUG)))
        (error "Hotplug capabilities are not supported on this platform"))
      
      (WITH-EXCL-POLLING
       (fli:with-dynamic-foreign-objects
           ((ptr  (:pointer libusb-device-handle)
                  :initial-element nil))
         (setf phand  ptr
               done   0)
         (unwind-protect
             (fli:with-dynamic-foreign-objects
                 ((hpin  libusb-hotplug-callback-handle
                         :initial-element nil)
                  (hpout libusb-hotplug-callback-handle
                         :initial-element nil))
               (call-libusb ()
                   (libusb-hotplug-register-callback
                    pctxt
                    (fli:enum-symbol-value 'libusb-hotplug-event
                                           'LIBUSB-HOTPLUG-EVENT-DEVICE-ARRIVED)
                    0
                    vendor-id
                    product-id
                    class-id
                    (fli:make-pointer :symbol-name "hotplug_callback")
                    nil
                    hpin)
                   (error "Error registering callback IN"))
               (call-libusb ()
                   (libusb-hotplug-register-callback
                    pctxt
                    (fli:enum-symbol-value 'libusb-hotplug-event
                                           'LIBUSB-HOTPLUG-EVENT-DEVICE-LEFT)
                    0
                    vendor-id
                    product-id
                    class-id
                    (fli:make-pointer :symbol-name "hotplug_callback_detach")
                    nil
                    hpout)
                   (error "Error registering callback OUT"))
               (do ()
                   ((<= 2 done))
                 (call-libusb ()
                     (libusb-handle-events pctxt)
                     (error "libusb_handle_events() failed: ~A"  last-error-name)
                   )))
           ;; unwind clauses
           (let ((hand (fli:dereference ptr)))
             (unless (fli:null-pointer-p hand)
               (close-dev hand))) ;; <-- !!
           (setf phand  nil
                 done   0))
         ))
      )))

;; -------------------------------------------------------------
;; PRINT-DEVS - print a list of devices installed in the machine USB
;; tree.

(defvar *npath*  8)  ;; path buffer size in bytes

(defun %print-devs (pdevs)
  (fli:with-dynamic-foreign-objects
      ((desc (:struct libusb-device-descriptor))
       (path  uint8-t :nelems *npath*))
    (um:nlet iter ((ix 0))
      (let ((pdev (fli:dereference pdevs :index ix)))
        (unless (fli:null-pointer-p pdev)
          (call-libusb ()
              (libusb-get-device-descriptor pdev desc)
              (error "failed to get device descriptor")
            (fli:with-foreign-slots (idVendor idProduct) desc
              (format t "~%~4,'0x:~4,'0x (bus ~d, device ~d)"
                      idVendor idProduct
                      (libusb-get-bus-number pdev)
                      (libusb-get-device-address pdev)))
            (let ((r (libusb-get-port-numbers pdev path *npath*)))
              (when (plusp r)
                (format t "~%  path: ~d" (fli:dereference path :index 0))
                (loop for jx from 1 below r do
                          (format t ".~d" (fli:dereference path :index jx)))))
            (go-iter (1+ ix)))
          )))))

(defun print-devs ()
  (with-libusb1 (pctxt)
    (call-libusb ;; has implicit WITH-LIBUSB1
        ((ppdevs (:pointer (:pointer libusb-device))
                 :initial-element nil))
        (libusb-get-device-list pctxt ppdevs)
        (error "Can't get devs")
      (let ((pdevs (fli:dereference ppdevs)))
        (unwind-protect
            (progn
              (%print-devs pdevs)
              (values))
          (libusb-free-device-list pdevs 1)))
      )))

;; -------------------------------------------------------------
;; Show that WITH-LIBUSB1 nests properly within one thread, i.e., no
;; logical deadlocks.

(defun test-nesting ()
  (WITH-LIBUSB1 ()
    ;; both of these call their own WITH-LIBUSB1
    (print (get-ver))
    (print-devs)))

#|
(β _
    (send β)
  (allow-recursive-ask
    (send println (list (ask (const 15)) (ask (const 32))))))
|#    

;; -------------------------------------------------------------
