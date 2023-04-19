;; libusb-dff-inl.lisp - translation of inline C functions
;;
;; DM/RAL  2023/04/19 05:25:20
;; ----------------------------------

(in-package :libusb1)


;; --------------------------------------------------------------------
;; Code added DM/RAL  2023/04/15 11:11:49
;;
;; Collection of inline functions from libusb.h - not carried over by
;; FOREIGN-PARSER.

(defconstant +libusb-hotplug-match-any+ -1)


#|
(let ((package (find-package :libusb1)))
  (do-symbols (x package)
    (multiple-value-bind (symbol accessibility)
        (find-symbol (symbol-name x) package)
      (unless (eql :inherited accessibility)
        (let ((name (symbol-name symbol)))
          (when (and (> (length name) 7)
                     (string-equal (subseq name 0 7) "libusb-"))
            (format t "~%#:~A" name)
            ))))))
|#

(defconstant +libusb-control-setup-size+ (fli:size-of 'libusb-control-setup))

(fli:define-c-union chimera
  (b  (:foreign-array :uint8 (2)))
  (w  :uint16))

(defun libusb-le16-to-cpu (x)
  (fli:with-dynamic-foreign-objects
      ((tmp  chimera))
    (setf (fli:foreign-aref (fli:foreign-slot-pointer tmp 'b) 0) (ldb (byte 8 0) x)
          (fli:foreign-aref (fli:foreign-slot-pointer tmp 'b) 1) (ldb (byte 8 8) x))
    (fli:foreign-slot-value tmp 'w)))
        
(defun libusb-cpu-to-le16 (x)
  (libusb-le16-to-cpu x))

(defun libusb-control-transfer-get-data (ptransfer)
  (let ((ptr (fli:foreign-slot-pointer ptransfer 'buffer)))
    (fli:incf-pointer ptr +libusb-control-setup-size+)))

(defun libusb-control-transfer-get-setup (ptransfer)
  (fli:foreign-slot-pointer ptransfer 'buffer
                            :type 'libusb-control-setup))

(defun libusb-fill-control-setup (buffer RequestType Request Value Index len)
  (let ((setup (fli:make-pointer :address buffer
                                 :type    'libusb-control-setup)))
    (fli:with-foreign-slots (bmRequestType bRequest wValue wIndex wLength) setup
      (setf bmRequestType RequestType
            bRequest      Request
            wValue        Value
            wIndex        Index
            wLength       len))
    ))

(defun libusb-fill-control-transfer (ptransfer adev-handle abuffer acallback auser-data atimeout)
  (fli:with-foreign-slots (dev-handle endpoint type timeout buffer length user-data callback) ptransfer
    (setf dev-handle adev-handle
          endpoint 0
          type     (fli:enum-symbol-value 'LIBUSB-TRANSFER-TYPE 'LIBUSB-TRANSFER-TYPE-CONTROL)
          timeout  atimeout
          buffer   abuffer
          length   (unless (fli:null-pointer-p abuffer)
                     (+ +LIBUSB-CONTROL-SETUP-SIZE+
                        (libusb-le16-to-cpu (fli:foreign-slot-value abuffer 'wLength
                                                                    :object-type '(:pointer libusb-control-setup))
                                            )))            
          user-data auser-data
          callback  acallback)
    ))

(defun libusb-fill-bulk-transfer (ptransfer adev-handle aendpoint pbuffer alength acallback puser-data atimeout)
  (fli:with-foreign-slots (dev-handle endpoint type timeout buffer length user-data callback) ptransfer
    (setf dev-handle  adev-handle
          endpoint    aendpoint
          type        (fli:enum-symbol-value 'libusb-transfer-type 'LIBUSB-TRANSFER-TYPE-BULK)
          timeout     atimeout
          buffer      pbuffer
          length      alength
          user-data   puser-data
          callback    acallback)))

(defun libusb-fill-bulk-stream-transfer (ptransfer adev-handle anendpoint astream-id pbuffer alength acallback puser-data atimeout)
  (libusb-fill-bulk-transfer ptransfer adev-handle anendpoint pbuffer alength acallback puser-data atimeout)
  (setf (fli:foreign-slot-value ptransfer 'type)
        (fli:enum-symbol-value 'libusb-transfer-type 'libusb-transfer-type-bulk-stream))
  (libusb-transfer-set-stream-id ptransfer astream-id))

(defun libusb-fill-interrupt-transfer (ptransfer adev-handle anendpoint pbuffer alength acallback puser-data atimeout)
  (fli:with-foreign-slots (dev-handle endpoint type timeout buffer length user-data callback) ptransfer
    (setf dev-handle  adev-handle
          endpoint    anendpoint
          type        (fli:enum-symbol-value 'libusb-transfer-type 'LIBUSB-TRANSFER-TYPE-INTERRUPT)
          timeout     atimeout
          buffer      pbuffer
          length      alength
          user-data   puser-data
          callback    acallback)))

(defun libusb-fill-iso-transfer (ptransfer adev-handle anendpoint pbuffer alength anum-iso-packets acallback puser-data atimeout)
  (fli:with-foreign-slots (dev-handle endpoint type timeout buffer length num-iso-packets user-data callback) ptransfer
    (setf dev-handle  adev-handle
          endpoint    anendpoint
          type        (fli:enum-symbol-value 'libusb-transfer-type 'LIBUSB-TRANSFER-TYPE-ISOCHRONOUS)
          timeout     atimeout
          buffer      pbuffer
          length      alength
          num-iso-packets anum-iso-packets
          user-data   puser-data
          callback    acallback)))

(defun libusb-set-iso-packet-lengths (ptransfer alength)
  (loop for ix from 0 below (fli:foreign-slot-value ptransfer 'num-iso-packets) do
          (setf (fli:foreign-slot-value
                 (fli:foreign-aref
                  (fli:foreign-slot-value ptransfer 'iso-packet-desc)
                  ix)
                 'length)
                alength)))

(defun libusb-get-iso-packet-buffer (ptransfer apacket)
  (when (< -1 apacket (fli:foreign-slot-value ptransfer 'num-iso-packets))
    (let ((ptr (fli:make-pointer :address (fli:foreign-slot-value ptransfer 'buffer)
                                 :type    :uint8)))
      (fli:incf-pointer ptr
                        (loop for ix from 0 below apacket sum
                                (fli:foreign-slot-value
                                 (fli:foreign-aref
                                  (fli:foreign-slot-value ptransfer 'iso-packet-desc)
                                  ix)
                                 'length)))
      )))

(defun libusb-get-iso-packet-buffer-simple (ptransfer apacket)
  (when (< -1 apacket (fli:foreign-slot-value ptransfer 'num-iso-packets))
    (let ((ptr  (fli:make-pointer :address (fli:foreign-slot-value ptransfer 'buffer)
                                  :type    :uint8)))
      (fli:incf-pointer ptr
                        (* apacket
                           (fli:foreign-slot-value
                            (fli:foreign-aref
                             (fli:foreign-slot-value ptransfer 'iso-packet-desc)
                             0)
                            'length)))
      )))

(defun libusb-get-descriptor (dev-handle desc-type desc-index pdata len)
  (libusb-control-transfer dev-handle
                           (fli:enum-symbol-value 'libusb-endpoint-direction 'LIBUSB-ENDPOINT-IN)
                           (fli:enum-symbol-value 'libusb-standard-request 'LIBUSB-REQUEST-GET-DESCRIPTOR)
                           (logior (ash desc-type 8) desc-index)
                           0 pdata len 1000))

(defun libusb-get-string-descriptor (dev-handle desc-index langid pdata length)
  (libusb-control-transfer dev-handle
                           (fli:enum-symbol-value 'libusb-endpoint-direction 'LIBUSB-ENDPOINT-IN)
                           (fli:enum-symbol-value 'libusb-request-descriptor 'LIBUSB-REQUEST-GET-DESCRIPTOR)
                           (logior (ash (fli:enum-symbol-value 'LIBUSB-DESCRIPTOR-TYPE 'LIBUSB-DT-STRING)
                                        8)
                                   desc-index)
                           langid pdata length 1000))

