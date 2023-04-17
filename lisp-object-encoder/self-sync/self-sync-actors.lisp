
(defpackage :ssact
  (:use :cl :ac)
  (:export
   ))

(in-package :ssact)

(um:eval-always
  (import '(scatter-vec:make-scatter-vector
            scatter-vec:xlength
            scatter-vec:xaref
            scatter-vec:xposition
            scatter-vec:xdovec
            scatter-vec:xupdate-digest
            scatter-vec:add-fragment
            scatter-vec:xwrite-sequence
            
            ubyte-streams:make-ubyte-output-stream
            ubyte-streams:stream-bytes
            ubyte-streams:with-output-to-ubyte-stream
            ubyte-streams:with-input-from-ubyte-stream

            useful-macros:nlet
            useful-macros:ash-dpbf
            useful-macros:while
            useful-macros:alet
            useful-macros:alet-fsm
            useful-macros:when-let
            )))

;; ------------------------------------------------------------------
;; Encoding
;;   +-----------+---------+------------+---------------+---------------+--//--+
;;   | #xFE #xFD | VER (1) | NSHORT (1) | Short Segment | Long Segments | ...  |
;;   +-----------+---------+------------+---------------+---------------+--//--+
;;
;;     Short Segment
;;     +-----------+-------------+------------------------+
;;     | CRC32 (4) | Enc Len (4) | First Bytes (NSHORT-8) |
;;     +-----------+-------------+------------------------+
;;
;;     Long Segment
;;     +---------+----------+------------+
;;     | Rem (1) | Quot (1) | More Bytes |
;;     +---------+----------+------------+
;;
;; Segments are msg chunks located between sequences of #xFE #xFD (the start seq).
;; Short Segment has max length of #xFC = 252 bytes.
;; Long Segments have max length of 252*252-1 = 63503 bytes.
;; Enocding has embedded start sequences elided - replaced at segment boundaries upon decoding.
;; No valid internal encodings have start sequences embedded.
;; CRC32 is over the 4 byte Enc Len + original message bytes.
;; Damaged encodings can be resync'd at next start sequence.
;;
;; ------------------------------------------------------------------
(defconstant +long-count-base+  #xFD)
(defconstant +max-short-count+  (1- +long-count-base+))
(defconstant +max-long-count+   (1- (* +long-count-base+ +long-count-base+)))
(defconstant +start-sequence+   #(#xFE #xFD))
;; ---------------------------------------------------------------

(defun make-ubv (size &rest args)
  (apply #'make-array size
         :element-type '(unsigned-byte 8)
         args))

(defun int-to-vec-le4 (n)
  (let ((ans (make-ubv 4)))
    (nlet iter ((ix 0)
                (n  n))
      (if (> ix 3)
          ans
        (multiple-value-bind (q r) (truncate n 256)
          (setf (aref ans ix) r)
          (go-iter (1+ ix) q)))
      )))

(defun vec-le4-to-int (vec)
  (let ((ans 0))
    (loop for ix from 3 downto 0 do
          (ash-dpbf ans 8 (xaref vec ix)))
    ans))

(defun crc32 (&rest vecs)
  (let ((dig (ironclad:make-digest :crc32)))
    (dolist (vec vecs)
      (xupdate-digest dig vec))
    (ironclad:produce-digest dig)))

;; ------------------------------------------------------------------
;; Note: Use of XAREF, XPOSITION, XLENGTH, XWRITE-SEQUENCE, allows for
;; scatter-gather vectors. Hopefully more efficient than copying and
;; concatenating vectors.

(defun find-start-seq (enc start end)
  (when (< start (1- end))
    (when-let (pos (xposition #xFE enc
                              :start start
                              :end   (1- end)))
      (if (eql #xFD (xaref enc (1+ pos)))
          pos
        (find-start-seq enc (1+ pos) end))
      )))

;; ------------------------------------------------------------------
;; Self-Sync Record Writing - soft migration to new versions is
;; possible so long as every version has the same starting sequence
;; #(#xFE #xFD <versionByte>). Beyond that can be version dependent.

(defun write-record (enc fout)
  (let* ((renc (make-scatter-vector))
         (len  (int-to-vec-le4 (xlength enc)))
         (crc  (crc32 len enc)))
    (add-fragment renc crc)
    (add-fragment renc len)
    (add-fragment renc enc)
    (let* ((start     0)
           (end       (xlength renc))
           (pos       (find-start-seq renc 0 end))
           (max-ct    +max-short-count+)
           (short-end (min +max-short-count+ (or pos end)))
           (nb        short-end))
      (write-sequence +start-sequence+ fout)
      (write-byte #x01 fout) ;; this code encodes Version 1
      (write-byte nb fout)
      (xwrite-sequence renc fout :start 0 :end short-end)
      (setf start short-end)
      (while (< start end)
        (when (< nb max-ct)
          (incf start 2))
        (when (and pos
                   (< pos start))
          (setf pos (find-start-seq renc start end)))
        (setf max-ct +max-long-count+)
        (let ((long-end (min (+ start +max-long-count+) (or pos end))))
          (setf nb (- long-end start))
          (multiple-value-bind (q r)
              (truncate nb +long-count-base+)
            ;; in little-endian form
            (write-byte r fout)
            (write-byte q fout))
          (xwrite-sequence renc fout :start start :end long-end)
          (setf start long-end)))
      )))
    
(defun encode (vec)
  (with-output-to-ubyte-stream (sout)
    (write-record vec sout)))

;; ---------------------------------------------------------------
;; Self-Sync Stream Decoding...

(defun stuffer-beh (aout stuff-fn needs-fefd?)
  (alambda
   ((cust :init size)
    (let ((new-needs-fefd? (< size +max-short-count+)))
      (setf (fill-pointer aout) 0) ;; FPL violation?
      (become (stuffer-beh aout stuff-fn new-needs-fefd?))
      (send cust :ok)))
   
   ((cust :init-frag size)
    (let ((new-needs-fefd? (< size +max-long-count+)))
      (become (stuffer-beh aout stuff-fn new-needs-fefd?))
      (send cust :ok)))
   
   ((cust :stuff b)
    (funcall stuff-fn b aout) ;; another FPL violation?
    (send cust :ok))
   
   ((cust :maybe-stuff-fefd)
    (when needs-fefd?
      (funcall stuff-fn #xFE aout)
      (funcall stuff-fn #xFD aout))
    (send cust :ok))
   
   ((cust :check-finish)
    (let (result)
      (when (>= (length aout) 8)
        (let* ((crc (subseq aout 0 4))
               (len (subseq aout 4 8))
               (ans (subseq aout 8))
               (chk (crc32 len ans)))
          (when (and (equalp crc chk)
                     (= (vec-le4-to-int len) (length ans)))
            (setf result ans))))
      (send cust result)))
   ))

;; ---------------------------------------------------------------

(defun make-stuffer (max-reclen)
  (let (aout stuffer)
    (if max-reclen
        (setf aout (make-ubv (+ max-reclen 8)
                             :fill-pointer 0)
              stuffer #'vector-push)
      (setf aout (make-ubv 256
                           :fill-pointer 0
                           :adjustable   t)
            stuffer #'vector-push-extend))
    (create (stuffer-beh aout stuffer nil))
    ))

(defun make-decoder-fsm (&key max-reclen)
  (create (start-beh (make-stuffer max-reclen))))

;; ---------------------------------------------------------------

(defun valid-code-p (b)
  (and (integerp b)
       (<= 0 b #xFC)))

(defun start-beh (stuffer)
  (lambda (cust b)
    (case b
      (#xFE
       (become (check-start-fd-beh stuffer))
       (send cust :NEXT))
      (:EOF
       (send cust :EOF))
      (t
       (send cust :NEXT))
      )))

(defun check-start-fd-beh (stuffer)
  (lambda (cust b)
    (case b
      (#xFD
       (become (check-version-beh stuffer))
       (send cust :NEXT))
      (:EOF
       (send cust :EOF))
      (t
       (become (start-beh stuffer))
       (repeat-send self))
      )))

(defun check-version-beh (stuffer)
  (lambda (cust b)
    (case b
      (#x01
       (become (read-short-count-beh stuffer))
       (send cust :NEXT))
      (:EOF
       (send cust :EOF))
      (t
       (become (start-beh stuffer))
       (repeat-send self))
      )))

(defun read-short-count-beh (stuffer)
  (lambda (cust b)
    (cond ((valid-code-p b)
           (become (read-frag-beh stuffer b))
           (β _
               (send stuffer β :init b)
             (send cust :NEXT)))

          ((eql b :EOF)
           (send cust :EOF))
          
          (t
           (become (start-beh stuffer))
           (repeat-send self))
          )))

(defun read-frag-beh (stuffer ct)
  (if (plusp ct)
      (lambda (cust b)
        (cond
         ((eql b :EOF)
          (send cust :EOF))
         
         ((and (> ct 1)
               (eql b #xFE))
          (become (check-frag-fd-beh stuffer ct))
          (send cust :NEXT))
         
         (t
          (become (read-frag-beh stuffer (1- ct)))
          (β _
              (send stuffer β :stuff b)
            (send cust :NEXT)))
         ))
    ;; else
    (read-long-count-beh stuffer)))

(defun check-frag-fd-beh (stuffer ct)
  (lambda (cust b)
    (cond ((eql b #xFD) ;; we encountered a start seq #xFE #xFD
           (become (check-version-beh stuffer))
           (send cust :NEXT))

          ((eql b :EOF)
           (send cust :EOF))
          
          (t
           (become (read-frag-beh stuffer (1- ct)))
           (let ((me self))
             (β _
                 (send stuffer β :stuff #xFE)
               (send me cust b))))
          )))

(defun read-long-count-beh (stuffer)
  (lambda (cust b)
    (cond ((valid-code-p b)
           (become (read-long-count-2-beh stuffer b))
           (β _
               (send stuffer β :maybe-stuff-fefd)
             (send cust :NEXT)))

          ((eql b :EOF)
           (β (ans)
               (send stuffer β :check-finish)
             (send cust (or ans :EOF))))

          (t
           (become (start-beh stuffer))
           (let ((me self))
             (β (ans)
                 (send stuffer β :check-finish)
               (if ans
                   (send cust ans)
                 (send me cust b))
               )))
          )))

(defun read-long-count-2-beh (stuffer r)
  (lambda (cust b)
    (cond ((valid-code-p b)
           (let ((new-ct (+ r (* +long-count-base+ b))))
             (become (read-frag-beh stuffer new-ct))
             (β _
                 (send stuffer β :init-frag new-ct)
               (send cust :NEXT))))

          ((eql b :EOF)
           (send cust :EOF))
          
          (t
           (become (start-beh stuffer))
           (repeat-send self))
          )))

;; ---------------------------------------------------------------

(defun decoder-beh (finp machine)
  (lambda (cust)
    (let (driver)
      (labels ((decode-byte ()
                 (send machine driver (read-byte finp nil :EOF)))
               (driver-beh (ans)
                 (cond ((eql ans :NEXT)  (decode-byte))
                       (t                (repeat-send cust))
                       )))
        (setf driver (create #'driver-beh))
        (decode-byte))
      )))

(defun decode (vec)
  (with-input-from-ubyte-stream (sin vec)
    (let ((decoder (create (decoder-beh sin (make-decoder-fsm)))))
      (ask decoder)
      )))

;; -----------------------------------------------------------------
;; Stream Decoding

(defun stream-decoder-beh (cust fsm wait-ix queue)
  (alambda
   ((:deliver bufix buf)
    (cond ((eql bufix wait-ix)
           (become (busy-stream-decoder-beh cust fsm bufix buf (length buf) 0 queue))
           (send self :next))
          
          ((> bufix wait-ix)
           (become (stream-decoder-beh cust fsm wait-ix (maps:add queue bufix buf))))
          ))

   ((ans) / (vectorp ans)
    (send cust ans))
   ))

(defun busy-stream-decoder-beh (cust fsm bufix buf len pos queue)
  (alambda
   ((:next)
    (cond ((< pos len)
           (send fsm self (aref buf pos))
           (become (busy-stream-decoder-beh cust fsm bufix buf len (1+ pos) queue)))
          
          (t
           (let* ((next-bufix (1+ bufix))
                  (next-buf   (maps:find queue next-bufix)))
             (cond (next-buf
                    (become (busy-stream-decoder-beh cust fsm next-bufix next-buf (length next-buf) 0
                                                     (maps:remove queue next-bufix)))
                    (send self :next))
                   
                   (t
                    (become (stream-decoder-beh cust fsm next-bufix queue))
                    (send fsm self :EOF)) ;; check for completed
                   )))
          ))
   
   ((:deliver next-bufix next-buf)
    (when (> next-bufix bufix)
      (become (busy-stream-decoder-beh cust fsm bufix buf len pos
                                       (maps:add queue next-bufix next-buf)))))

   ((ans) / (vectorp ans)
    (send cust ans)
    (send self :next))
   ))
   
(defun stream-decoder (cust)
  (let ((fsm (make-decoder-fsm)))
    (create (stream-decoder-beh cust fsm 0 (maps:empty)))
    ))


  
#|
(setf s  (hcl:file-binary-bytes "taxes.lisp"))
(setf se (encode s))
(map 'string #'code-char (self-sync:decode se))
(map 'string #'code-char (decode se))

(let ((se (encode (loenc:encode #(0 1 2 3 4 5)))))
  (print se)
  (decode se))

(let* ((s  (make-ubv 9
                     :initial-contents '(0 1 2 3 4 5 #xFE #xFE #xFD)))
       (se (encode s)))
  (print se)
  (decode se))

(self-sync:decode (self-sync:encode (loenc:encode #(0 1 2 3 4 5))))

(defun encser (ekey &rest objs)
  (let* ((enc        (apply #'loenc:encode (coerce objs 'vector)))
         (cmpr       (compress enc))
         (seq        (make-nonce))
         (emsg       (encrypt ekey seq cmpr))
         (sig        (sign ekey seq emsg))
         (packet     (vector seq emsg sig))
         (enc-packet (loenc:encode packet)))
    (self-sync:encode enc-packet)))

 |#
