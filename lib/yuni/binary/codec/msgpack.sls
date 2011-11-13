(library (yuni binary codec msgpack)
         (export generate-msgpack-buffer
                 )
         (import (rnrs)
                 (srfi :42)
                 (srfi :8))

;; Buffer (for R6RS)

(define (bnot x)
  (bitwise-not x))

(define (make-bytevector-buffer endian)
  (define pos (if (eq? endian 'little)
                (lambda (addr width) addr)
                (lambda (addr width) (- width addr 1))))
  (define (put-bf flip? port i addr width)
    (let* ((p (* 8 (pos addr width)))
           (b (bitwise-bit-field i p (+ p 8)))
           (o (if flip? (bnot b) b)))
      (put-u8 port o)))

  (receive (port proc) (open-bytevector-output-port)
    (case-lambda
      (()  ;; flush
       (proc))
      ((i) ;; u8 output
       (put-u8 port i))
      ((i width) ;; word output
       (let* ((flip? (< i 0))
              (x (if flip? (- (- i) 1) i )))
         (do-ec (: addr width)
                (put-bf flip? port x addr width)))))))

(define (make-buffer) (make-bytevector-buffer 'big))

;; Enc

(define UINTMAX32 (- (expt 2 32) 1))
(define UINTMAX64 (- (expt 2 64) 1))
(define INTMIN32 (- (expt 2 31)))
(define INTMIN64 (- (expt 2 63)))

;; Write objects other than Array/Map
(define (out/simple buf obj)
  (case obj
    ((#t) (buf #xc3))
    ((#f) (buf #xc2))
    (else
      (cond
        ((null? obj) (buf #xc0))
        ((integer? obj)
         (cond
           ((<= 0 obj 127)
            (buf obj))
           ((<= -32 obj 0)
            (buf obj))
           ((<= 0 obj 255)
            (buf #xcc)
            (buf obj))
           ((<= 0 obj 65535)
            (buf #xcd)
            (buf obj 2))
           ((<= 0 obj UINTMAX32)
            (buf #xce)
            (buf obj 4))
           ((<= 0 obj UINTMAX64)
            (buf #xcf)
            (buf obj 8))
           ((<= -128 obj 0)
            (buf #xd0)
            (buf obj))
           ((<= -32768 obj 0)
            (buf #xd1)
            (buf obj 2))
           ((<= INTMIN32 obj 0)
            (buf #xd2)
            (buf obj 4))
           ((<= INTMIN64 obj 0)
            (buf #xd3)
            (buf obj 8))
           (else (assertion-violation 'msgpack
                                      "integer overflow"
                                      obj))))
        (else (assertion-violation 'msgpack
                                   "unsupported object"
                                   obj))))))

(define (generate-msgpack-buffer obj)
  (define buf (make-buffer))
  (define (outbuf)
    (let ((bv (buf)))
      (cons bv (bytevector-length bv))))
  (define (out/array cur obj)
    (if (pair? obj)
      (let ((a (car obj))
            (d (cdr obj)))
        (out/array (out cur a) d))
      cur))
  (define (out/map cur obj)
    (define len (vector-length obj))
    (define (itr idx cur)
      (if (= idx len)
        cur
        (let ((x (vector-ref obj idx)))
          (unless (pair? x) 
            (assertion-violation 'msgpack
                                 "Invalid object in map"
                                 x)) 
          (let ((key (car x))
                (value (cdr x)))
            (let* ((a (out cur key))
                   (b (out a value)))
              (itr (+ idx 1) b))))))
    (itr 0 cur))
  (define (out cur obj)
    ;; List => Array
    ;; Vector-of-pair => Map
    (cond
      ((pair? obj)
       (let ((len (length obj)))
         (cond
           ((<= len 15) ;; fixed array
            (buf (+ #x90 len)))
           ((<= len 65535)
            (buf #xdc)
            (buf len 2))
           ((<= len UINTMAX32)
            (buf #xdd)
            (buf len 4))
           (else
             (assertion-violation 'msgpack
                                  "Array too long")))
         (out/array cur obj)) )
      ((vector? obj)
       (let ((len (vector-length obj)))
         (cond
           ((<= len 15)
            (buf (+ #x80 len )))
           ((<= len 65535)
            (buf #xde)
            (buf len 2))
           ((<= len UINTMAX32)
            (buf #xdf)
            (buf len 4))
           (else
             (assertion-violation 'msgpack
                                  "Map too long")))
         (out/map cur obj)))
      ((bytevector? obj)
       (let ((len (bytevector-length obj)))
         (cond
           ((<= len 31)
            (buf (+ #xa0 len)))
           ((<= len 65535)
            (buf #xda)
            (buf len 2))
           ((<= len UINTMAX32)
            (buf #xdb)
            (buf len 4))
           (else
             (assertion-violation 'msgpack
                                  "Raw object too long")))
         (cons (cons (bytevector-copy obj) len) 
               (cons (outbuf) cur))))
      ((string? obj)
       (out cur (string->utf8 obj)))
      (else (out/simple buf obj)
            cur)))
  (let* ((output (out '() obj))
         (finalbuf (buf))
         (len (bytevector-length finalbuf))
         (finaloutput (if (zero? len) output (cons (cons finalbuf len) output))))
    (reverse finaloutput)))

;; Dec

(define band bitwise-and)
(define (H4 x)
  (bitwise-arithmetic-shift-right (bitwise-and x #xff) 4))
(define (H3 x)
  (bitwise-arithmetic-shift-right (bitwise-and x #xff) 5))
(define (B4 x)
  (bitwise-and x #xf))
(define (B5 x)
  (bitwise-and x #x1f))

(define (bv-u16 bv) (bytevector-u16-ref bv 0 (endianness big)))
(define (bv-s16 bv) (bytevector-s16-ref bv 0 (endianness big)))
(define (bv-u32 bv) (bytevector-u32-ref bv 0 (endianness big)))
(define (bv-s32 bv) (bytevector-s32-ref bv 0 (endianness big)))
(define (bv-u64 bv) (bytevector-u64-ref bv 0 (endianness big)))
(define (bv-s64 bv) (bytevector-s64-ref bv 0 (endianness big)))

(define (make-msgpack-decoder/raw callback) ;; => (^[buf])
  ;; Emits additional objects:
  ;;   (array-start . SIZE)
  ;;   (map-start . SIZE)
  ;;   NB: These *-start are recursive.
  ;; callback = (^[obj])
  (define cur-buf)
  (define cur-buf-off)
  (define in-obj? #f)
  (define state) ;; = raw-head | raw | int | float?
  (define wait 0)
  (define (procdata bv off len)
    (define (next)
      (when (< 1 (- len off))
        (procdata bv (+ off 1) len)))
    (define (imm obj)
      (callback obj)
      (next))
    (define-syntax define-state
      (syntax-rules ()
        ((_ name)
         (define (name width)
           (set! cur-buf (make-bytevector width))
           (set! cur-buf-off 0)
           (set! state 'name)
           (set! wait width)
           (set! in-obj? #t)
           (next)))))
    (define-state int)
    (define-state uint)
    (define-state raw)
    (define-state raw-head)
    ;; TODO::
    (define short 0)
    (define double 0)
    (cond
      (in-obj?
        (let ((len (- (bytevector-length bv) off)))
          (cond 
            ((< len wait)
             (set! wait (- wait len))
             (bytevector-copy! buf off cur-buf cur-buf-off len))
            (else
              (bytevector-copy! buf off cur-buf cur-buf-off wait)
              ;; TODO::
              ))))
      (else
        (let ((header (bytevector-u8-ref bv off)))
          (cond
            ((<= 0 header 127)
             (imm header))
            ((<= #xe0 header #xff)
             (imm (- (+ 1 (bnot header)))))
            ((= (H4 header) #xa)
             (imm (cons 'array-start (B4 header))))
            ((= (H4 header) #x8)
             (imm (cons 'map-start (B4 header))))
            ((= (H3 header) #x5)
             (raw (B5 header)))
            (else
              (case header
                ((#xc3) (imm #t))
                ((#xc2) (imm #f))
                ((#xc0) (imm '()))
                ((#xcc) (uint 1))
                ((#xcd) (uint 2))
                ((#xce) (uint 4))
                ((#xcf) (uint 8))
                ((#xd0) (int 1))
                ((#xd1) (int 2))
                ((#xd2) (int 4))
                ((#xd3) (int 8))
                ((#xca) (short))
                ((#xcb) (double))
                ((#xda) (raw-head 2))
                ((#xdb) (raw-head 4))
                (else
                  (assertion-violation 'msgpack
                                       "unknown object header"
                                       header))))))))
    )
  (lambda (buf)
    (let ((data (car buf))
          (size (cdr buf)))
      (unless (= size 0)
        (procdata data 0 size)))))



)
