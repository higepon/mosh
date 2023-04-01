(library (yuni binary codec base64)
         (export 
           bytevector->base64 
           base64->bytevector)
         (import 
           (srfi :8)
           (rnrs))

(define encstr 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define (make-convtbl str)
  (list->vector (map char->integer (string->list str))))

(define convtbl (make-convtbl encstr))

(define (conv x) (vector-ref convtbl x))

(define pad (char->integer #\=))

(define decvec
  (let ((bv (make-bytevector 256 255)))
    (let loop ((idx 0))
      (if (= idx 64)
        bv
        (begin
          (bytevector-u8-set! bv (char->integer (string-ref encstr idx)) idx)
          (loop (+ idx 1)))))))

(define (dec x) ;; NB. eats eof-object
  (and (number? x)
       (not (= x pad))
       (bytevector-u8-ref decvec x)))

(define (base64->bytevector in)
  (define inp (open-bytevector-input-port in))
  (define (r) (let ((x (dec (get-u8 inp))))
                (cond
                  ((and x (= x 255)) (r))
                  (else x))))
  ;; IN
  ;;  543210  543210 543210  543210
  ;;
  ;;  543210+54 3210+5432 10+543210
  ;; OUT
  (receive (p proc) (open-bytevector-output-port)
    (define (loop)
      (let ((a (r)))
        (if a
          (let ((b (r)))
            (put-u8 p (+ (bitwise-arithmetic-shift-left a 2)
                         (bitwise-and #x3
                                      (bitwise-arithmetic-shift-right b 4))))
            (let ((c (r)))
              (if c
                (begin
                  (put-u8 p (+ (bitwise-arithmetic-shift-left
                                 (bitwise-and b #xf) 4)
                               (bitwise-arithmetic-shift-right
                                 (bitwise-and c #x3c) 2)))
                  (let ((d (r)))
                    (if d
                      (begin
                        (put-u8 p (+ (bitwise-arithmetic-shift-left
                                       (bitwise-and c 3) 6)
                                     (bitwise-and d #x3f)))
                        (loop))
                      (proc))))
                (proc))))
          (proc))))
    (loop)))

;; for benchmark...
(define (encout3 out out-start in0 in1 in2)
  ;; IN 
  ;;   76543210   76543210    76543210
  ;;
  ;;   765432 10+7654   3210+76 543210
  ;; OUT
  (define v0 (bitwise-arithmetic-shift-right in0 2))
  (define v1 (+ (bitwise-arithmetic-shift-left
                  (bitwise-and in0 3) 4)
                (bitwise-arithmetic-shift-right
                  (bitwise-and in1 #xf0) 4)))
  (define v2 (+ (bitwise-arithmetic-shift-left
                  (bitwise-and in1 #xf) 2)
                (bitwise-arithmetic-shift-right
                  (bitwise-and in2 #xc0) 6)))
  (define v3 (bitwise-and in2 #x3f))

  (bytevector-u8-set! out out-start (conv v0))
  (bytevector-u8-set! out (+ out-start 1) (conv v1))
  (bytevector-u8-set! out (+ out-start 2) (conv v2))
  (bytevector-u8-set! out (+ out-start 3) (conv v3)))

(define (encout2 out out-start in0 in1)
  (define v0 (bitwise-arithmetic-shift-right in0 2))
  (define v1 (+ (bitwise-arithmetic-shift-left
                  (bitwise-and in0 3) 4)
                (bitwise-arithmetic-shift-right
                  (bitwise-and in1 #xf0) 4)))
  (define v2 (bitwise-arithmetic-shift-left
               (bitwise-and in1 #xf) 2))
  (bytevector-u8-set! out out-start (conv v0))
  (bytevector-u8-set! out (+ out-start 1) (conv v1))
  (bytevector-u8-set! out (+ out-start 2) (conv v2))
  (bytevector-u8-set! out (+ out-start 3) pad))

(define (encout1 out out-start in0)
  (define v0 (bitwise-arithmetic-shift-right in0 2))
  (define v1 (bitwise-arithmetic-shift-left
               (bitwise-and in0 3) 4))
  (bytevector-u8-set! out out-start (conv v0))
  (bytevector-u8-set! out (+ out-start 1) (conv v1))
  (bytevector-u8-set! out (+ out-start 2) pad)
  (bytevector-u8-set! out (+ out-start 3) pad))

(define (enc3 in in-start in-end out out-start)
  (define in0 (bytevector-u8-ref in in-start))
  (define in1 (let ((o (+ 1 in-start)))
                (if (<= in-end o) #f (bytevector-u8-ref in o))))
  (define in2 (let ((o (+ 2 in-start)))
                (if (<= in-end o) #f (bytevector-u8-ref in o))))
  (if in1
    (if in2
      (encout3 out out-start in0 in1 in2)
      (encout2 out out-start in0 in1))
    (encout1 out out-start in0)))

(define (enc-out in in-off in-size out out-off out-size)
  (enc3 in in-off in-size out out-off)
  (when (< 0 (- in-size (+ in-off 3)))
    (enc-out in (+ 3 in-off) in-size out (+ 4 out-off) out-size)))

(define (bytevector->base64 in)
  (define in-size (bytevector-length in))
  (define out-size (* 4 (ceiling (/ in-size 3))))
  (define bv (make-bytevector out-size))
  (enc-out in 0 in-size bv 0 out-size)
  bv)
)
