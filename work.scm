(import (except (rnrs) bytevector-uint-ref bytevector-sint-ref bytevector-uint-set! bytevector-sint-set!
                bytevector->uint-list bytevector->sint-list uint-list->bytevector sint-list->bytevector

                )
        (mosh string))

(define-syntax unspecified
  (syntax-rules ()
    [(_)
     (if #f #f)]))

(define-syntax div256
  (syntax-rules ()
    ((_ x) (bitwise-arithmetic-shift x -8))))

(define-syntax mod256
  (syntax-rules ()
    ((_ x) (bitwise-and x 255))))

;; (define-macro (unspecified)
;;   '(if #f #f))

;; (define-macro (div256 x)
;;   `(bitwise-arithmetic-shift ,x -8))

;; (define-macro (mod256 x)
;;   `(bitwise-and ,x 255))


(define (bytevector-uint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (let ((end (+ index size)))
           (let loop ((i index) (acc 0))
             (if (>= i end)
                 acc
                 (loop (+ i 1) (+ (* 256 acc) (bytevector-u8-ref bv i)))))))
        ((eq? endien 'little)
         (let loop ((i (+ index size -1)) (acc 0))
           (if (< i index)
               acc
               (loop (- i 1) (+ (* 256 acc) (bytevector-u8-ref bv i))))))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~a, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-sint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (if (> (bytevector-u8-ref bv index) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        ((eq? endien 'little)
         (if (> (bytevector-u8-ref bv (+ index size -1)) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~a, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-uint-set! bv index val endien size)
  (cond ((= val 0)
         (let ((end (+ index size)))
           (let loop ((i index))
             (cond ((>= i end) (unspecified))
                   (else
                    (bytevector-u8-set! bv i 0)
                    (loop (+ i 1)))))))
        ((< 0 val (expt 256 size))
         (cond ((eq? endien 'big)
                (let ((start (- (+ index size) 1)))
                  (let loop ((i start) (acc val))
                    (cond ((< i index) (unspecified))
                          (else
                           (bytevector-u8-set! bv i (mod256 acc))
                           (loop (- i 1) (div256 acc)))))))
               ((eq? endien 'little)
                (let ((end (+ index size)))
                  (let loop ((i index) (acc val))
                    (cond ((>= i end) (unspecified))
                          (else
                           (bytevector-u8-set! bv i (mod256 acc))
                           (loop (+ i 1) (div256 acc)))))))))
        (else
         (assertion-violation 'bytevector-uint-set!
                              (format "value out of range, ~s as argument 3" val)
                              (list bv index val endien size))))
  (unspecified))

(define (bytevector-sint-set! bv index val endien size)
  (let* ((p-bound (expt 2 (- (* size 8) 1))) 
         (n-bound (- (+ p-bound 1))))
    (if (< n-bound val p-bound)
        (if (> val 0)
            (bytevector-uint-set! bv index val endien size)
            (bytevector-uint-set! bv index (+ val (expt 256 size)) endien size))
        (assertion-violation 'bytevector-sint-set!
                             (format "value out of range, ~s as argument 3" val)
                             (list bv index val endien size))))
  (unspecified))

(define bytevector->uint-list
  (lambda (bv endien size)
    (let loop ((i (- (bytevector-length bv) size)) (acc '()))
      (if (> i -1)
          (loop (- i size) (cons (bytevector-uint-ref bv i endien size) acc))
          (if (= i (- size))
              acc
              (assertion-violation 'bytevector->uint-list
                                   (format "expected appropriate element size as argument 3, but got ~a" size)
                                   (list bv endien size)))))))

(define (bytevector->sint-list bv endien size)
  (let loop ((i (- (bytevector-length bv) size)) (acc '()))
    (if (> i -1)
        (loop (- i size) (cons (bytevector-sint-ref bv i endien size) acc))
        (if (= i (- size))
            acc
            (assertion-violation 'bytevector->sint-list
                                 (format "expected appropriate element size as argument 3, but got ~a" size)
                                 (list bv endien size))))))

(define (uint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-uint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))

(define (sint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-sint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))


(define-syntax test
  (syntax-rules ()
    [(_ t expected)
     (if (equal? t expected)
         (display "ok\n")
         (format #t "~a : ~a" (quote (syntax->datum t)) (quote (syntax->datum expected))))]))

(test (bytevector-uint-ref
       (u8-list->bytevector '(17))
       0 'little 1)
      17)
(test (bytevector-uint-ref 
       (u8-list->bytevector '(17))
       0 'little 1)
      17)
(test (bytevector-uint-ref 
       (u8-list->bytevector '(17))
       0 'big 1)
      17)
(test (bytevector-uint-ref 
       (u8-list->bytevector '(17 54))
       0 'little 2)
      (+ 17 (* 54 256)))
(test (bytevector-uint-ref 
       (u8-list->bytevector (reverse '(17 54)))
       0 'big 2)
      (+ 17 (* 54 256)))
(test (bytevector-uint-ref 
       (u8-list->bytevector '(17 54 98))
       0 'little 3)
      (+ 17 (* 54 256) (* 98 256 256)))
(test (bytevector-uint-ref 
       (u8-list->bytevector (reverse '(17 54 98)))
       0 'big 3)
      (+ 17 (* 54 256) (* 98 256 256)))
(test (bytevector-uint-ref 
       (u8-list->bytevector '(17 54 98 120))
       0 'little 4)
      (+ 17 (* 54 256) (* 98 256 256) (* 120 256 256 256)))

(test (bytevector-uint-ref 
       (u8-list->bytevector
        '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
               #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12))
       0 'little 18)
      #x123897348738947983174893204982390489)
(test (bytevector-uint-ref 
       (u8-list->bytevector
        (reverse
         '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
                #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12)))
       0 'big 18)
      #x123897348738947983174893204982390489)
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->uint-list b 'little 2))
      '(513 65283 513 513))
(test (bytevector->u8-list
       (uint-list->bytevector '(513 65283 513 513) 'little 2))
      '(1 2 3 255 1 2 1 2))
(test (bytevector->u8-list
       (uint-list->bytevector '(513 65283 513 513) 'big 2))
      '(2 1 255 3 2 1 2 1))
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->sint-list b 'little 2))
      '(513 -253 513 513))
(test (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
        (bytevector->sint-list b 'big 2))
      '(513 -253 513 513))
(test (bytevector->u8-list
       (sint-list->bytevector '(513 -253 513 513) 'little 2))
      '(1 2 3 255 1 2 1 2))
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->sint-list b 'little 2))
      '(513 -253 513 513))
(test (let ((b (make-bytevector 16 -127)))
        (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'little 16)
        (list 
         (bytevector-uint-ref b 0 'little 16)
         (bytevector-sint-ref b 0 'little 16)
         (bytevector->u8-list b)))
      '(#xfffffffffffffffffffffffffffffffd
        -3
        (253 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 255)))
(test (let ((b (make-bytevector 16 -127)))
        (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'big 16)
        (list 
         (bytevector-uint-ref b 0 'big 16)
         (bytevector-sint-ref b 0 'big 16)
         (bytevector->u8-list b)))
      '(#xfffffffffffffffffffffffffffffffd
        -3
        (255 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 253)))
(test (bytevector->u8-list '#vu8(1 2 3 4))
      '(1 2 3 4))
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -1 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFFFFFF)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -256 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFFFF00)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFFFFFFFFFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 4)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFFFFFFFF00000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 7)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFF00000000000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'little 8)
        (bytevector-sint-ref b 0 'little 8))
      (- 1 (expt 2 63)))
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'little 4)
        (bytevector-sint-ref b 0 'little 4))
      #x7FFFFFFF)
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31)) 'little 4)
        (bytevector-sint-ref b 0 'little 4))
      #x-80000000)
(test (let ((b (make-bytevector 5 38)))
        (bytevector-sint-set! b 0 (- (expt 2 32)) 'little 5)
        (bytevector-sint-ref b 0 'little 5))
      #x-100000000)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -1 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFFFFFF)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -256 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFFFF00)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFFFFFFFFFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 4)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFFFFFFFF00000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 7)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFF00000000000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'big 8)
        (bytevector-sint-ref b 0 'big 8))
      (- 1 (expt 2 63)))
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'big 4)
        (bytevector-sint-ref b 0 'big 4))
      #x7FFFFFFF)
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31)) 'big 4)
        (bytevector-sint-ref b 0 'big 4))
      #x-80000000)
(test (let ((b (make-bytevector 5 38)))
        (bytevector-sint-set! b 0 (- (expt 2 32)) 'big 5)
        (bytevector-sint-ref b 0 'big 5))
      #x-100000000)
(test (bytevector-u16-ref '#vu8(255 253) 0 'little)
      65023)
(test (bytevector-u16-ref '#vu8(255 253) 0 'big)
      65533)
(test (bytevector-s16-ref '#vu8(255 253) 0 'little)
      -513)
(test (bytevector-s16-ref '#vu8(255 253) 0 'big)
      -3)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-native-set! v 0 12345)
        (bytevector-u16-native-ref v 0))
      12345)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-set! v 0 12345 'little)
        (bytevector-u16-ref v 0 'little))
      12345)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-set! v 0 12345 'big)
        (bytevector-u16-ref v 0 'big))
      12345)

(let ([b (make-bytevector 16 -127)])
  (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                        (endianness little) 16)
  
  (test (bytevector-uint-ref b 0 (endianness little) 16)
        #xfffffffffffffffffffffffffffffffd)

  (test (bytevector-sint-ref b 0 (endianness little) 16)
        -3)

  (test (bytevector->u8-list b)
        '(253 255 255 255 255 255 255 255
              255 255 255 255 255 255 255 255))

  (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                        (endianness big) 16)
  (test (bytevector-uint-ref b 0 (endianness big) 16)
        #xfffffffffffffffffffffffffffffffd)

  (test (bytevector-sint-ref b 0 (endianness big) 16) -3)

  (test (bytevector->u8-list b) 
        '(255 255 255 255 255 255 255 255
              255 255 255 255 255 255 255 253))

  (test
   (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
     (bytevector->sint-list b (endianness little) 2)) 
   '(513 -253 513 513))

  (test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
          (bytevector->uint-list b (endianness little) 2)) 
        '(513 65283 513 513)))

(let ([b (u8-list->bytevector
          '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))])
  
  (test (bytevector-u16-ref b 14 (endianness little)) 65023)
  (test (bytevector-s16-ref b 14 (endianness little)) -513)
  (test (bytevector-u16-ref b 14 (endianness big)) 65533)
  (test (bytevector-s16-ref b 14 (endianness big)) -3)

  (test (bytevector-u16-ref b 0 (endianness little)) 12345)
  
  (bytevector-u16-native-set! b 0 12345)
  (test (bytevector-u16-native-ref b 0) 12345)

  (bytevector-u16-ref b 0 (endianness little)))

(let ([b (u8-list->bytevector
          '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))])

  (test (bytevector-u32-ref b 12 (endianness little)) 4261412863)
  (test (bytevector-s32-ref b 12 (endianness little)) -33554433)
  (test (bytevector-u32-ref b 12 (endianness big)) 4294967293)
  (test (bytevector-s32-ref b 12 (endianness big)) -3))

(let ([b (u8-list->bytevector
          '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))])
  (test (bytevector-u64-ref b 8 (endianness little)) 18302628885633695743)
  (test (bytevector-s64-ref b 8 (endianness little)) -144115188075855873)
  (test (bytevector-u64-ref b 8 (endianness big)) 18446744073709551613)
  (test (bytevector-s64-ref b 8 (endianness big)) -3))
