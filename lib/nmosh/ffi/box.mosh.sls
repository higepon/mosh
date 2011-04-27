(library (nmosh ffi box)
         (export 
           make-ptr-box
           ptr-box-ref
           make-int-box
           int-box-ref
           int-box-set!
           make-ptr-array
           ptr-array-ref)
         (import (rnrs)
                 (mosh ffi))


(define (make-box-64) (make-bytevector 8))
(define (box-64-ref x)
  (bytevector-u64-native-ref x 0))
(define (box-64-ref-signed x)
  (bytevector-s64-native-ref x 0))
(define (box-64-set!-signed x v)
  (bytevector-s64-native-set! x 0 v))

(define (make-array-64 n) (make-bytevector (* 8 n)))
(define (array-64-ref x n)
  (bytevector-u64-native-ref x (* 8 n)))

(define (make-array-32 n) (make-bytevector (* 8 n)))
(define (array-32-ref x n)
  (bytevector-u32-native-ref x (* 8 n)))


(define (make-box-32) (make-bytevector 4))
(define (box-32-ref x)
  (bytevector-u32-native-ref x 0))
(define (box-32-ref-signed x)
  (bytevector-s32-native-ref x 0))
(define (box-32-set!-signed x v)
  (bytevector-s32-native-set! x 0 v))

;; We need run-time dispatch here. Because nmosh64 may execute nmosh32 cached
;; code..
(define-syntax sel32/64
  (syntax-rules ()
    ((_ q p32 p64)
     (case q
       ((4) p32)
       ((8) p64)))))
(define make-ptr-box (sel32/64 size-of-void* make-box-32 make-box-64))
(define ptr-box-ref
  (let ((ref (sel32/64 size-of-void* box-32-ref box-64-ref)))
    (lambda (x) (integer->pointer (ref x)))))

(define make-int-box (sel32/64 size-of-int make-box-32 make-box-64))
(define int-box-ref (sel32/64 size-of-int box-32-ref-signed box-64-ref-signed))
(define int-box-set! (sel32/64 size-of-int box-32-set!-signed box-64-set!-signed))

(define make-ptr-array (sel32/64 size-of-void* make-array-32 make-array-64))
(define ptr-array-ref-int (sel32/64 size-of-void* array-32-ref array-64-ref))
(define (ptr-array-ref x n)
  (integer->pointer (ptr-array-ref-int x n)))

)
