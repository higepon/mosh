;; Write Rust VM Ops as binary.
(import (rnrs arithmetic fixnums))
(import (match))
(import (mosh control))
(import (mosh test))
(import (only (rnrs) bytevector-s64-native-set! bytevector-u32-native-set! bytevector-u16-native-set! open-bytevector-output-port put-u8 put-bytevector))
(import (scheme base))
(import (scheme case-lambda))
(import (scheme write))

(define TAG_FIXNUM 0)
(define TAG_TRUE   1)
(define TAG_FALSE  2)
(define TAG_NIL    3)
(define TAG_CHAR   4)
(define TAG_SYMBOL 5)
(define TAG_STRING 6)
(define TAG_PAIR   7)

(define (put-s64 port n)
  (let1 bv (make-bytevector 8)
     (bytevector-s64-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u32 port n)
  (let1 bv (make-bytevector 4)
     (bytevector-u32-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u16 port n)
  (let1 bv (make-bytevector 2)
     (bytevector-u16-native-set! bv 0 n)
     (put-bytevector port bv)))

(define write-constant
  (case-lambda
    [(port c)
      (match c
        [(? char? c)
          (put-u8 port TAG_CHAR)
          (put-u32 port (char->integer c))]      
        [(? fixnum? n)
          (put-u8 port TAG_FIXNUM)
          (put-s64 port n)]
        [(? symbol? s)
          (put-u8 port TAG_SYMBOL)
          (let1 str (symbol->string s)
            (put-u16 port (string-length str))
            (for-each
              (lambda (c)
                (put-u32 port (char->integer c)))
              (string->list str)))]
        [(? string? str)
          (put-u8 port TAG_STRING)
          (put-u16 port (string-length str))
          (for-each
            (lambda (c)
              (put-u32 port (char->integer c)))
            (string->list str))] 
        [(first . second)
          (put-u8 port TAG_PAIR) 
          (write-constant port first)       
          (write-constant port second)]
        [#t
          (put-u8 port TAG_TRUE)]
        [#f
          (put-u8 port TAG_FALSE)]
        [()
          (put-u8 port TAG_NIL)]          
      )]
    [(c)
      (let-values ([(p get) (open-bytevector-output-port)])
        (write-constant p c)
        (get))]))

(display (write-constant 3))
(display (write-constant #t))
(display (write-constant #f))
(display (write-constant '()))
(display (write-constant #\a))
(display (write-constant 'hello))
(display (write-constant "abc"))
(display (write-constant '(a)))

(test-equal #vu8(0 3 0 0 0 0 0 0 0) (write-constant 3))
(test-equal #vu8(1) (write-constant #t))
(test-equal #vu8(2) (write-constant #f))
(test-equal #vu8(3) (write-constant '()))
(test-equal #vu8(4 97 0 0 0) (write-constant #\a))
(test-equal #vu8(5 5 0 104 0 0 0 101 0 0 0 108 0 0 0 108 0 0 0 111 0 0 0) (write-constant 'hello))
(test-equal #vu8(6 3 0 97 0 0 0 98 0 0 0 99 0 0 0) (write-constant "abc"))
(test-equal #vu8(7 5 1 0 97 0 0 0 3) (write-constant '(a)))
(test-results)
