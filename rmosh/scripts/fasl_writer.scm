;; Write Rust VM Ops as binary.
(import (rnrs arithmetic fixnums))
(import (match))
(import (mosh control))
(import (mosh test))
(import (only (rnrs) bytevector-s64-native-set! open-bytevector-output-port put-u8 put-bytevector))
(import (scheme base))
(import (scheme case-lambda))
(import (scheme write))

(define TAG_FIXNUM 0)
(define TAG_TRUE   1)

(define (put-s64 port n)
  (let1 bv (make-bytevector 8)
     (bytevector-s64-native-set! bv 0 n)
     (put-bytevector port bv)))

(define write-constant
  (case-lambda
    [(c port)
      (match c
        [(? fixnum? n)
          (put-u8 port TAG_FIXNUM)
          (put-s64 port n)]
        [#t
          (put-u8 port TAG_TRUE)]
        [any (write 'hoge) (write any) (any)]
      )]
    [(c)
      (let-values ([(p get) (open-bytevector-output-port)])
        (write-constant c p)
        (get))]))

(display (write-constant 3))
(display (write-constant #t))

(test-equal #vu8(0 3 0 0 0 0 0 0 0) (write-constant 3))
(test-equal #vu8(1) (write-constant #t))

(test-results)
