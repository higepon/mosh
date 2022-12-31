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

(define (put-s64 port n)
  (let1 bv (make-bytevector 8)
     (bytevector-s64-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (write-constant c port)
  (match c
    [(? fixnum? n)
      (put-u8 port TAG_FIXNUM)
      (put-s64 port n)]))

(let-values ([(p get) (open-bytevector-output-port)])
  (write-constant 3 p)
  (display (get)))

(test-results)
