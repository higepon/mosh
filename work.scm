(import (rnrs)
        (mosh)
        (mosh ffi)
        (mosh c-type)
        (mosh test))



(define libffitest (open-shared-library "./libffitest.so.1.0"))

(define real-size-of-st1 (c-function libffitest int sizeOfStruct1))
(define fill-st1! (c-function libffitest void fillStruct1 char*))
(define real-st1-a (c-function libffitest int aOfStruct1 char*))
(define real-st1-b (c-function libffitest int aOfStruct1 char*))
(define real-st1-c (c-function libffitest int aOfStruct1 char*))

(define-c-struct st1
  (struct
   (int a)
   (int b)
   (int c)))

(define-c-struct-accessors st1
  (int a) 0)

(test-begin "simple c-struct")

(test-assert size-of-st1)
(test-eqv (real-size-of-st1) (size-of-st1))
(test-assert make-st1)
(let ([st1 (make-st1)])
  (test-true (bytevector? st1))
  (fill-st1! st1)
  (display (st1-a st1))
  (display (real-st1-a st1)))

(let ()
(c-type-test)


)

(display align-of-int)
(test-end)
