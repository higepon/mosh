<<<<<<< .mine
(import (rnrs)
        (mosh))
(define (fib n) (if (< n 2) 1 (+ (fib (- n 2)) (fib (- n 1)))))
(time (fib 39))
=======
(let* ([handle (%ffi-open "./libffitest.so.1.0")]
            [sub (%ffi-lookup handle 'sub)])
       (display (%ffi-call->int sub 5 -11)))
>>>>>>> .r1999
