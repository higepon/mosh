(import (rnrs)
        (mosh)
        (system))

(define (fib n) (if (< n 2) 1 (+ (fib (- n 2)) (fib (- n 1)))))

 (disasm fib)
