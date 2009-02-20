(define (fib n)
  (if (<= n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))h

(unless (= 1346269 (fib 31))
  (error "fib failed"))
