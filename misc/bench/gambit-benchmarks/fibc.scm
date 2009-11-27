;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (zero? y)
    (k x)
    (addc (1+ x) (1- y) k)))

(define (fibc x c)
  (if (zero? x)
    (c 0)
    (if (zero? (1- x))
      (c 1)
      (addc (call-with-current-continuation (lambda (c) (fibc (1- x) c)))
            (call-with-current-continuation (lambda (c) (fibc (1- (1- x)) c)))
            c))))

(define (main)
  (run-benchmark
    "fibc"
    fibc-iters
    (lambda (result) (equal? result 2584))
    (lambda (x c) (lambda () (fibc x c)))
    18
    (lambda (n) n)))
