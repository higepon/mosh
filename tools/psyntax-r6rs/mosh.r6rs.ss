(define (void) (if #f #f))
(define (eval-core x) (eval x '()))

(define (dynamic-wind before thunk after)
  (before)
  (thunk)
  (after))

(load "./psyntax.pp")

