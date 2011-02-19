(library (nmosh global-flags)
         (export get-global-flag)
         (import (rnrs) (primitives symbol-value))

(define (get-global-flag sym)
  (guard
    (bogus (#t #f))
    (symbol-value sym)))
)
