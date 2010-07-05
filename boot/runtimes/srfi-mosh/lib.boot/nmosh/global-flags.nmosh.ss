(library (nmosh global-flags)
         (export get-global-flag)
         (import (rnrs) (mosh))

(define (get-global-flag sym)
  (guard
    (bogus (#t #f))
    (symbol-value sym)))
)
