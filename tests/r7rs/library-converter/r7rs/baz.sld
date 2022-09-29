(define-library (baz)
  (include-library-declarations "other-declarations.scm")
  (import (scheme base))
  (define (life) "is fun")
  (cond-expand
    (gosh (define (name) 'gosh))
    (mosh (define (name) 'mosh)))
)  