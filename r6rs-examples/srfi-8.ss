(library (srfi-8)
  (export receive)
  (import (rnrs))

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))
)
