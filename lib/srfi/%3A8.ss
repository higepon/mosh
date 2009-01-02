; SRFI 8: RECEIVE: Binding to multiple values
; http://srfi.schemers.org/srfi-8/srfi-8.html

(library (srfi :8)
         (export receive)
         (import (only (rnrs) define-syntax syntax-rules ... lambda call-with-values))

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...))))))


