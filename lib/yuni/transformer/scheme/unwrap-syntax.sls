(library (yuni transformer scheme unwrap-syntax)
         (export unwrap-syntax)
         (import (rnrs))
(define (unwrap-syntax x)
  (syntax-case x ()
    ((a . d)    ;; pair
     (cons (unwrap-syntax #'a)
           (unwrap-syntax #'d)))
    (()         ;; nil
     '())
    (#(obj ...) ;; vectors
     (list->vector (unwrap-syntax #'(obj ...))))
    (id?        ;; identifier
      (identifier? #'id?)
      #'id?)
    (obj        ;; else - unwrap it
      (syntax->datum #'obj))))
)
