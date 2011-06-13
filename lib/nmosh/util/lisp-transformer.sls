(library (nmosh util lisp-transformer)
         (export lisp-transformer)
         (import (rnrs))

;; R6RS Standard Libraries:12.6 Syntax-object and datum conversions
(define lisp-transformer
  (lambda (p)
    (lambda (x)
      (syntax-case x ()
        [(kwd . rest)
         (datum->syntax #'kwd
           (p (syntax->datum x)))]))))

)
