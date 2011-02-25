;; MIT/GNU scheme compatible er-macro-transformer in R6RS syncase
(library (yuni transformer scheme explicit-renaming)
         (export er-macro-transformer syntax)
         (import (rnrs)
                 (yuni transformer scheme unwrap-syntax))
(define-syntax er-macro-transformer
  (lambda (x)
    (syntax-case x ()
      ((k transformer) ;; transformer should be 3-ary: (exp rename compare)
       #'(lambda (input)
           (transformer
               (unwrap-syntax input)
               (lambda (sym) (datum->syntax #'k sym))
               free-identifier=?))))))
)
