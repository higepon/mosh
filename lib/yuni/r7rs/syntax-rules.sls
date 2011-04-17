(library (yuni r7rs syntax-rules)
         (export syntax-rules 
                 (rename (define-syntax/er define-syntax)))
         (import 
           (except (rnrs) error else syntax-rules => ... _)
           (for (yuni scheme explicit-renaming) run expand)
           (for (yuni scheme define-syntax-er) (meta -1) run expand)
           (for (yuni r7rs syntax-rules-transformer) expand))

(define (error msg . arg)
  (assertion-violation 'syntax-rules msg arg))

(define-syntax syntax-rules
  (er-macro-transformer+ syntax-rules-transformer))
)
