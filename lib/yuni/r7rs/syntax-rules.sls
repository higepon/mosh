(library (yuni r7rs syntax-rules)
         (export syntax-rules)
         (import 
           (for (except (rnrs) else syntax-rules => ... _) run expand (meta -1))
           (for (yuni scheme explicit-renaming) run expand)
           (for (yuni r7rs syntax-rules-transformer) expand))

(define-syntax syntax-rules
  (er-macro-transformer+ syntax-rules-transformer))
)
