#!r6rs
(library (r7b-impl write)
         (export display write write-simple write-shared )
         (import  (rename (rnrs) (write write-simple))
                  (mosh))
(define write write/ss)
(define write-shared write/ss)
)
