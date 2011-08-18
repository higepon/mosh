(library (yuni compat gensym)
         (export gensym ungensym)
         (import (rnrs))
(define counter 0)

(define (gensym . sym?)
  (set! counter (+ 1 counter))
  (if (pair? sym?)
    (string->symbol (string-append (symbol->string (car sym?)) "." (number->string counter)))
    (gensym 'GENSYM)))

(define (ungensym sym) sym)

(display "WARNING: gensym: cache-unsafe version!!\n" (current-error-port))
)
