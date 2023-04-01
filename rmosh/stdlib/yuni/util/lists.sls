(library (yuni util lists)
         (export sexp-map)
         (import (rnrs))
;; sexp-map from Andre van Tonder's macro implementation
(define (sexp-map f s)
  (cond ((null? s) '())
        ((pair? s) (cons (sexp-map f (car s))
                         (sexp-map f (cdr s))))
        ((vector? s)
         (apply vector (sexp-map f (vector->list s))))
        (else (f s))))

)
