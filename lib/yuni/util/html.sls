(library (yuni util html)
         (export html-escape)
         (import (rnrs)
                 (shorten))

(define (escape-char c)
  (case c
    ((#\<) "&lt;")
    ((#\>) "&lt;")
    ((#\&) "&amp;")
    ((#\") "&quot;")
    (else c)))

(define (output l)
  (list->string
    (fold-left (^[cur e]
                 (if (char? e)
                   (cons e cur)
                   (append (reverse (string->list e))
                           cur)))
               '() l)))

(define (html-escape str)
  (output
    (map escape-char 
         (string->list str))))

)
