(import (rnrs)
        (match)
        (mosh pp)
        (mosh test))

(define (parse-define-library exp)
  (match exp
    [('define-library (name* ...)
                      ('export export* ...)
                      ('import import* ...))
        (values #t name* export* import*)]
    [else (values #f #f)]))

(test-values (values #t '(my lib)  '(make rows (rename put! set!)) '((scheme base)))
  (parse-define-library '(define-library (my lib) (export make rows (rename put! set!)) (import (scheme base)))))

(test-results)

(newline)
