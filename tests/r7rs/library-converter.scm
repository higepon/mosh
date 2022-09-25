(import (rnrs)
        (match)
        (mosh pp)
        (mosh test))

(define (parse-define-library exp)
  (match exp
    [('define-library (name* ...)
                      ('export export* ...)
                      ('import import* ...)
       body* ...)
        (values name* export* import* body*)]
    [else (values #f #f)]))

(test-values (values '(my lib) '(make rows (rename put! set!)) '((scheme base)) '((begin 3)))
  (parse-define-library '(define-library (my lib)
                                         (export make rows (rename put! set!))
                                         (import (scheme base))
                           (begin 3))))

(test-values (values '(my lib) '(make rows (rename put! set!)) '((scheme base)) '())
  (parse-define-library '(define-library (my lib)
                                         (export make rows (rename put! set!))
                                         (import (scheme base)))))

  ;; empty body

(test-results)

(newline)
