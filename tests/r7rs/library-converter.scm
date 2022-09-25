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

;; empty body case.
(test-values (values '(my lib) '(make rows (rename put! set!)) '((scheme base)) '())
  (parse-define-library '(define-library (my lib)
                                         (export make rows (rename put! set!))
                                         (import (scheme base)))))

(define parse-library-body
  (case-lambda
   [(body*)
    (parse-library-body body* '())]
   [(body* include*)
     (match body*
       [(('include file) other* ...)
          (parse-library-body other* (cons file include*))]
       [else (values include*)])]))

(test-values (values '("my_lib.scm"))
  (parse-library-body '((include "my_lib.scm"))))

(test-values (values '("my_lib1.scm"))
  (parse-library-body '((include "my_lib1.scm") (include "my_lib2.scm"))))


(test-results)

(newline)
