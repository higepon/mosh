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
    (parse-library-body body* '() '())]
   [(body* include* begin*)
     (match body*
       [(('include file) other* ...)
          (parse-library-body other* (cons file include*) begin*)]
       [(('begin bbody* ...) other* ...)
          (parse-library-body other* include* (cons bbody* begin*))]
       [() (values (reverse include*) (reverse begin*))])]
       [else (syntax-violation 'parse-library-body "hoge")]))

(test-values (values '("my_lib.scm") '())
  (parse-library-body '((include "my_lib.scm"))))

(test-values (values '("my_lib1.scm" "my_lib2.scm") '())
  (parse-library-body '((include "my_lib1.scm") (include "my_lib2.scm"))))

(test-values (values '("my_lib1.scm" "my_lib2.scm") '(((a) 1 2)))
  (parse-library-body '((include "my_lib1.scm") (include "my_lib2.scm") (begin (a) 1 2))))

;; Combining
(let-values (((name export* import* body*) (parse-define-library
                                            '(define-library (my lib)
                                               (export make rows (rename put! set!))
                                               (import (scheme base))
                                               (begin 3)))))
    (test-equal '(my lib) name)
    (test-equal '(make rows (rename put! set!)) export*)
    (test-equal '((scheme base)) import*)
    (test-equal '((begin 3)) body*)
    (let-values (((include* begin*) (parse-library-body body*)))
      (test-equal '() include*)
      (test-equal '((3)) begin*)
))

;; todo multiple import/export
(test-results)

(newline)
