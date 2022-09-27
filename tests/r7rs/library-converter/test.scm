(import (rnrs)
        (mosh test)
        (mosh internal library-converter))

;; export
(test-equal '(make rows (rename (put! set!))) (rewrite-export '(make rows (rename put! set!))))

;; body with include.
(test-equal '((include "/foo/bar.scm")) (rewrite-body "/foo" '((include "bar.scm"))))

;; body with two include.
(test-equal '((include "/foo/bar.scm") (include "/foo/baz.scm")) (rewrite-body "/foo" '((include "bar.scm") (include "baz.scm"))))

(test-equal '((include "/foo/bar.scm") (include "/foo/baz.scm")) (rewrite-body "/foo" '((include "bar.scm" "baz.scm"))))

;; define-library
(test-values (values '(my lib) '(make rows (rename put! set!)) '((scheme base)) '((begin 3)))
    (parse-define-library '(define-library (my lib)
                                           (export make rows (rename put! set!))
                                           (import (scheme base))
                             (begin 3))))

;; define-library
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

(test-results)