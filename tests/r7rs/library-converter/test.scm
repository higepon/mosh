(import (rnrs)
        (mosh test)
        (psyntax r7rs-library-converter))

;; export
(test-equal '(make rows (rename (put! set!))) (rewrite-export '(make rows (rename put! set!))))

;; body with include.
(test-equal '((include "/foo/bar.scm")) (rewrite-body "/foo/" '((include "bar.scm"))))

;; body with two include.
(test-equal '((include "/foo/bar.scm") (include "/foo/baz.scm")) (rewrite-body "/foo/" '((include "bar.scm") (include "baz.scm"))))

(test-equal '((include "/foo/bar.scm") (include "/foo/baz.scm")) (rewrite-body "/foo/" '((include "bar.scm" "baz.scm"))))

;; body with include-library-declarations.
(test-equal '((export get set!)) (rewrite-body "r7rs/" '((include-library-declarations "default-declarations.scm"))))

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

(test-equal "/foo/bar/baz/" (path-dirname "/foo/bar/baz/hige.scm"))
(test-equal "" (path-dirname "hige.scm"))

;; new tests here

(test-equal '((export make (rename (put! set!))) (export get set!))
            (rewrite-lib-decl* "r7rs/" '((export make (rename put! set!))
                                         (include-library-declarations "default-declarations.scm"))))
(test-equal '((export make (rename (put! set!)))
              (include "r7rs/foo.scm")
              (include "r7rs/bar.scm")
              (export get set!)
              (export life))
            (rewrite-lib-decl* "r7rs/" '((export make (rename put! set!))
                                         (include "foo.scm" "bar.scm")
                                         (include-library-declarations "default-declarations.scm")
                                         (include-library-declarations "other-declarations.scm"))))


(test-results)