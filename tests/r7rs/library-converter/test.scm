(import (rnrs)
        (mosh test)
        (psyntax r7rs-library-converter))

;; export
(test-equal '(make rows (rename (put! set!))) (rewrite-export '(make rows (rename put! set!))))

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


(test-equal '((define foo #t) (define bar #f))
            (rewrite-lib-decl* "r7rs/" '((cond-expand (r7rs (define foo #t) (define bar #f))))))

(test-equal '((define foo #t) (define bar #f))
            (rewrite-lib-decl* "r7rs/" '((cond-expand (r8rs #t) (r7rs (define foo #t) (define bar #f))))))

(test-equal '()
            (rewrite-lib-decl* "r7rs/" '((cond-expand (r8rs #t) (r7rs)))))

(test-equal '("else")
            (rewrite-lib-decl* "r7rs/" '((cond-expand (r9rs 9) (r8rs) (else "else")))))

(test-equal '(#t)
            (rewrite-lib-decl* "r7rs/" '((cond-expand ((not r8rs) #t) (r7rs (define foo #t) (define bar #f))))))

(test-equal '("else")
            (rewrite-lib-decl* "r7rs/" '((cond-expand ((and r8rs r7rs) #t) (else "else"))))) 

(test-equal '("yay")
            (rewrite-lib-decl* "r7rs/" '((cond-expand ((and r6rs r7rs) "yay") (else "else"))))) 

(test-results)


