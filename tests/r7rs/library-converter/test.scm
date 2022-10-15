(import (rnrs)
        (mosh test)
        (psyntax r7rs-library-converter))

;; export
(test-equal '(rename (put! set!)) (rewrite-export '(rename put! set!)))

(test-equal "/foo/bar/baz/" (path-dirname "/foo/bar/baz/hige.scm"))
(test-equal "" (path-dirname "hige.scm"))

;; new tests here

;(test-equal '((export make (rename (put! set!))) (export get set!))
(let-values (((lib-decl* export* import* included-file*)
    (rewrite-lib-decl* "r7rs/" '((export make (rename put! set!))
                                (include-library-declarations "default-declarations.scm")))))
              (test-equal '() lib-decl*)
              (test-equal '() import*)
              (test-equal '(make (rename (put! set!)) get set!) export*)
              (test-equal '("r7rs/default-declarations.scm") included-file*)
)

(let-values (((lib-decl* export* import* included-file*)
  (rewrite-lib-decl* "r7rs/" '((export make (rename put! set!))
                               (include "foo.scm" "bar.scm")
                               (include-library-declarations "default-declarations.scm")
                               (include-library-declarations "other-declarations.scm")))))
    (test-equal '((include "r7rs/foo.scm") (include "r7rs/bar.scm")) lib-decl*)
    (test-equal '() import*)
    (test-equal '(make (rename (put! set!)) get set! life name) export*)
    (test-equal '("r7rs/foo.scm" "r7rs/bar.scm" "r7rs/default-declarations.scm" "r7rs/other-declarations.scm") included-file*)
)


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

(test-equal '("yay")
        (rewrite-lib-decl* "r7rs/" '((cond-expand ((or r8rs r7rs) "yay") (else "else")))))

(test-equal '("yay")
    (rewrite-lib-decl* "r7rs/" '((cond-expand ((or r9rs r8rs r7rs) "yay") (else "else")))))

(test-equal '("else")
    (rewrite-lib-decl* "r7rs/" '((cond-expand ((or r9rs r8rs r5rs) "yay") (else "else")))))

(test-equal '("yay")
(rewrite-lib-decl* "r7rs/" '((cond-expand ((or r9rs (library (mosh)) r5rs) "yay") (else "else")))))

;; A Scheme program converter
(test-equal '[(import (scheme base)) 3]
  (rewrite-program "./src" '[(import (scheme base)) 3]))

(test-equal '[(import (scheme base) (mosh)) 3]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) 3]))

;; cond-expand in top level.
(test-equal '[(import (scheme base) (mosh)) 3]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond-expand (mosh 3) (else 4))]))

;; cond-expand in define.
(test-equal '[(import (scheme base) (mosh)) (define a 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (define a (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (define (a x) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (define (a x) (cond-expand (mosh 3) (else 4)) 5)]))
(test-equal '[(import (scheme base) (mosh)) (define (a . x) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (define (a . x) (cond-expand (mosh 3) (else 4)) 5)]))

;; cond-expand in quote.
(test-equal '[(import (scheme base) (mosh))  (define a (quote (cond-expand (mosh 3) (else 4))))]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (define a (quote (cond-expand (mosh 3) (else 4))))]))

;; cond-expand in lambda.
(test-equal '[(import (scheme base) (mosh)) (lambda (a) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (lambda (a) (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (lambda (a b) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (lambda (a b) (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (lambda (a . b) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (lambda (a . b) (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (lambda a 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (lambda a (cond-expand (mosh 3) (else 4)))]))

;; cond-expand in if
(test-equal '[(import (scheme base) (mosh)) (if 3 5 6)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (if (cond-expand (mosh 3) (else 4)) 5 6)]))
(test-equal '[(import (scheme base) (mosh)) (if 5 3 6)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (if 5 (cond-expand (mosh 3) (else 4)) 6)]))
(test-equal '[(import (scheme base) (mosh)) (if 6 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (if 6 5 (cond-expand (mosh 3) (else 4)))]))

;; cond-expand in set!
(test-equal '[(import (scheme base) (mosh)) (set! a 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (set! a (cond-expand (mosh 3) (else 4)))]))

;; cond-expand in cond
(test-equal '[(import (scheme base) (mosh)) (cond [3 5])]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond [(cond-expand (mosh 3) (else 4)) 5])]))
(test-equal '[(import (scheme base) (mosh)) (cond [5 3])]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond [5 (cond-expand (mosh 3) (else 4))])]))
(test-equal '[(import (scheme base) (mosh)) (cond [1 2] [3 5])]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond [1 2] [(cond-expand (mosh 3) (else 4)) 5])]))
(test-equal '[(import (scheme base) (mosh)) (cond [1 2 3] [else 3])]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond [1 2 3] [else (cond-expand (mosh 3) (else 4))])]))
(test-equal '[(import (scheme base) (mosh)) (cond [1 2] [else 3 5])]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (cond [1 2] [else (cond-expand (mosh 3) (else 4)) 5])]))

;; cond-expand in case
(test-equal '[(import (scheme base) (mosh)) (case 3 ((4) #t))]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (case (cond-expand (mosh 3) (else 4)) ((4) #t))]))
(test-equal '[(import (scheme base) (mosh)) (case a ((4) 3 #t))]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (case a ((4)  (cond-expand (mosh 3) (else 4)) #t))]))

;; cond-expand in and or when unless
(test-equal '[(import (scheme base) (mosh)) (and 4 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (and 4 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (or 4 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (or 4 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (when 4 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (when 4 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (unless 4 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh)) (unless 4 (cond-expand (mosh 3) (else 4)))]))

;; cond-expand in let
(test-equal '[(import (scheme base) (mosh)) (let ([a 1] [b 3]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let ([a 1] [b (cond-expand (mosh 3) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let ([a 3] [b 2]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let ([a (cond-expand (mosh 3) (else 4))] [b 2])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let ([a 1] [b 2]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let ([a 1] [b 2]) 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let ([a 1] [b 2])
                               5 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let ([a 1] [b 2]) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)) 5)]))

(test-equal '[(import (scheme base) (mosh)) (let* ([a 1] [b 3]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let* ([a 1] [b (cond-expand (mosh 3) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let* ([a 3] [b 2]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let* ([a (cond-expand (mosh 3) (else 4))] [b 2])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let* ([a 1] [b 2]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let* ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let* ([a 1] [b 2]) 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let* ([a 1] [b 2])
                               5 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let* ([a 1] [b 2]) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let* ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)) 5)]))

(test-equal '[(import (scheme base) (mosh)) (letrec ([a 1] [b 3]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec ([a 1] [b (cond-expand (mosh 3) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (letrec ([a 3] [b 2]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec ([a (cond-expand (mosh 3) (else 4))] [b 2])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (letrec ([a 1] [b 2]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (letrec ([a 1] [b 2]) 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec ([a 1] [b 2])
                               5 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (letrec ([a 1] [b 2]) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)) 5)]))

(test-equal '[(import (scheme base) (mosh)) (letrec* ([a 1] [b 3]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec* ([a 1] [b (cond-expand (mosh 3) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (letrec* ([a 3] [b 2]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec* ([a (cond-expand (mosh 3) (else 4))] [b 2])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (letrec* ([a 1] [b 2]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec* ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (letrec* ([a 1] [b 2]) 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec* ([a 1] [b 2])
                               5 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (letrec* ([a 1] [b 2]) 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (letrec* ([a 1] [b 2])
                               (cond-expand (mosh 3) (else 4)) 5)]))

;; let-values
(test-equal '[(import (scheme base) (mosh)) (let-values ([(a b c) (values 1 2 3)]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let-values ([(a b c) (cond-expand (mosh (values 1 2 3)) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let-values ([(a b c) (values 1 2 3)]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let-values ([(a b c) (values 1 2 3)])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let-values ([(a b c) (values 1 2 3)]) 2 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let-values ([(a b c) (values 1 2 3)])
                               2
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let-values ([(a b c) (values 1 2 3)]) 3 2)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let-values ([(a b c) (values 1 2 3)])
                               (cond-expand (mosh 3) (else 4))
                               2)]))

;; let*-values
(test-equal '[(import (scheme base) (mosh)) (let*-values ([(a b c) (values 1 2 3)]) 4)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let*-values ([(a b c) (cond-expand (mosh (values 1 2 3)) (else 4))])
                               4)]))
(test-equal '[(import (scheme base) (mosh)) (let*-values ([(a b c) (values 1 2 3)]) 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let*-values ([(a b c) (values 1 2 3)])
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let*-values ([(a b c) (values 1 2 3)]) 2 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let*-values ([(a b c) (values 1 2 3)])
                               2
                               (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (let*-values ([(a b c) (values 1 2 3)]) 3 2)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (let*-values ([(a b c) (values 1 2 3)])
                               (cond-expand (mosh 3) (else 4))
                               2)]))

;; cond-expand in begin
(test-equal '[(import (scheme base) (mosh)) (begin 3 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (begin (cond-expand (mosh 3) (else 4)) 5)]))
(test-equal '[(import (scheme base) (mosh)) (begin 5 3)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (begin 5 (cond-expand (mosh 3) (else 4)))]))
(test-equal '[(import (scheme base) (mosh)) (begin (define a 3) 5)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (begin (define a (cond-expand (mosh 3) (else 4))) 5)]))
(test-equal '[(import (scheme base) (mosh)) (begin (define a 3) 9 27)]
  (rewrite-program "./src" '[(import (scheme base)) (import (mosh))
                             (begin (define a (cond-expand (mosh 3) (else 4))) (cond-expand (mosh 9) (else 4)) (cond-expand (mosh 27) (else 4)))]))
(test-results)


