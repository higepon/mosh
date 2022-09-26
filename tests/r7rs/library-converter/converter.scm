(import (rnrs)
        (match)
        (mosh file)
        (only (mosh pp) pp)
        (only (srfi :1) first second)
        (mosh test))

(define (parse-define-library exp)
  (match exp
    [('define-library (name* ...)
                      ('export export* ...)
                      ('import import* ...)
       body* ...)
        (values name* export* import* body*)]
    [else (values #f #f)]))

(define (rewrite-export exp)
   (match exp
     [(('rename from to) other ...)
        `((rename (,from ,to)) ,@(rewrite-export other))]
     [(one other ...)
        `(,one ,@(rewrite-export other))]
     [() '()]))

(test-equal '(make rows (rename put! set!)) (rewrite-export '(make rows (rename put! set!))))

(define (rewrite-define-library exp)
    (let-values (((name export* import* body*) (parse-define-library exp)))
        `(library ,name (export ,@(rewrite-export export*))
                        (import ,@import*)
            ,@body*)))

(define (main args)
    (let ([exp (first (file->sexp-list (second args)))])
        (pp (rewrite-define-library exp))))

(main (command-line))
