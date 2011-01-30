(import (rnrs)
        (mosh pp)
        (match)
        (yuni util files)
        (yuni core)
        (shorten))

(define input-file "boot/runtimes/psyntax-mosh/psyntax-buildscript-mosh.ss")

(define src (file->sexp-list input-file))

(define psyntax-system-macros)
(define library-legend)
(define identifier->library-map)

(define (scan sym)
  (define (itr cur)
    (if (pair? cur)
      (let ((a (car cur))
            (next (cdr cur)))
        (match a
               (('define dsym ('quote lis))
                (if (eq? sym dsym)
                  lis
                  (itr next)))
               (else (itr next))))
      (assertion-violation 'scan "cannot found" sym)))
  (itr src))

(define (init)
  (set! psyntax-system-macros (scan 'psyntax-system-macros))
  (set! library-legend (scan 'library-legend))
  (set! identifier->library-map (scan 'identifier->library-map)))

(define* lib
  (abbr name export*))

(define (makedummy lib)
  (let-with lib (name export*)
    `(library ,name
              (export ,export*)
              (import (DUMMY))
              ,@(map (^e `(define ,e)) export*))))

(define libs '())

(define (abbr->lib qabbr)
  (find (^e (let-with e (abbr) (equal? abbr qabbr)))
        libs))

(define (name->lib qname)
  (find (^e (let-with e (name) (equal? name qname)))
        libs))

(define (addexport! name sym)
  (let ((lib (abbr->lib name)))
    (touch! lib (export* (cons sym export*)))))

(define (newlib! abbr name)
  (let ((a (name->lib name)))
    (unless a
      (set! libs (cons (make lib 
                             (abbr abbr)
                             (name name) 
                             (export* '())) libs)))))

(define (proc)
  (for-each (^e (match e
                       ((abbr name visible? required?)
                        (newlib! abbr name))
                       (else
                         (assertion-violation
                           'proc
                           "library-legend syntax error"
                           e))))
            library-legend)
  (for-each (^e (match e
                       ((sym lib ...)
                        (addexport! '$all sym)
                        (for-each (^l (addexport! l sym))
                                  lib))
                       (else
                         (assertion-violation
                           'proc
                           "identifier->library-map syntax error"
                           e))))

            identifier->library-map)
  (for-each pp (map makedummy libs))
  )


(init)
(proc)


