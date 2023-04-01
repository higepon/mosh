#!r6rs
(library (r7b-util syntax-rules)
         (export syntax-rules)
         (import (rename (rnrs) (syntax-rules syntax-rules:r6)))

;; SRFI-46 style syntax-rules

;; FIXME: We should use with-syntax like:
;;   http://srfi.schemers.org/srfi-93/mail-archive/msg00024.html
(define-syntax syntax-rules
  (lambda (x)
    (define (filt elip x)
      (if (identifier? x)
        (cond ((free-identifier=? elip x) #'(... ...))
              ((free-identifier=? #'(... ...) x) #'bogus)
              (else x)
              )
        x))
    (define (emap elip in)
      (syntax-case in ()
        ((x . y) (cons (emap elip #'x)
                       (emap elip #'y)))
        (#(x ...) (list->vector (emap elip #'(x ...))))
        (x (filt elip #'x))))

    (syntax-case x ()
      ((_ (lit ...) (pat tmpl) ...) ;; short cut
       #'(syntax-rules:r6 (lit ...) (pat tmpl) ...))
      ((_ elip (lit ...) (pat tmpl) ...)
       (with-syntax (((clause ...) (emap #'elip #'((pat tmpl) ...))))
         #'(syntax-rules:r6 (lit ...) clause ...))))))

         
)
