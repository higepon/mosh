#!r6rs
(library (r7b-util case)
         (export case)
         (import (rename (rnrs)
                         (case r6:case)))

(define-syntax %r7case-clause
  (syntax-rules (else =>)
    ((_ obj (translated ...) ())
     (r6:case obj translated ...))
    ((_ obj (translated ...) (((e0 e1 ...) => f) rest ...))
     (%r7case-clause obj (translated ... ((e0 e1 ...) (f obj))) (rest ...)))
    ((_ obj (translated ...) ((else => f) rest ...))
     (%r7case-clause obj (translated ... (else (f obj))) (rest ...)))
    ((_ obj (translated ...) (otherwise rest ...))
     (%r7case-clause obj (translated ... otherwise) (rest ...)))))

(define-syntax case
  (syntax-rules ()
    ((_ key clause ...)
     (let ((obj key))
       (%r7case-clause obj () (clause ...))))))
)
