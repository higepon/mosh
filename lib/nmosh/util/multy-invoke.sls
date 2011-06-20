(library (nmosh util multy-invoke)
         (export multy-invoke)
         (import (rnrs))

(define-syntax multy-invoke/list
  (syntax-rules ()
    ((_ exp () () body ...)
     (let () body ...))
    ((_ exp () x body ...)
     (let ((x exp))
       body ...))
    ((_ exp (a0 a1 ...) x body ...)
     (let ((a0 (car exp))
           (d (cdr exp)))
       (multy-invoke/list d (a1 ...) x body ...)))))

(define-syntax multy-invoke
  (syntax-rules ()
    ((_ exp (a0 a1 ... . b) body ...)
     (multy-invoke/list exp (a0 a1 ...) b body ...))))


)
