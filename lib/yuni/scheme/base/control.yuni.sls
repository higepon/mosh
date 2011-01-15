(library (yuni scheme base control)
         (export and or)
         (import (yuni scheme base core))

(define-syntax and
  (syntax-rules ()
    ((_ e0 e1 ...)
     (if e0
       (and e1 ...)
       #f))
    ((_)
     #t)))

(define-syntax or
  (syntax-rules ()
    ((_ e0 e1 ...)
     (if e0
       #t
       (or e1 ...)))
    ((_)
     #f)))

)
