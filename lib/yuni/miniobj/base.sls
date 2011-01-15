(library (yuni miniobj base)
         (export define-miniobj-ref define-miniobj-set!)
         (import (rnrs))
(define-syntax define-miniobj-ref
  (syntax-rules ()
    ((_ name ref0 ref1 ... refnext term)
     (define-miniobj-ref name ref0 ref1 ... (lambda (obj slot) (refnext obj slot term))))
    ((_ name ref0 term)
     (define (name obj slot) (ref0 obj slot term)))))

(define-syntax define-miniobj-set!
  (syntax-rules ()
    ((_ name set0 set1 ... setnext term)
     (define-miniobj-set! name set0 set1 ... (lambda (obj slot value) (setnext obj slot value term))))
    ((_ name set0 term)
     (define (name obj slot value) (set0 obj slot value term)))))
)
