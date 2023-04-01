(library (nmosh util define-macro)
         (export define-macro)
         (import (rnrs) 
                 (for (nmosh util multy-invoke) expand)
                 (for (nmosh util lisp-transformer) expand))

(define-syntax define-macro
  (syntax-rules ()
    ((_ (name . (a . b)) body ...)
     (define-syntax name
       (lisp-transformer
         (lambda (input)
           (let ((exp (cdr input)))
             (multy-invoke exp (a . b) body ...))))))

    ((_ (name) body ...)
     (define-syntax name
       (lisp-transformer
         (lambda (x)
           body ...))))

    ((_ (name . x) body ...)
     (define-syntax name
       (lisp-transformer
         (lambda (input)
           (let ((x (cdr input)))
             body ...)))))))


)
