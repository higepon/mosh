#!r6rs
(library (r7b-impl lazy)
         (export 
delay delay-force
force make-promise
promise?         
         )
         (import (rnrs) (rnrs mutable-pairs))

(define (promise? obj) 
    (assertion-violation "promise? not supported"))

;; Taken from R7RS small.
(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
      (make-promise #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
      (delay-force (make-promise #t expression)))))

(define make-promise
    (lambda (done? proc)
      (list (cons done? proc))))

(define (force promise)
  (if (promise-done? promise)
    (promise-value promise)
    (let ((promise* ((promise-value promise))))
       (unless (promise-done? promise)
         (promise-update! promise* promise))
         (force promise))))

(define promise-done?
  (lambda (x) (car (car x))))

(define promise-value
   (lambda (x) (cdr (car x))))

(define promise-update!
  (lambda (new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old))))
)
