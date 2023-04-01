#!r6rs
(library (r7b-impl lazy)
         (export
delay delay-force
force make-promise
promise?
         )
         (import (rnrs) (rnrs mutable-pairs))

(define-record-type (<box> box box?)
  (fields (mutable value unbox set-box!)))

(define-record-type promise-object
  (fields (mutable done? promise-object-done? set-promise-object-done!)
          (mutable value promise-object-value set-promise-object-value!)))

;; R7RS small 7.3 Derived expression types.
(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise-internal #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise-internal #t expression)))))

(define (force promise)
   (let ((promise-obj (unbox promise)))
     (if (promise-object-done? promise-obj)
         (promise-object-value promise-obj)
         (let ((promise* ((promise-object-value promise-obj))))
           (unless (promise-object-done? promise-obj)
             (promise-update! promise* promise))
             (force promise)))))

(define (make-promise-internal done? proc)
  (box (make-promise-object done? proc)))

(define (promise-update! new old)
  (set-promise-object-done! (unbox old) (promise-object-done? (unbox new)))
  (set-promise-object-value! (unbox old) (promise-object-value (unbox new)))
  (set-box! new old))

(define (promise? x)
  (and (box? x) (promise-object? (unbox x))))

(define (make-promise value)
  (if (promise? value)
      value
      (delay value)))
)
