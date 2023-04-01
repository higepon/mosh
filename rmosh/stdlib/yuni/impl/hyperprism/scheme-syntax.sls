(library (yuni impl hyperprism scheme-syntax)
         (export 
           ;; core
           (rename (scheme:define define))
           quote set! lambda begin

           ;; expander local
           define-syntax syntax-rules _ ...

           ;; rnrs base
           ;;   core
           let letrec letrec* 
           ;;   derived
           let* cond 
           else => ;; aux-syntax
           case/pred ;; needs predicate
           )
         (import (rename (yuni impl hyperprism core-scheme)
                         (core:quote quote)
                         (core:set! set!)
                         (core:lambda lambda)
                         (core:begin begin)))

(define-syntax scheme:define
  (syntax-rules ()
    ((_ (name . arg) body ...)
     (core:define name (lambda arg body ...)))
    ((_ name body value)
     (core:define name body value))))

(define-syntax letrec
  (syntax-rules ()
    ((_ arg ...)
     (letrec* arg ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((_ ((var value) ...) body0 body1 ...)
     (let ()
       (scheme:define var value)
       ...
       (let () body0 body1 ...)))))

(define-syntax let
  (syntax-rules ()
    ((_ ((var value) ...) body0 body1 ...)
     ((lambda (var ...) body0 body1 ...) value ...))
    ((_ name ((var value)) body0 body1 ...)
     ((letrec ((name (lambda (var ...) body0 body1 ...))) name) value ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ () body0 body1 ...)
     (let () body0 body1 ...))
    ((_ ((var value) . rest) body0 body1 ...)
     (let ((var value)) 
       (let* (rest) body0 body1 ...)))
    ((_ ((var value)) body0 body1 ...)
     (let ((var value)) body0 body1 ...))))

(define-syntax =>
  (syntax-rules ()
    (any (core-invalid-form any))))

(define-syntax else
  (syntax-rules ()
    (any (core-invalid-form any))))

(define-syntax cond-clause
  (syntax-rules (=>)
    ((_ (chk body0 body1 ...) k)
     (if chk (begin body0 body1 ...) k))
    ((_ (chk => body) k)
     (let ((ret chk))
       (if chk (body chk) k)))
    ((_ (chk) k)
     (let ((ret chk))
       (if chk chk k)))))

(define-syntax cond-clause/else
  (syntax-rules (else)
    ((_ (else body0 body1 ...))
     (begin body0 body1 ...))
    ((_ clause)
     (cond-clause clause (if #f #f)))))

(define-syntax cond-helper
  (syntax-rules ()
    ((_ clause0)
     (cond-clause/else clause0))
    ((_ clause0 clause1 ...)
     (cond-clause clause0 (cond-helper clause1 ...)))))

(define-syntax cond
  (syntax-rules ()
    ((_ clause0 clause1 ...)
     (cond-helper clause0 clause1 ...))))

(define-syntax case/pred-clause
  (syntax-rules ()
    ((_ predicate exp ((obj ...) body0 body1 ...) k)
     (if (or (predicate exp obj) ...)
       (begin body0 body1 ...)
       k))))

(define-syntax case/pred-helper
  (syntax-rules ()
    ((_ predicate exp)
     (if #f #f))
    ((_ predicate exp clause0 clause1 ...)
     (case/pred-clause predicate exp clause0
                       (case/pred-helper predicate exp clause1 ...)))))

(define-syntax case/pred
  (syntax-rules ()
    ((_ predicate exp clause0 clause1 ...)
     (let ((target exp))
       (case/pred-helper predicate target clause0 clause1 ...)))))

)
