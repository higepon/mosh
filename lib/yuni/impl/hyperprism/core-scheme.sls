;; core-scheme includes:
;; if
;; set!
;; define
;; lambda
;; begin
;; quote
(library (yuni impl hyperprism core-scheme)
         (export core:quote
                 core:set!
                 core:define
                 core:lambda
                 core:begin)
         (import *core*)

(define-syntax core:quote
  (syntax-rules ()
    ((_ obj)
     (core-form quote (core-quote obj)))))

(define-syntax core:if
  (syntax-rules ()
    ((_ exp true)
     (core-form if exp true))
    ((_ exp true false)
     (core-form if exp true false))))

(define-syntax core:set!
  (syntax-rules ()
    ((_ obj value)
     (core-form set! obj value))))

(define-syntax core:begin
  (syntax-rules ()
    ((_ form ...)
     (core-form begin (begin form ...)))))

(define-syntax core:define
  (syntax-rules ()
    ((_ name def value)
     (define def value))))

(define-syntax core:lambda
  (syntax-rules ()
    ((_ (name ...) body ...)
     (core-extend (name ...)
                  (core-form lambda (name ...)
                             (core-body body ...))))
    ((_ (name ... . tail) body ...)
     (core-extend (name ... tail)
                  (core-form lambda (name ... . tail)
                             (core-body body ...))))
    ((_ name body ...)
     (core-extend (name)
                  (core-form lambda name
                             (core-body body ...))))))


)
