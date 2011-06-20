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
     (core-form if 
                (expand-form exp)
                (expand-form true)))
    ((_ exp true false)
     (core-form if
                (expand-form exp)
                (expand-form true)
                (expand-form false)))))

(define-syntax core:set!
  (syntax-rules ()
    ((_ obj value)
     (core-form set!
                (expand-form obj)
                (expand-form value)))))

(define-syntax core:define
  (syntax-rules ()
    ((_ name def ...)
     (core-extend-define (name)
                         (core-form define 
                                    (expand-form name)
                                    (expand-body def ...))))))

(define-syntax core:lambda
  (syntax-rules ()
    ((_ (name ...) body ...)
     (core-extend (name ...)
                  (core-form lambda
                             ((expand-form name) ...)
                             (expand-body body ...))))
    ((_ name body ...)
     (core-extend (name)
                  (core-form lambda
                             (expand-form name)
                             (expand-body body ...))))))

(define-syntax core:begin
  (syntax-rules ()
    ((_ form ...)
     (core-form begin (expand-begin form ...)))))

)
