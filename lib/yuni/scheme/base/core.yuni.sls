(library (yuni scheme base core)
         (export
           lambda define begin letrec let if)
         (import
           (*internal* yuni core-procedures)
           (*internal* yuni core-expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SCHEME CORE FORMS : (yuni scheme base core)
;;;
;;; build RnRS core forms from yuni's core expanders.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic RnRS lambda
;;
;; (lambda x hoge ...) =>
;;   (core-lambda* #f x (begin hoge ...))
;;
;; (lambda (x y) hoge ...) =>
;;   (core-lambda #f (x y) (begin 
(define-syntax %multy-lambda
  (syntax-rules ()
    ((_ all (x y ...) z body)
     (let ((x (car all))
           (rest (cdr all)))
       (%multy-lambda rest (y ...) z body)))
    ((_ all (x) z body)
     (let ((x (car all))
           (z (cdr all)))
       body))
    ((_ all () z body)
     (let ((z all))
       body))))

(define-syntax lambda
  (syntax-rules ()
    ((_ (x ...) body ...)
     (core-lambda #f (x ...) (begin body ...)))
    ((_ (x ... . z) body ...)
     (lambda all
       (%multy-lambda all (x ...) z (begin body ...))))
    ((_ x body ...)
     (core-lambda* #f x (begin body ...)))))

;; generic RnRS begin
(define-syntax begin
  (syntax-rules ()
    ((_ x0 x1 ...)
     (core-begin x0 x1 ...))
    ((_ x0)
     x0)))

;; generic RnRS define
(define-syntax define
  (syntax-rules ()
    ((_ (name arg ...) body ...)
     (define name (lambda (arg ...) body ...)))
    ((_ (name arg ... . rest) body ...)
     (define name (lambda (arg ... . rest) body ...)))
    ((_ name body)
     (core-define name body))))

;; generic RnRS let
(define-syntax let
  (syntax-rules ()
    ((_ name ((x y) ...) body ...)
     (let ()
       (define (name x ...)
         body ...)
       (name y ...)))
    ((_ ((x y) ...) body ...)
     (core-let ((x y) ...) body ..))))

(define-syntax letrec
  (syntax-rules ()
    ((_ ((x y) ...) body ...)
     (core-letrec ((x y) ...) body ...))))

(define-syntax if
  (syntax-rules ()
    ((_ v x)
     (core-if v x *undefined*))
    ((_ v x y)
     (core-if v x y))))

)
