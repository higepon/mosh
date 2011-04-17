(library (yuni r7rs define-record-type)
         (export define-record-type)
         (import 
           (yuni core)
           (except (rnrs) define-record-type)) ;; FIXME: we should use R7RS syntax-rules

(define-syntax drt-field-name
  (syntax-rules ()
    ((_ (field-name accessor-name))
     field-name)
    ((_ (field-name accessor-name modifier-name))
     field-name)))

(define-syntax drt-define-accessor
  (syntax-rules ()
    ((_ name (field-name accessor-name modifier-name))
     (drt-define-accessor name (field-name accessor-name)))
    ((_ name (field-name accessor-name))
     (define* (accessor-name (obj name))
       (let-with obj (field-name) field-name)))))

(define-syntax drt-define-modifier
  (syntax-rules ()
    ((_ name (field-name accessor-name))
     (begin))
    ((_ name (field-name accessor-name modifier-name))
     (define* (modifier-name (obj name) dat)
       (touch! obj (field-name dat))))))

(define-syntax drt-define-constructor
  (syntax-rules ()
    ((_ name (constructor-name field-name ...))
     (define (constructor-name field-name ...)
       (make name
             (field-name field-name) ...)))))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ name constructor pred field ...)
     (begin
       (define* name ((drt-field-name field) ...))
       (define (pred x) (is-a? x name))
       (drt-define-constructor name constructor)
       (drt-define-accessor name field) ...
       (drt-define-modifier name field) ...))))

)
