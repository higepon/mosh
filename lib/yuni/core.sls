(library (yuni core)
         (export ~ := 
                 define*
                 lambda*
                 let-with let-with*
                 is-a?
                 typeof
                 make touch!)
         (import (rnrs) 
                 (yuni miniobj)
                 (yuni miniobj minitype)
                 (yuni util invalid-form))

; internal
(define-syntax ref
  (syntax-rules ()
    ((_ target slot)
     (miniobj-ref target slot))))

; internal
(define-syntax refset!
  (syntax-rules ()
    ((_ target slot value)
     (miniobj-set! target slot value))))

; ~: generic, recursive ref/set syntax.
(define-syntax ~
  (syntax-rules (:=)
    ((_ target slot := obj)
     (refset! target slot obj))
    ((_ target slot)
     (ref target slot))
    ((_ target slot next-slot ...)
     (~ (ref target slot) next-slot ...))))

; define-composite
(define-syntax define-composite
  (syntax-rules ()
    ((_ typename slots)
     (define-minitype typename slots))))

; ~new
(define-syntax ~new
  (syntax-rules ()
    ((_ typename)
     (make-minitype-obj typename))))

; let-with
(define-syntax let-with
  (syntax-rules ()
    ((_ OBJ (specs0) body ...)
     (let-with-binder OBJ specs0 body ...))
    ((_ OBJ (specs0 specs1 ...) body ...)
     (let ((myobj OBJ))
       (let-with-binder myobj specs0
                        (let-with myobj (specs1 ...) body ...))))))

(define-syntax let-with*
  (syntax-rules ()
    ((_ (specs0 specs1 ...) body ...)
     (let-with specs0 (let-with* (specs1 ...) body ...)))
    ((_ (specs0) body ...)
     (let-with specs0 body ...))))

(define-syntax let-with-binder
  (syntax-rules ()
    ((_ OBJ (bindname name) body ...)
     (let ((bindname (~ OBJ 'name)))
       body ...))
    ((_ OBJ name body ...)
     (let ((name (~ OBJ 'name)))
       body ...))))

(define-syntax typeof
  (syntax-rules ()
    ((_ obj)
     (miniobj-typeof obj))))

(define-syntax is-a?
  (syntax-rules ()
    ((_ obj type)
     (eq? type
          (miniobj-typeof obj)))))

; make
(define-syntax make-apply-rule1!
  (syntax-rules ()
    ((_ NAME (slot body))
     (let ((result body))
       (~ NAME 'slot := result)))))

(define-syntax make
  (syntax-rules ()
    ((_ TYPE rule0 ...)
     (let ((new-object ((~new TYPE))))
       (make-apply-rule1! new-object rule0)
       ...
       new-object))))

(define-syntax touch!-bind-spec1
  (syntax-rules ()
    ((_ OBJ (slot) body ...)
     (begin body ...)) ; nothing to do (legacy form)
    ((_ OBJ (#f slot) body ...)
     (begin body ...)) ; nothing to do
    ((_ OBJ (bind slot) body ...)
     (let-with OBJ ((bind slot))
               body ...))
    ((_ OBJ slot body ...)
     (touch!-bind-spec1 OBJ (slot slot) body ...))))

(define-syntax touch!-bind-spec
  (syntax-rules ()
    ((_ OBJ (spec0) body ...)
     (touch!-bind-spec1 OBJ spec0 body ...))
    ((_ OBJ (spec0 spec1 ...) body ...)
     (touch!-bind-spec1 OBJ spec0
                        (touch!-bind-spec OBJ (spec1 ...) body ...)))))

(define-syntax touch!-apply-spec1!
  (syntax-rules ()
    ((_ OBJ (slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ (#f slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ (bind slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ slot body ...)
     (~ OBJ 'slot := body ...))))

(define-syntax touch!
  (syntax-rules ()
    ((_ OBJ (bind-spec0 body-spec0) ...)
     (let ((myobj OBJ))
       (touch!-bind-spec myobj (bind-spec0 ...)
                         (touch!-apply-spec1! myobj bind-spec0 body-spec0)
                         ...
                         myobj)))))


(define (type-check sym id-name type-name id type)
  (if (is-a? id type)
    'ok
    (begin ;; FIXME
      (display "type violation!!")(newline)
      (display (list sym id-name type-name id type))(newline)
      (car #f))))

(define-syntax annotate-check
  (syntax-rules ()
    ((_ sym (id type))
     (type-check sym 'id 'type id type))
    ((_ sym id)
     'ok)))

(define-syntax raw-name
  (syntax-rules ()
    ((_ id) id)
    ((_ (id type)) id)))

(define-syntax lambda*0-itr
  (syntax-rules ()
    ((_ sym (cur ...) (spec ...) ((id bogus) rest0 ...) body ...)
     (lambda*0-itr sym (cur ... id) ((id bogus) spec ...) (rest0 ...) body ...))
    ((_ sym (cur ...) (spec ...) ((id) rest0 ...) body ...)
     (let ((proxy id))
       (lambda*0-itr sym (cur ...) (spec ...) ((id proxy) rest0 ...) body ...)))
    ((_ sym (cur ...) (spec ...) (id rest0 ...) body ...)
     (lambda*0-itr sym (cur ... id) (spec ...) (rest0 ...) body ...))
    ((_ sym (cur ...) (spec ...) () body ...)
     (lambda (cur ...) 
       (annotate-check sym spec) ...
       body ...))))

(define-syntax lambda*0
  (syntax-rules ()
    ((_ sym (spec0 ...) body ...)
     (lambda*0-itr sym () () (spec0 ...) body ...))))

(define-syntax lambda*
  (syntax-rules ()
    ((_ sym (spec0 ...) body ...)
     (lambda*0 'lambda (spec0 ...) body ...))))

(define-syntax define*
  (syntax-rules ()
    ((_ (name spec0 ...) body ...)
     (define name (lambda*0 'name (spec0 ...) body ...)))
    ((_ name spec)
     (define-composite name spec))))

(define-invalid-form :=)

)
