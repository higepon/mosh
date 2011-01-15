(library (yuni miniobj minitype)
         (export miniobj-minitype-ref
                 miniobj-minitype-set!
                 define-minitype
                 make-minitype
                 make-minitype-obj
                 minitype-predicate
                 minitype-typeof
                 make-minitype-refslot
                 make-minitype-setslot!)
         (import (rnrs)
                 (yuni compat simple-struct))

(define (check? obj sym)
  (and
    (simple-struct? obj)
    (eq? sym
         (simple-struct-name obj))))
(define (minitype? obj)
  (check? obj '*yuni-minitype*))
(define (minitype-obj? obj)
  (check? obj '*yuni-minitype-obj*))

(define (check-minitype obj)
  (or (minitype? obj)
      (assertion-violation 'check-minitype "minitype needed" obj)))

(define (check-minitype-obj obj)
  (or (minitype-obj? obj)
      (assertion-violation 'check-minitype-obj "minitype-obj needed" obj)))

(define (check-minitype-obj-type obj minitype)
  (or (and (minitype-obj? obj)
           (minitype? minitype)
           (or (eq? (minitype-type (minitype-obj-type obj))
                    (minitype-type minitype))
               (assertion-violation 'check-minitype-obj "type unmatched"
                                    (list minitype obj))))
      (assertion-violation 'check-minitype-obj "minitype-obj needed" obj)))
(define (minitype-slot obj)
  (check-minitype obj)
  (simple-struct-ref obj 1))
(define (minitype-type obj)
  (check-minitype obj)
  (simple-struct-ref obj 0))
(define (minitype-obj-slot obj)
  (check-minitype-obj obj)
  (simple-struct-ref obj 1))
(define (minitype-obj-type obj)
  (check-minitype-obj obj)
  (simple-struct-ref obj 0))

(define (make-minitype name slots)
  (make-simple-struct 
    '*yuni-minitype* 
    2 (list name slots)))

(define (make-minitype-obj minitype)
  (check-minitype minitype)
  (lambda ()
    (make-simple-struct 
      '*yuni-minitype-obj* 
      2 (list 
          minitype
          (make-vector (length (minitype-slot minitype)))))))

(define (minitype-predicate obj minitype)
  (check-minitype minitype)
  (and
    (minitype-obj? obj)
    (eq? minitype
         (minitype-obj-type obj))))

(define (minitype-typeof obj) ;; FIXME: should allow RnRS types(like lists, vectors, ...)..
  (and
    (minitype-obj? obj)
    (minitype-obj-type obj)))


(define (scan-slot minitype slot)
  (define (number cur rest)
    (if (pair? rest)
      (if (eq? (car rest) slot)
        cur
        (number (+ 1 cur) (cdr rest)))
      #f))
  (let ((slots (minitype-slot minitype)))
    (number 0 slots)))

(define (make-minitype-refslot minitype slot)
  (define slotno (scan-slot minitype-slot))
  (check-minitype minitype)
  (lambda (obj)
    (check-minitype-obj obj minitype)
    (vector-ref (minitype-obj-slot obj) slotno)))

(define (make-minitype-setslot! minitype slot)
  (define slotno (scan-slot minitype-slot))
  (check-minitype minitype)
  (lambda (obj value)
    (check-minitype-obj obj minitype)
    (vector-set! (minitype-obj-slot obj) slotno value)))

(define (miniobj-minitype-ref obj slot k)
  (if (minitype-obj? obj)
    (vector-ref (minitype-obj-slot obj) (scan-slot (minitype-obj-type obj) slot))
    (k obj slot)))

(define (miniobj-minitype-set! obj slot value k)
  (if (minitype-obj? obj)
    (vector-set! (minitype-obj-slot obj) (scan-slot (minitype-obj-type obj) slot) value)
    (k obj slot value)))
(define-syntax define-minitype
  (syntax-rules ()
    ((_ name spec)
     (define name (make-minitype 'name 'spec)))))

)
