(library (mosh c-type helper)
         (export gen-id process-c-struct-field*)
         (import (rnrs) (mosh) (mosh test) (match))

(define (gen-id prefix name)
  (cond
   ;; (symbol? syntax?)
   [(symbol? prefix)
    (datum->syntax name (string->symbol (format "~a-~a" prefix (syntax->datum name))))]
   ;; (syntax? symbol?)
   [(symbol? name)
    (datum->syntax prefix (string->symbol (format "~a-~a" (syntax->datum prefix) name)))]
   ;; (syntax? syntax?)
   [else
    (datum->syntax prefix (string->symbol (format "~a-~a" (syntax->datum prefix) (syntax->datum name))))]))

(define (align->offset offset align)
  (bitwise-and (+ offset (- align 1)) (bitwise-not (- align 1))))

(define (size-of type)
  (let ([size (os-constant (string->symbol (format "size-of-~a" type)))])
    (assert size)
    size))

(define (align-of type)
  (let ([align (os-constant (string->symbol (format "align-of-~a" type)))])
    (assert align)
    align))

;; field (type var)
;; Returns list of (values sizeof-struc ((type var offset) ...))
(define (process-c-struct-field* field*)
  (let loop ([offset 0]
             [field* field*]
             [ret '()])
    (cond
     [(null? field*) (values offset ret)]
     [else
      (match (car field*)
        [(type var)
         (let ([type-offset (align->offset offset (align-of type))])
           (loop (+ type-offset (size-of type))
                 (cdr field*)
                 (cons `(,type ,var ,type-offset) ret)))]
        [else
         (error 'process-struct-field* "unknown field* spec" field*)])])))


(define (mosh-c-type-helper-test)
  (test-begin "test")

  (test-begin "test-align")
  (test-eqv 0 (align->offset 0 4))
  (test-eqv 4 (align->offset 1 4))
  (test-eqv 4 (align->offset 2 4))
  (test-eqv 4 (align->offset 3 4))
  (test-eqv 4 (align->offset 4 4))
  (test-eqv 8 (align->offset 5 4))
  (test-end)

  (test-begin "test-align")
  (let-values ([(struct-size field*) (process-c-struct-field* '((int a) (char b) (int c)))])
    (test-true (number? struct-size))
    (test-true (list? field*)))

  (test-end)

  (test-end)

)

)
