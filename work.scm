(import (rnrs)
        (mosh)
        (mosh test))

(define (make-record-printer name printer)
    (lambda x
      (display "record printer")
      (for-each display x)))

  (define (for-each-with-index proc lst)
    (do ((i 0 (+ i 1))
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

(define-syntax define-record-accessor
  (lambda (x)
    (define (str->syn id s)
      (datum->syntax id (string->symbol s)))
    (define (syn->str s)
      (symbol->string
       (syntax->datum s)))
    (syntax-case x ()
      [(_ name index)
       #'#f]
      [(_ name index field field* ...)
       (with-syntax ([getter-name (str->syn #'field (string-append (syn->str #'name) "-" (syn->str #'field)))]
                     [setter-name (str->syn #'field (string-append "set-" (syn->str #'name) "-" (syn->str #'field) "!"))]
                     [next-index (+ (syntax->datum #'index) 1)])
                    #'(begin
                        (define (getter-name x) (vector-ref x index))
                        (define (setter-name x val) (vector-set! x index val))
                        (define-record-accessor name next-index field* ...)))])))


(define-syntax define-record
  (lambda (x)
    (define (syn->str s)
      (symbol->string
       (syntax->datum s)))
    (define (str->syn id s)
      (datum->syntax id (string->symbol s)))
    (syntax-case x ()
      [(_ name (field* ...) printer)
       #`(begin
           (define-record name (field* ...))
           (define rp (make-record-printer 'name printer)))]
      [(_ name (field* ...))
       (with-syntax ([record-name (str->syn #'name (string-append "make-" (syn->str #'name)))]
                     [pred (str->syn #'name (string-append (syn->str #'name) "?"))]
                     [field-len (+ 1 (length #'(field* ...)))])
                    #`(begin
                        (define (record-name . args)
                          (let ([ret (make-vector field-len)])
                            (vector-set! ret 0 'name)
                            (let loop ([i 0]
                                       [args args])
                              (if (null? args)
                                  '()
                                  (begin
                                    (vector-set! ret (+ i 1) (car args))
                                    (loop (+ i 1) (cdr args)))))
                            ret))
                        (define (pred x)
                          (eq? (vector-ref x 0) 'name))
                        (define-record-accessor name 1 field* ...)))])))


(define-record stx (expr mark* subst* ae*)
    (lambda (x p wr)
      (display "#<syntax>" p)))


(let ([stx (make-stx 1 2 3 4)])
  (test-true (stx? stx))
  (test-eq 1 (stx-expr stx))
  (test-eq 2 (stx-mark* stx))
  (test-eq 3 (stx-subst* stx))
  (test-eq 4 (stx-ae* stx))
  (set-stx-ae*! stx 5)
  (test-eq 5 (stx-ae* stx))

)

(test-results)
