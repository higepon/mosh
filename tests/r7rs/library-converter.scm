(import (rnrs)
        (match)
        (mosh pp)
        (mosh test))

(define (parse-define-library exp)
  (match exp
    [('define-library (name))
        (values #t name)]
    [else (values #f #f)]))

(test-values (values #t 'test) (parse-define-library '(define-library (test))))

(test-results)

(display (parse-define-library '(define-library (test))))
                    ;(export make rows cols ref each (rename put! set!))
                   ; (import (scheme base))
;                    (begin 3))))

#|
(define-syntax r7rs-library
    (syntax-rules (define-library begin)
        [(_ (define-libray (name ...) (begin body ...)))
            '(library (name ...)
               body ...)]))

(pp (r7rs-library (define-library (example grid)
                    (export make rows cols ref each (rename put! set!))
                    (import (scheme base))
                    (begin 3))))

                    |#
(newline)
