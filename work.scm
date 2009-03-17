(import (rnrs)
        (mosh test))

(define-syntax test-rw
    (syntax-rules ()
      [(_ v)
       (test* (let ([p (open-string-input-port
                       (call-with-string-output-port
                        (lambda (p) (put-datum p v))))])
                (get-datum p))
             v)]))


(let ([p (open-string-input-port
                       (call-with-string-output-port
                        (lambda (p) (put-datum p '0))))])
                (write (get-datum p)))

(test-rw (string->symbol "0"))

(test-end)
