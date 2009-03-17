(import (rnrs)
        (mosh test))

(define-syntax test-transcoders
    (syntax-rules ()
      [(_ bytevector->string string->bytevector)
       (begin
         (string->bytevector "a\nb" (make-transcoder ))
         (string->bytevector "a\nb" (make-transcoder))
         )]))

(let (
          [string->bytevector-via-port
           (lambda (str tr)
             (let-values ([(p get) (open-bytevector-output-port tr)])
               (put-string p str)
               (get)
               ))])
      (test-transcoders bytevector->string-via-port
                        string->bytevector-via-port))

(test-end)
