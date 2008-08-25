(import (rnrs)
        (mosh string))

(define (repl . x)
  (define (rec)
    (display "mosh>")
    (guard (e
           [(who-condition? )
            (format #t "    &who: ~a\n" (condition-who e))]
           [(message-condition? e)
            (format #t "    &message: ~s\n" (condition-message e))]
           [(violation? e)
            (format #t "    ~a\n" (record-type-name (record-rtd e)))]
           [(irritants-condition? e)
            (format #t "    &irritants: ~s\n" (condition-irritants e))]
           [else
            (format #t "    ~a\n"  (record-type-name (record-rtd e)))])
           (let ([obj (read (current-input-port))])
             (if (eof-object? obj)
                 (exit)
                 (display (eval obj '())))))
    (rec))
  (rec))

(repl)

