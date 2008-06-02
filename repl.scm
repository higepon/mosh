(define (repl . x)
  (define (rec)
    (display "mosh>")
    (guard (e
            [e
             (print e)
             (rec)])
           (let1 obj (read (current-input-port))
             (if (eof-object? obj)
                 (exit)
                 (print (eval obj '())))))
    (rec))
  (rec))

(repl)

