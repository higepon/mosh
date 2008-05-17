(define (repl . x)
  (define (rec)
    (display "mosh>")
    (guard (e
            [e
             (print e)
             (rec)])
            (print (eval (read (current-input-port)) '())))
    (rec))
  (rec))

(repl)

