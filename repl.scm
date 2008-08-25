(import (rnrs)
        (mosh string))

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

(define (repl . x)
  (define (rec)
    (display "mosh>")
    (guard (e
            (e
       (for-each-with-index
        (lambda (i x)
          (cond
           [(who-condition? x)
            (format #t "   ~d. &who: ~a\n" i (condition-who x))]
           [(message-condition? x)
            (format #t "   ~d. &message: ~s\n" i (condition-message x))]
           [(violation? x)
            (format #t "   ~d. ~a\n" i (record-type-name (record-rtd x)))]
           [(irritants-condition? x)
            (format #t "   ~d. &irritants: ~s\n" i (condition-irritants x))]
           [else
            (format #t "   ~d. ~a\n" i (record-type-name (record-rtd x)))]))
        (simple-conditions e))))
           (let ([obj (read (current-input-port))])
             (if (eof-object? obj)
                 (exit)
                 (display (remp obj)))))
    (rec))
  (rec))

(repl)

