(import (rnrs)
        (rnrs eval)
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
            (#t
             (for-each-with-index
              (lambda (i x)
                (cond
                 [else
                  (let ([rtd (record-rtd x)])
                    (format #t "   ~d. ~a" i (record-type-name rtd))
                    (let ([v (record-type-field-names rtd)])
                      (case (vector-length v)
                        [(0) (newline)]
                        [(1)
                         (display ": ")
                         (write ((record-accessor rtd 0) x))
                         (newline)]
                        [else
                         (display ":\n")
                         (let f ([i 0])
                           (unless (= i (vector-length v))
                             (display "       ")
                             (display (vector-ref v i))
                             (display ": ")
                             (write ((record-accessor rtd i) x))
                             (newline)
                             (f (+ i 1))))])))]
                 ))
              (simple-conditions e))))
           (let loop ([line (get-line (current-input-port))]
                      [accum ""])
             (define (parentheses-ok? text)
               (let loop ([chars (string->list text)]
                          [p0 0]
                          [p1 0])
                 (if (null? chars)
                     (= 0 p0 p1)
                     (case (car chars)
                       [(#\()
                        (loop (cdr chars) (+ p0 1) p1)]
                       [(#\))
                        (loop (cdr chars) (- p0 1) p1)]
                       [(#\[)
                        (loop (cdr chars) p0 (+ p1 1))]
                       [(#\])
                        (loop (cdr chars) p0 (- p1 1))]
                       [else
                        (loop (cdr chars) p0 p1)]))))
             (define (eval-string-print text)
               (when (not (= 0 (string-length text)))
                 (write (eval (call-with-port (open-string-input-port text) read) (environment '(rnrs))))))
             (if (eof-object? line)
                 (begin
                   (eval-string-print accum)
                   (exit))
                 (let ([current (string-append accum line)])
                   (display (parentheses-ok? current))
                   (if (parentheses-ok? current)
                       (eval-string-print current)
                       (loop (get-line (current-input-port)) current))))))
    (newline)
    (rec))
  (rec))

(rep)
