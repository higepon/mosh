(import (rnrs)
        (mosh)
        (system))


(define eval-r6rs (symbol-value 'eval-r6rs))

(define shell-utilities '(
  (define (string-join strings d)
    (let loop ([strings strings]
               [ret ""])
      (if (null? strings)
          ret
          (loop (cdr strings) (format "~a~a~a" ret (if (string=? "" ret) "" d) (car strings))))))
  (define-syntax def-command
    (lambda (y)
    (syntax-case y ()
      [(_ command)
       #'(define-syntax command
         (lambda (x)
         (syntax-case x ()
           [(_ args (... ...))
            (call-process (string-join (cons (symbol->string (syntax->datum #'command)) (syntax->datum #'(args (... ...)))) #\space))]
           [_ (call-process (symbol->string (syntax->datum #'command)))]
           )))])))
  (define-syntax $def-command
    (lambda (y)
      (syntax-case y ()
        [(k command)
         (with-syntax ([$command (datum->syntax #'k (string->symbol
                                                     (format "$~a" (syntax->datum #'command))))])
         #'(define-syntax $command
             (lambda (x)
               (syntax-case x ()
                 [(_ args (... ...))
                  #'(string-split (call-process (string-join (cons (symbol->string (syntax->datum #'command)) (syntax->datum #'(args (... ...)))) #\space)) #\newline)]
                 [_ #'(string-split (call-process (symbol->string (syntax->datum #'command))) #\newline)]
                 )))]))))
  ;; (define-syntax $define
;;     (lambda (x)
;;       (syntax-case x ()
;;         ((k var val)
;;          (with-syntax ([var (datum->syntax #'k (string->symbol
;;                                                 (format "$~a" (syntax->datum #'var))))])
;;                       #'(define var val))))))
))

(for-each eval-r6rs shell-utilities)

;; eval of def-command causes error once.
;; this is a bug of Mosh
(define-syntax define-command
  (syntax-rules ()
    [(_ x)
    (guard (c (#t (eval-r6rs '(def-command x))
                  (eval-r6rs '($def-command x))
                  ))
           (eval-r6rs '(def-command x)))]))

(define-command ls)
(define-command cd)
(define-command pwd)

(define (conditioon-printer e port)
    (define (ref rtd i x)
      (let ([val ((record-accessor rtd i) x)])
        (if (symbol? val)
            (ungensym val)
            val)))
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
         (format port "   ~d. ~a" i (record-type-name rtd))
         (let ([v (record-type-field-names rtd)])
           (case (vector-length v)
             [(0) (newline port)]
             [(1)
              (display ": " port)
              (write (ref rtd 0 x) port)
              (newline port)]
             [else
              (display ":\n" port)
              (let f ([i 0])
                (unless (= i (vector-length v))
                  (display "       " port)
                  (display (vector-ref v i) port)
                  (display ": " port)
                  (write (ref rtd i x) port)
                  (newline port)
                  (f (+ i 1))))]))))
     (simple-conditions e)))

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
               (display "\n" (current-error-port))
               (conditioon-printer e (current-error-port))))
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
                 (unless (or (string=? "\n" text) (= 0 (string-length text)))
                   (display ((symbol-value 'eval-r6rs) (call-with-port (open-string-input-port text) read)))))
               (if (eof-object? line)
                   (begin
                     (eval-string-print accum)
                     (exit))
                   (let ([current (string-append accum line)])
                     (if (parentheses-ok? current)
                         (eval-string-print current)
                         (loop (get-line (current-input-port)) current))))))
      (newline)
      (rec))
    (rec))

(repl)
