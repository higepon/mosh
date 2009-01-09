(import (rnrs)
        (mosh)
        (system))

(define eval-r6rs (symbol-value 'eval-r6rs))

(define shell-utilities '(

  (import (mosh test))
  (import (srfi :98))
  (import (mosh))
  (define eval-r6rs (symbol-value 'eval-r6rs))
  (define-syntax begin0
    (syntax-rules ()
      ((_ e es ...)
       (let ((v e))
         es ...
         v))))

  (define (any->string x)
    (format "~a" x))

  (define (join strings d)
    (let loop ([strings strings]
               [ret ""])
      (if (null? strings)
          ret
          (loop (cdr strings) (format "~a~a~s" ret (if (string=? "" ret) "" d) (car strings))))))

  (define (port->string p)
    (let loop ([ret '()][c (read-char p)])
      (if (eof-object? c)
          (list->string (reverse ret))
          (loop (cons c ret) (read-char p)))))

  (define (spawn command args)
    (let-values  ([(pid cin cout cerr) (%spawn command args (list #f #f #f))])
      (%waitpid pid)
      #f))

  (define (spawn->string command args)
    (let-values ([(in out) (%pipe)])
      (let-values ([(pid cin cout cerr) (%spawn command args (list #f out #f))])
        (close-port out)
        (begin0
          (port->string (transcoded-port in (make-transcoder (utf-8-codec))))
          (close-port in)
          (%waitpid pid)))))

  (define (def-commands c)
    (for-each (lambda (y) (eval-r6rs `(def-command ,y))) c))

  (define-syntax def-alias
    (lambda (y)
      (syntax-case y()
        [(_ name body ...)
         #'(define-syntax name
             (lambda (x)
               (syntax-case x ()
                 [_ (begin body ...)])))])))

  (define-syntax def-command
    (lambda (y)
      (syntax-case y ()
        [(_ command)
         #'(define-syntax command
             (lambda (x)
               (syntax-case x ()
                 [(_ args (... ...))
                  #'(spawn (any->string (syntax->datum #'command)) (map any->string (syntax->datum #'(args (... ...)))))]
                 [_
                  #'(spawn (any->string (syntax->datum #'command)) '())]
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
                  #'(string-split
                     (spawn->string (any->string (syntax->datum #'command)) (map any->string (syntax->datum #'(args (... ...)))))
                                  #\newline)]
                 [_ #'(string-split
                       (spawn->string (any->string (syntax->datum #'command)) '())
                                    #\newline)]
                 )))]))))

  (define-syntax cd
    (lambda (x)
      (syntax-case x ()
        [(_ arg)
         #'(set-current-directory! (format "~a" (syntax->datum #'arg)))]
        [_ #'(set-current-directory! (get-environment-variable "HOME"))])))

(define-syntax pipe
  (lambda (x)
    (syntax-case x ()
      [(_ (cmd1 args1) (cmd2 args2) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list #f out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) ...)
           (do ([i 0 (+ i 1)])
               ((= i (length '((cmd2 args2) ...))))
             (%waitpid -1)))]
      [(_ "internal" in (cmd args))
       #'(begin
           (%spawn cmd args (list in #f #f))
           (close-port in))]
      [(_ "internal" in (cmd1 args1) (cmd2 args2) (cmd3 args3) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list in out1 #f))
           (close-port out1)
           (pipe "internal" in1 (cmd2 args2) (cmd3 args3) ...))]
      )))

(define-syntax $pipe
  (lambda (x)
    (syntax-case x ()
      [(_ out (cmd1 args1) (cmd2 args2) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list #f out1 #f))
           (close-port out1)
           ($pipe "internal" out in1 (cmd2 args2) ...)
           (do ([i 0 (+ i 1)])
               ((= i (length '((cmd2 args2) ...))))
             (%waitpid -1)))]
      [(_ "internal" out in (cmd args))
       #'(begin
           (%spawn cmd args (list in out #f))
           (close-port out)
           (close-port in))]
      [(_ "internal" out in (cmd1 args1) (cmd2 args2) (cmd3 args3) ...)
       #'(let-values ([(in1 out1) (%pipe)])
           (%spawn cmd1 args1 (list in out1 #f))
           (close-port out1)
           ($pipe "internal" out in1 (cmd2 args2) (cmd3 args3) ...))]
      )))

(define-syntax ->
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       #'(pipe ((any->string (if (pair? (syntax->datum #'x)) (car (syntax->datum #'x)) (syntax->datum #'x)))
                                (map any->string (if (pair? (syntax->datum #'x)) (cdr (syntax->datum #'x)) '())))
               ((any->string (if (pair? (syntax->datum #'y)) (car (syntax->datum #'y)) (syntax->datum #'y)))
                                (map any->string (if (pair? (syntax->datum #'y)) (cdr (syntax->datum #'y)) '())))
               ((any->string (if (pair? (syntax->datum #'z)) (car (syntax->datum #'z)) (syntax->datum #'z)))
                                (map any->string (if (pair? (syntax->datum #'z)) (cdr (syntax->datum #'z)) '()))) ...)]

      )))


(define-syntax $->
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       #'(let-values ([(in out) (%pipe)])
           ($pipe out
                  ((any->string (if (pair? (syntax->datum #'x)) (car (syntax->datum #'x)) (syntax->datum #'x)))
                   (map any->string (if (pair? (syntax->datum #'x)) (cdr (syntax->datum #'x)) '())))
                  ((any->string (if (pair? (syntax->datum #'y)) (car (syntax->datum #'y)) (syntax->datum #'y)))
                   (map any->string (if (pair? (syntax->datum #'y)) (cdr (syntax->datum #'y)) '())))
                  ((any->string (if (pair? (syntax->datum #'z)) (car (syntax->datum #'z)) (syntax->datum #'z)))
                   (map any->string (if (pair? (syntax->datum #'z)) (cdr (syntax->datum #'z)) '()))) ...)
           (port->string (transcoded-port in (make-transcoder (utf-8-codec)))))]
      )))

))

(for-each eval-r6rs shell-utilities)

;; eval of def-command causes error once.
;; this is a bug of Mosh
(define-syntax define-command
  (syntax-rules ()
    [(_ x)
    (guard (c (#t #t))
           (eval-r6rs '($def-command x))
           (eval-r6rs '(def-command x)))]))

(define-command ls)
(define-command pwd)

(eval-r6rs '(test* ls #f))
(eval-r6rs '(test* (ls -la) #f))
(eval-r6rs '(test* (list? $ls) #t))
(eval-r6rs '(test* (for-all string? $ls) #t))
(eval-r6rs '(test* (for-all string? ($ls -la)) #t))
(eval-r6rs '(test* (begin ($-> ls (grep cpp) (grep main))) "main.cpp\n"))

(eval-r6rs '(cd /usr/bin))
(eval-r6rs '(def-commands (filter (lambda (x) (not (eq? x 'cd))) (map string->symbol $ls))))
(eval-r6rs '(cd))


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
      (format #t "mosh:~a>" (current-directory))
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
