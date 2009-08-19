;; Generate test.scm for C++ from test-data.scm

(define (file->sexp file)
  (with-input-from-file file
    (lambda ()
      (let loop ([obj (read)]
                 [ret '()])
        (cond
         [(eof-object? obj)
          (reverse ret)]
         [else
          (loop (read) (cons obj ret))])))))

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))


(define (main args)
  (let* ([test* (file->sexp (second args))]
         [test-num (length test*)])
    (display (third args))
    (call-with-port (open-file-output-port (third args) (make-file-options '(no-fail)) 'block (native-transcoder))
      (lambda (p)
        (write '(define errors '()) p)
        (write '(define todo-num 0) p)
        (write '(define (add-error test expected got) (set! errors (append errors (list (list test expected got))))) p)
        (for-each-with-index
         (lambda (index test)
           (match test
             [('test test . expected)
              (write '(display "\r") p)
              (write `(let1 val (begin ,test)
                        (if (equal? ,@expected val)
                            (format #t " Running ~d/~d" ,index ,test-num)
                            (begin
                              (add-error (quote ,test) (quote ,@expected) val)))) p)
              (newline p)
              ]
             [('error . test)
              (write '(display "\r") p)
              (write `(if (equal? 'error (guard (con [#t 'error])
                                                ,@test))
                          (format #t " Running ~d/~d" ,index ,test-num)
                          (add-error (quote ,@test) 'error 'not-error)) p)]
             [('mosh-only expected . test)
              (write '(display "\r") p)
              (write `(let1 val (begin ,@test)
                        (if (equal? (quote ,expected) val)
                            (format #t " Running ~d/~d" ,index ,test-num)
                            (add-error (quote ,@test) (quote ,expected) val))) p)]
             [('definition . definitions)
              (for-each (lambda (x) (write x p)) definitions)]
             [('todo . test)
              (write '(set! todo-num (+ todo-num 1)) p)]
             [(expected . test)
              (write '(display "\r") p)
              (write `(let1 val (begin ,@test)
                        (if (equal? (quote ,expected) val)
                            (format #t " Running ~d/~d" ,index ,test-num)
                            (begin
                              (add-error (quote ,@test) (quote ,expected) val)))) p)
              (newline p)
              ]
             [else
              (write '(display "\r") p)
              ]))
         test*)
        (write '(display "\r") p)
        (write `(format #t " Running ~d/~d ~a" (- ,test-num (length errors) todo-num) ,test-num
                        (if (> todo-num 0)
                            (format "(~d ToDo)" todo-num)
                            ""
                            )) p)
        (write '(when (> (length errors) 0)
                  (newline)
                  (format #t "    === ~d error(s) =========\n" (length errors))
                  (for-each
                   (lambda (error)
                     (format #t "     ~a : expected ~a but got ~a.\n" (first error) (second error) (third error)))
                   errors
                   )) p)
        (write '(when (zero? (length errors)) (display "  \n ... passed")) p)
        (write '(newline) p)
        (write '(newline) p)))
      (exit 0)))

(main (command-line))
