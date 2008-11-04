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
    (write '(newline))
    (write '(define errors '()))
    (write '(define todo-num 0))
    (write '(define (add-error test expected got) (set! errors (append errors (list (list test expected got))))))
    (for-each-with-index
     (lambda (index test)
       (match test
         [('error . test)
          (write '(display "\r"))
          (write `(if (equal? 'error (guard (con [#t 'error])
                                    ,@test))
                      (format #t " Running ~d/~d" ,index ,test-num)
                        (add-error (quote ,@test) 'error val)))]
         [('mosh-only expected . test)
          (write '(display "\r"))
          (write `(let1 val (begin ,@test)
                    (if (equal? (quote ,expected) val)
                      (format #t " Running ~d/~d" ,index ,test-num)
                        (add-error (quote ,@test) (quote ,expected) val))))]
         [('todo . test)
          (write '(set! todo-num (+ todo-num 1)))]
         [(expected . test)
          (write '(display "\r"))
          (write `(let1 val (begin ,@test)
                    (if (equal? (quote ,expected) val)
                      (format #t " Running ~d/~d" ,index ,test-num)
                      (begin
                        (add-error (quote ,@test) (quote ,expected) val)))))
          (newline)
          ]
         [else
          (write '(display "\r"))
          ]))
     test*)
    (write '(display "\r"))
    (write `(format #t " Running ~d/~d ~a" (- ,test-num (length errors) todo-num) ,test-num
                    (if (> todo-num 0)
                        (format "(~d ToDo)" todo-num)
                        ""
                        )))
    (write '(when (> (length errors) 0)
              (write '(newline))
              (format #t "    === ~d error(s) =========\n" (length errors))
              (for-each
               (lambda (error)
                 (format #t "     ~a : expected ~a but got ~a.\n" (first error) (second error) (third error)))
               errors
               )))
    (write '(when (zero? (length errors)) (display "  \n ... passed")))
    (write '(newline))
    (write '(newline)))
  (exit 0))

(main (command-line))
