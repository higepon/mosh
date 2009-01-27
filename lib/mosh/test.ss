(library (mosh test)
  (export test* test/exception test/violation? test/t test-end)
  (import (only (rnrs) define define-syntax lambda let* if syntax-case syntax else +
                set! equal? quote begin syntax->datum exit ... guard violation? cons
                list cond > length display for-each current-error-port car cadr caddr)
          (mosh string)
          (mosh parameters))

  (define error* (make-parameter '()))
  (define counter (make-parameter 0))
  (define ok-counter (make-parameter 0))

  (define-syntax test*
    (lambda (x)
      (syntax-case x ()
        ((_ form test expected)
         (syntax
          (let* ([result test]
                 [test-ok? (equal? result expected)])
            (cond
             [test-ok?
              (format #t "\rTest Running ~d/~d" (ok-counter) (counter))
              (counter (+ (counter) 1))
              (ok-counter (+ (ok-counter) 1))]
             [else
              (error* (cons (list 'form result expected) (error*)))
              (counter (+ (counter) 1))]
                ))))
        ((_ test expected)
         (syntax
          (test* test expected test))))))

  (define-syntax test/t
    (lambda (x)
      (syntax-case x ()
        [(_ form)
         #'(test* form #t)])))

  (define-syntax test/exception
    (lambda (x)
      (syntax-case x ()
        ((_ pred? test ...)
         (syntax
          (test* (test/exception pred? test ...) (guard (con [(pred? con) #t])
                             test ...) #t))))))

  (define-syntax test/violation?
    (lambda (x)
      (syntax-case x ()
        ((_ test ...)
         (syntax
          (test/exception violation? test ...))))))

  (define (test-end)
    (cond
     [(> (length (error*)) 0)
      (display "\n** Error(s)\n")
      (for-each
       (lambda (x)
         (format (current-error-port) "  ~a got ~a but ~a expected\n" (car x) (cadr x) (caddr x)))
       (error*))
      (exit -1)]
     [else
      (format #t "\rTest Running ~d/~d ... ok\n" (ok-counter) (counter))]))

)
