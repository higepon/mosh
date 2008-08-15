(import (rnrs)
        (mosh test))

  (define-syntax test/exception
    (lambda (x)
      (syntax-case x ()
        ((_ pred? test ...)
         (syntax
          (test* (guard (con [(pred? con) #t])
                        test ...) #t))))))

  (define-syntax test/violation?
    (lambda (x)
      (syntax-case x ()
        ((_ test ...)
         (syntax
          (test/exception violation? test ...))))))


  (test/violation? (car 3))

