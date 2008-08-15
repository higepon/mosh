(library (mosh test)
  (export test* test/exception test/violation?)
  (import (rnrs)
          (mosh string))

  (define-syntax test*
    (lambda (x)
      (syntax-case x ()
        ((_ form test expected)
         (syntax
          (let* ([result test]
                 [test-ok? (equal? result expected)])
            (if test-ok?
                (format #t "OK: ~a => ~a\n" (quote form)  result)
                (format #t "NG: ~a => ~a expected ~a\n"
                        (syntax->datum (syntax test))
                        result
                        expected)))))
        ((_ test expected)
         (syntax
          (test* test expected test))))))

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

)
