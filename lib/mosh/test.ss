(library (mosh test)
  (export test*)
  (import (rnrs)
          (mosh string))
  (define-syntax test*
    (lambda (x)
      (syntax-case x ()
        ((_ test expected)
         (syntax
          (let* ([result test]
                 [test-ok? (equal? result expected)])
            (if test-ok?
                (format #t "OK: ~a => ~a\n" (syntax->datum (syntax test)) result)
                (format #t "NG: ~a => ~a expected ~a\n"
                        (syntax->datum (syntax test))
                        result
                        expected)))))))))
