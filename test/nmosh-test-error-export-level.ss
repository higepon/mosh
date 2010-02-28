(library (test)
         (export foo)
         (import (rnrs))
(define (foo x) (+ 1 x)))

(import (rnrs) (test))

(define-syntax boo
  (lambda (x)
    (let ((l (syntax->datum x)))
      (datum->syntax #'x (foo (cadr l))))))

(display (boo 2))
