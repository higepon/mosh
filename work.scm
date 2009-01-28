(import (rnrs))


(define-syntax p
  (lambda (x)
  (syntax-case x ()
    [(_ obj)
     #'(call-with-values (lambda () obj) (lambda x (display x) (newline) (apply values x)))])))

(p (values 1 2))
(let-values (((a b) (p (values 1 2))))
  (display (+ a b)))
(p 1)
