
(define-macro (receive . args)
 `(call-with-values (lambda () ,(cadr args)) (lambda ,(car args) ,@(cddr args))))

(define-macro (acond . clauses)
  (if (null? clauses)
      '()
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

; todo check malformed guard
(define-macro (guard x . body)
  `(with-exception-handler
    (lambda (,(car x))
      (cond ,@(cdr x)
            (else (raise ,(car x)))))
    (lambda ()
      ,@body)))
