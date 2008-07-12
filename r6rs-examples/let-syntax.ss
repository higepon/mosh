(import (rnrs))
(let-syntax ((when2 (syntax-rules ()
                     ((when2 test stmt1 stmt2 ...)
                      (if test
                          (begin stmt1
                                 stmt2 ...))))))
  (let ((if #t))
    (when2 if (set! if 'now))
    (display if)))
(let ()
  (define even?
    (lambda (x)
      (or (= x 0) (odd? (- x 1)))))
  (define-syntax odd?
    (syntax-rules ()
      ((odd?  x) (not (even? x)))))
  (display (even? 10)) )
