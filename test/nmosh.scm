(import (rnrs)
        (mosh test)
	(primitives ex:expand-sequence/debug ex:repl))

(define (case1)
  (ex:expand-sequence/debug
    '(
      (import (rnrs))
      (define a 10)
      (define a 20) ;; DISallows multiple definition
      (display a))))

(test-error syntax-violation? (case1))

(guard (c
	 (#t
	  (test-true (and
		       (syntax-violation? c)
		       (eq? 'define (condition-who c))))))
       (case1))

(test-equal #t
	    (begin
	      (ex:repl '((import (rnrs))
			 (define a 10)
			 (define a 20) ;; allows multiple definition
			 a))
	      #t))

(test-results)
