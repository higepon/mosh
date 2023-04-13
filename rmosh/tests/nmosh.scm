(import (nmosh)
        (mosh test)
	(primitives ex:expand-sequence/debug ex:repl))

(define (case1)
  (ex:expand-sequence/debug
    "my test-case"
    '(
      (import (rnrs))
      (define a 10)
      (define a 20) ;; DISallows multiple definition
      (display a)) #t))

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

(test-error syntax-violation? (load "tests/nmosh-test-error-export-level.ss"))
(test-error syntax-violation? (load "tests/nmosh-test-error-invalid-export.ss"))
(test-true (begin (load "tests/nmosh-test-multiple-libs.ss") #t))

(test-results)
