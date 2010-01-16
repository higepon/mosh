(library (nmosh runlib)
	(export runlib)
	(import 
	  (only (rnrs base) define quasiquote unquote unquote-splicing)
	  (primitives ex:run-r6rs-sequence))

(define (runlib lib top)
  (ex:run-r6rs-sequence `((import ,@lib) (,top))))

)
