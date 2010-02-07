(library (nmosh runlib)
	(export runlib runlib/fallback)
	(import 
	  (rnrs)
	  (primitives ex:run-r6rs-sequence))

(define (runlib/fallback k lib top . params)
  (guard
    (e
      (#t (k e)))
    (apply runlib (cons lib (cons top params)))))

(define (runlib lib top . params)
  (ex:run-r6rs-sequence `((import ,@lib) (,top ,@params))))

)
