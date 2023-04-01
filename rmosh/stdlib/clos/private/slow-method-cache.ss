
(library (clos private slow-method-cache)

  (export invalidate-method-caches!
          make-cached-dispatch)

  (import (only (rnrs) define list set! let lambda if not eq? begin letrec null? let* cdr or
                hashtable-ref hashtable-set! car make-eq-hashtable)
          (clos introspection)
          (clos bootstrap standard-classes))

  (define *global-token* (list "t"))
  
  (define (invalidate-method-caches!)
    (set! *global-token* (list "t")))

  (define (make-cached-dispatch generic handle-cache-miss)
    (let ((token #f) (chain #f))
      (lambda (args)
        (if (not (eq? token *global-token*))
            (begin
	      (set! token *global-token*)
	      (set! chain (make-chain handle-cache-miss))))
        (chain args args))))
  
  (define (make-chain handle-cache-miss)
    (letrec ((final-handler 
              (lambda (args)
                (let ((procedure (handle-cache-miss args)))
                  (set! final-handler
                        (lambda (args) procedure))
                  procedure))))
      (let ((chains (make-eq-hashtable)))
        (lambda (tail args)
          (if (null? tail)
              (final-handler args)
              (let* ((class (class-of (car tail)))
                     (chain (or (hashtable-ref chains class #f)
                                (let ((new-chain 
                                       (make-chain handle-cache-miss)))
                                  (hashtable-set! chains class new-chain) 
                                  new-chain))))
                (chain (cdr tail) args)))))))
  
  ) ;; library (clos private method-cache)

