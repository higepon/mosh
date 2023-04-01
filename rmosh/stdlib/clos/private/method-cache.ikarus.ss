

(library (clos private method-cache)

  (export invalidate-method-caches!
          make-cached-dispatch)

  (import (rnrs)
          (prefix (clos private fast-method-cache) fast:)
          (prefix (clos private slow-method-cache) slow:))

  (define (make-cached-dispatch generic handle-cache-miss)
    (fast:make-cached-dispatch generic
     (slow:make-cached-dispatch generic handle-cache-miss)))

  (define (invalidate-method-caches!)
    (fast:invalidate-method-caches!)
    (slow:invalidate-method-caches!))
 
  ) ;; library (clos private method-cache)
