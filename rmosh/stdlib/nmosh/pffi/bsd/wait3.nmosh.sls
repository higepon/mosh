(library (nmosh pffi bsd wait3)
         (export try_wait3)
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (prefix
                   (nmosh stubs wait3)
                   stub:))

(define rusage-size #f)

(define (try_wait3) ;; => (pid code signal rusage)
  (unless rusage-size
    (set! rusage-size (stub:size_rusage)))
  (let ((rusage (make-bytevector rusage-size))
        (code (make-int-box))
        (signal (make-int-box)))
    (let ((r (stub:try_wait3 code signal rusage)))
      (case r
        ((0 -1) (values r #f #f #f))
        (else (values r 
                      (int-box-ref code) 
                      (int-box-ref signal) 
                      rusage))))))

)
