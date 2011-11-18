(library (nmosh net msgpack)
         (export make-msgpack-client-socket)
         (import (rnrs)
                 (shorten)
                 (nmosh io tcp0)
                 (yuni binary codec msgpack))
;;  
(define (make-msgpack-client-socket name port recv-callback write-callback error-callback)
  (let ((deser (make-msgpack-deserializer recv-callback)))
    (define (reader fd buf len)
      (if buf
        (deser (cons buf len))
        (error-callback fd)))
    (make-client-socket 
      name port 
      (^[fd] 
        (when write-callback
          (let ()  ;; why ??
            (define (writer obj callback)
              (define (send-step cur)
                (^[fd] 
                  (if (pair? cur)
                    (let ((bv (caar cur))
                          (next (cdr cur)))
                      (socket-write fd bv (send-step next)))
                    (callback))))
              ((send-step (generate-msgpack-buffer obj)) fd)) 
            (write-callback writer))) 
        (start-read fd reader)))))

)
