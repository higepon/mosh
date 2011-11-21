(library (nmosh net msgpack)
         (export make-msgpack-client-socket
                 make-msgpack-server-socket
                 start-msgpack-talker)
         (import (rnrs)
                 (shorten)
                 (nmosh io tcp0)
                 (srfi :39)
                 (yuni binary codec msgpack))

;;
(define (start-msgpack-talker fd recv-callback write-callback error-callback)
  ;; recv-callback = (^[obj] ...)
  ;; write-callback = (^[procedure] ...)/#f
  ;; error-callback = (^[fd] ...)
  (let ((deser (make-msgpack-deserializer recv-callback)))
    (define (reader fd buf len)
      (if buf
        (deser (cons buf len))
        (error-callback fd)))

    (when write-callback
      (let ()  ;; why ??
        (define in-progress? #f)
        (define queue '())
        (define seg/queue '())
        (define seg/callback '())
        (define (writer obj callback)
          (define (send-callback fd)
            (cond
              ((pair? seg/queue)
               (socket-write fd (caar seg/queue) send-callback)
               (set! seg/queue (cdr seg/queue)))
              (else
                ;; Callback
                (seg/callback)
                (cond
                  ((pair? queue)
                   (set! seg/queue (generate-msgpack-buffer (caar queue))) 
                   (set! seg/callback (cdar queue)) 
                   (set! queue (cdr queue)) 
                   ;; Kick next
                   (send-callback fd))
                  (else 
                    (set! in-progress? #f)
                    (set! seg/callback #f))))))

          (cond
            (in-progress?
             (set! queue (append queue (list (cons 
                                               obj
                                               callback)))))
            (else
              (set! in-progress? #t)
              (set! seg/queue (generate-msgpack-buffer obj))
              (set! seg/callback callback)
              (send-callback fd))))

        (write-callback writer))) 
    (start-read fd reader)))

;;  
(define (make-msgpack-server-socket name port accept-callback error-callback)
  ;; accept-callback = (^[fd inetname] ...)
  ;; error-callback = (^[fd] ...)
  (make-server-socket
    name port
    (^[fd]
      (socket-accept fd (^[fd inetname]
                          (accept-callback fd inetname))))))

;;  
(define (make-msgpack-client-socket name port recv-callback write-callback error-callback)

  (make-client-socket 
    name port 
    (^[fd] 
      (start-msgpack-talker fd recv-callback write-callback error-callback))))

)
