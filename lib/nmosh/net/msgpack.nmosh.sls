(library (nmosh net msgpack)
         (export make-msgpack-client-socket
                 make-msgpack-server-socket
                 start-msgpack-talker)
         (import (rnrs)
                 (shorten)
                 (nmosh io tcp0)
                 (yuni binary codec msgpack))

;;
(define (start-msgpack-talker fd recv-callback write-callback error-callback)
  ;; recv-callback = (^[obj] ...)
  ;; write-callback = (^[procedure] ...)
  ;; error-callback = (^[fd] ...)
  (let ((deser (make-msgpack-deserializer recv-callback)))
    (define (reader fd buf len)
      (if buf
        (deser (cons buf len))
        (error-callback fd)))

    (when write-callback
      (let ()  ;; why ??
        (define (writer obj callback)
          (define in-progress? #f)
          (define queue '())
          (define (send-step callback cur)
            (^[fd] 
              (cond 
                ((pair? cur) 
                 (let ((bv (caar cur))
                       (next (cdr cur)))
                   (socket-write fd bv (send-step callback next)))) 
                (else
                  (callback) 
                  (next fd)))))

          (define (first-step callback sendobj)
            ((send-step callback (generate-msgpack-buffer sendobj)) fd))  

          (define (next fd)
            (cond
              (in-progress?
                (cond
                  ((pair? queue)
                   (let ((callback (cdar queue))
                         (sendobj (caar queue)))
                     (first-step callback sendobj))
                   (set! queue (cdr queue))
                   (unless (pair? queue)
                     (set! in-progress? #f)))
                  (else (set! in-progress? #f))))))
          (cond
            (in-progress?
              (set! queue (append queue (list (cons 
                                                (generate-msgpack-buffer obj)
                                                callback)))))
            (else
              (set! in-progress? #t)
              (first-step callback obj))))

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
