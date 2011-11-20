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
        (define in-progress? (make-parameter #f))
        (define queue (make-parameter '()))
        (define (writer obj callback)
          (define (send-step callback cur)
            (^[fd] 
              (cond 
                ((pair? cur) 
                 (let ((bv (caar cur))
                       (next (cdr cur)))
                   (socket-write fd bv 
                                 (^[fd] 
                                   ((send-step callback next) fd))))) 
                (else
                  ;; Enqueue next object
                  (when (in-progress?)
                    (cond
                      ((pair? queue)
                       (let ((callback (cdar (queue)))
                             (sendobj (caar (queue))))
                         ;(write (list 'DeQ: (length (queue)) obj callback))(newline)
                         (first-step callback sendobj))
                       (queue (cdr (queue)))
                       (unless (pair? (queue))
                         (in-progress? #f)))
                      (else (in-progress? #f))))

                  ;; callback
                  (callback)))))

          (define (first-step callback sendobj)
            (let ((l (generate-msgpack-buffer sendobj)))
              ((send-step callback l) fd)))  

          (cond
            ((in-progress?)
             ;(write (list 'EnQ: (length (queue)) obj callback))(newline)
              (queue (append (queue) (list (cons 
                                             obj
                                             callback)))))
            (else
              (in-progress? #t)
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
