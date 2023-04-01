(library (nmosh io tcp0)
         (export
           make-client-socket 
           make-server-socket
           start-read
           socket-write
           socket-accept
           socket-close
           )
         (import (rnrs)
                 (shorten)
                 (nmosh aio platform)
                 (nmosh io core)
                 (nmosh io master-queue))

;;

(define (socket-accept fd callback)
  ;; callback = (^[fd inetname])
  (queue-accept nmosh-io-master-queue fd callback))

(define (make-server-socket name port socket-callback result-callback)
  ;; result-callback = (^[success? inetname/message])
  ;; socket-callback = (^[fd])
  (define (do-listen inetname)
    (cond ((pair? inetname)
           (let ((lname (queue-listen nmosh-io-master-queue (car inetname) 
                                      socket-callback)))
             (if lname
               (result-callback #t lname)
               (result-callback #f "Listen failed"))) )
          (else
            (result-callback #f "Invalid name"))))

  (resolve-socketname/4 nmosh-io-master-queue
                        name (or port 0)
                        do-listen))

(define (make-client-socket name port callback)
  ;; callback = (^[fd])
  (define (do-connect inetname)
    (queue-connect nmosh-io-master-queue (car inetname) callback)  )
  (resolve-socketname/4 nmosh-io-master-queue
                        name port
                        do-connect))

(define (start-read fd callback)
  ;; callback = (^[fd buf len])
  (queue-read0 nmosh-io-master-queue fd callback))

(define (socket-write fd data callback)
  ;; callback = (^[fd/#f])
  (queue-write0 nmosh-io-master-queue fd data callback))

(define (socket-close fd)
  (queue-close0 nmosh-io-master-queue fd))

)

