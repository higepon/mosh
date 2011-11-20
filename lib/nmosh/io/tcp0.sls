(library (nmosh io tcp0)
         (export
           make-client-socket 
           make-server-socket
           start-read
           socket-write
           socket-accept
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

(define (make-server-socket name port callback)
  (define (do-listen inetname)
    (queue-listen nmosh-io-master-queue (car inetname) callback))

  ;; callback = (^[fd])
  (resolve-socketname/4 nmosh-io-master-queue
                        name port
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
  ;; callback = (^[fd])
  (queue-write0 nmosh-io-master-queue fd data callback))

)

