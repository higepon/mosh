(library (nmosh net msgpack)
         (export 
           make-msgpack-client-socket
           mainloop0 ;; temp
           )
         (import (rnrs)
                 (shorten)
                 (mosh socket) ;; temp
                 (yuni binary codec msgpack))
;;  
(define sock) ;; temp
(define deser) ;; temp
(define (make-msgpack-client-socket name port recv-callback) ;; => (^[obj])
  (set! deser (make-msgpack-deserializer recv-callback))
  (let ((s (make-client-socket name port)))
    (set! sock s)
    (lambda (obj)
      (for-each (^e (socket-send s (car e))) 
                (generate-msgpack-buffer obj)))))

(define (mainloop0)
  (let ((bv (socket-recv sock 65536)))
    (when (and  (bytevector? bv) (< 0  (bytevector-length bv))) 
      (display (list 'RECV bv))(newline)
      (deser (cons bv (bytevector-length bv))) 
      (mainloop0))))

)
