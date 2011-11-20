(library (nmosh aio impl win32 queue-iocp)
         (export
           queue
           queue-dispose
           queue-wait
           queue-wait/timeout
           queue-dispatch
           queue-peek

           ;queue-create+register-window
           queue-register-handle

           ;; internal
           Q
           )
         (import (rnrs)
                 (srfi :8)
                 (nmosh pffi interface)
                 (nmosh pffi win32 aio)
                 (yuni core))


(define* event (bytes key ovl))
(define* Q (iocp evt handles callbacks ovls))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))
(define* (queue-wait (Q))
  (queue-wait/timeout Q -1))

(define* (queue-register-handle (Q) handle cb key-obj)
  (define key (object->pointer key-obj))
  (let-with Q (iocp handles callbacks)
    ;; FIXME: Log handles here..
    (hashtable-set! callbacks key-obj cb)
    (hashtable-set! handles key-obj handle)
    (win32_iocp_assoc iocp handle key)))

(define (call-callback ht evt)
  (let-with evt (bytes key ovl)
    (let ((cb (hashtable-ref ht key)))
      (and (procedure? cb)
           (cb bytes 
               ovl
               (pointer->object key))))))

(define* (queue-dispatch (Q))
  (let-with Q (evt)
    (and evt
         (touch! Q (evt #f))
         (let-with Q (callbacks)
           (call-callback callbacks evt)))))

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (evt iocp)
    (or evt ; if evt = true, thereis no need to wait
        (receive (ret bytes key ovl) (win32_iocp_pop iocp timeout)
          (cond
            ((= ret 0) #f)
            (else
              (touch! Q
                (evt (make event
                           (bytes bytes)
                           (key key)
                           (ovl ovl))))
              #t))))))

(define* (queue-dispose (Q))
  (let-with Q (iocp)
    ;; FIXME: dispose all registered handle
    (win32_handle_close iocp)
    (touch! Q (iocp #f))))

(define (queue) ;; => Q
  (make Q
        (evt #f)
        (iocp (win32_iocp_create))
        (handles (make-eq-hashtable))
        (callbacks (make-eq-hashtable))))

)

