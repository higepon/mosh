(library (nmosh aio impl win32 queue-iocp)
         (export
           queue
           queue-dispose
           queue-wait
           queue-wait/timeout
           queue-dispatch
           queue-peek

           queue-invoke-ffithread

           queue-register-handle
           queue-unregister-handle

           queue-window-register
           queue-window-destroy
           ;; internal
           Q
           )
         (import (rnrs)
                 (srfi :8)
                 (nmosh pffi interface)
                 (nmosh pffi win32 aio)
                 (nmosh pffi win32 misc)
                 (nmosh pffi win32 gui)
                 (yuni core))


(define* event (bytes key ovl))
(define* Q (iocp evt handles ovls))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))
(define* (queue-wait (Q))
  (queue-wait/timeout Q -1))

(define (generic-callback bytes ovl key)
  (let ((callback (pointer->object (win32_overlapped_getmydata ovl))))
    ;(display (list 'iocp-callback-for ovl callback))(newline)
    (unless (procedure? callback)
      (assertion-violation 'generic-callback
                           "Invalid object set with ovl"
                           callback))
    (let ((err (win32_overlapped_geterror ovl)))
      ;(display (list 'calling-back err))(newline)
      (if (= 0 err)
        (callback err bytes ovl key)
        (callback err #f ovl #f)))))

(define* (queue-register-handle (Q) handle key-obj) ;; => boolean
  (define key (pointer->integer (object->pointer key-obj)))
  (let-with Q (iocp handles)
    (hashtable-set! handles key handle)
    ;(display (list 'REGISTER-handle (handle->pointer handle) key))(newline)
    (let ((r (win32_iocp_assoc iocp handle (integer->pointer key))))
      (= r 0))))

(define* (queue-unregister-handle (Q) handle key-obj)
  (define key (pointer->integer (object->pointer key-obj)))
  ;;(display (list 'UNREGISTER-handle (handle->pointer handle) key))(newline)
  (let-with Q (handles)
    (hashtable-delete! handles key)))

(define (call-callback evt)
  (let-with evt (bytes key ovl)
    (generic-callback bytes 
                      ovl
                      key)))

(define* (queue-dispatch (Q))
  (let-with Q (evt)
    (and evt
         (touch! Q (evt #f))
         (call-callback evt))))

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (evt iocp)
    (or evt ; if evt = true, thereis no need to wait
        (receive (ret bytes key ovl) (win32_iocp_pop iocp timeout)
          (cond
            ((= 0 (pointer->integer ovl))
             ;(display "something wrong(IOCP)..\n")
             #f)
            (else
              (touch! Q
                (evt (make event
                           (bytes (if (= ret 0) #f bytes))
                           (key key)
                           (ovl ovl))))
              #t))))))

(define* (queue-dispose (Q))
  (let-with Q (iocp)
    ;; FIXME: dispose all registered handle
    (win32_handle_close iocp)
    (touch! Q (iocp #f))))

(define* (queue-invoke-ffithread (Q) func in0 in1 cb)
  (define (callback err bytes ovl key)
    ;; key = out0
    ;; bytes = out1
    (cb (pointer->integer key) (pointer->integer bytes)))
  (define ovl (win32_overlapped_alloc))
  (win32_overlapped_setmydata ovl (object->pointer callback))
  (win32_invoke_ffithread (handle->pointer (~ Q 'iocp))
                          func
                          (integer->pointer in0)
                          (integer->pointer in1)
                          ovl))

(define (queue) ;; => Q
  (make Q
        (evt #f)
        (iocp (win32_iocp_create))
        (handles (make-eq-hashtable))))


(define* (queue-window-register (Q) w callback) ;; => hwnd
  (win32_overlapped_setmydata w (object->pointer callback))
  (let* ((hwnd (win32_window_create (~ Q 'iocp) w)))
    hwnd))

(define* (queue-window-destroy (Q) hwnd)
  (win32_window_destroy hwnd)
  ;; FIXME: Unregister it?
  )

(win32_registerwindowclass)
)

