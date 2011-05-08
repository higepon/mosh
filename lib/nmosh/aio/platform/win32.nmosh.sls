(library (nmosh aio platform win32)
         (export queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 queue-process-launch
                 
                 ;; I/O objects constructors
                 discard
                 ;std/in
                 ;std/out
                 ;std/err
                 ;file/in
                 ;file/out
                 ;file/overwrite
                 ;file/append
                 ;pipe/push
                 ;pipe/pull
                 pipe/in

                 ;; 
                 ;process-kill
                 )
         (import (rnrs)
                 (srfi :8)
                 (yuni core)
                 (mosh ffi)
                 (primitives object->pointer
                             pointer->object)
                 (nmosh gensym)
                 (nmosh win32 aio))

(define* Q (iocp evt io-objects))

;; I/O objects

(define* io-object
  (type  ;; stdin stdout stderr , file-in file-out ,
         ;; pipe-push, pipe-pull, pipe-in
         ;; process, discard
    filename
    bufsize
    bufstart
    buf
    proc
    param ;; string(file) / ???(socket) / 
    handle ;; win32-handle (if available)
    overlapped ;; overlapped
    Q
    realized?))

(define* (register-io-object (Q) (io-object))
  (define ovl (win32_overlapped_alloc))
  (let-with Q (io-objects)
    (hashtable-set! io-objects 
                    (pointer->integer (object->pointer io-object))
                    io-object))
  (touch! io-object
          (Q Q)
          (overlapped ovl))
  (win32_overlapped_setmydata ovl (object->pointer io-object))
  (let-with io-object (handle type)
    (let-with Q (iocp)
      (win32_iocp_assoc iocp handle (object->pointer io-object)))))


(define* (dispose-io-object (io-object))
  (let-with io-object (Q handle type overlapped)
    (let-with Q (io-objects)
      (hashtable-delete! io-objects (pointer->integer
                                      (object->pointer io-object))))
    (case type
      ((process) 'nothing-to-do)
      (else (win32_handle_close handle)))
    (win32_overlapped_free overlapped)))

;; proc = (^[bv offset count] ...) => bufptr
(define (do-pipe/in proc bufsize)
  (make io-object
        (type 'pipe-in/wait)
        (bufsize bufsize)
        (buf (make-bytevector bufsize))
        (bufstart 0)
        (proc proc)
        (handle #f)
        (realized? #f)))

(define pipe/in
  ;; we ignore bufsize here...
  (case-lambda
    ((proc)
     (do-pipe/in proc 4096))))

(define (io-object? x)
  (is-a? x io-object))

(define (discard)
  (make io-object
        (type 'discard)
        (param #f)
        (handle #f)
        (realized? #t)))

(define (discard? x)
  (and (io-object? x)
       (let-with x (type)
         (eq? type 'discard))))
(define* (realized? (io-object))
  (let-with io-object (realized?) realized?))

(define (make-pipename)
  (string-append
    "\\\\.\\pipe\\"
    (symbol->string (gensym))))

(define (overlapped->io-object ovl)
  (let ((p (win32_overlapped_getmydata ovl)))
    (pointer->object p)))

;; realize/queue 
;; (create HANDLE+OVERLAPPED from io-object and process internally)
(define* (realize/queue/pipe-in (Q) (io-object))
  (let* ((pipename (make-pipename))
         (hpipe (win32_create_named_pipe_async pipename)))
    (touch! io-object
            (handle hpipe)
            (filename pipename)
            (realized? #t))
    (register-io-object Q io-object)
    (let-with io-object (overlapped)
      (win32_wait_named_pipe_async hpipe overlapped))))

(define (compose-process-spec spec)
  (define (needs-escape? str)
    (find char-whitespace? (string->list str)))
  (define (escape str)
    (if (needs-escape? str)
      (string-append "\"" str "\"")
      str))
  (fold-left (lambda (cur e)
               (string-append cur " " (escape e)))
             (car spec)
             (cdr spec)))

(define* (queue-process-launch
           (Q)
           spec start-dir env*
           (in io-object) (out io-object) (err io-object) result-proc)
  (define (pass x)
    (cond
      ((discard? x)
       #f)
      (else ;; FIXME non pipe-in support...
        (realize/queue/pipe-in Q x)
        (let-with x (filename) filename))))
  ;; FIXME: process env*

  (let ((proc (win32_process_redirected_child2 (compose-process-spec spec)
                                               start-dir
                                               (pass in)
                                               (pass out)
                                               (pass err))))
    (let ((procobj (make io-object
                         (Q Q) ;; FIXME: Why ??
                         (type 'process)
                         (proc result-proc)
                         (realized? #t)
                         (handle proc))))
      (register-io-object Q procobj)
      (let-with Q (iocp)
        (let-with procobj (overlapped)
          (win32_process_wait_async proc iocp (integer->pointer 0) 
                                    overlapped))))))

;; realize/external
;; (create HANDLE from io-object and process externally)
;(define* (realize/external (io-object))
;  )


(define* (launch-io-event (io io-object) bytes)
  (let-with io (type buf bufstart bufsize proc param handle overlapped)
    ;(display (list 'launch-io type))(newline)
    (case type
      ((pipe-in/wait)
       (touch! io (type 'pipe-in))
       (win32_handle_read_async handle 0 bufsize buf overlapped))
      ((pipe-in)
       (let ((offset (proc buf bufstart (pointer->integer bytes))))
         (cond 
           (offset (touch! io (bufstart offset))
                   (win32_handle_read_async handle offset (- bufsize offset) buf overlapped))
           ((not offset) ;; close
            (dispose-io-object io)))))
      ((process)
       (proc (pointer->integer bytes))
       (dispose-io-object io)))))

(define* (queue-dispatch (Q))
  (let-with Q (evt)
    (when evt
      (touch! Q (evt #f))
      (let-with evt (ovl bytes)
        (let ((io (overlapped->io-object ovl)))
          (launch-io-event io bytes))))))

(define* event
  (bytes key ovl))

(define (queue) ;; => Q
  (make Q
        ;; current event
        (evt #f)

        ;; IO Completion Port
        (iocp (win32_iocp_create))

        ;; associated IO object
        (io-objects (make-eq-hashtable))))

(define* (queue-dispose (Q))
  (let-with Q (iocp)
    (win32_handle_close iocp)))

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (evt iocp)
    (if evt
      #t ;; there is unprocessed event
      (receive (ret bytes key ovl) (win32_iocp_pop iocp timeout)
        (cond 
          ((= ret 0)
           ;; timeout
           #f)
          (else
            (touch! Q
                    (evt (make event
                               (bytes bytes)
                               (key key)
                               (ovl ovl))))
            #t))))))

(define* (queue-get (Q))
  (let-with Q (evt) 
    (cond 
      (evt (touch! Q (evt #f))
           evt)
      (else #f))))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))

(define* (queue-wait (Q))
  (queue-wait/timeout Q -1))

)
