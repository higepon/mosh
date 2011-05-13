(library (nmosh aio platform win32)
         (export ;; core functions
                 queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 
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

                 ;; process
                 queue-process-launch
                 ;process-kill

                 ;; GUI related
                 queue-create-window
                 )
         (import (rnrs)
                 (srfi :8)
                 (yuni core)
                 (mosh ffi)
                 (primitives object->pointer
                             pointer->object)
                 (nmosh gensym)
                 (nmosh win32 gui)
                 (nmosh win32 aio))

(define* Q (iocp evt io-objects))

;; I/O objects

(define* io-object
  (type  ;; stdin stdout stderr , file-in file-out ,
         ;; pipe-push, pipe-pull, pipe-in
         ;; process, discard, window
    filename
    bufsize
    bufstart
    buf
    proc
    param ;; string(file) / ???(socket) / io-object list(process)
    handle ;; win32-handle (if available)
    overlapped ;; overlapped
    Q
    realized?))

;; debug
(define count 0)
(define (connection-count) count)
(define (inc-count) (set! count (+ 1 count)))
(define (dec-count) (set! count (- count 1)))

;; FIXME: won't work with (Q)...
(define* (register-io-object (queue Q) (io-object))
  (define ovl 
    (let-with io-object (type)
      (case type
        ((window) (win32_window_alloc))
        (else (win32_overlapped_alloc)))))
  (win32_overlapped_setmydata ovl (integer->pointer #xdeadbeef))
  (let-with queue (io-objects)
    (hashtable-set! io-objects 
                    (pointer->integer (object->pointer io-object))
                    io-object))
  (touch! io-object
          (Q queue)
          (overlapped ovl))
  (win32_overlapped_setmydata ovl (object->pointer io-object))
  (let-with io-object (handle type)
    (case type
      ((window)
       (let-with queue (iocp)
         (win32_window_create iocp ovl)))
      ((pipe-in/wait process)
       (let-with queue (iocp)
         (win32_iocp_assoc iocp handle (object->pointer io-object))))
      (else
        (assertion-violation 'register-io-object
                             "invalid io-object type"
                             type)))))

(define* (dispose-io-object (io-object))
  (let-with io-object (Q handle type overlapped)
    (inc-count)
    (when overlapped ;; to prevent double dispose
      (dec-count)
      (when Q
        (let-with Q (io-objects)
          (hashtable-delete! io-objects (pointer->integer
                                          (object->pointer io-object)))))
      (case type
        ((process window) 'nothing-to-do)
        (else (win32_handle_close handle)))
      (win32_overlapped_free overlapped)
      (touch! io-object (overlapped #f)))))

;; proc = (^[bv offset count] ...) => bufptr
(define (do-pipe/in proc bufsize)
  (make io-object
        (Q #f)
        (type 'pipe-in/wait)
        (bufsize bufsize)
        (buf (make-bytevector bufsize))
        (bufstart 0)
        (proc proc)
        (handle #f)
        (overlapped #f)
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

(define* (realized? (io-object))
  (let-with io-object (realized?) realized?))

(define (make-pipename)
  (string-append
    "\\\\.\\pipe\\"
    (symbol->string (gensym))))

(define (overlapped->io-object ovl)
  (let ((p (win32_overlapped_getmydata ovl)))
    (case (pointer->integer p)
      ((0)
       #f)
      ((#xdeadbeef) ;; trap uninitialized OVPAIR
       (error 'beef "dead beef")
       #f)
      (else (pointer->object p)))))

;; realize/queue 
;; (create HANDLE+OVERLAPPED from io-object and process internally)
(define* (realize/queue/pipe-in (Q) (io-object))
  (let* ((pipename (make-pipename))
         (hpipe (win32_create_named_pipe_async pipename)))
    (touch! io-object
            (handle hpipe)
            (overlapped #f)
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

(define class-registered? #f)
(define* (queue-create-window (Q) handler)
  (unless class-registered?
    (win32_registerwindowclass)
    (set! class-registered? #t))
  (let ((win (make io-object
                   (Q Q)
                   (type 'window)
                   (param #f)
                   (overlapped #f)
                   (realized? #t)
                   (proc handler)
                   (handle #f))))
    (register-io-object Q win)
    win))

(define* (queue-process-launch
           (Q)
           spec start-dir env*
           (in io-object) (out io-object) (err io-object) result-proc)
  (define passed-objects '())
  (define (pass x)
    (case (and (io-object? x)
               (let-with x (type) type))
      ((discard)
       #f)
      ((pipe-in/wait) ;; FIXME non pipe-in support...
       (set! passed-objects (cons x passed-objects))
       (realize/queue/pipe-in Q x)
       (let-with x (filename) filename))
      (else
        (assertion-violation 'pueue-process-launch
                             "invalid argument"
                             x))))
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
                         (param passed-objects)
                         (overlapped #f)
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


(define* (launch-io-event (io io-object) key bytes)
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
                   (win32_handle_read_async 
                     handle offset (- bufsize offset) buf overlapped))
           ((not offset) ;; close
            (dispose-io-object io)))))
      ((process)
       (for-each (lambda (e) 
                   (dispose-io-object e))
                 param)
       (dispose-io-object io)
       (proc (pointer->integer bytes)))
      ((window)
       (proc overlapped
             (pointer->integer key)
             (pointer->integer bytes)))
      (else
        (display "WARNING: unrecognized event type: " (current-error-port))
        (display type (current-error-port))
        (newline (current-error-port))))))

(define (warn-disappear-event ovl)
  (display "WARNING: event had been disappeared at " (current-error-port))
  (display ovl (current-error-port))
  (newline (current-error-port)))

(define* (queue-dispatch (Q))
  (let-with Q (evt)
    (when evt
      (touch! Q (evt #f))
      (let-with evt (ovl key bytes)
        (let ((io (overlapped->io-object ovl)))
          (if io 
            (launch-io-event io key bytes)
            (warn-disappear-event ovl)))))))

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
