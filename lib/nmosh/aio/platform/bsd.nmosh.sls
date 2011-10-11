(library (nmosh aio platform bsd)
         (export resolve-socketname*
                 discard
                 pipe/in

                 queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch

                 ;; process
                 queue-process-launch
                 )
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (srfi :42)
                 (nmosh pffi posix fd)
                 (nmosh pffi posix socket)
                 (nmosh pffi bsd kqueue))

(define q-depth 16)
(define* Q
  (kqueue queue ev* fd-hash user-hash))

(define* fd-event (fd r/w eof? count))

(define* io-object
  (type ;; pipe/in
    realized?
    fd-pass
    fd-proc ;; only for pipe
    Q
    proc/read
    proc/write ;; only for file/socket fd
    ))

(define* user-event (key))

(define discard-id (list 'discard))

(define (discard? obj)
  (eq? discard-id obj))

(define (discard) discard-id)

(define (pipe/in proc)
  (make io-object
    (type 'pipe/in)
    (realized? #f)
    (Q #f)
    (proc/read proc)))

(define* (realize-io-object (io-object))
  (let-with io-object (type Q proc)
    (define (do-fd-assoc/read fd) (queue-fd-assoc/read! Q fd proc))
    (case type
      ((pipe/in)
       (receive (in out) (fd_pipe)
         (touch! io-object
                 (fd-pass in)
                 (fd-proc out))
         (do-fd-assoc/read out)))
      (else
        (assertion-violation 'realize-io-object
                             "invalid argument"
                             type)))))

(define* (dispose-io-object (io-object))
  (let-with io-object (Q fd-proc)
    (queue-fd-close Q fd-proc)
    (touch! io-object
      (realized? #f)
      (Q #f))))

;; read/write

;; core APIs
(define (queue) ;; => Q
  (make Q
        (kq (kq_create))
        (queue '())
        (ev* (kevent_alloc q-depth))
        (fd-hash (make-eq-hashtable))
        (user-hash (make-eq-hashtable))))

(define* (queue-dispose (Q))
  (let-with Q (kq ev*)
    (fd_close kq)
    (kevent_dispose ev*)))

(define* (queue-fd-close (Q) fd)
  (let-with Q (fd-hash)
    (hashtable-delete! fd-hash (fd->int fd))
    (fd_close fd)))

(define* (queue-register-fd/read! (Q) fd)
  (define ke (kevent_alloc 1))
  (kevent_set_readevent! ke 0 fd)
  (let-with Q (kqueue)
    (kevent_exec kqueue ke 0 1 #f 0 0 -1))
  (kevent_dispose ke))

(define* (queue-fd-assoc/read! (Q) fd key)
  (let-with Q (fd-hash)
    (queue-register-fd/read! Q fd)
    (hashtable-set! fd-hash (fd->int fd) key)))

(define* (extract-ev-item (Q) ev* offset)
  (let ((id (kevent_ident ev* offset)))
    (case (kevent_type ev* offset)
      ((USER)
       (make user-event (key id)))
      ((FILE) ;; includes sockets
       (receive (r/w eof? data) (kevent_decode_fd ev* offset)
         (make fd-event
               (fd id)
               (r/w r/w)
               (eof? eof?)
               (count data))))
      ((ERROR)
       #f)
      (else #f))))

(define (issue-request fd r/w count)
  (if (eq? r/w 'read)
    (let* ((bv (make-bytevector count))
           (r (fd_read fd bv count)))
      (if (<= 0 r)
        (values bv r)))
    (values #f #f)))

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (kq queue ev*)
    (or (pair? queue)
        (let ((r (kevent_exec kq #f #f #f ev* q-depth 0 timeout)))
          (if (< r 0)
            (begin ;; move kq content into local queue
              (touch! Q (queue (list-ec (: i r) (extract-ev-item ev* i))))
              #t)
            #f)))))

(define* (queue-wait (Q))
  (queue-wait/timeout (Q) -1))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))

(define* (dispatch-event (Q) head)
  (let-with (Q) (fd-hash user-hash)
    (cond
      ((is-a? head fd-event)
       (let-with head (fd r/w eof? count)
         (let ((key (hashtable-ref fd-hash (fd->int fd))))
           (receive (data size) (issue-request fd r/w count)
             (key data size 0)))))
      ((is-a? head user-event)
       (let-with head (key)
         (let ((key (hashtable-ref user-hash key)))
           (make user-event
                 (key key)
                 (data #f)
                 (size #f)
                 (offset #f)
                 (event head))))))))

(define* (queue-dispatch (Q))
  (let-with Q (queue)
    (and (not (null? queue))
         (let ((head (car queue))
               (rest (cdr queue)))
           (touch! Q (queue rest))
           (dispatch-event Q head)))))

(define* (queue-process-launch
           (Q)
           spec start-dir env* (in io-object) (out io-object) (err io-object)
           result-proc)
  ;; FIXME

  'ok
  )


;; support API (name resolv)

(define (resolve-socketname** name service mode proto) ;; => (inetname ...)
  (define (addrinfo->list addrinfo)
    (if addrinfo
      (receive (inetname next) (socket_addrinfo_read addrinfo)
        (cons inetname
              (addrinfo->list next)))
      '()))
  (let ((addrinfo (socket_getaddrinfo name service mode proto)))
    (if addrinfo
      (let ((l (addrinfo->list addrinfo)))
        (socket_freeaddrinfo addrinfo) ;; Don't forget to do this..
        l)
      #f)))

(define (resolve-socketname*/4 name service)
  (resolve-socketname** name service 4 0))

(define (resolve-socketname*/6 name service)
  (resolve-socketname** name service 6 0))

(define (resolve-socketname* name service)
  (resolve-socketname** name service 0 0))

(define (resolve-socketname/TCP name service)
  (resolve-socketname** name service 0 1))

(define (resolve-socketname/UDP name service)
  (resolve-socketname** name service 0 2))

(define (resolve-socketname/TCP4 name service)
  (resolve-socketname** name service 4 1))

(define (resolve-socketname/UDP4 name service)
  (resolve-socketname** name service 4 2))

(define (resolve-socketname/TCP6 name service)
  (resolve-socketname** name service 6 1))

(define (resolve-socketname/UDP6 name service)
  (resolve-socketname** name service 6 2))

)
