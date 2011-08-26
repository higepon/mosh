(library (nmosh aio impl posix queue-fd-poll)
         (export 
           queue
           queue-dispose
           queue-wait/timeout
           queue-wait
           queue-dispatch
           queue-peek
           
           ;; fd-based queue only procedure
           ;;  provided events are :
           ;;  READ, WRITE, READ+WRITE
           ;;  (error conditions: HUP, NVAL, ERROR)
           queue-register-fd/read
           queue-register-fd/write
           queue-register-fd/read+write
           queue-unregister-fd)
         (import
           (rnrs)
           (yuni core)
           (srfi :8)
           (srfi :42)
           (nmosh pffi posix poll))

(define initial-queue-size 1)

(define* Q (pollfds n io-objects ev-queue))

(define* (queue-dispose (Q))
  (let-with Q (pollfds)
    (poll_dispose pollfds)))

(define (pollfds-init n)
  (let ((p (poll_alloc n)))
    (do-ec (: i n)
           (poll_set_fd p i #f))
    p))

(define (queue) ;; => Q
  (let ((p (pollfds-init initial-queue-size)))
    (make Q
      (pollfds p)
      (n initial-queue-size)
      (io-objects (make-vector initial-queue-size #f))
      (ev-queue '()))))

(define* (q-resize! (old-queue Q) new-n)
  (let-with old-queue ((old-pollfds pollfds) 
                       (old-n n) 
                       (old-io-objects io-objects)
                       (old-ev-queue ev-queue))
    (define (new-obj)
      (let ((p (pollfds-init new-n))
            (new-io-objects (make-vector new-n #f)))
        (let loop ((c 0) (t 0))
          (when (< c old-n)
            (if (poll_get_fd old-pollfds c)
              (begin
                (poll_copy! old-pollfds c p t)
                (vector-set! new-io-objects
                             t
                             (vector-ref old-io-objects c))
                (loop (+ c 1) (+ t 1)))
              (loop (+ c 1) t))))
        (poll_dispose old-pollfds)
        (values p new-io-objects)))
    (receive (new-pollfds new-io-objects) (new-obj)
      (touch! old-queue
        (pollfds new-pollfds)
        (n new-n)
        (io-objects new-io-objects)
        (ev-queue old-ev-queue)))))

(define* (q-scanfd (Q) fd) ;; => n / #f
  (let-with Q (pollfds n)
    (define (itr c)
      (and (< c n)
           (or (and (eq? (poll_get_fd pollfds c) fd) c)
               (itr (+ c 1)))))
    (itr 0)))

(define* (q-scanfree (Q)) ;; => n / #f
  (q-scanfd Q #f))

(define* (q-register-fd! (Q) fd proc) ;; => n
  (let ((c (q-scanfree Q)))
    (cond
      (c
        (let-with Q (pollfds io-objects)
          (vector-set! io-objects c proc)
          (poll_set_fd pollfds c fd)
          c))
      (else
        (let-with Q (n)
          (q-resize! Q (+ n 3))
          (q-register-fd! Q fd proc))))))

(define* (q-scan (Q) r) ;; => ((proc fd . events) ...)
  (let-with Q (n pollfds io-objects)
    (define (scan cur c rest)
      (if (and (> rest 0) (< c n))
        (let ((nval? (poll_get_pollnval pollfds c))
              (hup? (poll_get_pollhup pollfds c))
              (err? (poll_get_pollerr pollfds c))
              (in? (poll_get_pollin pollfds c))
              (out? (poll_get_pollout pollfds c)))
          (let ((n (cond (nval? 'NVAL)
                         (hup? 'HUP)
                         (err? 'ERROR)
                         ((and in? out?) 'READ+WRITE)
                         (in? 'READ)
                         (out? 'WRITE)
                         (else #f))))
            (if n
              (scan (cons (cons (vector-ref io-objects c)
                                (cons (poll_get_fd pollfds c)
                                      n))
                          cur)
                    (+ c 1)
                    (- rest 1))
              (scan cur (+ c 1) rest))))
        cur))
    (scan '() 0 r)))

(define* (queue-dispatch (Q))
  (let-with Q (ev-queue)
    (if (pair? ev-queue)
      (let ((proc (caar ev-queue))
            (fd (cadar ev-queue))
            (evt (cddar ev-queue))
            (d (cdr ev-queue)))
        (touch! Q (ev-queue d))
        (proc fd evt)))))

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (ev-queue pollfds n)
    (unless (pair? ev-queue)
      (let ((r (poll_exec pollfds n timeout)))
        (let ((p (q-scan Q r)))
          (when (pair? p)
            (touch! Q (ev-queue p))))))))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))

(define* (queue-wait (Q))
  (queue-wait/timeout Q -1))

(define* (queue-register-fd/read+write (Q) fd proc)
  (let ((c (q-register-fd! Q fd proc)))
    (let-with Q (pollfds)
      (poll_set_pollin pollfds c)
      (poll_set_pollout pollfds c))))

(define* (queue-register-fd/read (Q) fd proc)
  (let ((c (q-register-fd! Q fd proc)))
    (let-with Q (pollfds)
      (poll_set_pollin pollfds c))))

(define* (queue-register-fd/write (Q) fd proc)
  (let ((c (q-register-fd! Q fd proc)))
    (let-with Q (pollfds)
      (poll_set_pollout pollfds c))))

(define* (queue-unregister-fd (Q) fd)
  (let ((c (q-scanfd Q fd)))
    (unless c
      (assertion-violation 'queue-unregister-fd
                           "fd not found in queue"
                           Q
                           fd))
    (let-with Q (pollfds io-objects)
      (poll_set_fd pollfds c #f)
      (vector-set! io-objects c #f))))

)
