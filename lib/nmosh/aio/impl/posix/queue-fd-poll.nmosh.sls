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
           queue-unregister-fd
           queue-unregister-fd/write
           )
         (import
           (rnrs)
           (yuni core)
           (srfi :8)
           (srfi :42)
           (nmosh pffi posix poll))

(define initial-queue-size 1)

(define* Q (pollfds n io-handler/read io-handler/write ev-queue))

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
      (io-handler/read (make-vector initial-queue-size #f))
      (io-handler/write (make-vector initial-queue-size #f))
      (ev-queue '()))))

(define* (q-resize! (old-queue Q) new-n)
  (let-with old-queue ((old-pollfds pollfds) 
                       (old-n n) 
                       (old-io-handler/read io-handler/read)
                       (old-io-handler/write io-handler/write)
                       (old-ev-queue ev-queue))
    (define (new-obj)
      (let ((p (pollfds-init new-n))
            (new-io-handler/read (make-vector new-n #f)) 
            (new-io-handler/write (make-vector new-n #f)) )
        (let loop ((c 0) (t 0))
          (when (< c old-n)
            (if (poll_get_fd old-pollfds c)
              (begin
                (poll_copy! old-pollfds c p t)
                (vector-set! new-io-handler/read
                             t
                             (vector-ref old-io-handler/read c))
                (vector-set! new-io-handler/write
                             t
                             (vector-ref old-io-handler/write c))
                (loop (+ c 1) (+ t 1)))
              (loop (+ c 1) t))))
        (poll_dispose old-pollfds)
        (values p new-io-handler/read new-io-handler/write)))
    (receive (new-pollfds new-io-handler/read new-io-handler/write) (new-obj)
      (touch! old-queue
        (pollfds new-pollfds)
        (n new-n)
        (io-handler/read new-io-handler/read)
        (io-handler/write new-io-handler/write)
        (ev-queue old-ev-queue)))))

(define* (q-scanfd (Q) fd) ;; => n / #f
  (let-with Q (pollfds n)
    (define (itr c)
      (and (< c n)
           (or (and (eq? (poll_get_fd pollfds c) fd) c)
               (itr (+ c 1)))))
    (itr 0)))

(define* (q-scanfd/write (Q) fd) ;; => n / #f
  (let-with Q (pollfds n io-handler/write)
    (define (itr c)
      (and (< c n)
           (or (and (eq? (poll_get_fd pollfds c) fd) 
                    (vector-ref io-handler/write c)
                    c)
               (itr (+ c 1)))))
    (itr 0)))

(define* (q-scanfree (Q)) ;; => n / #f
  (q-scanfd Q #f))

(define* (q-register-fd! (Q) fd) ;; => n
  (let ((c (q-scanfree Q)))
    (cond
      (c
        (let-with Q (pollfds)
          (poll_set_fd pollfds c fd)
          c))
      (else
        (let-with Q (n)
          (q-resize! Q (+ n 3))
          (q-register-fd! Q fd))))))

(define* (q-scan (Q) r) ;; => (((proc/read . proc/write) fd . events) ...)
  (let-with Q (n pollfds io-handler/read io-handler/write)
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
              (scan (cons (cons (cons (vector-ref io-handler/read c)
                                      (vector-ref io-handler/write c)) 
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
      (let ((proc/read (caaar ev-queue))
            (proc/write (cdaar ev-queue))
            (fd (cadar ev-queue))
            (evt (cddar ev-queue))
            (d (cdr ev-queue)))
        ;(display (list 'DISPATCH fd evt))
        (touch! Q (ev-queue d))
        (case evt
          ((NVAL HUP ERROR)
           ;; warn?
           ;(assert #f)
           (when proc/write (proc/write fd evt))
           (when proc/read (proc/read fd evt)))
          ((READ READ+WRITE)
           (when proc/write (proc/write fd evt))
           (when proc/read (proc/read fd evt)))
          ((WRITE)
           (when proc/write (proc/write fd evt))))))))

(define* (q-set-read-proc (Q) c proc)
  (let-with Q (io-handler/read)
    (vector-set! io-handler/read c proc)))

(define* (q-set-write-proc (Q) c proc)
  (let-with Q (io-handler/write)
    (vector-set! io-handler/write c proc)))

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

(define* (queue-register-fd/read+write (Q) fd read-proc write-proc)
  (let ((c (q-register-fd! Q fd)))
    (q-set-read-proc Q c read-proc)
    (q-set-write-proc Q c write-proc)
    (let-with Q (pollfds)
      (poll_set_pollin pollfds c)
      (poll_set_pollout pollfds c))))

(define* (queue-register-fd/read (Q) fd proc)
  (let ((c (q-register-fd! Q fd)))
    (q-set-read-proc Q c proc)
    (let-with Q (pollfds)
      (poll_set_pollin pollfds c))))

(define* (queue-register-fd/write (Q) fd proc)
  (let ((c (q-register-fd! Q fd)))
    (q-set-write-proc Q c proc)
    (let-with Q (pollfds)
      (poll_set_pollout pollfds c))))

(define* (queue-unregister-fd (Q) fd)
  (let ((c (q-scanfd Q fd)))
    (when c
      (let-with Q (pollfds)
        (poll_set_fd pollfds c #f)
        (q-set-read-proc Q c #f)
        (q-set-write-proc Q c #f)))))

(define* (queue-unregister-fd/write (Q) fd)
  (let ((c (q-scanfd/write Q fd)))
    (unless c
      (assertion-violation 'queue-unregister-fd
                           "fd not found in queue"
                           Q
                           fd))
    (let-with Q (pollfds)
      (poll_set_fd pollfds c #f)
      (q-set-read-proc Q c #f)
      (q-set-write-proc Q c #f))))

)
