(library (nmosh aio impl posix queue-poll)
         (export 
           queue
           queue-dispose
           queue-wait/timeout
           queue-wait
           queue-dispatch
           queue-peek)
         (import
           (rnrs)
           (yuni core)
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
      (io-objects (make-vector initial-queue-size)))))

(define* (q-scan (Q) r)
  'fixme)

(define* (queue-dispatch (Q))
  'fixme)

(define* (queue-wait/timeout (Q) timeout)
  (let-with Q (pollfds n)
    (let ((r (poll_exec pollfds n timeout)))
      (q-scan Q r))))

(define* (queue-peek (Q))
  (queue-wait/timeout Q 0))

(define* (queue-wait (Q))
  (queue-wait/timeout Q -1))

)
