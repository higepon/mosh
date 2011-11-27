(import (rnrs)
        (shorten)
        (match)
        (nmosh io core)
        (nmosh net msgpack)
        (srfi :42)
        (yuni async))

(define (err obj) (assert #f))

(define count 0)
(define vcount 0)
(define latest #f)

(define jobs 0)

(define (gen)
  (set! count (+ count 1))
  (when (= (mod count 100) 0)
    (display (list 'SEND: count 'VERIFY: vcount (list latest) 'JOBS: jobs))
    (newline))
  `(,count ,(number->string count)))

(define (verify obj)
  (set! vcount (+ vcount 1))
  (match obj
         ((num dat)
          (assert (= num (string->number (utf8->string dat))))
          (set! latest num))))

(define (start-writer output)
  (define (loop)
    (define callobj (list-ec (: i 1000) (list (gen))))
    (seq (=> apply/async 60 output callobj => lis)
         (let ((v (list->vector lis)))
           (do-ec (: i 1000)
                  (unless (vector-ref v i)
                    (assertion-violation 'loop
                                         "send error"
                                         (vector-ref v i)
                                         i))))
         (loop)))
  (loop))

(make-msgpack-client-socket
  "127.0.0.1"
  9988
  (^[obj]
    (verify obj))
  (^[proc]
    (start-writer proc))
  err)

(io-dispatch-loop)
