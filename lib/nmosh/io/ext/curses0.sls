(library (nmosh io ext curses0)
         (export curses-acquire)
         (import (rnrs)
                 (nmosh io core)
                 (nmosh io master-queue)
                 (nmosh aio platform)
                 (nmosh pffi ext curses))

(define keyloop (mcur_getkeyloop))
(define key-resize (mcur_key_resize))
(define key-mouse (mcur_key_mouse))

(define (curses-acquire callback)
  (define (cb ret chr)
    (callback 'key chr #f #f #f))
  (mcur_acquire)
  (queue-invoke-ffithread nmosh-io-master-queue
                          keyloop
                          0
                          0
                          cb))

)
