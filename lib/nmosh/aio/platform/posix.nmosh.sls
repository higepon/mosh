(library (nmosh aio platform posix)
         (export queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 
                 queue-read0
                 queue-write0
                 queue-close0
                 resolve-socketname/4
                 resolve-socketname/6
                 queue-listen
                 queue-accept
                 queue-connect

                 queue-spawn0
                 
                 queue-process-launch
                 discard
                 pipe/in
                 )
         (import (nmosh aio impl posix posix-socket)
                 (nmosh aio impl posix fd-ops)
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh aio impl cygwin process-ops)
                 (rnrs))
         
(define (queue-process-launch . x)
  (assertion-violation 'queue-process-launch
                       "unimplemented"))

(define (discard . x)
  (assertion-violation 'discard
                       "unimplemented"))
         
(define (pipe/in . x)
  (assertion-violation 'pipe/in
                       "unimplemented"))
         
)
