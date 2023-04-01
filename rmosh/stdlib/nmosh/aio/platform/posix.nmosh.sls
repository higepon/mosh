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

                 queue-invoke-ffithread
                 )
         (import (nmosh aio impl posix posix-socket)
                 (nmosh aio impl posix fd-ops)
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh aio impl posix thread-ops)
                 (nmosh aio impl cygwin process-ops))
         
         
)
