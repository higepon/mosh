(library (nmosh aio platform cygwin)
         (export queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 
                 queue-read0
                 queue-write0
                 resolve-socketname/4
                 resolve-socketname/6
                 queue-listen
                 queue-accept)
         (import (nmosh aio impl posix posix-socket)
                 (nmosh aio impl posix queue-fd-poll)))
