(library (nmosh aio platform win32)
         (export ;; core functions
                 queue
                 queue-dispose
                 queue-wait/timeout
                 queue-wait
                 queue-peek
                 queue-dispatch
                 

                 queue-write0
                 queue-read0
                 queue-close0
                 queue-spawn0
                 resolve-socketname/4
                 resolve-socketname/6
                 queue-listen
                 queue-accept
                 queue-connect
                 ;; I/O objects constructors
                 ;discard
                 ;std/in
                 ;std/out
                 ;std/err
                 ;file/in
                 ;file/out
                 ;file/overwrite
                 ;file/append
                 ;pipe/push
                 ;pipe/pull
                 ;pipe/in

                 ;; process
                 ;queue-process-launch
                 ;process-kill

                 ;; GUI related
                 ;queue-create-window
                 ;dispose-io-object
                 )
         (import (rnrs)
                 (nmosh aio impl win32 handle-ops)
                 (nmosh aio impl win32 queue-iocp)
                 (nmosh aio impl win32 socket-ops)
                 (nmosh aio impl win32 process-ops)
                 )



)
