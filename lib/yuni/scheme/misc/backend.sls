(library (yuni scheme misc backend)
         (export 
           put-datum
           get-datum
           current-output-port
           current-input-port
           car pair? define if)
         (import
           (only (rnrs)
                 put-datum
                 current-output-port
                 current-input-port
                 car
                 pair?
                 if
                 define)
           (yuni scheme refimpl r6rs-reader impl reader)))

