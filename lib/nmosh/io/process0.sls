(library (nmosh io process0)
         (export start-process)
         (import (rnrs)
                 (srfi :8)
                 (shorten)
                 (nmosh aio platform)
                 (nmosh io core)
                 (nmosh io master-queue))
;;

(define (buffer-callback) ;; => callback proc
  (receive (port proc) (open-bytevector-output-port)
    (let ((cb (^[fd buf len]
                ;(write (list 'LEN: len))(newline)
                (when buf
                  (put-bytevector port buf 0 len)))))
      (values cb proc))))

(define (start-process path args workdir stdin callback)
  ;; callback = ^[code signal stdout stderr]
  (define out-cb #f)
  (define out-proc #f)
  (define err-cb #f)
  (define err-proc #f)
  (define (in-cb writer)
    (cond
      ((bytevector? stdin)
       (writer stdin))))
  (define (cb pid code signal)
    (callback
      (if (= signal 0) code #f) 
      (if (= signal 0) #f signal)
      (and out-proc (out-proc))
      (and err-proc (err-proc))))

  (receive (cb proc) (buffer-callback)
    (set! out-cb cb)
    (set! out-proc proc))
  (receive (cb proc) (buffer-callback)
    (set! err-cb cb)
    (set! err-proc proc))

  (queue-spawn0 nmosh-io-master-queue
                path
                (cons path args)
                #f
                workdir
                in-cb
                out-cb
                err-cb
                cb))

)
