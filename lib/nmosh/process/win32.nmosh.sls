(library (nmosh process win32)
         (export process-launch/win32 
                 process-wait/win32
                 process-result/win32
                 process-stdout/win32
                 process-stderr/win32)
         (import (primitives %win32_process_redirected_child
                             %win32_process_wait)
                 (srfi :8)
                 (rnrs)
                 (mosh ffi)
                 (nmosh win32 handle)
                 (nmosh win32 util))


(define (handle-read-all h)
  (define BUFSIZ (* 1024 1024 1))
  (define (output buf* buf-last buf-last-size)
    (define buflength
      (+ buf-last-size
         (fold-left (lambda (cur e)
                      (+ cur (bytevector-length e)))
                    0 buf*)))
    (define out (make-bytevector buflength))
    (define (itr pos cur)
      (cond
        ((pair? cur)
         (let ((a (car cur))
               (rest (cdr cur)))
           (bytevector-copy! a 0 out pos (bytevector-length a))
           (itr (+ pos (bytevector-length a)) rest)))
        (else
          (bytevector-copy! buf-last 0 out 0 buf-last-size))))
    (itr 0 buf*)
    out)
  (define (itr cur)
    (let ((bv (make-bytevector BUFSIZ)))
      (let ((res (win32-handle-read h bv BUFSIZ)))
        (cond
          ((and res (= BUFSIZ res))
           (itr (cons bv cur)))
          (res
            (output (reverse cur) bv res))
          (else
            (output (reverse cur) (make-bytevector 0) 0))))))
  (itr '()))

(define process-status-sym '*nmosh-process-win32-status*)

(define (compose-exec exec-path arg*)
  (fold-left (lambda (cur e)
               (string-append cur " " e))
             exec-path
             arg*))

(define (process-wait/win32 process-status)
  (define (close-if-handle x)
    (when (win32-handle? x)
      (win32-handle-close x)))
  (define (read-all-if-handle x)
    (if (win32-handle? x)
      (handle-read-all x)
      #f))

  (unless (eq? process-status-sym (vector-ref process-status 0))
    (assertion-violation 'process-wait/win32 "invalid argument" process-status))
  (let ((h (vector-ref process-status 2)))
    (let ((res (%win32_process_wait h)))
      (let ((stdin (vector-ref process-status 3))
            (stdout (vector-ref process-status 4))
            (stderr (vector-ref process-status 5)))
        (define out (read-all-if-handle stdout))
        (define err (read-all-if-handle stderr))
        (win32-handle-close h)
        (close-if-handle stdin)
        (close-if-handle stdout)
        (close-if-handle stderr)
        (vector-set! process-status 1 res)
        (vector-set! process-status 2 #f)
        (vector-set! process-status 3 #f)
        (vector-set! process-status 4 out)
        (vector-set! process-status 5 err))
      res)))

(define (process-result/win32 process-status)
  (unless (eq? process-status-sym (vector-ref process-status 0))
    (assertion-violation 'process-wait/win32 "invalid argument" process-status))
  (let ((res (vector-ref process-status 1)))
    (if (integer? res) res #f)))

(define (process-stdout/win32 process-status)
  (unless (eq? process-status-sym (vector-ref process-status 0))
    (assertion-violation 'process-wait/win32 "invalid argument" process-status))
  (vector-ref process-status 4))

(define (process-stderr/win32 process-status)
  (unless (eq? process-status-sym (vector-ref process-status 0))
    (assertion-violation 'process-wait/win32 "invalid argument" process-status))
  (vector-ref process-status 5))

(define (process-launch/win32 exec-path startup-path arg* std-in std-out std-err)
  (define (portarg x)
    (when (port? x)
      (assertion-violation 'process-launch/win32 "port argument is not supported yet.."))
    (when (bytevector? x)
      (assertion-violation 'process-launch/win32 "bytevector argument is not supported yet.."))
    (when (string? x)
      (assertion-violation 'process-launch/win32 "string argument is not supported yet.."))
    (if x
      (receive (write read) (win32-handle-pipe)
        (cons read write))
      (cons #f #f)))
  (define (pass-handle x)
    (if x x null-pointer))
  (let ((curdir (if startup-path
                  (string->utf16-bv startup-path)
                  (make-bytevector 0)))
        (exec (string->utf16-bv (compose-exec exec-path arg*)))
        (stdin (portarg std-in))
        (stdout (portarg std-out))
        (stderr (portarg std-err)))
    (let ((my-stdin (cdr stdin))
          (child-stdin (pass-handle (car stdin)))
          (my-stdout (car stdout))
          (child-stdout (pass-handle (cdr stdout)))
          (my-stderr (car stderr))
          (child-stderr (pass-handle (cdr stderr))))
      (define h 
        (%win32_process_redirected_child
          exec curdir child-stdin child-stdout child-stderr))
      (unless h
        ;; Free pipes here..
        (assertion-violation 'process-launch/win32 "error"))
      (vector process-status-sym 'running h my-stdin my-stdout my-stderr))))

)
