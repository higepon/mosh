(library (nmosh aio impl win32 process-ops)
         (export queue-spawn0)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (yuni core)
                 (nmosh gensym)
                 (nmosh pffi interface)
                 (nmosh pffi win32 aio)
                 (nmosh pffi win32 util)
                 (nmosh aio impl win32 handle-ops)
                 (nmosh aio impl win32 socket-ops) ;; close0
                 (nmosh aio impl win32 queue-iocp))

;;
(define (escape str)
  (define s? (find (^e (char=? #\space e))
                   (string->list str)))
  (if s?
    (string-append "\"" str "\"")
    str))


(define (queue-spawn0 Q path args envs workdir stdin stdout stderr result)
  ;; => handle
  ;; result ^[code #f #f] 
  ;; stdin = ^[writer]
  ;;   writer = ^[buf]
  ;; stdout/err = ^[h buf len]
  (define stdin/name #f)
  (define stdout/name #f)
  (define stderr/name #f)
  (define stdout/finish? #f)
  (define stderr/finish? #f)
  (define proc/pid #f)
  (define proc/retval #f)
  (define stdin/stream #f)
  (define (callback-result)
    ;(display (list 'CALLBACK-RESULT proc/retval stdout/finish? stderr/finish?))
    ;(newline)
    (when (and stdout/finish? stderr/finish? proc/retval)
      (when stdin/stream
        (queue-close0 Q stdin/stream))
      (result proc/pid proc/retval #f)))
  (define-syntax check+close
    ;; FIXME: Fail callback may called twice...
    (syntax-rules ()
      ((_ var proc)
       (letrec ((closed? #f)
                (ret
                  (^[fd buf len]
                    (proc fd buf len)
                    (unless buf
                      (unless closed?
                        (set! closed? #t)
                        (queue-close0 Q fd) 
                        ;(display (list 'CLOSE: 'var len))(newline) 
                        (set! var #t) 
                        (callback-result))))))
         ret))))
  (define spec
    (fold-left (^[cur e]
                 (string-append cur " " (escape e))) 
               (string-append "\"" path "\"") 
               (cdr args)))

  ;; create named-pipe and waiter
  ;; cb = ^[h/#f]
  (define (create-named-pipe cb) ;; => name
    (define name (string-append
                   "\\\\.\\pipe\\"
                   (symbol->string (gensym 'PIPE))))
    (define p (win32_create_named_pipe_async name))
    (define ovl (win32_overlapped_alloc))
    (define (callback err bytes ovl key)
      ;; FIXME: error?
      (win32_overlapped_free ovl)
      (cb p))
    (win32_overlapped_setmydata ovl (object->pointer callback))
    (queue-register-handle Q p ovl)
    ;; FIXME: check if 2
    (win32_wait_named_pipe_async p ovl)
    name)

  ;; stdin
  (cond
    ((procedure? stdin)
     (set! stdin/name
       (create-named-pipe
         (^[h]
           (let ((str (handle->stream Q h)))
             (set! stdin/stream str)
             (stdin (^[buf]
                      (queue-write0 
                        Q str buf 
                        (^[result]
                          (unless result
                            (set! stdin/stream #f)
                            (queue-close0 Q str)))))))))))
    (else (set! stdin/name stdin)))

  ;; stdout
  (cond
    ((procedure? stdout)
     (set! stdout/name
       (create-named-pipe
         (^[h]
           (let ((str (handle->stream Q h)))
             (queue-read0 Q str (check+close stdout/finish? stdout)))))))
    (else 
      (set! stdout/finish? #t)
      (set! stdout/name stdout)))

  ;; stderr
  (cond
    ((procedure? stderr)
     (set! stderr/name
       (create-named-pipe
         (^[h]
           ;(display (list 'STDERR: (handle->pointer h)))(newline)
           (let ((str (handle->stream Q h)))
             (queue-read0 Q str (check+close stderr/finish? stderr)))))))
    (else 
      (set! stderr/finish? #t)
      (set! stderr/name stderr)))

  (let ((h (win32_process_redirected_child2 
             spec workdir
             stdin/name
             stdout/name
             stderr/name)))
    ;; FIXME: error check
    (define (callback err bytes ovl key)
      ;(display (list 'PROCESS-FINISH (pointer->integer bytes)))(newline)
      (win32_overlapped_free ovl)
      (set! proc/retval (pointer->integer bytes))
      (callback-result))
    (define ovl (win32_overlapped_alloc))
    (win32_overlapped_setmydata ovl (object->pointer callback))
    (win32_process_wait_async h (~ Q 'iocp) (integer->pointer 0) ovl)
    ;; Use handle value as pid..
    (set! proc/pid (pointer->integer (handle->pointer h)))
    proc/pid))

)
