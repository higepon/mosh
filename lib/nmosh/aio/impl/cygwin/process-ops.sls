;; Interface for sigchld-handler and debugee-spawn
(library (nmosh aio impl cygwin process-ops)
         (export queue-spawn0)
         (import 
           (rnrs)
           (shorten)
           (srfi :8)
           (yuni core)
           (nmosh aio impl posix queue-fd-poll)
           (nmosh aio impl posix fd-ops)
           (nmosh pffi bsd wait3)
           (nmosh pffi posix debugee)
           (nmosh pffi posix fd)
           (nmosh pffi posix sigchld-handler))

(define proc-ht (make-eq-hashtable))
(define has-sigchld-handler? #f)

(define (register-process pid proc)
  (hashtable-set! proc-ht pid proc))

(define (callback pid code signal)
  (let ((r (hashtable-ref proc-ht pid #f)))
    (when r (r pid code signal))
    (hashtable-delete! proc-ht pid)))

(define (invoke)
  (receive (pid code signal rusage) (try_wait3)
    (cond
      ((or (= pid 0) (= pid -1)) ;; No process
       'ok)
      (else
        (callback pid code signal)
        (invoke)))))

(define (init Q)
  (define buf (make-bytevector 1))
  (define (proc-sigchld-handler fd evt)
    (case evt
      ((READ READ+WRITE)
       ;; Read a byte
       (fd_read fd buf 1)
       ;; Perform wait3
       (invoke))
      (else
        (assertion-violation 'cygwin-process-ops
                             "Invalid event for sigchld-handler"
                             evt
                             fd))))

  (receive (in out) (fd_pipe)
    (sigchld_handler_install out)
    (queue-register-fd/read Q in proc-sigchld-handler)))

(define fileactions-size #f)

(define (queue-spawn0 Q path args envs workdir stdin stdout stderr result) 
  ;; => pid
  ;; Result = ^[code signal rusage]
  ;; stdin = ^[writer]
  ;;   writer = ^[buf]
  ;; stdout/stderr = ^[fd buf len]
  
  ;; fd list to close after fork
  (define trash '())
  (unless has-sigchld-handler? (init Q))
  (unless fileactions-size
    (set! fileactions-size (debugee_fileactionssize)))

  (let ((action (make-bytevector fileactions-size)))
    (define (procout num obj)
      (cond
        ((procedure? obj)
         (receive (in out) (fd_pipe)
           (debugee_fileactions_adddup2 action out (int->fd num))
           (set! trash (cons out trash))
           (queue-read0 Q in obj)))
        ((not obj)
         ;; close
         (debugee_fileactions_addclose action (int->fd num)))
        ((string? obj)
         ;; Redirect to file
         ;; FIXME: implement it..
         'ok)
        (else
          ;; Do nothing (graft to current stdin/out/err)
          'ok)))
    (define (procin num obj)
      (cond
        ((procedure? obj)
         ;; Prepare read callback
         (receive (in out) (fd_pipe)
           (debugee_fileactions_adddup2 action in (int->fd num))
           (set! trash (cons in trash))
           (obj (^[data]
                  (queue-write0 Q out data 
                                (^[obj] 'ok)))))) 
        ((string? obj)
         ;; Redirect from file
         ;; FIXME: implement it..
         'ok)
        (else
          ;; close
          (debugee_fileactions_addclose action (int->fd num)))))

    (debugee_fileactions_init action)
    (procin 0 stdin)
    (procout 1 stdout)
    (procout 2 stderr)
    ;; Perform spawn
    (let ((pid (debugee_spawn #f path action args envs)))
      (debugee_fileactions_destroy action)
      (for-each (^[fd] (fd_close fd)) trash)
      (unless (<= pid 0) (register-process pid result))
      pid)))

)
