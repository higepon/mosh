(library (nmosh process mosh)
         (export process-launch/mosh
                 process-wait/mosh
                 process-result/mosh
                 process-stdout/mosh
                 process-stderr/mosh)
         (import (rnrs)
                 (mosh)
                 (srfi :8)
                 (mosh process))

(define process-status-sym '*nmosh-process-status-mosh*)

(define (check-status process-status)
  (unless (eq? process-status-sym (vector-ref process-status 0))
    (assertion-violation 'process-wait/win32 "invalid argument" process-status)))

(define (process-wait/mosh process-status)
  (check-status process-status)
  (receive (pid code) (waitpid (vector-ref process-status 2))
    (vector-set! process-status 1 code)
    (let ((stdout (vector-ref process-status 4))
          (stderr (vector-ref process-status 5)))
      (when (port? stdout)
        (vector-set! process-status 4 (get-bytevector-all stdout)))
      (when (port? stderr)
        (vector-set! process-status 5 (get-bytevector-all stderr)))
      (when (port? stdout)
        (close-port stdout))
      (when (port? stderr)
        (close-port stdout))
      )
    code))

(define (process-result/mosh process-status)
  (check-status process-status)
  (vector-ref process-status 1))

(define (process-stdout/mosh process-status)
  (check-status process-status)
  (vector-ref process-status 4))

(define (process-stderr/mosh process-status)
  (check-status process-status)
  (vector-ref process-status 5))

(define (process-launch/mosh exec-path startup-path arg* std-in std-out std-err)
  (define (portarg/write x) ;; for std-in => (port . port)
    (cond
      ((not x)
       (cons #f #f))
      (else
        (assertion-violation 'process-launch/mosh "not implemented yet!(std-in)" x))))
  (define (portarg/read x) ;; for  std-out/err => (port . port)
    (cond
      ((not x)
       (cons #f #f))
      ((eq? #t x)
       (receive (in out) (pipe)
         (cons in out)))
      (else
        (assertion-violation 'process-launch/mosh "invalid argument" x))))
  (let ((stdin (portarg/write std-in))
        (stdout (portarg/read std-out))
        (stderr (portarg/read std-err)))
    (let ((my-stdin (car stdin))
          (child-stdin (cdr stdin))
          (my-stdout (car stdout))
          (child-stdout (cdr stdout))
          (my-stderr (car stderr))
          (child-stderr (cdr stderr))
          (wd (current-directory)))
      (receive (pid in out err) 
        (when startup-path
          (set-current-directory! startup-path))
        (spawn exec-path arg* (list child-stdin child-stdout child-stderr))
        (when startup-path
          (set-current-directory! wd))
        (when (port? child-stdin)
          (close-port child-stdin))
        (when (port? child-stdout)
          (close-port child-stdout))
        (when (port? child-stderr)
          (close-port child-stderr))
        (vector process-status-sym
                'running pid my-stdin my-stdout my-stderr)))))

)
