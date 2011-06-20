(library (yuni util library-writer)
         (export 
           library-spec
           write-library)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (only (mosh pp) pp)
                 (yuni core))

(define* library-spec
  (name export import comment code))

(define* (write-library output (library-spec))
  (define (prepare-port p)
    (cond
      ((port? p) p)
      ((string? p)
       (open-file-output-port p (file-options no-fail) 
                              'block (native-transcoder)))
      (else
        (assertion-violation 'prepare-port
                             "invalid argument"
                             p))))
  (define (tabbed-out l)
    ;; FIXME: tab-it..
    (receive (port proc) (open-string-output-port)
      (pp l port)
      (let ((str (proc)))
        str)))

  (define (format-comment comment)
    ;; FIXME: implement-it..
    "")

  (let-with library-spec (export import comment code name)
    (define p (prepare-port output))
    (display (format-comment (if (string? comment) 
                               (list comment)
                               comment)) p)
    (display "(library " p)
    (write name p)
    (newline p)
    (display (tabbed-out (cons 'export export)) p)
    (display (tabbed-out (cons 'import import)) p)
    (for-each (^e (pp e p)) code)
    (display ")" p)
    (close-port p)))

)

