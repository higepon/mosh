(library (nmosh process)
         (export process-launch
                 process-wait
                 process-result
                 process-stdout
                 process-stderr
                 process-stdout-string
                 process-stderr-string)
         (import (nmosh process win32)
                 (nmosh process mosh)
                 (only (mosh) host-os)
                 (rnrs))

(define-syntax define-if
  (syntax-rules ()
    ((_ name std win32)
     (define name
       (if (string=? "win32" (host-os))
         win32
         std)))))

(define-if process-launch
           process-launch/mosh
           process-launch/win32)
(define-if process-wait
           process-wait/mosh
           process-wait/win32)
(define-if process-result
           process-result/mosh
           process-result/win32)
(define-if process-stdout
           process-stdout/mosh
           process-stdout/win32)
(define-if process-stderr
           process-stderr/mosh
           process-stderr/win32)

(define (return-string proc x)
  (define bv (proc x))
  (if (bytevector? bv)
    (bytevector->string bv (native-transcoder))
    bv))

(define (process-stdout-string x)
  (return-string process-stdout x))

(define (process-stderr-string x)
  (return-string process-stderr x))

)
