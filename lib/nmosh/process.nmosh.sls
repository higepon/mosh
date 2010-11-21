(library (nmosh process)
         (export process-launch
                 process-wait
                 process-result
                 process-stdout
                 process-stderr
                 process-stdout-string
                 process-stderr-string)
         (import (nmosh process win32)
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
           #f
           process-launch/win32)
(define-if process-wait
           #f
           process-wait/win32)
(define-if process-result
           #f
           process-result/win32)
(define-if process-stdout
           #f
           process-stdout/win32)
(define-if process-stderr
           #f
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
