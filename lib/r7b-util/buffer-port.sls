#!r6rs
(library (r7b-util buffer-port)
         (export
           %get-buffered-data
           %create-buffer)
         (import (rnrs) (r7b-util metadata))

(define %buffer-port-id '*buffer-port*)
(define (%get-buffered-data port)
  (let ((d (metadata-ref port)))
    (unless d
      (assertion-violation '%get-buffered-data
                           "Lost accumulated data...(premature collection?)"
                           port))
    (unless (eq? (car d) %buffer-port-id)
      (assertion-violation '%get-buffered-data
                           "something wrong with the metadata"
                           d))
    ((cdr d))))

(define (%create-buffer proc)
  (lambda ()
    (let-values (((port getter) (proc)))
                (metadata-set! port (cons %buffer-port-id getter))
                port)))

)
