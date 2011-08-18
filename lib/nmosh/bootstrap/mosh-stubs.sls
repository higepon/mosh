;; stub library for (mosh)
(library (nmosh bootstrap mosh-stubs)
         (export source-info
                 set-source-info!
                 print
                 hashtable-for-each
                 annotated-cons
                 )
         (import (rnrs) (srfi :8))
(define (source-info . x) #f)
(define (set-source-info! . x) (values))
(define annotated-cons cons)
(define print write)
(define (hashtable-for-each proc ht)
  (receive (k v) (hashtable-entries ht)
    (for-each proc 
              (vector->list k)
              (vector->list v))))


)
