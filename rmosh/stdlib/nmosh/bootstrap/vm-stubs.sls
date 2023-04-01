(library (nmosh bootstrap vm-stubs)
         (export test-end
                 test*
                 *test-error*
                 print
                 top-level-macros
                 hash-table-get
                 make-hash-table
                 )
         (import (rnrs))
(define *test-error* #f)
(define (test* . x) #f)
(define (test-end . x)
  #f)
(define print display)
(define top-level-macros #f)
(define hash-table-get hashtable-ref)
(define make-hash-table make-hashtable)
)
