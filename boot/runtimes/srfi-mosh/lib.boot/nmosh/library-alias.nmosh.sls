;; FIXME: should be auto-generated...
(library (nmosh library-alias)
         (export init-library-alias-table)
         (import (rnrs)
                 (primitives
                   host-os
                   set-library-rename-table!))
(define library-alias-table
  '(
    (("win32")
     .
     [ (nmosh aio platform) . (nmosh aio platform win32) ])
    (("bsd")
     .
     [ (nmosh aio platform) . (nmosh aio platform bsd) ])))

(define (calc-table)
  (define os (host-os))
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (if (member os (car a))
          (itr (cons (cdr a) cur) d)
          (itr cur d)))
      cur))
  (itr '() library-alias-table))

(define (init-library-alias-table)
  (set-library-rename-table! (calc-table)))


)
