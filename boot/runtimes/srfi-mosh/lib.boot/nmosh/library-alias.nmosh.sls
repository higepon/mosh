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
     [ (nmosh pffi locale) . (nmosh pffi win32 locale) ])
    (("cygwin" "linux" "bsd" "darwin")
     .
     [ (nmosh pffi locale) . (nmosh pffi posix locale) ])
    (("win32")
     .
     [ (nmosh aio platform) . (nmosh aio platform win32) ])
    (("bsd")
     .
     [ (nmosh aio platform) . (nmosh aio platform bsd) ])
    (("cygwin")
     .
     [ (nmosh aio platform) . (nmosh aio platform cygwin) ])
    ))

(define (calc-table)
  (define os (host-os))
  (define (itr cur rest)
    (if (pair? rest)
      (let ((platforms (caar rest))
            (lib (cdar rest))
            (d (cdr rest)))
        (if (or (eq? #t platforms) (member os platforms))
          (itr (cons lib cur) d)
          (itr cur d)))
      cur))
  (itr '() library-alias-table))

(define (init-library-alias-table)
  (set-library-rename-table! (calc-table)))


)
