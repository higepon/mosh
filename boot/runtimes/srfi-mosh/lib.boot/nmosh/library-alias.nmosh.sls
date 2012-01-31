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
    (("win32") ;; IOCP + Win32 GUI
     .
     [ (nmosh aio platform) . (nmosh aio platform win32) ])
    (("bsd" "darwin" "linux") ;; Posix for now.
     .
     [ (nmosh aio platform) . (nmosh aio platform posix) ])
    (("cygwin") ;; Cygwin has no Posix-spawn. Uses vfork().
     .
     [ (nmosh aio platform) . (nmosh aio platform cygwin) ])
    (("win32") ;; Win32 only..
     .
     [ (nmosh ffi pffi-plugin platform)
      . 
      (nmosh ffi pffi-plugin platform win32) ])
    (#t ;; rest
     .
     [ (nmosh ffi pffi-plugin platform)
      . 
      (nmosh ffi pffi-plugin platform null) ])))

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
  (itr '() (reverse library-alias-table)))

(define (init-library-alias-table)
  (set-library-rename-table! (calc-table)))


)
