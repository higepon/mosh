;; generated from src/posix/terminal/Library.scm DO NOT EDIT!!
(library (nmosh stubs terminal)
(export
  terminal_isatty
  terminal_getsize
  terminal_release
  terminal_acquire)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'terminal))


(define
  terminal_acquire
  (pffi-c-function %library void terminal_acquire))
(define
  terminal_release
  (pffi-c-function %library void terminal_release))
(define
  terminal_getsize
  (pffi-c-function %library int terminal_getsize))
(define
  terminal_isatty
  (pffi-c-function
    %library
    int
    terminal_isatty
    int))
)
