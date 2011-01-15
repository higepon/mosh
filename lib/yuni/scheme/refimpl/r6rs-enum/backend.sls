(library (yuni scheme refimpl r6rs-enum backend)
         (export
           define
           define-syntax
           syntax-rules
           let*
           do
           call-with-values
           lambda
           values
           null?
           let
           cdr
           +
           if
           vector-ref
           begin
           vector-set!
           >
           set!
           symbol-hash
           car
           =
           make-vector
           *
           -
           expt
           floor
           log
           length
           quote
           cond
           eq?
           and
           else
           filter
           error
           for-each
           <=
           fixnum-width
           reverse
           cons
           not
           symbol?
           list->vector
           list?
           memq
           (rename
             (exact inexact->exact)
             (zero? exact-zero?)
             (fxfirst-bit-set fixnum-first-bit-set)
             (fxcopy-bit fixnum-copy-bit)
             (fxarithmetic-shift-left fixnum-arithmetic-shift-left)
             (bitwise-arithmetic-shift-left exact-arithmetic-shift-left)
             (fx>? fixnum>)
             (fx+ fixnum+)
             (fx- fixnum-)
             (fxmod fixnum-mod)
             (mod exact-mod)
             (fxior fixnum-ior)
             (bitwise-copy-bit exact-copy-bit)
             (bitwise-first-bit-set exact-first-bit-set)
             (bitwise-ior exact-ior)
             (bitwise-not exact-not)
             (bitwise-and exact-and))
           make
           let-with
           define*
           )
         (import (rnrs)
                 (yuni core)
                 ))

