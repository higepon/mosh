(library (yuni scheme refimpl arithmetic r6rs fixnums)
         (export
           (rename (*width* fixnum-width))
           fixnum? least-fixnum greatest-fixnum
           fx=? fx>? fx<? fx>=? fx<=?
           fxzero? fxpositive? fxnegative?
           fxodd? fxeven?
           fxmax fxmin
           fx+ fx- fx*
           fxdiv-and-mod fxdiv fxmod
           fxdiv0-and-mod0 fxdiv0 fxmod0
           fx+/carry fx-/carry fx*/carry
           fxnot fxand fxior fxxor
           fxif fxbit-count fxlength
           fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
           fxrotate-bit-field fxreverse-bit-field
           fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)
         (import
           (yuni scheme refimpl arithmetic impl custom)
           (yuni scheme refimpl arithmetic impl fixnum)))
