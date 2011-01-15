(library (yuni scheme refimpl r6rs-unicode backend)
         (export 
           define let if or
           bytevector-u8-ref
           u8-list->bytevector
           bytevector-length
           cond else quote
           vector-ref vector-length map
           do
           eq?
           error
           integer->char
           char->integer
           and
           case
           memv
           char<=?  char>=?  char>?  char<?  char=?
           let*
           string-ref
           string-length
           vector
           lambda
           length
           car
           apply
           string
           not
           cons
           char?
           null?
           string-set!
           cdr
           make-string
           list->string
           reverse
           append
           string->list
           begin
           substring
           assertion-violation
           list
           string>=?  string<=?  string>?  string<?  string=?
           cadr
           call-with-values values
           =>
           + * <= > - >= = < div
           mod zero?  min max
           string-copy

           )
         (import 
           (rnrs)
           (rnrs mutable-strings)
           )
         )
