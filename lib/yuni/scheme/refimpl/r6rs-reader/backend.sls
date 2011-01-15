(library (yuni scheme refimpl r6rs-reader backend)
         (export 
           define
           let*
           quote
           error
           string-append
           symbol->string string
           case else
           apply map
           lambda if
           memq null?
           cons car cdr let
           string->symbol list->string
           reverse
           char=?
           cond and or
           call-with-values
           string-ref
           string->list substring string-length
           make-string
           eq?
           eof-object?
           char-general-category
           string-set!
           begin
           eof-object
           list->vector
           u8-list->bytevector
           integer->char char->integer
           values
           not
           char?  char-whitespace?
           do
           set!
           read-char peek-char
           string=?
           list

           ;; arith
           exact?  integer?
           = < + - >= > <= *
           string->number
           )
         (import
           (except (rnrs)
                   exact? integer?
                   = < + - >= > <= *
                   string->number)
           (only (yuni scheme refimpl arithmetic r6rs base)
                 exact? integer?
                 = < + - >= > <= *
                 string->number)
           (rnrs mutable-strings)
           ) 
)
