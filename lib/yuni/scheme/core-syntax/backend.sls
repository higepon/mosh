(library (yuni scheme core-syntax backend)
         (export define-syntax
                 lambda
                 syntax-case
                 ...
                 syntax
                 define
                 for-all
                 identifier?
                 map
                 begin
                 syntax-violation
                 quote
                 let
                 if
                 null?
                 car
                 cdr
                 vector->list
                 list
                 and
                 >
                 or
                 free-identifier=?
                 -
                 =
                 generate-temporaries
                 +
                 append
                 )
         (import (rnrs)))
