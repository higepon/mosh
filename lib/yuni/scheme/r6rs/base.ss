(library (yuni scheme r6rs base)         
  
  (export 
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   assert
   quasiquote unquote unquote-splicing
   syntax-rules 
   identifier-syntax
   
   ;; R5RS primitives:
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char? char->integer
   complex? cons cos 
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! zero? vector?
   
   ;; R6RS additional procedures:
   
   real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
   nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
   symbol=? string-for-each vector-map vector-for-each error assertion-violation
   (rename (call-with-current-continuation call/cc))
   
   ;; MOSH
	   char>=?
	   char<=?
	   char<?
	   char>?
	   char=?
	   angle
   )
  
  (import (yuni scheme r6rs))
  ) ;; rnrs base

