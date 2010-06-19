(library (rnrs (6))         
  
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
   syntax-rules identifier-syntax
   
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
   vector-fill! vector-length vector-ref vector-set! vector? zero?
   
   ;; R6RS additional procedures:
   
   real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
   nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
   symbol=? string-for-each vector-map vector-for-each error assertion-violation
   call/cc
   
   ;; From (rnrs syntax-case)
   
   make-variable-transformer
   identifier? bound-identifier=? free-identifier=?
   generate-temporaries datum->syntax syntax->datum 
   syntax-violation syntax syntax-case quasisyntax 
   unsyntax unsyntax-splicing with-syntax 
   
   ;; From (rnrs control)
   
   when unless do case-lambda
   
   ;; From (rnrs lists)
   
   find for-all exists filter partition fold-left fold-right
   remp remove remq remv memp member memv memq
   assp assoc assv assq
   
   ;; From (rnrs io simple)
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char
   
   ;; From (rnrs unicode)
   
   char-upcase char-downcase char-titlecase char-foldcase
   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
   char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? char-title-case?
   char-general-category

   string-upcase string-downcase string-titlecase string-foldcase
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   string-normalize-nfd string-normalize-nfkd
   string-normalize-nfc string-normalize-nfkc
   
   ;; From (rnrs sorting)
   
   list-sort vector-sort vector-sort!

   ;; From (rnrs records procedural)
 
   make-record-type-descriptor record-type-descriptor?
   make-record-constructor-descriptor record-constructor
   record-predicate record-accessor record-mutator

   ;; From (rnrs records inspection)
   
   record? record-rtd record-type-name record-type-parent record-type-uid
   record-type-generative? record-type-sealed? record-type-opaque?
   record-type-field-names record-field-mutable?
   
   ;; From (rnrs arithmetic fixnums)

   fixnum? fixnum-width least-fixnum greatest-fixnum
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
   fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right

   ;;; From (rnrs arithmetic flonums)

   flonum?
   real->flonum
   fl=? fl<? fl>? fl<=? fl>=?
   flinteger? flzero? flpositive? flnegative? flodd? fleven?
   flfinite? flinfinite? flnan?
   flmax flmin
   fl+ fl* fl- fl/
   flabs
   fldiv-and-mod fldiv flmod
   fldiv0-and-mod0 fldiv0 flmod0
   flnumerator fldenominator
   flfloor flceiling fltruncate flround
   flexp fllog flsin flcos fltan flasin flacos flatan
   flsqrt flexpt
   fixnum->flonum

   ;; FIXME
   ;; &no-infinities make-no-infinities-violation no-infinities-violation
   ;; &no-nans make-no-nans-violation no-nans-violation

   ;; From (rnrs arithmetic bitwise)

   bitwise-not
   bitwise-and
   bitwise-ior
   bitwise-xor
   bitwise-if
   bitwise-bit-count
   bitwise-length
   bitwise-first-bit-set
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-bit-field
   bitwise-copy-bit-field
   bitwise-rotate-bit-field
   bitwise-reverse-bit-field
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right
   
   ;; From (rnrs files)
   
   file-exists? delete-file
   
   ; MOSH
   ;; from (rnrs records syntactic)
   define-record-type
   record-type-descriptor
   record-constructor-descriptor

   ;; FIXME! mosh libs needs them...
  fields mutable immutable parent protocol sealed opaque nongenerative parent-rtd


   ;; from (rnrs hashtables)
	make-eq-hashtable make-eqv-hashtable make-hashtable hashtable?  hashtable-size hashtable-ref
	hashtable-set!  hashtable-delete!  hashtable-contains?  hashtable-update!  hashtable-copy hashtable-clear!
	hashtable-keys hashtable-entries hashtable-equivalence-function hashtable-hash-function hashtable-mutable?
	equal-hash string-hash string-ci-hash symbol-hash
   ;; from (rnrs io port)
	binary-port?  buffer-mode buffer-mode?  bytevector->string call-with-bytevector-output-port call-with-port
	call-with-string-output-port close-port eol-style error-handling-mode file-options flush-output-port
	get-bytevector-all get-bytevector-n get-bytevector-n!  get-char get-datum get-line get-string-all get-string-n
	get-string-n!  get-u8 
	
	;&i/o &i/o-decoding i/o-decoding-error?  &i/o-encoding i/o-encoding-error-char
	;i/o-encoding-error?  i/o-error-filename i/o-error-port i/o-error-position i/o-error?  &i/o-file-already-exists
	;i/o-file-already-exists-error?  &i/o-file-does-not-exist i/o-file-does-not-exist-error?  &i/o-file-is-read-only
	;i/o-file-is-read-only-error?  &i/o-file-protection i/o-file-protection-error?  &i/o-filename
	;i/o-filename-error?  &i/o-invalid-position i/o-invalid-position-error?  &i/o-port
	;i/o-port-error?  &i/o-read i/o-read-error?  &i/o-write i/o-write-error?  
	lookahead-char
	lookahead-u8 make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port
	make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port
	;make-i/o-decoding-error make-i/o-encoding-error make-i/o-error make-i/o-file-already-exists-error
	;make-i/o-file-does-not-exist-error make-i/o-file-is-read-only-error make-i/o-file-protection-error
	;make-i/o-filename-error make-i/o-invalid-position-error make-i/o-port-error make-i/o-read-error
	;make-i/o-write-error 
	latin-1-codec make-transcoder native-eol-style native-transcoder open-bytevector-input-port
	open-bytevector-output-port open-file-input-port open-file-input/output-port open-file-output-port
	open-string-input-port open-string-output-port output-port-buffer-mode port-eof?  port-has-port-position?
	port-has-set-port-position!?  port-position port-transcoder 
	put-bytevector put-char put-datum put-string
	put-u8 set-port-position!  standard-error-port standard-input-port standard-output-port string->bytevector
	textual-port?  transcoded-port transcoder-codec transcoder-eol-style transcoder-error-handling-mode utf-16-codec
	utf-8-codec 
	current-error-port eof-object 
	get-bytevector-some
	; R5RS
	;input-port?  output-port?  current-input-port current-output-port 
	;eof-object?
	;port?  

   ;; from (rnrs io simple)
   input-port? output-port? port?

   ;; from (rnrs exceptions)
           raise
           raise-continuable
           with-exception-handler
           guard

   ;; from (rnrs conditions)

    &condition condition simple-conditions condition?  condition-predicate condition-accessor
    &message make-message-condition message-condition?  condition-message
    &warning make-warning warning?  &serious make-serious-condition serious-condition?  &error make-error error?
    &violation make-violation violation?
    &assertion make-assertion-violation assertion-violation?
    &irritants make-irritants-condition irritants-condition?  condition-irritants &who make-who-condition who-condition?  condition-who
    &non-continuable make-non-continuable-violation non-continuable-violation?  &implementation-restriction make-implementation-restriction-violation
    implementation-restriction-violation?  &lexical make-lexical-violation
    lexical-violation?  &syntax make-syntax-violation syntax-violation?  syntax-violation-form syntax-violation-subform
    &undefined make-undefined-violation undefined-violation?  &i/o make-i/o-error i/o-error?  &i/o-read make-i/o-read-error i/o-read-error?
    &i/o-write make-i/o-write-error i/o-write-error?  &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error?  i/o-error-position
    &i/o-filename make-i/o-filename-error i/o-filename-error?  i/o-error-filename &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
    &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?  &i/o-file-already-exists make-i/o-file-already-exists-error
    i/o-file-already-exists-error?  &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
    &i/o-port make-i/o-port-error i/o-port-error?  i/o-error-port &i/o-decoding
    make-i/o-decoding-error i/o-decoding-error?  &i/o-encoding make-i/o-encoding-error i/o-encoding-error?  i/o-encoding-error-char
    &no-infinities make-no-infinities-violation no-infinities-violation?  &no-nans make-no-nans-violation no-nans-violation?

    ; deprecated 

    #|
    &condition-rtd &condition-rcd &message-rtd &message-rcd
    &warning-rtd &warning-rcd &serious-rtd &serious-rcd
    &error-rtd &error-rcd &violation-rtd &violation-rcd
    &assertion-rtd &assertion-rcd &irritants-rtd
    &irritants-rcd &who-rtd &who-rcd &non-continuable-rtd
    &non-continuable-rcd &implementation-restriction-rtd
    &implementation-restriction-rcd &lexical-rtd &lexical-rcd
    &syntax-rtd &syntax-rcd &undefined-rtd &undefined-rcd

    &i/o-rtd &i/o-rcd &i/o-read-rtd &i/o-read-rcd
    &i/o-write-rtd &i/o-write-rcd &i/o-invalid-position-rtd
    &i/o-invalid-position-rcd &i/o-filename-rtd
    &i/o-filename-rcd &i/o-file-protection-rtd
    &i/o-file-protection-rcd &i/o-file-is-read-only-rtd
    &i/o-file-is-read-only-rcd &i/o-file-already-exists-rtd
    &i/o-file-already-exists-rcd &i/o-file-does-not-exist-rtd
    &i/o-file-does-not-exist-rcd &i/o-port-rtd &i/o-port-rcd
    &i/o-decoding-rtd &i/o-decoding-rcd &i/o-encoding-rtd
    &i/o-encoding-rcd &no-infinities-rtd &no-infinities-rcd
    &no-nans-rtd &no-nans-rcd
    |#

    ;
    define-condition-type

   ;; from (rnrs base)
   char>?
   char<?
   char>=?
   char<=?
   char=?
   angle

   ;; from (rnrs bytevectors)

	native-endianness

	bytevector? make-bytevector bytevector-length
	bytevector=?
	bytevector-fill! bytevector-copy! bytevector-copy

	bytevector-u8-ref bytevector-s8-ref
	bytevector-u8-set! bytevector-s8-set!
	bytevector->u8-list u8-list->bytevector

	bytevector-uint-ref bytevector-sint-ref
	bytevector-uint-set! bytevector-sint-set!
	bytevector->uint-list bytevector->sint-list
	uint-list->bytevector sint-list->bytevector

	bytevector-u16-ref bytevector-s16-ref
	bytevector-u16-native-ref bytevector-s16-native-ref
	bytevector-u16-set! bytevector-s16-set!
	bytevector-u16-native-set! bytevector-s16-native-set!

	bytevector-u32-ref bytevector-s32-ref
	bytevector-u32-native-ref bytevector-s32-native-ref
	bytevector-u32-set! bytevector-s32-set!
	bytevector-u32-native-set! bytevector-s32-native-set!

	bytevector-u64-ref bytevector-s64-ref
	bytevector-u64-native-ref bytevector-s64-native-ref
	bytevector-u64-set! bytevector-s64-set!
	bytevector-u64-native-set! bytevector-s64-native-set!

	bytevector-ieee-single-native-ref
	bytevector-ieee-single-ref
	bytevector-ieee-double-native-ref
	bytevector-ieee-double-ref
	bytevector-ieee-single-native-set!
	bytevector-ieee-single-set!
	bytevector-ieee-double-native-set!
	bytevector-ieee-double-set!

	string->utf8 string->utf16 string->utf32
        utf8->string utf16->string utf32->string

	endianness
   ;; from (rnrs lists)
   	cons*
   ;; from (rnrs programs)
        exit
	command-line
   ;; from (rnrs enums)
           make-enumeration
           enum-set-universe
           enum-set-indexer
           enum-set-constructor
           enum-set->list
           enum-set-member?
           enum-set-subset?
           enum-set=?
           enum-set-union
           enum-set-intersection
           enum-set-difference
           enum-set-complement
           enum-set-projection
           define-enumeration
   )
  
  (import (for (except (rnrs base) syntax-rules identifier-syntax _ ... set!) run expand)
          (for (only (rnrs base) set!)                                        run expand)
          (for (core syntax-rules)                                            run expand)   
          (for (core identifier-syntax)                                       run expand)
          (for (rnrs control)                                                 run expand)
          (for (rnrs lists)                                                   run expand)
          (for (rnrs syntax-case)                                             run expand)
          (for (rnrs io simple)                                               run expand)
          (for (rnrs unicode)                                                 run expand)
          (for (rnrs sorting)                                                 run expand)
          (for (rnrs records procedural)                                      run expand)
          (for (rnrs records inspection)                                      run expand)
          (for (rnrs files)                                                   run expand)
          (for (rnrs arithmetic fixnums)                                      run expand)
          (for (rnrs arithmetic flonums)                                      run expand)
          (for (rnrs arithmetic bitwise)                                      run expand)
	  ; MOSH
	  (for (rnrs records syntactic) run expand)
	  (for (except (rnrs io ports) 
	    input-port?  output-port?  current-input-port current-output-port eof-object?  port?  )
	       run expand)
	  (for (rnrs exceptions) run expand)
	  (for (rnrs conditions) run expand)
	  (for (rnrs bytevectors) run expand)
	  (for (rnrs hashtables) run expand)
	  (for (rnrs programs) run expand)
	  (for (rnrs enums) run expand)
          )
  
  ) ;; rnrs
