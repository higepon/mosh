;; nmosh REPL default library
(library (nmosh)
         (export 
           begin if lambda quote set! and or
           define define-syntax let-syntax letrec-syntax
           _ ...
           let let* letrec letrec* let-values let*-values
           case cond else =>
           assert
           quasiquote unquote unquote-splicing
           syntax-rules identifier-syntax
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
           real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
           nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
           symbol=? string-for-each vector-map vector-for-each error assertion-violation
           call/cc
           make-variable-transformer
           identifier? bound-identifier=? free-identifier=?
           generate-temporaries datum->syntax syntax->datum 
           syntax-violation syntax syntax-case quasisyntax 
           unsyntax unsyntax-splicing with-syntax 
           when unless do case-lambda
           find for-all exists filter partition fold-left fold-right
           remp remove remq remv memp member memv memq
           assp assoc assv assq
           call-with-input-file call-with-output-file 
           close-input-port close-output-port current-input-port current-output-port
           display eof-object? newline open-input-file open-output-file peek-char
           read read-char with-input-from-file with-output-to-file write write-char
           char-upcase char-downcase char-titlecase char-foldcase
           char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
           char-alphabetic? char-numeric? char-whitespace?
           char-upper-case? char-lower-case? char-title-case?
           char-general-category
           string-upcase string-downcase string-titlecase string-foldcase
           string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
           string-normalize-nfd string-normalize-nfkd
           string-normalize-nfc string-normalize-nfkc
           list-sort vector-sort vector-sort!
           make-record-type-descriptor record-type-descriptor?
           make-record-constructor-descriptor record-constructor
           record-predicate record-accessor record-mutator
           record? record-rtd record-type-name record-type-parent record-type-uid
           record-type-generative? record-type-sealed? record-type-opaque?
           record-type-field-names record-field-mutable?
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
           flonum?  real->flonum fl=? fl<? fl>? fl<=? fl>=?
           flinteger? flzero? flpositive? flnegative? flodd? fleven?
           flfinite? flinfinite? flnan?  flmax flmin
           fl+ fl* fl- fl/ flabs fldiv-and-mod fldiv flmod fldiv0-and-mod0 fldiv0 flmod0
           flnumerator fldenominator flfloor flceiling fltruncate flround
           flexp fllog flsin flcos fltan flasin flacos flatan flsqrt flexpt fixnum->flonum
           bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-if bitwise-bit-count
           bitwise-length bitwise-first-bit-set bitwise-bit-set?  bitwise-copy-bit
           bitwise-bit-field bitwise-copy-bit-field bitwise-rotate-bit-field
           bitwise-reverse-bit-field bitwise-arithmetic-shift bitwise-arithmetic-shift-left
           bitwise-arithmetic-shift-right
           file-exists? delete-file
           define-record-type
           record-type-descriptor
           record-constructor-descriptor
           fields mutable immutable parent protocol sealed opaque nongenerative parent-rtd
           make-eq-hashtable make-eqv-hashtable make-hashtable hashtable?  hashtable-size hashtable-ref
           hashtable-set!  hashtable-delete!  hashtable-contains?  hashtable-update!  hashtable-copy hashtable-clear!
           hashtable-keys hashtable-entries hashtable-equivalence-function hashtable-hash-function hashtable-mutable?
           equal-hash string-hash string-ci-hash symbol-hash
           binary-port?  buffer-mode buffer-mode?  bytevector->string call-with-bytevector-output-port call-with-port
           call-with-string-output-port close-port eol-style error-handling-mode file-options flush-output-port
           get-bytevector-all get-bytevector-n get-bytevector-n!  get-char get-datum get-line get-string-all get-string-n
           get-string-n!  get-u8 
           lookahead-char
           lookahead-u8 make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port
           make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port
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
           input-port? output-port? port?
           raise
           raise-continuable
           with-exception-handler
           guard
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
           define-condition-type
           char>?
           char<?
           char>=?
           char<=?
           char=?
           angle
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
           endianness cons* exit command-line
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

           ;; from (rnrs load)
           load

           ;; from (shorten)
           ^ ^_
           ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k* ^l* ^m* 
           ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w* ^x* ^y* ^z*
           ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z
           )

         (import
           (for (rnrs) run expand)
           (rnrs load)
           (for (shorten) run expand)))

