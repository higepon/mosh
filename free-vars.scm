(define-free-vars
    (number? number?)
    (cons cons)
    (car car)
    (cdr cdr)
    (null? null?)
    (set-car! set-car!)
    (set-cdr! set-cdr!)
    (sys-display display)
    (rxmatch rxmatch)
    (regexp? regexp?)
    (regexp->string regexp->string)
    (rxmatch-start rxmatch-start)
    (rxmatch-end rxmatch-end)
    (rxmatch-after rxmatch-after)
    (rxmatch-before rxmatch-before)
    (rxmatch-substring rxmatch-substring)
    (make-string make-string)
    (string-set! string-set!)
    (string-length string-length)
    (string->symbol string->symbol)
    (string->number string->number)
    (string-append string-append)
    (string-split string-split)
    (string string)
    (number->string number->string)
    (reverse reverse)
    (eof-object? eof-object?)
    (read-char read-char)
    (char=? char=?)
    (string? string?)
    (get-environment-variable sys-getenv)
    get-environment-variables
    (equal? equal?)
    (open-string-input-port open-input-string)
    (sys-open-output-string open-output-string)
    (sys-port-seek port-seek)
    (open-output-file open-output-file)
    (close-output-port close-output-port)
    (digit->integer digit->integer)
    (get-remaining-input-string get-remaining-input-string)
    (readdir sys-readdir)
    (file-exists? file-exists?)
    delete-file
    (sys-get-output-string get-output-string)
    (string->regexp string->regexp)
    (char->integer char->integer)
    (integer->char integer->char)
    (format format)
    (current-input-port current-input-port)
    (current-output-port current-output-port)
    (set-current-input-port! current-input-port)
    (set-current-output-port! current-output-port)
    (char? char?)
    (write write)
    (gensym gensym)
    (string=? string=?)
    symbol=?
    boolean=?
    (vector? vector?)
    (list? list?)
    (memq memq)
    (eq? eq?)
    (eqv? eqv?)
    (member member)
    (boolean? boolean?)
    (symbol->string symbol->string)
    (string-ref string-ref)
    (error error)
    (assertion-violation error)
    (get-timeofday (lambda () (receive (a b) (sys-gettimeofday) (cons a b))))
    (make-eq-hashtable (lambda a (make-hash-table 'eq?)))
    (make-eqv-hashtable (lambda a (make-hash-table 'eqv?)))
    (hashtable-set! hash-table-put!)
    (hashtable-ref hash-table-get)
    (hashtable-keys hash-table-keys)
    string-hash
    eqv-hash
    string-ci-hash
    symbol-hash
    equal-hash
    (eq-hashtable-copy eq-hashtable-copy)
    (current-error-port current-error-port)
    values
    (vm/apply vm/apply)
    (pair? pair?)
    (make-custom-binary-input-port (lambda (id read! get-position set-position! close) (display "make-custom-binary-input-port not implemented")))
    get-u8
    bytevector-u8-set!
    transcoded-port
    latin-1-codec
    utf-8-codec
    utf-16-codec
    make-transcoder
    (eof-object (lambda () (if #f #f)))
    sys-open-bytevector-output-port
    sys-get-bytevector
    bytevector-length
    (standard-input-port standard-input-port)
    (standard-output-port standard-output-port)
    (standard-error-port standard-error-port)
    get-bytevector-n
    (open-file-output-port open-output-file)
    (open-file-input-port open-input-file)
    (open-input-file open-input-file)
    (close-input-port close-input-port)
    (vector vector)
    (regexp-replace regexp-replace)
    (regexp-replace-all regexp-replace-all)
    (source-info (lambda (x) #f))
    (eval (lambda a (evaluate (car a))))
    (apply apply-proc)
    (assq assq)
    (exit exit)
    (macroexpand-1 pass1/macroexpand)
    (memv memv)
    (procedure? vector?)
    load
    (symbol? symbol?)
    (char<=? char<=?)
    (char<? char<?)
    (char>=? char>=?)
    (char>? char>?)
    read
    (vector->list vector->list)
    (set-source-info! (lambda e #f))
    call-process
    %get-closure-name
    (append append)
    (append2 append)
    (append! append!)
    (pass3/find-free pass3/find-free)
    (pass3/find-sets pass3/find-sets)
    (pass4/fixup-labels pass4/fixup-labels)
    (make-code-builder make-code-builder)
    (code-builder-put-extra1! code-builder-put-extra1!)
    (code-builder-put-extra2! code-builder-put-extra2!)
    (code-builder-put-extra3! code-builder-put-extra3!)
    (code-builder-put-extra4! code-builder-put-extra4!)
    (code-builder-put-extra5! code-builder-put-extra5!)
    (code-builder-append! code-builder-append!)
    (code-builder-emit code-builder-emit)
    (code-builder-put-insn-arg0! code-builder-put-insn-arg0!)
    (code-builder-put-insn-arg1! code-builder-put-insn-arg1!)
    (code-builder-put-insn-arg2! code-builder-put-insn-arg2!)
    (length length)
    (list->vector list->vector)
    (pass3/compile-refer pass3/compile-refer)
    (pass1/find-symbol-in-lvars pass1/find-symbol-in-lvars)
    ($label $label)
    ($local-ref $local-ref)
    symbol-value
    set-symbol-value!
    make-hashtable
    hashtable?
    hashtable-size
    hashtable-delete!
    hashtable-contains?
    hashtable-copy
    hashtable-mutable?
    hashtable-clear!
    hashtable-keys
    hashtable-equivalence-function
    hashtable-hash-function
    (make-record-type-descriptor (lambda e 'make-record-type-descriptor-dummy))
    (make-record-constructor-descriptor (lambda e 'make-record-constructor-descriptor-dummy))
    record-predicate
    (record-constructor (lambda e 'record-constructor-dummy))
    (record-accessor (lambda e 'record-accessor-dummy))
    record-mutator
    record?
    record-rtd
    record-type-name
    record-type-parent
    record-type-uid
    record-type-generative?
    record-type-sealed?
    record-type-opaque?
    record-type-field-names
    record-field-mutable?
    record-type-descriptor?
    condition
    simple-conditions
    condition?
    condition-accessor
    condition-predicate
    throw
    (< <)
    (<= <=)
    (> >)
    (>= >=)
    (= =)
    (+ +)
    (- -)
    (* *)
    (/ /)
    (max max)
    (min min)
    lookahead-char
    bytevector?
    current-directory
    standard-library-path
;    quotient
;    remainder
    native-endianness
    make-bytevector
    make-bytevector
    bytevector-length
    bytevector=?
    bytevector-fill!
    bytevector-copy!
    bytevector-copy
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector-s8-ref
    bytevector-s8-set!
    bytevector->u8-list
    u8-list->bytevector
    bytevector-u16-ref
    bytevector-s16-ref
    bytevector-u16-native-ref
    bytevector-s16-native-ref
    bytevector-u16-set!
    bytevector-s16-set!
    bytevector-u16-native-set!
    bytevector-s16-native-set!
    bytevector-u32-ref
    bytevector-s32-ref
    bytevector-u32-native-ref
    bytevector-s32-native-ref
    bytevector-u32-set!
    bytevector-s32-set!
    bytevector-u32-native-set!
    bytevector-s32-native-set!
    bytevector-u64-ref
    bytevector-s64-ref
    bytevector-u64-native-ref
    bytevector-s64-native-ref
    bytevector-u64-set!
    bytevector-s64-set!
    bytevector-u64-native-set!
    bytevector-s64-native-set!
    bytevector->string
    string->bytevector
    string->utf8
    utf8->string
    string->utf16
    string->utf32
    utf16->string
    utf32->string
    get-line
    close-port
    make-instruction
    make-compiler-instruction
    fasl-write
    fasl-read
    get-string-n
    stat-mtime
    rational?
    flonum?
    fixnum?
    bignum?
    fixnum-width
    least-fixnum
    greatest-fixnum
    make-rectangular
    real-part
    imag-part
    exact?
    inexact?
    exact
    inexact
    nan?
    infinite?
    finite?
    real->flonum
    fl=?
    fl<?
    fl>?
    fl>=?
    fl<=?
    flinteger?
    flzero?
    flpositive?
    flnegative?
    flodd?
    fleven?
    flfinite?
    flinfinite?
    flnan?
    flmax
    flmin
    fl+
    fl*
    fl-
    fl/
    flabs
    fldiv
    flmod
    fldiv0
    flmod0
    flnumerator
    fldenominator
    flfloor
    flceiling
    fltruncate
    flround
    flexp
    fllog
    flsin
    flcos
    fltan
    flasin
    flacos
    flatan
    flsqrt
    flexpt
    fixnum->flonum
    bitwise-not
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-bit-count
    bitwise-length
    bitwise-first-bit-set
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-arithmetic-shift
    complex?
    real?
    rational?
    integer?
    real-valued?
    rational-valued?
    integer-valued?
    fx=?
    fx>?
    fx<?
    fx>=?
    fx<=?
    fxzero?
    fxpositive?
    fxnegative?
    fxodd?
    fxeven?
    fxmax
    fxmin
    fx+
    fx*
    fx-
    fxdiv
    fxmod
    fxdiv0
    fxmod0
    fxnot
    fxand
    fxior
    fxxor
    fxif
    fxbit-count
    fxlength
    fxfirst-bit-set
    fxbit-set?
    fxcopy-bit
    fxbit-field
    fxcopy-bit-field
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxrotate-bit-field
    fxreverse-bit-field
    bytevector-ieee-single-native-ref
    bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref
    bytevector-ieee-double-ref
    bytevector-ieee-single-native-set!
    bytevector-ieee-single-set!
    bytevector-ieee-double-native-set!
    bytevector-ieee-double-set!
    even?
    odd?
    abs
    (div quotient)
    div0
    numerator
    denominator
    floor
    ceiling
    truncate
    round
    exp
    log
    sin
    cos
    tan
    asin
    acos
    sqrt
    magnitude
    angle
    atan
    expt
    make-polar
    string-copy
    vector-fill!
    ungensym
    (disasm (lambda (closure)
              (let ([code (vector-ref closure 0)]
                    [ht (make-hash-table)])
                (print code)
                (let loop ([i 0])
                  (if (= i (vector-length code))
                      '()
                      (cond
                       [(eq? (vector-ref code i) 'LOCAL_JMP)
                        (let1 n (vector-ref code (+ i 1))
                          (hash-table-put! ht (+ i n 1) (format "<LABEL~a>" (+ i n 1)))
                          (vector-set! code (+ i 1) (format "<To~a>" (+ i n 1)))
                          (loop (+ i 2)))]
                       [(eq? (vector-ref code i) 'TEST)
                        (let1 n (vector-ref code (+ i 1))
                          (hash-table-put! ht (+ i n 1) (format "<LABEL~a>" (+ i n 1)))
                          (vector-set! code (+ i 1) (format "<To~a>" (+ i n 1)))
                          (loop (+ i 2)))]
                       [else
                        (loop (+ i 1))])))
                (let loop ([i 0])
                  (if (= i (vector-length code))
                      '()
                      (begin
                        (when (hash-table-get ht i #f)
                            (format #t "~a " (hash-table-get ht i)))
                        (format #t "~a " (vector-ref code i))
                        (loop (+ i 1))))))))

;;     (disasm (lambda (closure)
;;               (for-each
;;                (lambda (c)
;;                  (format #t "~a " c))
;;               (vector->list (vector-ref closure 0)))))
    print-stack
    fast-equal?
    native-eol-style
    buffer-mode?
    microseconds
    local-tz-offset
    %fork
    %exec
    %waitpid
    %pipe
    current-directory
    set-current-directory!
    binary-port?
    input-port?
    lookahead-u8
    ;; ffi
    %ffi-open
    %ffi-lookup
    %ffi-call->int
    %ffi-call->void
    %ffi-call->void*
    %ffi-call->string-or-zero
    %ffi-pointer->string
    %ffi-pointer-ref
    %ffi-supported?
    output-port?
    textual-port?
    port?
    )
