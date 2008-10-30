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
    string-ci-hash
    symbol-hash
    equal-hash
    (eq-hashtable-copy eq-hashtable-copy)
    (current-error-port current-error-port)
    values
    (vm/apply vm/apply)
    (pair? pair?)
    (init-library-table init-library-table) ;; for test
    (make-custom-binary-input-port (lambda (id read! get-position set-position! close) (display "make-custom-binary-input-port not implemented")))
    get-u8
    bytevector-u8-set!
    transcoded-port
    utf-8-codec
    make-transcoder
    (eof-object (lambda () (if #f #f)))
    sys-open-bytevector-output-port
    sys-get-bytevector
    bytevector-length
    (standard-input-port standard-input-port)
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
    mod
    div
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
    quotient
    remainder
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
    )
