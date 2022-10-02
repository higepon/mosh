(define-free-vars
  (number? number?)
  (cons cons)
  ;(cons* cons*)
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
  peek-char
  (char=? char=?)
  (string? string?)
  (get-environment-variable sys-getenv)
  get-environment-variables
  (equal? equal?)
  (open-string-input-port open-input-string)
  (open-output-string open-output-string)
  (sys-port-seek port-seek)
;  (open-output-file open-output-file)
  (close-output-port close-output-port)
  (digit->integer digit->integer)
  (get-remaining-input-string get-remaining-input-string)
  (directory-list sys-readdir)
  (file-exists? file-exists?)
  delete-file
  (get-output-string get-output-string)
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
  (caaaar caaaar)
  (caaadr caaadr)
  (caaar caaar)
  (caadar caadar)
  (caaddr caaddr)
  (caadr caadr)
  (caar caar)
  (cadaar cadaar)
  (cadadr cadadr)
  (cadar cadar)
  (caddar caddar)
  (cadddr cadddr)
  (caddr caddr)
  (cadr cadr)
  (cdaaar cdaaar)
  (cdaadr cdaadr)
  (cdaar cdaar)
  (cdadar cdadar)
  (cdaddr cdaddr)
  (cdadr cdadr)
  (cdar cdar)
  (cddaar cddaar)
  (cddadr cddadr)
  (cddar cddar)
  (cdddar cdddar)
  (cddddr cddddr)
  (cdddr cdddr)
  (cddr cddr)
  symbol=?
  boolean=?
  (vector? vector?)
  (list? list?)
  (list list)
  (memq memq)
  (eq? eq?)
  (eqv? eqv?)
  (member member)
  (boolean? boolean?)
  (symbol->string symbol->string)
  (string-ref string-ref)
;  (error error)
;  (assertion-violation error)
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
  (make-custom-binary-output-port (lambda (id write! get-position set-position! close) (display "make-custom-binary-output-port not implemented")))
  (make-custom-textual-input-port (lambda (id read! get-position set-position! close) (display "make-custom-textual-input-port not implemented")))
  (make-custom-textual-output-port (lambda (id write! get-position set-position! close) (display "make-custom-textual-output-port not implemented")))
  get-u8
  put-u8
  put-string
  flush-output-port
  output-port-buffer-mode
  bytevector-u8-set!
  port-has-port-position?
  port-has-set-port-position!?
  port-position
  set-port-position!
  get-bytevector-n!
  get-bytevector-some
  get-bytevector-all
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
;  (open-input-file open-input-file)
  (close-input-port close-input-port)
  (vector vector)
  (regexp-replace regexp-replace)
  (regexp-replace-all regexp-replace-all)
  (source-info (lambda (x) #f))
  (eval (lambda a (evaluate (car a))))
  eval-compiled
  (apply apply-proc)
  (assq assq)
  (assoc assoc)
  (assv assv)
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
  %call-process
  %confstr
  %dup
  %start-process
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
  (list-transpose+ (lambda ll
                     (define (map1 f l)
                       (if (null? l)
                           l
                           (cons (f (car l)) (map1 f (cdr l)))))
                     (define (map proc . ll)
                       (if (null? (car ll))
                           '()
                           (if (null? (cdr ll))
                               (map1 proc (car ll))
                               (let ((tetes (map1 car ll))
                                     (queues (map1 cdr ll)))
                                 (cons (apply proc tetes)
                                       (apply map (cons proc queues)))))))
                     (let loop ([lst ll]
                                [ret '()])
                       (cond
                        [(null? (car lst)) (reverse ret)]
                        [else
                         (loop (map cdr lst) (cons (map car lst) ret))]))))
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
;;   (make-record-type-descriptor (lambda e 'make-record-type-descriptor-dummy))
;;   (make-record-constructor-descriptor (lambda e 'make-record-constructor-descriptor-dummy))
;;   record-predicate
;;   (record-constructor (lambda e 'record-constructor-dummy))
;;   (record-accessor (lambda e 'record-accessor-dummy))
;;   record-mutator
;;   record?
;;   record-rtd
;;   record-type-name
;;   record-type-parent
;;   record-type-uid
;;   record-type-generative?
;;   record-type-sealed?
;;   record-type-opaque?
;;   record-type-field-names
;;   record-field-mutable?
;;   record-type-descriptor?
;;   condition
;;   simple-conditions
;;   condition?
;;   condition-accessor
;;   condition-predicate
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
  get-char
  lookahead-char
  get-string-n
  get-string-n!
  get-string-all
  get-line
  get-datum
  bytevector?
  current-directory
  standard-library-path
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
  null-terminated-bytevector->string
  null-terminated-utf8->string
  string->utf16
  string->utf32
  utf16->string
  utf32->string
  close-port
  make-instruction
  make-compiler-instruction
  fasl-write
  fasl-read
  get-string-n
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
  %getpid
  current-directory
  set-current-directory!
  binary-port?
  input-port?
  port-eof?
  lookahead-u8
  open-bytevector-input-port
  ;; ffi
  %ffi-open
  %ffi-lookup
  %ffi-call
  %ffi-supported?
  %ffi-malloc
  %ffi-free
  %ffi-make-c-callback-trampoline
  %ffi-free-c-callback-trampoline
  %ffi-close
  %ffi-error
  host-os
  output-port?
  textual-port?
  port?
  port-transcoder
  native-transcoder
  put-bytevector
  put-char
  write-char
  transcoder-codec
  transcoder-eol-style
  transcoder-error-handling-mode
  ;; r5rs
  quotient
  remainder
  modulo
  ;; i/o
  open-file-input/output-port
  make-custom-binary-input/output-port
  make-custom-textual-input/output-port
  ;;
  put-datum
  (list-ref list-ref)
  (list-tail list-tail)
  time-usage
  mosh-executable-path

  ;; socket
  socket?
  socket-accept
  make-client-socket
  make-server-socket
  os-constant
  socket-recv
  socket-recv!
  socket-send
  socket-close
  socket-shutdown
  socket-port
  ;; mutliple vm
  make-vm
  vm-start!
  vm?
  vm-set-value!
  vm-join!
  main-vm?
  vm-self
  register
  whereis
  ;; condition-variable
  make-condition-variable
  condition-variable-wait!
  condition-variable-notify!
  condition-variable-notify-all!
  ;; mutex
  mutex?
  make-mutex
  mutex-lock!
  mutex-try-lock!
  mutex-unlock!

  make-vector
  vector-length
  vector-ref
  vector-set!

  ;; file system
  create-directory
  delete-directory
  rename-file
  create-symbolic-link
  file-directory?
  file-symbolic-link?
  file-regular?
  file-readable?
  file-executable?
  file-writable?
  file-size-in-bytes
  file-stat-mtime
  file-stat-atime
  file-stat-ctime

  ;; pointer
  pointer?
  pointer->integer
  integer->pointer ;; temp
  pointer-ref-c-uint8
  pointer-ref-c-uint16
  pointer-ref-c-uint32
  pointer-ref-c-uint64
  pointer-ref-c-int8
  pointer-ref-c-int16
  pointer-ref-c-int32
  pointer-ref-c-int64
  pointer-ref-c-signed-char
  pointer-ref-c-unsigned-char
  pointer-ref-c-signed-short
  pointer-ref-c-unsigned-short
  pointer-ref-c-signed-int
  pointer-ref-c-unsigned-int
  pointer-ref-c-signed-long
  pointer-ref-c-unsigned-long
  pointer-ref-c-signed-long-long
  pointer-ref-c-unsigned-long-long
  pointer-ref-c-float
  pointer-ref-c-double
  pointer-ref-c-pointer
  pointer-set-c-int8!
  pointer-set-c-int16!
  pointer-set-c-int32!
  pointer-set-c-int64!
  pointer-set-c-uint8!
  pointer-set-c-uint16!
  pointer-set-c-uint32!
  pointer-set-c-uint64!
  pointer-set-c-char!
  pointer-set-c-short!
  pointer-set-c-int!
  pointer-set-c-long!
  pointer-set-c-long-long!
  pointer-set-c-float!
  pointer-set-c-double!
  pointer-set-c-pointer!
  pointer-copy!
  bytevector-pointer
  shared-errno
  simple-struct?
  make-simple-struct
  simple-struct-ref
  simple-struct-set!
  simple-struct-name
  lookup-nongenerative-rtd
  nongenerative-rtd-set!
  ;; for faster psyntax.
  same-marks*?
  same-marks?
  id->real-label
  join-wraps
  gensym-prefix-set!

  ;; for dynamic-wind
  current-dynamic-winders

  ;; for nmosh
  sexp-map
  sexp-map/debug
  write/ss

  ;; for Mona
  %monapi-message-send
  %monapi-name-whereis
  %monapi-message-receive
  %monapi-name-add!
  %monapi-message-send-receive
  %monapi-message-reply
  %monapi-make-stream
  %monapi-stream-handle
  %monapi-stream-write
  %monapi-stream-read

  process-list
  process-terminate!

  socket-sslize!
  ssl-socket?
  ssl-supported?

  file->string

  ;; annotated pair
  (annotated-cons cons)
  annotated-pair?
  get-annotation
  set-annotation!

  ;; for nmosh
  pointer->object
  object->pointer

  ;; for 0.2.8
  (set-current-error-port! current-error-port)
  )
