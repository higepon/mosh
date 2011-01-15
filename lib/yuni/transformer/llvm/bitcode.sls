(library (yuni transformer llvm bitcode)
         (export bc->sexp-list)
         (import (rnrs)
                 (shorten)
                 (yuni core)
                 (yuni binary bitstream)
                 (yuni transformer llvm bitcode-dict)
                 (yuni transformer llvm bitcode-decoder))

(define* bitreader
  (bitstream abbcount 
             current-block-id current-blockinfo-block-id
             blockinfo*
             abbrevs* abbrev-bits block-ctx*))

(define* abbrev
  (block-id id op*))

(define* abbrev-op
  ;;type ::= literal array fixed vbr blob align32
  (type value))

(define* block-ctx
  (blockid abbcount abbrevs* abbrev-bits))

(define* (bs-read-vbr (bs bitstream) width)
  (define r (- width 1))
  (define (calc-next pos cur add)
    ;(display (list 'CALC-NEXT pos cur add))(newline)
    (+ cur
       (bitwise-arithmetic-shift-left
         (bitwise-copy-bit add r 0) pos)))
  (define (itr pos cur)
    (let* ((w (bitstream-read bs width))
           (next (calc-next pos cur w)))
      (if (bitwise-bit-set? w r)
        (itr (+ pos r) next)
        next)))
  (let ((r (itr 0 0)))
    ;(display (list '=> r))(newline)
    r))

(define (bitreader-new port)
  (make bitreader
        (current-block-id #f)
        (current-blockinfo-block-id #f)
        (blockinfo* '())
        (abbcount 4)
        (bitstream (port->bitstream port 32 (endianness little)))
        (block-ctx* '())
        (abbrevs* '())
        (abbrev-bits 2)))

(define (bs-read-n bs width n)
  (define (itr cur rest)
    (if (= rest 0)
      (reverse cur)
      (itr (cons (bitstream-read bs width) cur) (- rest 1))))
  (itr '() n))

(define (checkheader br)
  (let-with br ((bs bitstream))
    (let ((m (bs-read-n bs 4 8)))
      (if (equal? m '(2 4 3 4 0 #xC #xE #xD)) ;; BC C0DE
        #t
        #f))))

(define* (br-lookup-abbrev (br bitreader) id)
  (let-with br (current-block-id abbrevs*)
    (define (do-lookup lst)
      (find (^e (let-with e (block-id (target-id id)) 
                  (= target-id id)))
            lst))
    (let ((m (do-lookup abbrevs*)))
      (unless m
        (assertion-violation 'br-lookup-abbrev "unknown abbrev!" id))
      m)))

(define (decode-char6 x)
  (define (char x)
    (char->integer x))
  (cond
    ((<= 0 x 25)  ;; a - z
     (+ x (char #\a)))
    ((<= 26 x 51) ;; A - Z
     (let ((a (- x 26)))
       (+ a (char #\A))))
    ((<= 52 x 61) ;; 0 - 9
     (let ((a (- x 52)))
       (+ a (char #\0))))
    ((= 62 x) ;; .
     (char #\.))
    ((= 63 x) ;; _
     (char #\_))))

(define* (br-run-abbrev (br bitreader) (abb abbrev))
  (let-with br (bitstream)
    (define (read-one continue op)
      (let-with op (type value)
        ;(display (list 'READ-ONE type value))(newline)
        (case type
          ((literal) (continue value))
          ((fixed) 
           (let ((r (bitstream-read bitstream value)))
             (continue r)))
          ((vbr) (continue (bs-read-vbr bitstream value)))
          ((char6) (continue (decode-char6 (bitstream-read bitstream 6))))
          (else (assertion-violation 'br-run-abbrev "invalid op" type)))))
    (define (run-list unit count)
      (define (itr cur rest)
        (define (continue e)
          (itr (cons e cur) (- rest 1)))
        ;(display (list 'run-list rest))(newline)
        (if (= rest 0)
          (reverse cur)
          (read-one continue unit)))
      (itr '() count))
    (define (run-bytevector count)
      (define (read8) (bitstream-read bitstream 8))
      (define (itr cur rest)
        (if (= rest 0)
          (u8-list->bytevector (reverse cur))
          (itr (cons (read8) cur) (- rest 1))))
      (itr '() count))
    (define (run-op* cur op*)
      (if (pair? op*)
        (let ((op (car op*))
              (rest (cdr op*)))
          (let-with op (type value)
            (case type
              ((array)
               (let ((unit (car rest)))
                 (let* ((count (bs-read-vbr bitstream 6))
                        (l (run-list unit count)))
                   ;(display (list '*****ARRAY count unit))(newline)
                   (append (reverse cur) l))))
              ((blob)
               (let* ((len (bs-read-vbr bitstream 6))
                      (bogus0 (bitstream-skip-alignment bitstream 32))
                      (blob (run-bytevector len))
                      (bogus1 (bitstream-skip-alignment bitstream 32)))
                 (reverse (cons blob cur))))
              (else
                (read-one (lambda (e) (run-op* (cons e cur) rest)) op)))))
        (reverse cur)))
    (let-with abb (op*)
      ;(display (list 'RUN op*))(newline)
      (run-op* '() op*))))

(define* (bitcode-read (br bitreader)) ;; => sexp/eof-object
  (define (next) (bitcode-read br))
  (let-with br ((bs bitstream) abbrev-bits current-block-id abbcount abbrevs*)
    (define (return x) ;; process BLOCKINFO (metadata) and return to user
      (when (= current-block-id 0)
        (let ((code (car x)))
          (case code
            ((1) ;; SETBID
             (let ((id (cadr x)))
               (touch! br 
                 (current-blockinfo-block-id id)
                 ;; RESET abbcount...
                 (abbcount 4)))))))
      (if (not (list? x)) (eof-object) 
        (begin
          ;(display x)(newline)
          x)))
    (define (push-blockinfo! abb)
      (let-with br (blockinfo* abbcount abbrevs* (cid current-blockinfo-block-id))
        (let* ((bi0 (find (lambda (e) (= (car e) cid)) blockinfo*)))
          (cond
            (bi0 ;; append
              ;(display (list 'APPEND cid abb))(newline)
              (let ((ctx (cdr bi0)))
                (let-with ctx (abbcount abbrevs*)
                  (touch! ctx 
                    (abbcount (+ abbcount 1))
                    (abbrevs* (cons abb abbrevs*))))))
            (else
              ;(display (list 'NEW cid abb))(newline)
              (touch! br (blockinfo*
                           (cons (cons cid
                                       (make block-ctx
                                             (abbcount 5) ;; first=4
                                             (abbrevs* (list abb))))
                                 blockinfo*))))))))

    (define (load-blockinfo! id)
      (let-with br (blockinfo*)
        (let ((ent (find (lambda (e) (= (car e) id)) blockinfo*)))
          (cond
            (ent (let ((bi (cdr ent)))
                   (let-with bi ((new-abbcount abbcount) (new-abbrevs* abbrevs*))
                     (touch! br
                       (abbcount new-abbcount)
                       (abbrevs* new-abbrevs*))
                     )))
            (else
              (touch! br
                (abbcount 4)
                (abbrevs* '())))))))
    (define (read-vbr6-list count)
      (define (read-one) (bs-read-vbr bs 6))
      (define (itr cur rest)
        (if (= rest 0)
          (reverse cur)
          (itr (cons (read-one) cur) (- rest 1))))
      (itr '() count))
    (define (read-ops count)
      (define (read-one)
        (let ((flag (bitstream-read bs 1)))
          (case flag
            ((1) ;; literal
             (let ((litvalue (bs-read-vbr bs 8)))
               (make abbrev-op
                     (type 'literal)
                     (value litvalue))))
            ((0)
             (let ((encoding (bitstream-read bs 3)))
               (case encoding
                 ((1) ;; Fixed
                  (let ((value (bs-read-vbr bs 5)))
                    (make abbrev-op
                          (type 'fixed)
                          (value value))))
                 ((2) ;; VBR
                  (let ((value (bs-read-vbr bs 5)))
                    (make abbrev-op
                          (type 'vbr)
                          (value value))))
                 ((3) ;; Array
                  (make abbrev-op
                        (type 'array)
                        (value #f)))
                 ((4) ;; Char6
                  (make abbrev-op
                        (type 'char6)
                        (value #f)))
                 ((5) ;; blob
                  (make abbrev-op
                        (type 'blob)
                        (value #f)))))))))
      (define (itr cur rest)
        (if (= rest 0)
          (reverse cur)
          (itr (cons (read-one) cur) (- rest 1))))
      (itr '() count))
    (let ((a (bitstream-read bs abbrev-bits)))
      ;(display (list 'ABB a abbrev-bits 'BITS))(newline)
      (if (eof-object? a)
        a
        (case a ;; something other than an EOF
          ((1) ;; ENTER_SUBBLOCK
           (let* ((blockid (bs-read-vbr bs 8))
                  (newabblen (bs-read-vbr bs 4))
                  (bogus (bitstream-skip-alignment bs 32))
                  (len (bitstream-read bs 32)))
             ;(display (list 'ENTER-SUBBLOCK blockid newabblen bogus len))(newline)
             (touch! br
               (block-ctx* (cons (make block-ctx
                                       (blockid current-block-id)
                                       (abbcount abbcount)
                                       (abbrevs* abbrevs*)
                                       (abbrev-bits abbrev-bits))
                                 block-ctx*))
               (abbrev-bits newabblen)
               (current-block-id blockid))
             (load-blockinfo! blockid)
             `(ENTER_SUBBLOCK ,blockid)))
          ((0) ;; END_BLOCK
           (let* ((bogus (bitstream-skip-alignment bs 32)))
             (let-with br (block-ctx*)
               (if (not (pair? block-ctx*))
                 (touch! br ;; last block closed
                   (block-ctx* '())
                   (abbrevs* '())
                   (abbcount 4)
                   (abbrev-bits 2)
                   (current-block-id #f))
                 (let ((top (car block-ctx*)) ;; nested block
                       (rest (cdr block-ctx*)))
                   (let-with top ((old-blockid blockid)
                                  (old-abbcount abbcount)
                                  (old-abbrevs* abbrevs*)
                                  (old-abbrev-bits abbrev-bits))
                     ;; clear abbrevs into initial state
                     (touch! br
                       (block-ctx* rest) ;; pop block ctx
                       (abbrevs* old-abbrevs*)
                       (abbcount old-abbcount)
                       (abbrev-bits old-abbrev-bits)
                       (current-block-id old-blockid)))))))
           '(END_BLOCK))
          ((2) ;; DEFINE_ABBREV
           (let* ((count (bs-read-vbr bs 5))
                  (op* (read-ops count))
                  (abb (make abbrev
                             (block-id current-block-id)
                             (id abbcount)
                             (op* op*))))
             (cond
               ((= current-block-id 0) ;; BLOCKINFO
                ;(display (list 'DEFINE_ABBREV_BLOCKINFO abbcount op*))(newline)
                (push-blockinfo! abb)
                (touch! br
                  (abbcount (+ 1 abbcount))))
               (else
                 ;(display (list 'DEFINE_ABBREV abbcount op*))(newline)
                 (touch! br ;; add to block local
                   (abbcount (+ 1 abbcount))
                   (abbrevs* (cons abb abbrevs*))))))
           (next))
          ((3) ;; UNABBREV_RECORD
           (let* ((code (bs-read-vbr bs 6))
                  (numops (bs-read-vbr bs 6)))
             (return (cons code (read-vbr6-list numops)))))
          (else ;; lookup and read
            (let* ((m (br-lookup-abbrev br a))
                   (x (br-run-abbrev br m)))
              (return x))))))))

(define (port->bitreader port)
  (let ((br (bitreader-new port)))
    (if (checkheader br)
      br
      (assertion-violation 'port->bitreader "invalid header id"))))

(define (data->string l)
  (list->string (map integer->char l)))

(define (decode-pass2 l)
  (define (dec-paramattr e)
    (define (decode-idx x)
      (case x
        ((#xFFFFFFFF) 'function)
        ((0) 'return)
        (else x)))
    (define (decode-attr x)
      (define (alignstack)
        (let ((n (bitwise-bit-field x 37 40)))
          (if (= n 0)
            '()
            (let ((r (- n 1)))
              (list 'alignstack (expt 2 r))))))
      (define (alignment)
        (let ((n (bitwise-bit-field x 16 32)))
          (if (= n 0)
            '()
            (list 'alignment n))))
      (define (b i) (bitwise-bit-set? x i))
      (define-syntax bit
        (syntax-rules ()
          ((_ num sym)
           (if (b num) '(sym) '()))))
      (append
        (bit 0 zeroext)
        (bit 1 signext)
        (bit 2 noreturn)
        (bit 3 inreg)
        (bit 4 sret)
        (bit 5 nounwind)
        (bit 6 noalias)
        (bit 7 byval)
        (bit 8 nest)
        (bit 9 readnone)
        (bit 10 readonly)
        (bit 11 noinline)
        (bit 12 alwaysinline)
        (bit 13 optsize)
        (bit 14 ssp)
        (bit 15 sspreq)
        (bit 32 nocapture)
        (bit 33 noredzone)
        (bit 34 noimplicitfloat)
        (bit 35 naked)
        (bit 36 inlinehint)
        (alignment)
        (alignstack)))
    (define (decode l)
      (if (pair? l)
        (let ((idx (car l))
              (attr (cadr l))
              (next (cddr l)))
          (cons (cons (decode-idx idx)
                      (decode-attr attr))
                (decode next)))
        '()))
    (if (pair? e)
      (let ((sym (car e)))
        (case sym
          ((ENTRY)
           (cons sym (decode (cdr e))))
          (else e)))
      e))
  (define (dec-symtab e)
    (if (pair? e)
      (let ((sym (car e)))
        (case sym
          ((ENTRY)
           (list sym (cadr e) (data->string (cddr e))))
          (else e)))
      e))
  (define (dec-function e)
    (if (pair? e)
      (let* ((code (car e))
             (arg (cdr e))
             (sym (if (number? code) (record-FUNCTION_BLOCK code) code)))
        (case sym
          ((VALUE_SYMTAB_BLOCK TYPE_SYMTAB_BLOCK)
           (cons sym (map dec-symtab (cdr e))))
          ((BINOP)
           (case (length arg)
             ((3)
              (let ((opval0 (car arg))
                    (opval1 (cadr arg))
                    (opcode (op-binary (caddr arg))))
                (list sym opval0 opval1 opcode))) ;; FIXME: ???
             ((4)
              (let ((opval0 (car arg))
                    (ty (cadr arg))
                    (opval1 (caddr arg))
                    (opcode (op-binary (cadddr arg))))
                (list sym opval0 ty opval1 opcode)))
             (else
               (cons sym arg))))
          (else (cons sym arg))))
      e))
  (define (dec-constants e)
    (if (pair? e)
      (let* ((code (car e))
             (sym (if (number? code) (record-CONSTANTS_BLOCK code) code)))
        (case sym
          ((INTEGER WIDE_INTEGER)
           (let* ((num (cadr e))
                  (res (if (bitwise-bit-set? num 0)
                         (- (bitwise-arithmetic-shift-right num 1))
                         (bitwise-arithmetic-shift-right num 1))))
             (list sym res)))
          ((STRING CSTRING)
           (list sym (data->string (cdr e))))
          (else (cons sym (cdr e)))))
      e))
  (define (dec-globalvar e)
    (let ((pointer-type (list-ref e 0))
          (isconst (if (= 0 (list-ref e 1)) #f #t))
          (initid (let ((a (list-ref e 2)))
                    (if (= a 0) #f (- a 1))))
          (linkage (code-linkage (list-ref e 3)))
          (alignment (list-ref e 4))
          (section (if (< 5 (length e))
                     (list-ref e 5)
                     #f))
          (visibility (if (< 6 (length e))
                        (code-visibility (list-ref e 6))
                        #f))
          (threadlocal (if (< 7 (length e))
                         (if (= 0 (list-ref e 7)) #f #t)
                         #f)))
      `(,pointer-type ,isconst ,initid ,linkage ,alignment
                      ,@(if section (list section) '())
                      ,@(if visibility (list visibility) '())
                      ,@(if threadlocal (list threadlocal) '()))))

  (define (dec-function/module e)
    (let ((type (list-ref e 0))
          (callingconv (code-callingconv (list-ref e 1)))
          (isproto (if (= 0 (list-ref e 2)) #f #t))
          (linkage (code-linkage (list-ref e 3)))
          (paramattr (list-ref e 4))
          (alignment (list-ref e 5))
          (section (list-ref e 6))
          (visibility (code-visibility (list-ref e 7)))
          (gc (if (= 8 (length e))
                #f
                (list-ref e 8))))
      (if gc
        (list type callingconv isproto linkage paramattr 
              alignment section visibility 
              gc)
        (list type callingconv isproto linkage paramattr
              alignment section visibility))))
  (define (dec e)
    (if (pair? e)
      (let ((sym (car e)))
        (case sym
          ((PARAMATTR_BLOCK)
           (cons sym (map dec-paramattr (cdr e))))
          ((CONSTANTS_BLOCK)
           (cons sym (map dec-constants (cdr e))))
          ((GLOBALVAR)
           (cons sym (dec-globalvar (cdr e))))
          ((FUNCTION)
           (cons sym (dec-function/module (cdr e))))
          ((FUNCTION_BLOCK)
           (cons sym (map dec-function (cdr e))))
          ((TRIPLE DATALAYOUT STRING ASM SECTIONNAME DEPLIB GCNAME)
           (list sym (data->string (cdr e))))
          ((MODULE_BLOCK)
           (cons sym (map dec (cdr e))))
          ((METADATA_BLOCK)
           (cons sym (map dec (cdr e))))
          ((VALUE_SYMTAB_BLOCK TYPE_SYMTAB_BLOCK)
           (cons sym (map dec-symtab (cdr e))))
          (else e)))
      e))
  (map dec l))

(define (decode-pass1 l)
  (define (itr k id cur rest)
    ;(display (list 'ITR k))(newline)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (case (car a)
          ((ENTER_SUBBLOCK)
           (let ((block-id (cadr a)))
             ;(display (list 'ENTERING block-id))(newline)
             (itr (lambda (e next) 
                    (itr k id (cons (cons 
                                      (if (= block-id 0)
                                        'BLOCKINFO
                                        (llvm-ir-blockid block-id))
                                      e) cur) next))
                  block-id '() d)))
          ((END_BLOCK)
           (if k 
             (k (reverse cur) d)
             (begin
               (unless (not (pair? d))
                 (assertion-violation 'decode "non matched BLOCKs" d))
               (reverse cur))))
          (else
            (if (= id 0) ;; skip decode when current block is BLOCKINFO
              (itr k id (cons a cur) d)
              (begin
                ;(display (list 'decode id a '=> (bitcode-decode id a)))(newline)
                (itr k id (cons (bitcode-decode id a) cur) d))))))
      (reverse cur)))
  (itr #f 0 '() l))

(define (decode l)
  (decode-pass2
    (decode-pass1 l)))

(define (bc->sexp-list bv)
  (define (return bv)
    (define (itr cur br)
      (let ((r (bitcode-read br)))
        (if (eof-object? r)
          (decode (reverse cur))
          (itr (cons r cur) br))))
    (let* ((port (open-bytevector-input-port bv))
           (br (port->bitreader port)))
      (itr '() br)))
  (let ((magic? (bytevector-u32-ref bv 0 (endianness little))))
    (cond
      ((= magic? #xB17C0DE) ;; BIT CODE
       (let ((magic (bytevector-u32-ref bv 0 (endianness little)))
             (version (bytevector-u32-ref bv 4 (endianness little)))
             (offset (bytevector-u32-ref bv 8 (endianness little)))
             (size (bytevector-u32-ref bv 12 (endianness little)))
             (cputype (bytevector-u32-ref bv 16 (endianness little))))
         (unless (= version 0)
           (assertion-violation 'bc->sexp-list "invalid wrapper version" version))
         (let ((bv-bs (make-bytevector size)))
           (bytevector-copy! bv offset bv-bs 0 size)
           (return bv-bs))))
      (else (return bv)))))


)
