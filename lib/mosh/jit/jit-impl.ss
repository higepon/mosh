;; refactoring
;; opcode
;; (& base-reg
;; 引数の順序
;; r/m 64 と r/64 の順序関係ないね

;; http://home.earthlink.net/~krautj/sassy/sassy-Z-H-7.html#node_sec_5.1.1

;       movq    80(%rsp), %rbx

;;     Object ac_;  // accumulator     register
;;     Object dc_;  // display closure register, used for refer-free
;;     Object cl_;  // current closure register, used for profiler.
;;     Object* fp_; // frame pointer   register
;;     Object* sp_; // stack pointer   register
;;     Object* pc_; // program counter register

;;
;; x86-64 ABI : http://homepage1.nifty.com/herumi/prog/x64.html
;;
;; ========================================================
;;                                             JIT
;;     rdi            argument1       -        theVM
;;     rsi            argument2       -        argc
;;     rdx            argument3       -        argv
;;     rcx            argument4       -          -
;;     r8             argument5       -          -
;;     r9             argument6       -          -
;;     r10, r11           -           -          -
;;     r12〜r15           -      callee saved    -
;;     rbx, rbp, rsp      -      callee saved    -
;; ========================================================

;; r64->number of r8-r15 over 2^3, so rex.prefix is used for one more bit

(define (REFER_GLOBAL id)
  (let ([not-found-case (gensym)]
        [not-raw-pointer-case (gensym)]
        [heap-object-case (gensym)])
    `((movq rbx ,(obj->integer id))
      (testb bl 3)                  ;; rbx.isRawPointer?
      (jne ,not-raw-pointer-case)   ;;
      (movq rax  (& rbx))           ;; rax = *rbx
      (movq rdx rax)                ;; rdx = rax
      (andl edx 3)                  ;; rdx.isHeapObject
      (cmpq rdx 3)                  ;;
      (je ,not-raw-pointer-case)    ;;
      ;; rdx.isHeapObject
      ,(DEBUGGER)
      (label ,heap-object-case)
      ,(DEBUGGER)
      (label ,not-found-case)
      ,(DEBUGGER)
      (label ,not-raw-pointer-case)
      (movq rax ,(vm-register 'namespace)) ;; rax = namespace
      (push rdi)
      (push rsi)
      (push rdx)
      (movq rdx ,(vm-register 'not-found)) ;; rdx = not_found arg3
      (movq rsi rbx)                       ;; rsi = id        arg2
      (movq rdi (& rax 8))                 ;; rdi = namespace.toEqHashTable
      (movq rax (& rdi))
;;     movq    8(%rax), %rdi  ;rdi = 

;;     movq    56(%rsp), %rdx ;;VM
;;     movq    %rbx, %rsi     ;;rbx
;;     movq    80(%rdx), %rax ;rax = namespace
;;     movq    88(%rdx), %rdx ;rdx = not_found
;;     movq    8(%rax), %rdi  ;rdi = 
;;     movq    (%rdi), %rax


;;     movq    80(%rsp), %rsi ;; rsi = vm
;;     movq    (%rcx), %rax   ;; rax = *namespace
;;     movq    88(%rsi), %rdx ;; rdx = not_found arg3
;;     movq    %rcx, %rdi     ;; rdi = namespace arg1
;;     movq    %rbx, %rsi     ;; rsi = id        arg2
;;     call    *24(%rax)      ;; call

;      ,(DEBUGGER)
      (callq (& rax 24))                   ;; call
;      ,(DEBUGGER)
      (pop rdx)
      (pop rsi)
      (pop rdi)
      (cmpq ,(vm-register 'not-found) rax) ;; rax = not_found ?
      (je ,not-found-case)
      (movq rdx (& rax 8))                 ;; ac = gloc.value
      (movq rdx (& rdx))

      (movq ,(vm-register 'ac) rdx))))

;;     movq    96(%rsp), %rcx ;; rcx = namespace
;;     movq    80(%rsp), %rsi ;; rsi = vm
;;     movq    (%rcx), %rax   ;; rax = *namespace
;;     movq    88(%rsi), %rdx ;; rdx = not_found arg3
;;     movq    %rcx, %rdi     ;; rdi = namespace arg1
;;     movq    %rbx, %rsi     ;; rsi = id        arg2
;;     call    *24(%rax)      ;; call
;;     movq    80(%rsp), %rdx ;; rdx = vm
;;     cmpq    %rax, 88(%rdx) ;; rax = not_found ?
;;     je  .L1212 ;; error
;;     movq    %rdx, %rbx     ;; rbx = vm
;;     movq    8(%rax), %rdx  ;; rdx =
;;     movq    (%rdx), %rdx
;;     movq    %rdx, 8(%rbx)  ;; ac = gloc.value
;;     movq    48(%rbx), %rdx ;; rdx = pc
;;     movq    %rax, -8(%rdx) ;; *(pc - 1) = gloc


    ;; todo gloc cache
;    (movq (& rbx 8) rdx)
;;     movq    %rdx, %rbx     ;; rbx = vm
;;     movq    8(%rax), %rdx  ;; rdx =
;;     movq    (%rdx), %rdx
;;     movq    %rdx, 8(%rbx)  ;; ac = gloc.value
;;     movq    48(%rbx), %rdx ;; rdx = pc
;;     movq    %rax, -8(%rdx) ;; *(pc - 1) = gloc

;; # -- REFER_GLOBAL start
;;     movq    80(%rsp), %rcx ;; rcx = vm
;;     movq    48(%rcx), %rax ;; rax = pc
;;     movq    (%rax), %rbx   ;; rbx = *pc
;;     addq    $8, %rax       ;;
;;     movq    %rax, 48(%rcx) ;; pc = pc+8
;;     testb   $3, %bl        ;; rbx.isRawPointer?
;;     jne .L598              ;; rbx is not RawPointer
;;     movq    (%rbx), %rax   ;; rax = *rbx
;;     movq    %rax, %rdx     ;; rdx = rax
;;     andl    $3, %edx       ;; rdx = heapObject?
;;     cmpq    $3, %rdx
;;     je  .L1211             ;; goto heap object case
;; .L598:
;;     movq    96(%rsp), %rcx ;; rcx = namespace
;;     movq    80(%rsp), %rsi ;; rsi = vm
;;     movq    (%rcx), %rax   ;; rax = *namespace
;;     movq    88(%rsi), %rdx ;; rdx = not_found arg3
;;     movq    %rcx, %rdi     ;; rdi = namespace arg1
;;     movq    %rbx, %rsi     ;; rsi = id        arg2
;;     call    *24(%rax)      ;; call
;;     movq    80(%rsp), %rdx ;; rdx = vm
;;     cmpq    %rax, 88(%rdx) ;; rax = not_found ?
;;     je  .L1212 ;; error
;;     movq    %rdx, %rbx     ;; rbx = vm
;;     movq    8(%rax), %rdx  ;; rdx =
;;     movq    (%rdx), %rdx
;;     movq    %rdx, 8(%rbx)  ;; ac = gloc.value
;;     movq    48(%rbx), %rdx ;; rdx = pc
;;     movq    %rax, -8(%rdx) ;; *(pc - 1) = gloc
;; .L599:
;; # -- REFER_GLOBAL end

;; .L1211:
;;     cmpq    $135, %rax
;;     jne .L598
;;     movq    8(%rbx), %rax
;;     movq    (%rax), %rax
;;     movq    %rax, 8(%rcx)
;;     .p2align 4,,3
;;     .p2align 3
;;     jmp .L599


(define vm-register* '(ac dc cl fp sp pc _ _ _ namespace not-found))

;; VM register offset depends on your architecture.
(include/resolve ("mosh" "jit") "offset.ss")

(define (vm-register reg)
  `(& rdi ,(* (+ (receive (_ index) (find-with-index (cut eq? <> reg) vm-register*)
                   index) 1) vm-register-offset)))

(define r64* '(rax rcx rdx rbx rsp rbp rsi rdi r8  r9  r10 r11 r12 r13 r14 r15))
(define r32* '(eax ecx edx ebx esp ebp esi edi))

(define r8* '(al  cl  dl bl  ah  ch  dh  bh))
(define (r64? obj) (memq obj r64*))
(define (r32? obj) (memq obj r32*))
(define (r8? obj) (memq obj r8*))

(define (find-index-eq x x*)
  (receive (_ index) (find-with-index (cut eq? <> x) x*)
    index))

(define (r64->number reg) (find-index-eq reg r64*))
(define (r32->number reg) (find-index-eq reg r32*))
(define (r8->number reg) (find-index-eq reg r8*))

(define (number->r8 n)
  (list-ref r8* n))
(define (number->r32 n)
  (list-ref r32* n))
(define (number->r64 n)
  (list-ref r64* n))


(define (r32->64 reg)
  (number->r64 (r32->number reg)))

(define (r64->32 reg)
  (number->r32 (r64->number reg)))

(define (find-with-index pred lst)
  (let loop ([i 0]
             [lst lst])
    (cond
     [(null? lst)
      (values #f #f)]
     [(pred (car lst))
      (values (car lst) i)]
     [else
      (loop (+ i 1) (cdr lst))])))


;; See Table 2-1. 16-Bit Addressing Forms with the ModR/M Byte
(define mod.disp0    #b00)
(define mod.disp8    #b01)
(define mod.disp32   #b10)
(define mod.register #b11)

(define sib.scale1 #b00)
(define sib.scale4 #b10)
(define sib.scale8 #b11)

(define (mod-r-r/m mod reg-n r/m-n)
  (+ (bitwise-arithmetic-shift-left mod 6)
     (bitwise-arithmetic-shift-left (bitwise-and reg-n #b111) 3)
     (bitwise-and r/m-n #b111)))

(define (sib scale index base)
  (+ (bitwise-arithmetic-shift-left scale 6)
     (bitwise-arithmetic-shift-left (bitwise-and index #b111) 3)
     (bitwise-and base #b111)))

(define (compose-reg mod r64 r/m64)
  (values
   (rex-prefix #t (ext-reg? r64) #f (ext-reg? r/m64))
   (mod-r-r/m mod r64 r/m64)
   (cond
    ;; r/m = #b*100 (rsp, r12) requires sib except mod=#x11
    ;;   see http://www.mztn.org/lxasm64/amd05.html
    [(and (not (= mod mod.register)) (memq r/m64 '(#b0100 #b1100)))
     (let ([r64-index #b100])   ;; #b100 means "none", no index.
       (list (sib sib.scale1 r64-index r/m64)))]
    [else '()])))

(define (compose-reg+sib mod r64 scale index base)
  (values
   (rex-prefix #t (ext-reg? r64) (ext-reg? index) #f)
   (mod-r-r/m mod r64 #b100) ;; r/m64 = #b100 is SIB flag.
   (list (sib scale index base))))

(define (pack-op bits64? opcode mod r64 r/m64 scale index base disp0/8/32 imm8/32)
  (receive (rex-prefix modrr/m sib)
      (if scale (compose-reg+sib mod r64 scale index base) (compose-reg mod r64 r/m64))
    (values `(,@(if bits64? (list rex-prefix) '()) ,opcode ,modrr/m ,@sib ,@disp0/8/32 ,@imm8/32) #f)))

;; asm* : list of asm
;; asm  : (assembled-byte* addr-of-net-instruction label-to-fixup)
(define (assemble code*)
  (let loop ([code* code*]
             [addr 0]
             [asm* '()]
             [label* '()])
    (cond
     [(null? code*)
      (append-map (match-lambda
                      [(byte* addr #f) byte*]
                      [(byte* addr label-to-fixup)
                       (cond
                        [(assoc label-to-fixup label*) =>
                         (lambda (x)
                           (let1 offset (- (cdr x) addr)
                            (assert (imm8? offset))
                           (append (drop-right byte* 1) (imm8->u8* offset))))]
                        [else
                         (error 'assemble (format "BUG: label:~a not found on ~a" label-to-fixup label*))])])
                  (reverse asm*))]
     [else
      (match (car code*)
        [('label label)
         (loop (cdr code*)
               addr
               asm*
               (cons (cons label addr) label*))]
        [x
         (let-values (([asm label-to-fixup] (assemble1 x)))
           (loop (cdr code*)
                 (+ (length asm) addr)
                 (cons (list asm (+ (length asm) addr) label-to-fixup) asm*)
                 label*))])])))

(define (imm8? n)
  (and (integer? n) (<= -128 n 127)))

(define (imm16? n)
  (and (integer? n) (<= (- (expt 2 15)) n (- (expt 2 15) 1))))

(define (imm32? n)
  (and (integer? n) (<= (- (expt 2 31)) n (- (expt 2 31) 1))))

(define (imm64? n)
  (and (integer? n) (<= (- (expt 2 63)) n (- (expt 2 63) 1))))

(define (imm8->u8* n)
  (assert (imm8? n))
  (list (bitwise-and n #xff)))

(define (imm32->u8* n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)))

(define (imm64->u8* n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 32) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 40) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 48) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 56) #xff)))

(define (ext-reg? reg)
  (> reg 8))

;; REX prefix
;;    w - operand width.  #t - 64bit
;;    r - modrr/m reg register extra bit
;;    x - sib index register extra bit
;;    b - modrr/m r/m register extra bit
(define (rex-prefix w? r? x? b?)
  (+ #b01000000
     (if w? #b1000 0)
     (if r? #b100  0)
     (if x? #b10   0)
     (if b? #b1    0)))


;; (push r64)
(define ($r64 opcode r64)
  (values `(,(+ opcode r64)) #f))


;; (oprand dest src)
;; returns (values byte* label-to-fixup)
(define (assemble1 code) ;; todo refactoring
  (define rex.w (rex-prefix #t #f #f #f))
  (match code
    ;; NEG r/m64
    ;;   REX.W + F7 /3
    [('negq (? r64? (= r64->number r/m64)))
     (pack-op #t #xf7 mod.register 3 r/m64 #f #f #f '() '())]
    ;; CMOVC r64, r/m64
    ;;   REX.W + 0F 4C /r
    [('cmovl (? r64? (= r64->number r64)) (? r64? (= r64->number r/m64)))
     (values `(,(rex-prefix #t (ext-reg? r64) #f (ext-reg? r/m64)) #x0f #x4c ,(mod-r-r/m mod.register r64 r/m64)) #f)]
    ;; CDQE Valid N.E. RAX ← sign-extend of EAX.
    ;;   REX.W + 98
    [('cltq)
     (values `(,rex.w #x98) #f)]
    ;; TEST r/m8, imm8
    ;;   F6 /0 ib
    [('testb (? r8? (= r8->number r/m8)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #f #xf6 mod.register 0 r/m8 #f #f #f '() imm8)]
    ;; LEAVE
    ;;   C9
    [('leave) (values '(#xc9) #f)]
    ;; PUSH r64
    ;;   50+rd
    [('push (? r64? (= r64->number r64)))
     ($r64 #x50 r64)]
    [('pop (? r64? (= r64->number r64)))
     ($r64 #x58 r64)]
    [('je (? symbol? label))
     (values `(#x74 #x00) label)]
    [('ja (? symbol? label))
     (values `(#x77 #x00) label)]
    [('jmp (? symbol? label))
     (values `(#xeb #x00) label)]
    [('jne (? symbol? label))
     (values `(#x75 #x00) label)]
    ;; CMP r/m64,r64
    ;;   REX.W + 39 /r
    [('cmpq (? r64? (= r64->number r/m64)) (? r64? (= r64->number r64)))
     (pack-op #t #x39 mod.register r64 r/m64 #f #f #f '() '())]
    [('cmpq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? r64? (= r64->number r64)))
     (pack-op #t #x39 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    ;; CMP RAX, imm32 REX.W + 3D id
    [('cmpq 'rax (? imm32? (= imm32->u8* u8*)))
     (values `(,rex.w #x3d ,@u8*) #f)]
    ;; CMP r/m64, imm8
    ;;   REX.W + 83 /7 ib
    [('cmpq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.disp8 7 r/m64 #f #f #f disp8 imm8)]
    [('cmpq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 7 r/m64 #f #f #f '() imm8)]

    ;; CALL r/m64
    ;;   FF /2
    [('callq (? r64? (= r64->number r/m64)))
     (pack-op #f #xff mod.register 2 r/m64 #f #f #f '() '())]
    [('callq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #f #xff mod.disp8 2 r/m64 #f #f #f disp8 '())]
    ;; INT 3
    [('int 3) (values '(#xcc) #f)]
    ;; MOV r/m64, imm32 Valid
    ;;   REX.W + C7 /0
    [('movq (? r64? (= r64->number r/m64)) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.register 0 r/m64 #f #f #f '() imm32)]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq (? r64? (= r64->number r/m64)) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.register r64 r/m64 #f #f #f '() '())]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number base)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8b mod.disp0 r64 #b100 sib.scale8 index base '() '())]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) 0)) ;; special case for zero
     (pack-op #t #x8b mod.disp0 r64 r/m64 #f #f #f '() '())]
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x8b mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    [('movq (? r64? r64) ('& (? r64? r/m64)))
     (assemble1 `(movq ,r64 (& ,r/m64 0)))]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq ('& (? r64? (= r64->number r/m64)) 0) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp0 r64 r/m64 #f #f #f '() '())]
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    [('movq ('& (? r64? (= r64->number r/m64))) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp0 r64 r/m64 #f #f #f '() '())]
    ;; MOV r/m32,r32
    ;;   89 /r
    [('movl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (pack-op #f #x89 mod.register r32 r/m32 #f #f #f '() '()))
    ;; MOV r32,imm32
    ;;   B8+ rd
    [('movl (? r32? (= r32->number r32)) (? imm32? (= imm32->u8* u8*)))
     (values `(,(+ #xb8 r32) ,@u8*) #f)]
    ;; AND r/m32, imm8
    ;;   83 /4 ib
    [('andl (? r32? (= r32->number r/m32)) (? imm8? (= imm8->u8* imm8)))
     (values `(,#x83 ,(mod-r-r/m mod.register 4 r/m32) ,@imm8) #f)]
    ;; SUB AL, imm8
    ;;   2C ib
    [('subb 'al (? imm8? (= imm8->u8* imm8)))
     (values `(,#x2c ,@imm8) #f)]
    ;; SUB r/m32, r32
    ;;   29 /r
    [('subl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (values `(,#x29 ,(mod-r-r/m mod.register r32 r/m32)) #f)]
    ;; MOV r/m64, imm32
    ;;   REX.W + C7 /0
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.disp8 0 r/m64 #f #f #f disp8 imm32)]
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm32? (= imm32->u8* disp32))) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.disp32 0 r/m64 #f #f #f disp32 imm32)]
    ;; ADD r/m64, imm8
    ;;   REX.W + 83 /0 ib Valid N.E.
    [('addq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 0 r/m64 #f #f #f '() imm8)]
    ;; ADD r64, r/m64
    ;;   REX.W + 03 /r
    [('addq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x03 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    ;; SUB r/m64, imm8
    ;;   REX.W + 83 /5 ib
    [('subq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 5 r/m64 #f #f #f '() imm8)]
    ;; RET
    ;;   C3
    [('retq)
     (values '(#xc3) #f)]
    ;; SAR r/m64, imm8
    ;;   REX.W + C1 /7 ib
    [('sarq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #xc1 mod.register 7 r/m64 #f #f #f '() imm8)]
    ;; MOVSXD r64, r/m32
    ;;   REX.W** + 63 /r
    [('movslq (? r64? (= r64->number r64)) (? r32? (= r32->number r/m32)))
     (pack-op #t #x63 mod.register r64 r/m32 #f #f #f '() '())]
    ;; LEA r64,m
    ;;   REX.W + 8D /r
    [('leaq (? r64? (= r64->number r64)) ('& (? imm32? (= imm32->u8* disp32)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8d mod.disp0 r64 #b100 sib.scale8 index (r64->number 'rbp) disp32 '())] ;; base is omitted and disp32 => base = rbp
    [('leaq dest-reg ('& (? r64? src-reg)))
     (assemble1 `(leaq ,dest-reg (& ,src-reg 0)))]
    ;; (& displacement (* scale index-reg)) is disp32
    [('leaq (? r64? r64) ('& ('* (? r64? index-reg) 8)))
     (assemble1 `(leaq ,r64 (& 0 (* ,index-reg 8))))]
    [('leaq (? r64? (= r64->number r64)) ('& (? imm32? (= imm32->u8* disp32)) ('* (? r64? (= r64->number index)) 4)))
     (pack-op #t #x8d mod.disp0 r64 #b100 sib.scale4 index (r64->number 'rbp) disp32 '())] ;; base is omitted and disp32 => base = rbp
    [('leaq (? r64? (= r64->number r64)) ('& (? imm8? (= imm8->u8* disp8)) (? r64? (= r64->number base)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8d mod.disp8 r64 #b100 sib.scale8 index base disp8 '())]
    [('leaq (? r64? (= r64->number r64))  ('& (? r64? (= r64->number base)) 0))
     (pack-op #t #x8d mod.disp0 r64 base #f #f #f '() '())]
    [('leaq (? r64? (= r64->number r64))  ('& (? r64? (= r64->number base)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x8d mod.disp8 r64 base #f #f #f disp8 '())]
    [('leaq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number base)) (? imm32? (= imm32->u8* disp32))))
     (pack-op #t #x8d mod.disp32 r64 base #f #f #f disp32 '())]
    [x
     (error 'assemble "assemble error: invalid syntax" x)]))

(define (vm-make-fixnum n)
  (+ (bitwise-arithmetic-shift-left n 2) 1))

(define (macro-to-fixnum reg)
  `((sarq ,reg 2)                        ; reg = reg >> 2
   (movslq ,reg ,(r64->32 reg)))) ; reg = reg32 (with sign)

(define (macro-refer-local dest-reg fp-reg index-reg)
  `((movq ,dest-reg (& ,fp-reg (* ,index-reg 8)))))

(define (macro-push sp-reg value-reg)
  `((movq (& ,sp-reg) ,value-reg)
    (addq ,sp-reg 8)))



;;  movq    56(%rsp), %rdx ;; rdx = vm
;;  movq    56(%rsp), %rcx ;; rcx = vm
;;  movq    40(%rdx), %rax ;; rax = sp
;;  leaq    -8(%rax), %rdx ;; rdx = sp - 8
;;  movq    %rdx, 40(%rcx) ;; sp = rdx
;;  movq    -8(%rax), %rbp ;; rbp = *(sp - 8)
;;  movl    %ebp, %eax     ;; eax = (32bit)ebp
;;  andl    $3, %eax       ;; eax.isFixnum
;;  subb    $1, %al        ;;
;;  je  .L1231
;;  movq    56(%rsp), %rcx
;;  .L528:                 ;; not fixnum case
;;  movq    %rbp, %rdi     ;; rdi = arg1
;;  movq    8(%rcx), %rsi  ;; rsi = ac
;;  call    _ZN6scheme10Arithmetic3subENS_6ObjectES1_
;;  movq    56(%rsp), %rbx
;;  movq    %rax, 8(%rbx)
;;  movq    56(%rsp), %rsi
;;  cmpq    $86, 8(%rsi)
;;  je  .L1232
;;  movq    56(%rsp), %rdx
;;  movq    40(%rdx), %rax
;;  movq    %rdx, %rcx
;;  movq    8(%rdx), %rdx
;;  movq    %rdx, (%rax)
;;  addq    $8, %rax
;;  movq    %rax, 40(%rcx)


(define (NUMBER_SUB_PUSH)
  (let ([label1 (gensym)]
        [label2 (gensym)]
        [label3 (gensym)]
        )
  `((movq rax ,(vm-register 'sp)) ; rax = sp
    (leaq rdx (& rax -8))         ; rdx = sp - 8
    (movq ,(vm-register 'sp) rdx) ; sp = sp - 8
    (movq rbp (& rax -8))         ; rbp = *(sp - 8) == POP
    (movl eax ebp)                ; eax = (32bit)ebp
    (andl eax 3)                  ; eax.isFixnum
    (subb al 1)                   ;
    (je ,label1)
    ,(DEBUGGER) ;; stack arg is not Fixnum
    (label ,label2)
    ,(DEBUGGER) ;; ac arg is not Fixnum
    (label ,label3)
    ,(DEBUGGER) ;; sub result is Bignum
    (label ,label1) ;; eax.isFixnum
    (movq rdx ,(vm-register 'ac)) ; ac.isFixnum?
    (movl eax edx)
    (andl eax 3)
    (subb al 1)
    (jne ,label2)
    ;; both are fixnum
    (movq rax rbp)
    (sarq rdx 2) ;; ac.toFixnum
    (sarq rax 2) ;; arg.toFixnum
    (subl eax edx) ;; arg - ac
 ;   ,(DEBUGGER)
    (movslq r12 eax) ;; r12 = (32bit)arg1
;    ,(DEBUGGER)
    (leaq rax (& r12 536870912)) ;; rax = r12 + max-fixnum
    (leaq rdx (& 1 (* r12 4))) ;; rdx = makeFixnum(r12)
    (cmpq rax 1073741823)
    (ja ,label3) ;; jump if rax > 1073741823 => Bignum
    (movq ,(vm-register 'ac) rdx) ;; ac = result of subtraction
    (movq rax ,(vm-register 'sp)) ;; PUSH
    (movq (& rax) rdx)
    (addq rax 8)
    (movq ,(vm-register 'sp) rax)
    )
))

;;  .L1231:
;;  movq    8(%rcx), %rdx ;; rdx = ac
;;  movl    %edx, %eax    ;; eax = (32bit)edx
;;  andl    $3, %eax      ;; eax.isFinum
;;  subb    $1, %al
;;  jne .L528
;;  movq    %rbp, %rax    ;; rax = arg1
;;  sarq    $2, %rdx      ;; ac.toFixnum
;;  sarq    $2, %rax      ;; rax =arg1.toFixnum
;;  subl    %edx, %eax    ;; arg1 = arg1 - ac
;;  movslq  %eax,%r12     ;; r12 = (32bit)arg1
;;  leaq    536870912(%r12), %rax ;; rax = r12 + xxx
;;  leaq    1(,%r12,4), %rdx
;;  cmpq    $1073741823, %rax
;;  ja  .L1301
;;  movq    56(%rsp), %rbx
;;  movq    %rdx, 8(%rbx)
;;  jmp .L990

;;  .L990:
;;  movq    56(%rsp), %rsi
;;  cmpq    $86, 8(%rsi) ;; ac.isFalse?
;;  je  .L1232
;;  movq    56(%rsp), %rdx ;; rdx = vm
;;  movq    40(%rdx), %rax ;; rax = sp
;;  movq    %rdx, %rcx     ;; rcx = vm
;;  movq    8(%rdx), %rdx  ;; rdx = ac
;;  movq    %rdx, (%rax)   ;; *sp = ac
;;  addq    $8, %rax       ;; rax = rax + 8
;;  movq    %rax, 40(%rcx) ;; sp = sp + 8



;;  .LVL1232:
;;  movq    %rcx, %rax    ;; rax =
;;  andl    $3, %eax
;;  cmpq    $3, %rax
;;  jne .L785


(define (REFER_LOCAL_PUSH_CONSTANT index constant)
  `((movq rcx ,(vm-register 'sp))
    (movq rdx ,(vm-make-fixnum index))
    (movq rax ,(vm-register 'fp))
    ,@(macro-to-fixnum 'rdx)
    ,@(macro-refer-local 'rax 'rax 'rdx)
    ,@(macro-push 'rcx 'rax)
    (movq ,(vm-register 'sp) rcx)
    (movq rcx ,constant)
    (movq ,(vm-register 'ac) rcx)))

;; *[mosh] RETURN
;; movq 56(%rsp), %rsi ;; ras = vm
;; movq 48(%rsi), %rax ;; rax = pc
;; movq (%rax), %rdx   ;; rdx = *pc
;; addq $8, %rax       ;; rax = pc + 8
;; movq %rax, 48(%rsi) ;; pc = rax
;; sarq $2, %rdx         ;; rdx = rdx.toFixnum()
;; leaq 0(,%rdx,8), %rax ;; rax = rdx * 8 * 1 ;; (leaq rax (& (* 8 rdx)))
;; negq %rax             ;; rax = -rax
;; movq 56(%rsp), %rcx ;; rcx = vm
;; movq %rax, %rdx      ;; rdx = rax
;; addq 40(%rcx), %rdx  ;; rdx = rdx + sp : Object* const sp = sp_ - operand.toFixnum();
;; movq -8(%rdx), %rax  ;; rax = *(sp - 8)
;; movq %rax, 32(%rcx)  ;; fp = rax : fp_ = fpObject.toObjectPointer();
;; movq -16(%rdx), %rax ;; rax = *(sp - 16)
;; movq %rax, 24(%rcx)  ;; cl = rax : cl_ = index(sp, 1);
;; movq -24(%rdx), %rax ;; rax = *(sp - 24)
;; movq %rax, 16(%rcx)  ;; dc = rax : dc_ = index(sp, 2);
;; movq -32(%rdx), %rax ;; rax = *(sp - 32)
;; leaq -32(%rdx), %rcx ;; rcx = sp - 32
;; movq 56(%rsp), %rbx  ;; vm = rbx
;; movq %rax, 48(%rbx)  ;; pc = rax ; pc_ = pcObject.toObjectPointer();
;; movq %rcx, 40(%rbx)  ;; sp = rcx

;; frame_entry:
;;     const Object n = fetchOperand();
;;     VM_ASSERT(n.isFixnum());
;;     const int skipSize = n.toFixnum();
;;     push(Object::makeObjectPointer(pc_ + skipSize - 1));
;;     push(dc_);
;;     push(cl_);
;;     push(Object::makeObjectPointer(fp_));
;;     asm volatile(" \t # -- FRAME end");


;; *[mosh] FRAME
;; movq 56(%rsp), %rsi ;; rax = vm
;; movq 48(%rsi), %rcx ;; rcx = pc
;; movq 40(%rsi), %rdx ;; rdx = sp
;; movq (%rcx), %rax   ;; rax = *pc
;; addq $8, %rcx       ;; rcx = rcx + 8
;; movq %rcx, 48(%rsi) ;; pc = rcx
;; sarq $2, %rax       ;; rax.toFixnum
;; cltq                   ;; rax = (32bit)eax
;; leaq -8(%rcx,%rax,8), %rax ;; rax = [rcx + rax * 8 - 8]

;; movq %rax, (%rdx)   push(Object::makeObjectPointer(pc_ + skipSize - 1));
;; movq 16(%rsi), %rax
;; movq %rax, 8(%rdx)  ;; push(dc_);
;; movq 24(%rsi), %rax
;; movq %rax, 16(%rdx) ;;  push(cl_);
;; movq 32(%rsi), %rax
;; movq %rax, 24(%rdx) ;; push(Object::makeObjectPointer(fp_));
;; addq $32, %rdx
;; movq %rdx, 40(%rsi)

(define (FRAME label) ;; label is not used. pc is
  `((movq rdx ,(vm-register 'sp))
    (movq rax ,(vm-register 'pc)) ;; push(pc)
    (movq (& rdx) rax)            ;; Other JIT instructions don't sync pc. So pc is not coreect. JIT CALL discards pc on FRAME.
    (movq rax ,(vm-register 'dc)) ;; push(dc_)
    (movq (& rdx 8) rax)
    (movq rax ,(vm-register 'cl)) ;; push(cl_)
    (movq (& rdx 16) rax)
    (movq rax ,(vm-register 'fp)) ;; push(fp_)
    (movq (& rdx 24) rax)
    (addq rdx 32)
    (movq ,(vm-register 'sp) rdx)))
;; *[mosh] RETURN
;; movq 56(%rsp), %rsi ;; ras = vm
;; movq 48(%rsi), %rax ;; rax = pc
;; movq (%rax), %rdx   ;; rdx = *pc
;; addq $8, %rax       ;; rax = pc + 8
;; movq %rax, 48(%rsi) ;; pc = rax
;; sarq $2, %rdx         ;; rdx = rdx.toFixnum()
;; leaq 0(,%rdx,8), %rax ;; rax = rdx * 8 * 1 ;; (leaq rax (& (* 8 rdx)))
;; negq %rax             ;; rax = -rax
;; movq 56(%rsp), %rcx ;; rcx = vm
;; movq %rax, %rdx      ;; rdx = rax
;; addq 40(%rcx), %rdx  ;; rdx = rdx + sp : Object* const sp = sp_ - operand.toFixnum();
;; movq -8(%rdx), %rax  ;; rax = *(sp - 8)
;; movq %rax, 32(%rcx)  ;; fp = rax : fp_ = fpObject.toObjectPointer();
;; movq -16(%rdx), %rax ;; rax = *(sp - 16)
;; movq %rax, 24(%rcx)  ;; cl = rax : cl_ = index(sp, 1);
;; movq -24(%rdx), %rax ;; rax = *(sp - 24)
;; movq %rax, 16(%rcx)  ;; dc = rax : dc_ = index(sp, 2);
;; movq -32(%rdx), %rax ;; rax = *(sp - 32)
;; leaq -32(%rdx), %rcx ;; rcx = sp - 32
;; movq 56(%rsp), %rbx  ;; vm = rbx
;; movq %rax, 48(%rbx)  ;; pc = rax ; pc_ = pcObject.toObjectPointer();
;; movq %rcx, 40(%rbx)  ;; sp = rcx
;; *[mosh] RETURN
;; movq 56(%rsp), %rsi ;; ras = vm
;; movq 48(%rsi), %rax ;; rax = pc
;; movq (%rax), %rdx   ;; rdx = *pc
;; addq $8, %rax       ;; rax = pc + 8
;; movq %rax, 48(%rsi) ;; pc = rax
;; sarq $2, %rdx         ;; rdx = rdx.toFixnum()
;; leaq 0(,%rdx,8), %rax ;; rax = rdx * 8 * 1 ;; (leaq rax (& (* 8 rdx)))
;; negq %rax             ;; rax = -rax
;; movq 56(%rsp), %rcx ;; rcx = vm
;; movq %rax, %rdx      ;; rdx = rax
;; addq 40(%rcx), %rdx  ;; rdx = rdx + sp : Object* const sp = sp_ - operand.toFixnum();
;; movq -8(%rdx), %rax  ;; rax = *(sp - 8)
;; movq %rax, 32(%rcx)  ;; fp = rax : fp_ = fpObject.toObjectPointer();
;; movq -16(%rdx), %rax ;; rax = *(sp - 16)
;; movq %rax, 24(%rcx)  ;; cl = rax : cl_ = index(sp, 1);
;; movq -24(%rdx), %rax ;; rax = *(sp - 24)
;; movq %rax, 16(%rcx)  ;; dc = rax : dc_ = index(sp, 2);
;; movq -32(%rdx), %rax ;; rax = *(sp - 32)
;; leaq -32(%rdx), %rcx ;; rcx = sp - 32
;; movq 56(%rsp), %rbx  ;; vm = rbx
;; movq %rax, 48(%rbx)  ;; pc = rax ; pc_ = pcObject.toObjectPointer();
;; movq %rcx, 40(%rbx)  ;; sp = rcx


(define (RETURN n)
  `(;(movq rax ,(vm-register 'pc))
 ;   ,(DEBUGGER)
;    (movq rdx (& rax))
;    (addq rax 8)
 ;   (movq ,(vm-register 'pc) rax)
;    (sarq rdx 2)
;    (leaq rax (& (* rdx 8)))
;    (negq rax)
;    (movq rdx rax)
    (movq rdx ,n)
    (leaq rdx (& (* rdx 8)))
    (negq rdx)
    (addq rdx ,(vm-register 'sp)) ; rdx = sp - n
    (movq rax (& rdx -8))         ; sp -
    (movq ,(vm-register 'fp) rax)
    (movq rax (& rdx -16))
    (movq ,(vm-register 'cl) rax)
    (movq rax (& rdx -24))
    (movq ,(vm-register 'dc) rax)
    (movq rax (& rdx -32))
    (leaq rcx (& rdx -32))
    (movq ,(vm-register 'pc) rax)
    (movq ,(vm-register 'sp) rcx)))

;; (define (RETURN n) ;; pc いらん
;;   `((movq rax ,(vm-register 'pc))
;;     (movq rdx (& rax))
;;     (addq rax 8)
;;     (movq ,(vm-register 'pc) rax)
;;     (sarq rdx 2)
;;     (leaq rax (& (* rdx 8)))
;;     (negq rax)
;;     (movq rdx rax)
;;     (addq rdx (& rcx 40))
;;     (movq rax (& rdx -8))
;;     (movq ,(vm-register 'fp) rax)
;;     (movq rax (& rdx -16))
;;     (movq ,(vm-register 'cl) rax)
;;     (movq rax (& rdx -24))
;;     (movq ,(vm-register 'dc) rax)
;;     (movq rax (& rdx -32))
;;     (leaq rcx (& rdx -32))
;;     (movq ,(vm-register 'pc) rax)
;;     (movq ,(vm-register 'sp) rcx)))


;;             NUM_CMP_LOCAL(<, <, lt);
;;             BRANCH_ON_FALSE;

;;    const Object n = pop();                                                                  \
;;    if (n.isFixnum() && ac_.isFixnum()) {                                                    \
;;        ac_ = Object::makeBool(n.toFixnum() op ac_.toFixnum());                              \
;;    } else if (n.isFlonum() && ac_.isFlonum()) {                                             \
;;        ac_ = Object::makeBool(Flonum::func(n.toFlonum(), ac_.toFlonum()));                  \
;;    } else {                                                                                 \
;;        if (n.isNumber() && ac_.isNumber()) {                                                \
;;            ac_ = Object::makeBool(Arithmetic::func(n, ac_));                                \
;;        } else {                                                                             \
;;            callWrongTypeOfArgumentViolationAfter(this, #opstring, "number", L2(n, ac_));    \
;;            NEXT1;                                                                           \

;; #define BRANCH_ON_FALSE                         \
;;     if (ac_.isFalse()) {                        \
;;         const Object skipSize = fetchOperand(); \
;;         MOSH_ASSERT(skipSize.isFixnum());       \
;;         skip(skipSize.toFixnum() - 1);          \
;;     } else {                                    \
;;         pc_++;                                  \
;;     }



;;       # -- BRANCH_NOT_LT start
;; # 0 "" 2
;; #NO_APP
;; .loc 6 128 0
;; movq 56(%rsp), %rbx ; rbx = vm
;; movq 40(%rbx), %rax ; rax = sp
;; leaq -8(%rax), %rdx ; rdx = sp - 8
;; movq %rdx, 40(%rbx) ; sp = rdx
;; movq -8(%rax), %rdx ; rdx = *sp
;; movl %edx, %eax     ; eax = (32bit)(rdx)
;; movq %rdx, 368(%rsp) ; どこか = *sp

;; andl $3, %eax  ; n.isFixnum
;; subb $1, %al
;; je   .L1270

;; .L1270:
;; movq 8(%rbx), %rcx ; rcx = ac
;; movl %ecx, %eax    ; eax = (32bit)ecx
;; andl $3, %eax      ; ac.isFixnum
;; subb $1, %al
;; jne  .L796
;; sarq $2, %rdx      ; rdx.toFixnum()
;; sarq $2, %rcx      ; rcx.toFixnum()
;; movl $_ZN6scheme6Object5FalseE, %eax ; eax = False
;; cmpq %rcx, %rdx                      ; rcx < rdx ?
;; movl $_ZN6scheme6Object4TrueE, %edx  ; edx = True
;; cmovl    %rdx, %rax                      ; 条件成立で rax = true
;; movq (%rax), %rax                    ; ac = 条件の結果
;; movq %rax, 8(%rbx)                   ;
;; .L799:
;; movq 56(%rsp), %rsi                  ; rsi = vm
;; cmpq $86, 8(%rsi)                    ; ac is false?
;; je   .L1304
;; addq $8, 48(%rsi)
;;       # -- BRANCH_NOT_LT end


;; .L1304:
;; movq 48(%rsi), %rdx    ; rdx = pc
;; movq (%rdx), %rax      ; rax = *pc
;; sarq $2, %rax          ; rax.toFixnum()
;; subl $1, %eax          ; eax = eax - 1
;; cltq                      ; rax = (signed)eax
;; leaq 8(%rdx,%rax,8), %rax ; rax = pc + skip
;; movq %rax, 48(%rsi)   ; pc = pc
;; jmp  .L807

;; .L807:
;;       # -- BRANCH_NOT_LT end













;; movq 56(%rsp), %rbx ; rbx = vm
;; movq 40(%rbx), %rax ; rax = sp
;; leaq -8(%rax), %rdx ;
;; movq %rdx, 40(%rbx) ; sp = rdx
;; movq -8(%rax), %rdx ; rdx = *sp
;; movl %edx, %eax     ; eax = (32bit)(rdx)
;; movq %rdx, 368(%rsp) ; どこか = *sp

;; andl $3, %eax  ; n.isFixnum
;; subb $1, %al

;; (define (BRANCH_NOT_LT label)
;;   '()
;; )

(define (BRANCH_NOT_LT label)
  (let ([label1 (gensym)]
        [label2 (gensym)])
  `((movq rax ,(vm-register 'sp)) ; rax = sp
    (leaq rdx (& rax -8))         ; rdx = sp - 8
    (movq rdx ,(vm-register 'sp)) ; sp = sp - 8
    (movq rdx (& rax -8))         ; rax = *sp
    (movl eax edx)                ; eax = (32bit)(rdx)
    (andl eax 3)                  ; n.isFixnum
    (subb al 1)                   ;
    (je ,label1)                  ;
    ;; ToDo : not Fixnum case here
    ,(DEBUGGER)
    (label ,label2)                  ; ac or n is not fixnum case
    ,(DEBUGGER)
    (label ,label1)
    (movq rcx ,(vm-register 'ac)) ; rcx = ac
    (movl eax ecx)                ; eax = (32bit)(rcx)
    (andl eax 3)                  ; ac.isFixnum
    (subb al 1)
    (jne ,label2)
    (sarq rdx 2)                  ; rdx.toFixnum()
    (sarq rcx 2)                   ; rcx.toFixnum()
    (movq rax ,(get-c-address 'Object::False)) ; eax = pointer to False
    (cmpq rdx rcx)                ; lt condition?
    (movq rdx ,(get-c-address 'Object::True)) ; edx = pointer to True
    (cmovl rax rdx)               ; if condition? then rax = rdx
    (movq rax (& rax))             ; rax = True or False
;    ,(DEBUGGER)
    (movq ,(vm-register 'ac) rax)
    (cmpq ,(vm-register 'ac) 86)  ; ac.isFalse()
    (je ,label))))



;; movq 8(%rbx), %rcx ; rcx = ac
;; movl %ecx, %eax    ; eax = (32bit)ecx
;; andl $3, %eax      ; ac.isFixnum
;; subb $1, %al
;; jne  .L796
;; sarq $2, %rdx      ; rdx.toFixnum()
;; sarq $2, %rcx      ; rcx.toFixnum()
;; movl $_ZN6scheme6Object5FalseE, %eax ; eax = False
;; cmpq %rcx, %rdx                      ; rcx < rdx ?
;; movl $_ZN6scheme6Object4TrueE, %edx  ; edx = True
;; cmovl    %rdx, %rax                      ; 条件成立で rax = true
;; movq (%rax), %rax                    ; ac = 条件の結果
;; movq %rax, 8(%rbx)                   ;
;; .L799:
;; movq 56(%rsp), %rsi                  ; rsi = vm
;; cmpq $86, 8(%rsi)                    ; ac is false?
;; je   .L1304
;; addq $8, 48(%rsi)
;;       # -- BRANCH_NOT_LT end


(define (CONSTANT val)
  `((movq ,(vm-register 'ac) ,val)
    (movq rax ,(vm-register 'ac))))


;; (define (REFER_LOCAL_PUSH_CONSTANT index constant)
;; `(
;; ;  (movq rax ,(vm-register 'pc))
;;   (movq rcx ,(vm-register 'sp))
;;   (movq rdx ,(make-fixnum index))
;; ;  (movq ,(vm-register 'pc) rax)
;;   (movq rax ,(vm-register 'fp))
;;   (sarq rdx 2)                    ;; rdx = rdx >> 2 (= toFixnum())
;;   (movslq rdx edx)       ;; rdx = edx (with sign)
;;   (movq rax (& rax rdx 8))       ;; rax = *(fp + n)
;;   (movq (& rcx) rax)    ;; *sp = rax
;; ;  (movq rdx ,(vm-register 'pc))
;;   (addq rcx 8)
;;   (movq ,(vm-register 'sp) rcx)
;;   (movq rcx ,constant)
;;   (movq ,(vm-register 'ac) rcx)
;; ))

;; This is not VM instruction, but useful for test.
(define (POP)
  `((movq rcx ,(vm-register 'sp))
    (subq rcx 8)
    (movq rax (& rcx))
    (movq ,(vm-register 'ac) rax)
    (movq ,(vm-register 'sp) rcx)))

;; This is not VM instruction, but useful for test.
(define (PUSH)
  `((push rcx)
    (push rax)
    (movq rcx ,(vm-register 'sp))
    (movq rax ,(vm-register 'ac))
    (movq (& rcx) rax)
    (addq rcx 8)
    (movq ,(vm-register 'sp) rcx)
    (pop rax)
    (pop rcx)))


;; Just remove
(define (POP2)
  `((movq rcx ,(vm-register 'sp))
    (subq rcx 8)
    (movq ,(vm-register 'sp) rcx)))


(define (FOREVER)
  '(#xeb #xfe))

(define (DEBUGGER)
  '(int 3))

;;    0:   48 8b 5c 24 38          mov    0x38(%rsp),%rbx
;;    5:   48 8b 43 30             mov    0x30(%rbx),%rax
;;    9:   48 8b 4b 28             mov    0x28(%rbx),%rcx
;;    d:   48 8b 10                mov    (%rax),%rdx
;;   10:   48 83 c0 08             add    $0x8,%rax
;;   14:   48 89 43 30             mov    %rax,0x30(%rbx)
;;   18:   48 8b 43 20             mov    0x20(%rbx),%rax
;;   1c:   48 c1 fa 02             sar    $0x2,%rdx
;;   20:   48 63 d2                movslq %edx,%rdx
;;   23:   48 8b 04 d0             mov    (%rax,%rdx,8),%rax
;;   27:   48 89 01                mov    %rax,(%rcx)
;;   2a:   48 8b 53 30             mov    0x30(%rbx),%rdx
;;   2e:   48 83 c1 08             add    $0x8,%rcx
;;   32:   48 89 4b 28             mov    %rcx,0x28(%rbx)
;;   36:   48 8b 0a                mov    (%rdx),%rcx
;;   39:   48 8d 42 08             lea    0x8(%rdx),%rax
;;   3d:   48 89 43 30             mov    %rax,0x30(%rbx)
;;   41:   48 89 4b 08             mov    %rcx,0x8(%rbx)

(define (gas->sassy gas)
  (cond
   ;; mov %rsp,%rbx
   [(#/([^\s]+)\s+%([^\s]+),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3)) ,(string->symbol (m 2))))]
   ;; mov 0x30(%rbx),%rdx
   [(#/([^\s]+)\s+0x([\d]+)\(%([^\s]+)\),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 4))
        (& ,(string->symbol (m 3)) , (string->number (m 2) 16))))]
   ;; leaq  -8(%rax), %rdx
   [(#/([^\s]+)\s+(-?[\d]+)\(%([^\s]+)\),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (m 1)) ,(string->symbol (m 4))
        (& ,(string->symbol (m 3)) , (string->number (m 2) 16))))]
   ;; mov (%rbx),%rdx
   [(#/([^\s]+)\s+\(%([^\s]+)\),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3))
        (& ,(string->symbol (m 2)))))]
   ;; mov    %rcx,0x28(%rbx)
   [(#/([^\s]+)\s+%([^\s]+),0x(\d+)\(%([^\s]+)\)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q"))
        (& ,(string->symbol (m 4)) ,(string->number (m 3) 16))
        ,(string->symbol (m 2))
        ))]
   ;; add    $0x8,%rcx
   [(#/([^\s]+)\s+\$0x(\d+),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3))
        ,(string->number (m 2) 16)))]
   [else
    (error 'gas->sassy "invalid form" gas)]
   ))

(define (u8*->c-procedure+retq lst)
  (u8-list->c-procedure (append lst (assemble '((retq))))))

;; '(movq rbx (& rsp #x38))

;; )
;; (let ([p (open-string-input-port (car (string-split "mov   0x38(%rsp),%rbx" #\,)))])
;;   (display (read p))
;;   (display (read p)))
;; (gas->sassy "mov   0x38(%rsp),%rbx")

;; ToDo
;; (0) make constant op directory
;; (1) make constant op through assemble
;; (1) vm->reg offset support
