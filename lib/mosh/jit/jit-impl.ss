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

;; register64->number of r8-r15 over 2^3, so rex.prefix is used for one more bit

(define vm-register* '(ac dc cl fp sp pc))

;; VM register offset depends on your architecture.
(include/resolve ("mosh" "jit") "offset.ss")

(define (vm-register reg)
  `(& rdi ,(* (+ (receive (_ index) (find-with-index (cut eq? <> reg) vm-register*)
                   index) 1) vm-register-offset)))

(define register64* '(rax rcx rdx rbx rsp rbp rsi rdi r8  r9  r10 r11 r12 r13 r14 r15))
(define register32* '(eax ecx edx ebx esp ebp esi edi))

(define register8* '(al  cl  dl bl  ah  ch  dh  bh))
(define (register64? obj) (memq obj register64*))
(define (register32? obj) (memq obj register32*))
(define (register8? obj) (memq obj register8*))

(define (find-index-eq x x*)
  (receive (_ index) (find-with-index (cut eq? <> x) x*)
    index))

(define (register64->number reg) (find-index-eq reg register64*))
(define (register32->number reg) (find-index-eq reg register32*))
(define (register8->number reg) (find-index-eq reg register8*))

(define (number->register8 n)
  (list-ref register8* n))
(define (number->register32 n)
  (list-ref register32* n))
(define (number->register64 n)
  (list-ref register64* n))


(define (register32->64 reg)
  (number->register64 (register32->number reg)))

(define (register64->32 reg)
  (number->register32 (register64->number reg)))

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

(define (mod-r-m-encode mod reg-n r/m-n)
  (+ (bitwise-arithmetic-shift-left mod 6)
     (bitwise-arithmetic-shift-left (bitwise-and reg-n #b111) 3)
     (bitwise-and r/m-n #b111)))

(define (mod-r-m mod r/m reg)
  (assert (for-all register64? (list r/m reg)))
  (mod-r-m-encode mod (register64->number reg) (register64->number r/m)))

(define (mod-r-m8 mod r/m reg)
  (assert (for-all register8? (list r/m reg)))
  (mod-r-m-encode mod (register8->number reg) (register8->number r/m)))

(define (mod-r-m32 mod r/m reg)
  (assert (for-all register32? (list r/m reg)))
  (mod-r-m-encode mod (register32->number reg) (register32->number r/m)))

(define-syntax opcode
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'v])))

(define (effective-addr+disp mod r/m-reg reg-reg)
  (assert (for-all register64? (list r/m-reg reg-reg)))
  (values
   (mod-r-m mod r/m-reg reg-reg)
   (cond
    [(eq? r/m-reg 'rsp)
     (list (sib sib.scale1 'rsp 'rsp))]
    [(eq? r/m-reg 'r12)
     (list (sib sib.scale1 'r12 'r12))]
    [else
       '()])))

(define (effective-addr+scale mod r/m-reg reg-reg scale base-reg index-reg)
  (assert (for-all register64? (list r/m-reg reg-reg)))
  (values
   (mod-r-m mod r/m-reg reg-reg)
   (if (eq? r/m-reg 'rsp)
       (list (sib scale base-reg index-reg))
       '())))


(define (effective-addr r/m-reg reg-reg)
  (assert (for-all register64? (list r/m-reg reg-reg)))
  (values (mod-r-m mod.disp0 r/m-reg reg-reg)
          '()))


;; sib = scale, index, base
;; base[index] scale = size of base
(define (effective-addr+scale8 r/m-reg reg-reg base-reg index-reg)
  (effective-addr+scale mod.disp0 r/m-reg reg-reg sib.scale8 base-reg index-reg))

(define (effective-addr+scale88 r/m-reg reg-reg base-reg index-reg)
  (effective-addr+scale mod.disp8 r/m-reg reg-reg sib.scale8 base-reg index-reg))

(define (effective-addr+scale48 r/m-reg reg-reg base-reg index-reg)
  (effective-addr+scale mod.disp0 r/m-reg reg-reg sib.scale4 base-reg index-reg))






(define (sib scaled-index base-reg src-reg)
  (+ (bitwise-arithmetic-shift-left scaled-index 6)
     (bitwise-arithmetic-shift-left (bitwise-and (register64->number src-reg) #b111) 3)
     (bitwise-and (register64->number base-reg) #b111)))


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
                           (append (drop-right byte* 1) (list (imm8->u8 offset)))))]
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

(define (imm16? n)
  (and (integer? n) (<= (- (expt 2 15)) n (- (expt 2 15) 1))))

(define (imm32? n)
  (and (integer? n) (<= (- (expt 2 31)) n (- (expt 2 31) 1))))

(define (imm64? n)
  (and (integer? n) (<= (- (expt 2 63)) n (- (expt 2 63) 1))))


(define (imm8? n)
  (and (integer? n) (<= -128 n 127)))

(define (imm8->u8 n)
  (assert (imm8? n))
  (bitwise-and n #xff))


(define (imm32->u8-list n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)))

(define (imm64->u8-list n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 32) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 40) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 48) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 56) #xff)))

(define (rex-prefix w? r? x? b?)
  (+ #b01000000
     (if w? #b1000 0)
     (if r? #b100  0)
     (if x? #b10   0)
     (if b? #b1    0)))


(define (reg-need-ext-bit? reg)
  (> (register64->number reg) 8))


;; (oprand dest src)
;; returns (values byte* label-to-fixup)
(define (assemble1 code) ;; todo refactoring
  (define rex.w #x48)
  (match code
    ;; NEG r/m64
    ;;   REX.W + F7 /3
    [('negq (? register64? dest))
     (values `(,rex.w ,(opcode #xf7) ,(mod-r-m mod.register dest (number->register64 3))) #f)]
    ;; CMOVC r64, r/m64
    ;;   REX.W + 0F 4C /r
    [('cmovl (? register64? dest) (? register64? src))
     (values `(,rex.w ,(opcode #x0f) #x4c ,(mod-r-m mod.register src dest)) #f)]
    ;; CDQE Valid N.E. RAX ← sign-extend of EAX.
    ;;   REX.W + 98
    [('cltq)
     (values `(,rex.w ,(opcode #x98)) #f)]
    ;; TEST r/m8, imm8
    ;;   F6 /0 ib
    [('testb (? register8? dest) (? imm8? imm8))
     (values `(,(opcode #xf6) ,(mod-r-m8 mod.register dest (number->register8 0)) ,(imm8->u8 imm8)) #f)]
    ;; LEAVE
    ;;   C9
    [('leave) (values '(#xc9) #f)]
    ;; PUSH r64
    ;;   50+rd
    [('push (? register64? reg))
     (values `(,(+ (opcode #x50) (register64->number reg))) #f)]
    [('pop (? register64? reg))
     (values `(,(+ (opcode #x58) (register64->number reg))) #f)]
    [('je (? symbol? label))
     (values `(,(opcode #x74) #x00) label)]
    [('ja (? symbol? label))
     (values `(,(opcode #x77) #x00) label)]
    [('jmp (? symbol? label))
     (values `(,(opcode #xeb) #x00) label)]
    [('jne (? symbol? label))
     (values `(,(opcode #x75) #x00) label)]
    ;; CMP r64, r/m64
    ;;   REX.W + 39 /r
    [('cmpq (? register64? dest) (? register64? src))
     (values `(,rex.w ,(opcode #x39) ,(mod-r-m mod.register dest src)) #f)]
    ;; CMP RAX, imm32
    ;;    REX.W + 3D id
    [('cmpq 'rax (? imm32? imm32))
     (values `(,rex.w ,(opcode #x3d) ,@(imm32->u8-list imm32)) #f)]
    ;; CMP r/m64, imm8
    ;;   REX.W + 83 /7 ib
    [('cmpq ('& (? register64? dest) (? imm8? displacement)) (? imm8? imm8))
     (receive (modrm sib) (effective-addr+disp mod.disp8 dest (number->register64 7))
       (values `(,rex.w ,(opcode #x83) ,modrm ,@sib ,(imm8->u8 displacement) ,(imm8->u8 imm8)) #f))]
    ;; CALL r/m64
    ;;   FF /2
    [('callq (? register64? dest))
     (values `(,(opcode #xff) ,(mod-r-m mod.register dest (number->register64 2))) #f)]
    ;; INT 3
    [('int 3) (values '(#xcc) #f)]
    ;; MOV r/m64, imm32 Valid : Move imm32 sign extended to 64-bits to r/m64.
    ;;   REX.W + C7 /0
    [('movq (? register64? dest) (? imm32? imm32))
     (values `(,rex.w ,(opcode #xc7) ,(mod-r-m mod.register dest (number->register64 0)) ,@(imm32->u8-list imm32)) #f)]
;;     ;; MOV r/m64, imm64 Valid
;;     ;;   REX.W + B8+ rd
;;     [('movq (? register64? dest) (? imm64? imm64))
;;      (values `(,rex.w ,(opcode #xb8) ,(mod-r-m mod.register dest (number->register64 0)) ,@(imm64->u8-list imm64)) #f)]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq (? register64? dest) (? register64? src))
     (values `(,(rex-prefix #t (reg-need-ext-bit? src) #f (reg-need-ext-bit? dest)) ,(opcode #x89) ,(mod-r-m mod.register dest src)) #f)]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq (? register64? dest-reg) ('& base-reg ('* (? register64? index-reg) 8)))
     (receive (modrm sib) (effective-addr+scale8 (number->register64 4) dest-reg base-reg index-reg)
         (values `(,rex.w ,(opcode #x8b) ,modrm ,@sib) #f))]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq dest-reg ('& src-reg displacement))
     (cond
      [(zero? displacement)
       (receive (modrm sib) (effective-addr src-reg dest-reg)
         (values `(,rex.w ,(opcode #x8b) ,modrm ,@sib) #f))]
      ;; disp8
      [(< displacement #xff)
       (receive (modrm sib) (effective-addr+disp mod.disp8 src-reg dest-reg)
         (values `(,rex.w ,(opcode #x8b) ,modrm ,@sib ,(imm8->u8 displacement)) #f))]
      [else
         (error 'assemble "not implemented")])]
    [('movq dest-reg ('& src-reg))
     (assemble1 `(movq ,dest-reg (& ,src-reg 0)))]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq ('& (? register64? dest-reg) displacement) (? register64? src-reg))
     (cond
      [(zero? displacement)
       (receive (modrm sib) (effective-addr dest-reg src-reg)
         (values `(,rex.w ,(opcode #x89) ,modrm) #f))]
      [else
       (receive (modrm sib) (effective-addr+disp mod.disp8 dest-reg src-reg)
         (values `(,rex.w ,(opcode #x89) ,modrm ,(imm8->u8 displacement)) #f))])]
    [('movq ('& (? register64? dest-reg)) (? register64? src-reg))
     (assemble1 `(movq (& ,dest-reg 0) ,src-reg))]
    ;; MOV r/m32,r32
    ;;   89 /r
    [('movl (? register32? dest-reg) (? register32? src-reg))
     (values `(,(opcode #x89) ,(mod-r-m32 mod.register dest-reg src-reg)) #f)]
    ;; MOV r32, imm32
    ;;   B8+ rd
    [('movl (? register32? dest-reg) (? imm32? imm32))
     (values `(,(+ (opcode #xb8) (register32->number dest-reg)) ,@(imm32->u8-list imm32)) #f)]
    ;; AND r/m32, imm8
    ;;   83 /4 ib
    [('andl (? register32? dest-reg) (? imm8? imm8))
     (values `(,(opcode #x83) ,(mod-r-m32 mod.register dest-reg (number->register32 4)) ,(imm8->u8 imm8)) #f)]
    ;; SUB AL, imm8
    ;;   2C ib
    [('subb 'al (? imm8? imm8))
     (values `(,(opcode #x2c) ,(imm8->u8 imm8)) #f)]
    [('subl (? register32? dest-reg) (? register32? src-reg))
     (values `(,(opcode #x29) ,(mod-r-m32 mod.register dest-reg src-reg)) #f)]
    ;; MOV r/m64, imm32
    ;;   REX.W + C7 /0
    [('movq ('& (? register64? dest-reg) (? imm8? displacement)) (? imm32? imm32))
       (receive (modrm sib) (effective-addr+disp mod.disp8 dest-reg (number->register64 0))
         (values `(,rex.w ,(opcode #xc7) ,modrm ,(imm8->u8 displacement) ,@(imm32->u8-list imm32)) #f))]
    [('movq ('& (? register64? dest-reg) (? imm32? displacement)) (? imm32? imm32))
       (receive (modrm sib) (effective-addr+disp mod.disp32 dest-reg (number->register64 0))
         (values `(,rex.w ,(opcode #xc7) ,modrm ,@sib ,@(imm32->u8-list displacement) ,@(imm32->u8-list imm32)) #f))]
    ;; ADD r/m64, imm8 : REX.W + 83 /0 ib Valid N.E.
    [('addq (? register64? dest-reg) (? imm8? imm8))
     (values `(,rex.w ,(opcode #x83) ,(mod-r-m mod.register dest-reg (number->register64 0)) ,(imm8->u8 imm8)) #f)]
    [('addq (? register64? dest-reg) ('& (? register64? src-reg) (? imm8? displacement)))
       (receive (modrm sib) (effective-addr+disp mod.disp8 src-reg dest-reg )
         (values `(,rex.w ,(opcode #x03) ,modrm ,(imm8->u8 displacement)) #f))]
    [('subq (? register64? dest-reg) (? imm8? imm8))
     (values `(,rex.w ,(opcode #x83) ,(mod-r-m mod.register dest-reg (number->register64 5)) ,(imm8->u8 imm8)) #f)]
    ;; RET : C3
    [('retq)
     (values '(#xc3) #f)]
    ;; SAR r/m64, imm8 : REX.W + C1 /7 ib
    [('sarq (? register64? dest-reg) (? imm8? imm8))
     (values `(,rex.w ,(opcode #xc1) ,(mod-r-m mod.register dest-reg (number->register64 7)) ,(imm8->u8 imm8)) #f)]
    ; MOVSXD r64, r/m32 : REX.W** + 63 /r
    [('movslq (? register64? dest-reg) (? register32? src-reg))
     (values `(,rex.w ,(opcode #x63) ,(mod-r-m mod.register dest-reg (register32->64 src-reg))) #f)
     ]
    [('leaq dest-reg ('& (? register64? src-reg)))
     (assemble1 `(leaq ,dest-reg (& ,src-reg 0)))]
    ;; (& displacement (* scale index-reg)) is disp32
    [('leaq (? register64? dest-reg) ('& ('* (? register64? index-reg) 8)))
     (assemble1 `(leaq ,dest-reg (& 0 (* ,index-reg 8))))]
    [('leaq (? register64? dest) ('& (? imm32? displacement) ('* (? register64? index-reg) 8)))
     ;; base is omitted and disp32 => base = rbp
     (receive (modrm sib) (effective-addr+scale8 (number->register64 4) dest 'rbp index-reg)
       (values `(,rex.w ,(opcode #x8d) ,modrm ,@sib ,@(imm32->u8-list displacement)) #f))]
    [('leaq (? register64? dest) ('& (? imm32? displacement) ('* (? register64? index-reg) 4)))
     ;; base is omitted and disp32 => base = rbp
     (receive (modrm sib) (effective-addr+scale48 (number->register64 4) dest 'rbp index-reg)
       (values `(,(rex-prefix #t #f (reg-need-ext-bit? index-reg) #f) ,(opcode #x8d) ,modrm ,@sib ,@(imm32->u8-list displacement)) #f))]
    [('leaq (? register64? dest) ('& (? imm8? displacement) (? register64? base-reg) ('* (? register64? index-reg) 8)))
     (receive (modrm sib) (effective-addr+scale88 (number->register64 4) dest base-reg index-reg)
       (values `(,rex.w ,(opcode #x8d) ,modrm ,@sib ,(imm8->u8 displacement)) #f))]
    [('leaq dest-reg ('& src-reg (? imm8? displacement)))
     (cond
      [(zero? displacement)
       (receive (modrm sib) (effective-addr src-reg dest-reg)
         (values `(,rex.w ,(opcode #x8d) ,modrm ,@sib) #f))]
      [else
       (receive (modrm sib) (effective-addr+disp mod.disp8 src-reg dest-reg)
         (values `(,rex.w ,(opcode #x8d) ,modrm ,@sib ,(imm8->u8 displacement)) #f))])]
    [('leaq dest-reg ('& src-reg (? imm32? displacement)))
       (receive (modrm sib) (effective-addr+disp mod.disp32 src-reg dest-reg)
         (values `(,(rex-prefix #t #f #f (reg-need-ext-bit? src-reg)) ,(opcode #x8d) ,modrm ,@sib ,@(imm32->u8-list displacement)) #f))]
    [x
     (error 'assemble "assemble error: invalid syntax" x)]))

(define (vm-make-fixnum n)
  (+ (bitwise-arithmetic-shift-left n 2) 1))

(define (macro-to-fixnum reg)
  `((sarq ,reg 2)                        ; reg = reg >> 2
   (movslq ,reg ,(register64->32 reg)))) ; reg = reg32 (with sign)

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

(define (PUSH)
  `((movq rcx ,(vm-register 'sp))
    (movq (& rcx) ,value-reg)
    (addq ,sp-reg 8)))
    (movq ,(vm-register 'sp) rcx)


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

(define (u8-list->c-procedure+retq lst)
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
