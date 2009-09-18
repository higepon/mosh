;; http://home.earthlink.net/~krautj/sassy/sassy-Z-H-7.html#node_sec_5.1.1

;       movq    80(%rsp), %rbx

;;     Object ac_;  // accumulator     register
;;     Object dc_;  // display closure register, used for refer-free
;;     Object cl_;  // current closure register, used for profiler.
;;     Object* fp_; // frame pointer   register
;;     Object* sp_; // stack pointer   register
;;     Object* pc_; // program counter register

(define vm-register* '(ac dc cl fp sp pc))

;; depends on architecture.
(define vm-register-offset 8)

(define (vm-register reg)
  `(& rdi ,(* (+ (receive (_ index) (find-with-index (cut eq? <> reg) vm-register*)
                   index) 1) vm-register-offset)))

(define register* '(rax rcx rdx rbx rsp rbp rsi rdi
                    r8  r9  r10 r11 r12 r13 r14 r15))

(define (register? obj) (memq obj register*))

(define (register->number reg)
  (receive (_ index) (find-with-index (cut eq? <> reg) register*)
    index))

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
(define mod.register #b11)

(define (mod-r-m mod r/m reg)
  (assert (for-all register? (list r/m reg)))
  (let ([reg-n (register->number reg)]
        [r/m-n (register->number r/m)])
    (+ (bitwise-arithmetic-shift-left mod 6)
       (bitwise-arithmetic-shift-left reg-n 3)
       r/m-n)))

(define-syntax opcode
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'v])))

(define (effective-addr+disp8 r/m-reg other-reg)
  (assert (for-all register? (list r/m-reg other-reg)))
  (values
   (mod-r-m mod.disp8 r/m-reg other-reg)
   (if (eq? r/m-reg 'rsp)
       (list (sib #b00 r/m-reg))
       '())))

(define (effective-addr base-reg other-reg)
  (assert (for-all register? (list base-reg other-reg)))
  (values
   (mod-r-m mod.disp0 base-reg other-reg)
   (if (eq? base-reg 'rsp)
       (list (sib #b00 base-reg))
       '())))

(define (sib scaled-index reg)
  (+ (bitwise-arithmetic-shift-left scaled-index 6)
     (bitwise-arithmetic-shift-left (register->number reg) 3)
     (register->number reg)))

(define (assemble code*)
  (append-map assemble1 code*))

(define (imm32? n)
  (and (integer? n) (<= (- (expt 2 31)) n (- (expt 2 31) 1))))

(define (imm32->u8-list n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-left n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-left n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-left n 24) #xff)))

;; (oprand dest src)
(define (assemble1 code)
  (define rex.w #x48)
  (match code
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq (? register? dest) (? register? src))
     `(,rex.w ,(opcode #x89) ,(mod-r-m mod.register dest src))]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq dest-reg ('& src-reg displacement))
     (cond
      [(zero? displacement)
       (receive (modrm sib) (effective-addr src-reg dest-reg)
         `(,rex.w ,(opcode #x8b) ,modrm ,@sib))]
      ;; disp8
      [(< displacement #xff)
       (receive (modrm sib) (effective-addr+disp8 src-reg dest-reg)
         `(,rex.w ,(opcode #x8b) ,modrm ,@sib ,displacement))]
      [else
         (error 'assemble "not implemented")])]
    [('movq dest-reg ('& src-reg))
     (assemble1 `(movq ,dest-reg (& ,src-reg 0)))]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq ('& (? register? dest-reg) displacement) (? register? src-reg))
       (receive (modrm sib) (effective-addr+disp8 dest-reg src-reg)
         `(,rex.w ,(opcode #x89) ,modrm ,displacement))]
    [('movq ('& (? register? dest-reg)) (? register? src-reg))
     (assemble1 `(movq (& ,dest-reg 0) ,src-reg))]
    ;; MOV r/m64, imm32
    ;;   REX.W + C7 /0
    [('movq ('& dest-reg displacement) (? imm32? imm32))
       ;; rax means 0
       (receive (modrm sib) (effective-addr+disp8 dest-reg 'rax)
         `(,rex.w ,(opcode #xc7) ,modrm ,displacement ,@(imm32->u8-list imm32)))]
    ;; RET
    ;;   C3
    [('retq)
     '(#xc3)]
    [('leaq dest-reg ('& src-reg displacement))
     (if (< displacement #xff);; disp8
         (receive (modrm sib) (effective-addr+disp8 src-reg dest-reg)
           `(,rex.w ,(opcode #x8d) ,modrm ,@sib ,displacement))
         (error 'assemble "not implemented"))]
    [x
     (error 'assemble "assemble error: unknown syntax" x)]))


;;    (define (compile insn*)
;;      (match insn*
;;        [('CONSTANT val)
;;         (assemble
;;         ]
;;        [else
;;         (error 'compile "unknwon instruction" x)]))

(define (CONSTANT val)
  `((movq ,(vm-register 'ac) ,val)
    (movq rax ,(vm-register 'ac))
    (retq)))

;; ToDo
;; (0) make constant op directory
;; (1) make constant op through assemble
;; (1) vm->reg offset support
