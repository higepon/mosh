;; http://home.earthlink.net/~krautj/sassy/sassy-Z-H-7.html#node_sec_5.1.1

;       movq    80(%rsp), %rbx

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
(define (mod-r-m mod dest src)
  (assert (for-all register? (list dest src)))
  (let ([reg (register->number src)]
        [r/m (register->number dest)])
    (+ (bitwise-arithmetic-shift-left mod 6)
       (bitwise-arithmetic-shift-left reg 3)
       r/m)))

(define-syntax opcode
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'v])))

(define (addr+disp8 base-reg other-reg)
  (assert (for-all register? (list base-reg other-reg)))
  (values
   (mod-r-m #b01 base-reg other-reg)
   (if (eq? base-reg 'rsp)
       (list (sib #b00 base-reg))
       '())))

(define (addr base-reg other-reg)
  (assert (for-all register? (list base-reg other-reg)))
  (values
   (mod-r-m #b00 base-reg other-reg)
   (if (eq? base-reg 'rsp)
       (list (sib #b00 base-reg))
       '())))

(define (sib scaled-index reg)
  (+ (bitwise-arithmetic-shift-left scaled-index 6)
     (bitwise-arithmetic-shift-left (register->number reg) 3)
     (register->number reg)))

;; (oprand dest src)
(define (assemble code)
  (define rex.w #x48)
  (match code
    [('movq (? register? dest) (? register? src))
     ;; rex.w
     `(,rex.w ,(opcode #x89) ,(mod-r-m #b11 dest src))
     ]
    ;; (movq rbx (& rsp 80))

    ;;
    [('movq dest-reg ('& src-reg displacement))
     ;; REX.W + 8B /r MOV r64,r/m64 Valid N.E. Move r/m64 to r64.
     (cond
      [(zero? displacement)
       (receive (modrm sib) (addr src-reg dest-reg)
         `(,rex.w ,(opcode #x8b) ,modrm ,@sib))]
      [(< displacement #xff);; disp8
       (receive (modrm sib) (addr+disp8 src-reg dest-reg)
         `(,rex.w ,(opcode #x8b) ,modrm ,@sib ,displacement))]
      [else
         (error 'assemble "not implemented")])]
    [('movq dest-reg ('& src-reg))
     (assemble `(movq ,dest-reg (& ,src-reg 0)))]
    [('leaq dest-reg ('& src-reg displacement))
     (if (< displacement #xff);; disp8
         (receive (modrm sib) (addr+disp8 src-reg dest-reg)
           `(,rex.w ,(opcode #x8d) ,modrm ,@sib ,displacement))
         (error 'assemble "not implemented"))]))
