(library (mosh jit test)
  (export test)
  (import (rnrs) (mosh test) (mosh)
          (mosh control)
          (match)
          (srfi :8)
          (srfi :26)
          (system)
          (mosh)
          (only (srfi :1) append-map drop-right)
          (only (srfi private include) include/resolve))

  (include/resolve ("mosh" "jit") "jit-impl.ss")

  (define (test)
    (test-true (r64? 'rdi))
    (test-false (r64? 'rrr))

    (receive (found index) (find-with-index even? '(1 3 4 5))
       (test-eq 4 found)
       (test-eq 2 index))

    (test-eq #xe3 (mod-r-r/m #b11 (r64->number 'rsp) (r64->number 'rbx)))

    (test-eq 4 (r64->number 'rsp))
    (test-eq 'rdi (number->r64 (r64->number 'rdi)))

    ;; c-function address
    (test-true (number? (get-c-address 'Object::isNumber)))

    ;; vm registers
    (test-equal '(& rdi 8) (vm-register 'ac))

    (test-equal '(13 0 0 0) (imm32->u8* 13))

    ;; assemble1

    ;; mov family
    (test-equal '(#x48 #x89 #xe3) (assemble1 '(movq rbx rsp)))
    (test-equal '(#x48 #x89 #xeb) (assemble1 '(movq rbx rbp)))
    (test-equal '(#x48 #x89 #xc4) (assemble1 '(movq rsp rax)))

    (test-equal '(#x48 #x8b #x5c #x24 #x50) (assemble1 '(movq rbx (& rsp 80))))
    (test-equal '(#x48 #x8b #x45 #x50) (assemble1 '(movq rax (& rbp 80))))
    (test-equal '(#x48 #x8d #x48 #x08) (assemble1 '(leaq rcx (& rax 8))))
    (test-equal '(#x48 #x8b #x50 #xf8) (assemble1 '(movq rdx (& rax -8))))
    (test-equal '(#x48 #x8b #x10) (assemble1 '(movq rdx (& rax))))
    (test-equal '(#x48 #x8b #x00) (assemble1 '(movq rax (& rax))))
    (test-equal '(#x48 #x8b #x5c #x24 #x38) (assemble1 '(movq rbx (& rsp #x38))))
    (test-equal '(#x48 #x8b #x43 #x30)    (assemble1 '(movq rax (& rbx #x30))))
    (test-equal '(#x48 #x8b #x4b #x28)    (assemble1 '(movq rcx (& rbx #x28))))
    (test-equal '(#x48 #xc7 #xc0 #xb0 #x11 #x42 #x00) (assemble1 '(movq rax #x4211b0)))
    (test-equal '(#x48 #xc7 #x84 #x24 #x96 #x00 #x00 #x00 #x0d #x00 #x00 #x00) (assemble1 '(movq (& rsp #x96) 13)))
    (test-equal '(#x49 #x89 #xfa) (assemble1 '(movq r10 rdi)))
    (test-equal '(#x4c #x89 #xd7) (assemble1 '(movq rdi r10)))
    (test-equal '(#x89 #xd0) (assemble1 '(movl eax edx)))
    (test-equal '(#xba #x78 #x56 #x34 #x12) (assemble1 '(movl edx #x12345678)))

    (test-equal '(#x4c #x63 #xe0) (assemble1 '(movslq r12 eax)))

    ;; negq
    (test-equal '(#x48 #xf7 #xd8) (assemble1 '(negq rax)))

    ;; cltq
    (test-equal '(#x48 #x98) (assemble1 '(cltq)))

    ;; conditinal move
    (test-equal '(#x48 #x0f #x4c #xc2) (assemble1 '(cmovl rax rdx)))

    ;; cmp
    (test-equal '(#x48 #x39 #xc3) (assemble1 '(cmpq rbx rax)))
    (test-equal '(#x48 #x83 #x7e #x08 #x56) (assemble1 '(cmpq (& rsi 8) #x56)))
    (test-equal '(#x48 #x3d #xff #xff #xff #x3f) (assemble1 '(cmpq rax #x3fffffff)))
    (test-equal '(#x48 #x39 #x42 #x58) (assemble1 '(cmpq (& rdx 88) rax)))
    (test-equal '(#x48 #x83 #xfa #x03) (assemble1 '(cmpq rdx 3)))

    ;; test
    (test-equal '(#xf6 #xc2 #x03) (assemble1 '(testb dl 3)))
    (test-equal '(#xf6 #xc3 #x03) (assemble1 '(testb bl 3)))

    ;; andl
    (test-equal '(#x83 #xe0 #x03) (assemble1 '(andl eax 3)))

    ;; addq
    (test-equal '(#x48 #x03 #x51 #x28) (assemble1 '(addq rdx (& rcx 40))))

    ;; sub family
    (test-equal '(#x2c #x01) (assemble1 '(subb al 1)))
    (test-equal '(#x29 #xd0) (assemble1 '(subl eax edx)))

    ;; push
    (test-equal '(#x55) (assemble1 '(push rbp)))
    (test-equal '(#x50) (assemble1 '(push rax)))
    (test-equal '(#x48 #x89 #xe5) (assemble1 '(movq rbp rsp)))
    (test-equal '(#x48 #x83 #xec #x10) (assemble1 '(subq rsp #x10)))
    (test-equal '(#xc9) (assemble1 '(leave)))

    ;; pop
    (test-equal '(#x58) (assemble1 '(pop rax)))

    ;; leaq
    (test-equal '(#x48 #x8d #x38) (assemble1 '(leaq rdi (& rax))))
    (test-equal '(#x48 #x8d #x7c #x24 #x70) (assemble1 '(leaq rdi (& rsp 112))))
    (test-equal '(#x48 #x8d #x04 #xd5 #x00 #x00 #x00 #x00) (assemble1 '(leaq rax (& (* rdx 8)))))
    (test-equal '(#x48 #x8d #x44 #xc1 #xf8) (assemble1 '(leaq rax (& -8 rcx (* rax 8)))))
    (test-equal '(#x49 #x8d #x84 #x24 #x00 #x00 #x00 #x20) (assemble1 '(leaq rax (& r12 #x20000000))))
    (test-equal '(#x4a #x8d #x14 #xa5 #x01 #x00 #x00 #x00) (assemble1 '(leaq rdx (& 1 (* r12 4)))))


    ;; call
    (test-equal '(#xff #xd0) (assemble1 '(callq rax)))
    (test-equal '(#xff #xd2) (assemble1 '(callq rdx)))
    (test-equal '(#xff #x50 #x18) (assemble1 '(callq (& rax 24))))

    ;; jmp/label
    (test-equal '(#x74 #x00) (assemble '((je a) (label a))))
    (test-equal '(#x74 #xfe) (assemble '((label a) (je a))))
    (test-equal '(#xeb #x00) (assemble '((jmp a) (label a))))
    (test-equal '(#x75 #xfe) (assemble '((label a) (jne a))))

    (test-equal '(#x48 #x89 #x4b #x30) (assemble1 '(movq (& rbx #x30) rcx)))
    (test-equal '(#x48 #x89 #x53 #x08) (assemble1 '(movq (& rbx #x08) rdx)))

    (test-equal '(#x48 #xc7 #xc2 #x01 0 0 0) (assemble1 '(movq rdx 1)))

    ;; cltq
    (test-equal '(#x48 #x98) (assemble1 '(cltq)))

    ;; (movq dest-reg immediate)
    (test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00) (assemble1 '(movq (& rdi 8) 13)))

    ;; assemble
    (test-equal '(#x48 #x89 #xe3 #x48 #x89 #xeb) (assemble '((movq rbx rsp) (movq rbx rbp))))


    (let* ([asm (assemble `((movq ,(vm-register 'ac) 13)
                            (movq rax ,(vm-register 'ac))
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00
                                             #x48 #x8b #x47 #x08
                                             #xc3) asm)
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    ;; je
    (let* ([label (gensym)]
           [asm (assemble `((movq rax ,(vm-make-fixnum 2))
                            (movq rbx ,(vm-make-fixnum 2))
                            (cmpq rax rbx)
                            (je ,label)
                            (movq rax ,(vm-make-fixnum 3))
                            (label ,label)
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-true (procedure? proc))
      (test-eq 2 (proc)))

    (display (number->string (get-c-address 'Object::isNumber) 16))
    (newline)

    (display "***********************\n")

    ;; leave
    (let* ([asm (assemble `((push rbp)
                            (movq rbp rsp)
                            (subq rsp 16)
                            (movq (& rbp 16) ,(vm-make-fixnum 5))
                            (movq rax (& rbp 16))
                            (movq ,(vm-register 'ac) rax)
                            (leave)
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-true (procedure? proc))
      (test-eq 5 (proc)))

    ;; obj->integer
    (let* ([code1 (assemble
                   `((movq rax ,(obj->integer 'hoge))))]
           [proc (u8*->c-procedure+retq code1)])
      (test-eq 'hoge (proc)))


    ;; N.B.
    ;;   vm-register refers rdi.

    ;; call
    (let* ([asm (assemble `((push rbp)
                            (movq rbp rsp)
                            (subq rsp 8)
                            (movq rax ,(get-c-address 'Object::isNumber))
                            (movq r10 rdi)                        ;; save rdi
                            (leaq rdi (& rbp 16))                 ;; 1st argument : this pointer
                            (movq (& rbp 16) ,(vm-make-fixnum 3))
                            (callq rax)                           ;; 3.isNumber?
                            (movq rdi r10)                        ;; restore rdi
                            (movq rbx 1)
                            (cmpq rax rbx)
                            (je a)
                            (movq ,(vm-register 'ac) ,(vm-make-fixnum 0)) ;; refer rdi
                            (movq rax ,(vm-make-fixnum 0))
                            (leave)
                            (retq)
                            (label a)
                            (movq ,(vm-register 'ac) ,(vm-make-fixnum 1)) ;; refer rdi
                            (movq rax ,(vm-make-fixnum 1))
                            (leave)
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-true (procedure? proc))
      (test-eq 1 (proc)))

    ;; CONSTANT instruction
    ;;   CONSTANT 3
    ;;     vm: %rdi
    ;;     vm->ac: 0x8(rdi)
    (let ([proc (u8-list->c-procedure '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00   ;movq   $0xd,0x8(%rdi)
                                             #x48 #x8b #x47 #x08                  ;movq    0x8(%rdi),%rax
                                             #xc3))]) ;;retq
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    (test-eq 13 (vm-make-fixnum 3))

    (let* ([asm (assemble `((movq ,(vm-register 'ac) 13)
                            (movq rax ,(vm-register 'ac))
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00
                                             #x48 #x8b #x47 #x08
                                             #xc3) asm)
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    ;; VM instruction tests.
    ;;   We don't test the assmebled code, but a behaviour of VM instruction.

    ;; CONSTANT instruction
;;     (let* ([code (assemble (CONSTANT 13))]
;;            [proc (u8*->c-procedure+retq code)])
;;       (test-true (procedure? proc))
;;       (test-eq 3 (proc)))

    ;; REFER_LOCAL_PUSH_CONSTANT instruction
;;     (let* ([code1 (assemble (REFER_LOCAL_PUSH_CONSTANT 0 13))]
;;            [code2  (assemble (POP))]
;;            [proc (u8*->c-procedure+retq (append code1 code2))])
;;       (test-true (procedure? proc))
;;       (test-eq 1234 (proc 1234)))

;;     (let* ([code1 (assemble (REFER_LOCAL_PUSH_CONSTANT 1 13))]
;;            [code2  (assemble (POP))]
;;            [proc (u8*->c-procedure+retq (append code1 code2))])
;;       (test-true (procedure? proc))
;;       (test-eq 1235 (proc 1234 1235)))

    ;; BRANCH_NOT_LT
#;    (let* ([label (gensym)]
           [code1 (assemble (append (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 1))
                                    (BRANCH_NOT_LT label)
                                    (CONSTANT (vm-make-fixnum 3))
                                    `((label ,label))
                                    (POP2)))]
           [proc (u8*->c-procedure+retq code1)])
      (test-true (procedure? proc))
      ;; ac_ is result of compare
      (test-eq #f (proc 2)))

;;     ;; FRAME
;;     (let* ([code1 (assemble
;;                    (append
;;                     (FRAME)
;;                     (POP2)
;;                     (POP2)
;;                     (POP2)
;;                     (POP2)
;;                     (CONSTANT (vm-make-fixnum 3))))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 3 (proc)))

;;     ;; NUMBER_SUB_PUSH
;;     (let* ([label (gensym)]
;;            [code1 (assemble
;;                    (append
;;                     (CONSTANT (vm-make-fixnum 10))
;;                     (PUSH)
;;                     (CONSTANT (vm-make-fixnum 2))
;;                     (NUMBER_SUB_PUSH)
;;                     (POP)))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 8 (proc)))

;;     ;; NUMBER_ADD
;;     (let* ([label (gensym)]
;;            [code1 (assemble
;;                    (append
;;                     (CONSTANT (vm-make-fixnum 10))
;;                     (PUSH)
;;                     (CONSTANT (vm-make-fixnum 2))
;;                     (NUMBER_ADD)
;;                     ))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 12 (proc)))


;;     ;; REFER_GLOBAL
;;     (let* ([code1 (assemble
;;                    (append
;;                     (REFER_GLOBAL 'zero?)
;;                    (list `(movq rax ,(vm-register 'ac)))
;;                     ))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq zero? (proc)))


;;     ;; RETURN
;;     (let ([ERROR (gensym)])
;;       (define (save-vm-registers)
;;         `((movq rax ,(vm-register 'fp))
;;           (push rax)
;;           (movq rax ,(vm-register 'cl))
;;           (push rax)
;;           (movq rax ,(vm-register 'dc))
;;           (push rax)
;;           (movq rax ,(vm-register 'pc))
;;           (push rax)))
;;       (define (restore-vm-registers)
;;         `((pop rax)
;;           (movq ,(vm-register 'pc) rax)
;;           (pop rax)
;;           (movq ,(vm-register 'dc) rax)
;;           (pop rax)
;;           (movq ,(vm-register 'cl) rax)
;;           (pop rax)
;;           (movq ,(vm-register 'fp) rax)))
;;     (let* ([code1 (assemble
;;                    (append
;;                     (save-vm-registers)
;;                     ;; set test data to vm registers
;;                     `((movq ,(vm-register 'fp) 1)
;;                       (movq ,(vm-register 'cl) 2)
;;                       (movq ,(vm-register 'dc) 3)
;;                       (movq ,(vm-register 'pc) 4))
;;                     (FRAME)
;;                     ;; vm registers are dirty
;;                     `((movq ,(vm-register 'fp) 5)
;;                       (movq ,(vm-register 'cl) 6)
;;                       (movq ,(vm-register 'dc) 7)
;;                       (movq ,(vm-register 'pc) 8))
;;                     `((movq rdx ,(vm-register 'sp))
;;                       (movq rax ,(vm-make-fixnum 12)) ;; (push 12)
;;                       (movq (& rdx) rax)
;;                       (addq rdx 8)
;;                       (movq ,(vm-register 'sp) rdx))
;;                     (RETURN 1) ;; todo RETURN 1
;;                     `(
;;                       ;; check restored fp
;;                       (movq rax ,(vm-register 'fp))
;;                       (movq rcx 1)
;;                       (cmpq rax rcx)
;;                       (jne ,ERROR)
;;                       ;; check restored cl
;;                       (movq rax ,(vm-register 'cl))
;;                       (movq rcx 2)
;;                       (cmpq rax rcx)
;;                       (jne ,ERROR)
;;                       ;; check restored dc
;;                       (movq rax ,(vm-register 'dc))
;;                       (movq rcx 3)
;;                       (cmpq rax rcx)
;;                       (jne ,ERROR)
;;                       ;; check restored pc
;;                       (movq rax ,(vm-register 'pc))
;;                       (movq rcx 4)
;;                       (cmpq rax rcx)
;;                       (jne ,ERROR)
;;                       ,@(restore-vm-registers))
;;                      (CONSTANT (vm-make-fixnum 10))
;;                     `((retq)
;;pp                       (label ,ERROR)
;;                       ,@(restore-vm-registers)
;;                       ,@(CONSTANT (vm-make-fixnum 11))
;;                       (retq))))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 10 (proc))))

;;     ;; CALL
;;     (let* ([code1 (assemble
;;                    (append
;;                     (FRAME)
;;                     (CONSTANT (vm-make-fixnum 1))
;;                     (PUSH)
;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (obj->integer +)) ;; we know + is cprocedure
;;                     (CALL 2)
;;                     ))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 1235 (proc 1234)))

;;     ;; PUSH_FRAME
;;     (let* ([code1 (assemble
;;                    (append
;;                     (PUSH_FRAME)
;;                     (RETURN 0)
;;                     (POP)
;;                     (FRAME)
;;                     (CONSTANT (vm-make-fixnum 1))
;;                     (PUSH)
;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (obj->integer +)) ;; we know + is cprocedure
;;                     (CALL 2)
;;                     ))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 1235 (proc 1234)))

    ;; todo
    ;; jump imm32
    ;; return に retq 足す
    ;; stack trace

    ;; REFER_LOCAL_PUSH_CONSTANT instruction
    (let* ([code1 (let ([label (gensym)])
                    (assemble
                     (append
                      (DEBUGGER 9997)
                      (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 2))
                      (BRANCH_NOT_LT label)
                      (CONSTANT (vm-make-fixnum 1))
                      (RETURN 1)
                      `((movq rax ,(vm-register 'ac)))
                      (DEBUGGER 9998)
                      `((retq))
                      `((label ,label))
                      (FRAME)
                      (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 2))
;                      (DEBUGGER 4009)
                      (NUMBER_SUB_PUSH)
                      (DEBUGGER 4010)
                      (REFER_GLOBAL (obj->integer 'fib))
                      (CALL 1)
                      (PUSH_FRAME)
                      (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 1))
                      (NUMBER_SUB_PUSH)
                      (REFER_GLOBAL(obj->integer 'fib))
                      (CALL 1)
                      (NUMBER_ADD)
                      (RETURN 1)
                      )))]
           [proc (u8*->c-procedure+retq code1)])
      (test-true (procedure? proc))
      (test-eq 1234 (proc 1)))

#;    (let* ([code1 (let ([branch-label (gensym)])
                  (assemble
                   (append
                    (REFER_LOCAL_PUSH_CONSTANT 0 13 ;; (vm-make-fixnum 2)
                                               )
                    (POP)
;;                     (BRANCH_NOT_LT branch-label)
;;                     (CONSTANT (vm-make-fixnum 1))
;;                     (RETURN 1)
;;                     `((label ,branch-label))
;;                     (FRAME)
;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 2))
;;                     (NUMBER_SUB_PUSH)
;;                     (REFER_GLOBAL (obj->integer 'fib))
;;                     (CALL 1)
;;                     (PUSH_FRAME)
;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (vm-make-fixnum 1))
;;                     (NUMBER_SUB_PUSH)
;;                     (REFER_GLOBAL(obj->integer 'fib))
;;                     (CALL 1)
;;                     (NUMBER_ADD)
;;                     (RETURN 1)
                    )))]
           [fib (u8*->c-procedure+retq code1)])
;       (test-true (procedure? fib))
      (fib 1234)
;;       (set-symbol-value! 'fib fib)
;;       (let1 x (symbol-value 'fib)
;;         (display x)
;;         (display (x 1)))
    )


    ;; gas -> sassy
    (test-equal '(movq rbx rsp) (gas->sassy "mov   %rsp,%rbx"))
    (test-equal '(movq rdx (& rbx #x30)) (gas->sassy "mov    0x30(%rbx),%rdx"))
    (test-equal '(addq rcx #x8) (gas->sassy "add    $0x8,%rcx"))
    (test-equal '(movq (& rbx #x28) rcx) (gas->sassy "mov    %rcx,0x28(%rbx)"))
    (test-equal '(movq rcx (& rdx)) (gas->sassy "mov    (%rdx),%rcx"))
    (test-equal '(leaq rdx (& rax -8)) (gas->sassy "leaq    -8(%rax), %rdx"))

;; movq 56(%rsp), %rbx ; rbx = vm
;; movq 40(%rbx), %rax ; rax = sp
;; leaq -8(%rax), %rdx ; rdx = sp - 8
;; movq %rdx, 40(%rbx) ; sp = rdx
;; movq -8(%rax), %rdx ; rdx = *sp
;; movl %edx, %eax     ; eax = (32bit)(rdx)
;; movq %rdx, 368(%rsp) ; どこか = *sp

;; andl $3, %eax  ; n.isFixnum
;; subb $1, %al

;    (display (gas->sassy "movq %rdx, 40(%rbx)"))
    )
)
