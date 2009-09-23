(library (mosh jit test)
  (export test)
  (import (rnrs) (mosh test) (mosh)
          (match)
          (srfi :8)
          (srfi :26)
          (system)
          (only (srfi :1) append-map)
          (only (srfi private include) include/resolve))

  (include/resolve ("mosh" "jit") "jit-impl.ss")

  (define (test)
    (test-true (register64? 'rdi))
    (test-false (register64? 'rrr))

    (receive (found index) (find-with-index even? '(1 3 4 5))
       (test-eq 4 found)
       (test-eq 2 index))

    (test-eq #xe3 (mod-r-m #b11 'rbx 'rsp))
    (test-eq #x5c (effective-addr+disp8 'rsp 'rbx))

    (test-eq 4 (register64->number 'rsp))
    (test-eq 'rdi (number->register64 (register64->number 'rdi)))

    ;; vm registers
    (test-equal '(& rdi 8) (vm-register 'ac))

    (test-equal '(13 0 0 0) (imm32->u8-list 13))

    ;; assemble1
    (test-equal '(#x48 #x89 #xe3) (assemble1 '(movq rbx rsp)))
    (test-equal '(#x48 #x89 #xeb) (assemble1 '(movq rbx rbp)))
    (test-equal '(#x48 #x89 #xc4) (assemble1 '(movq rsp rax)))

    (test-equal '(#x48 #x8b #x5c #x24 #x50) (assemble1 '(movq rbx (& rsp 80))))
    (test-equal '(#x48 #x8b #x45 #x50) (assemble1 '(movq rax (& rbp 80))))
    (test-equal '(#x48 #x8d #x48 #x08) (assemble1 '(leaq rcx (& rax 8))))
    (test-equal '(#x48 #x8b #x10) (assemble1 '(movq rdx (& rax))))
    (test-equal '(#x48 #x8b #x5c #x24 #x38) (assemble1 '(movq rbx (& rsp #x38))))
    (test-equal '(#x48 #x8b #x43 #x30)    (assemble1 '(movq rax (& rbx #x30))))
    (test-equal '(#x48 #x8b #x4b #x28)    (assemble1 '(movq rcx (& rbx #x28))))


    (test-equal '(#x48 #x89 #x4b #x30) (assemble1 '(movq (& rbx #x30) rcx)))
    (test-equal '(#x48 #x89 #x53 #x08) (assemble1 '(movq (& rbx #x08) rdx)))

    (test-equal '(#x48 #xc7 #xc2 #x01 0 0 0) (assemble1 '(movq rdx 1)))

    ;; (movq dest-reg immediate)
    (test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00) (assemble1 '(movq (& rdi 8) 13)))

    ;; assemble
    (test-equal '(#x48 #x89 #xe3 #x48 #x89 #xeb) (assemble '((movq rbx rsp) (movq rbx rbp))))

    ;; CONSTANT instruction
    ;;   CONSTANT 3
    ;;     vm: %rdi
    ;;     vm->ac: 0x8(rdi)
    (let ([proc (u8-list->c-procedure '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00   ;movq   $0xd,0x8(%rdi)
                                             #x48 #x8b #x47 #x08                  ;movq    0x8(%rdi),%rax
                                             #xc3))]) ;;retq
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    (test-eq 13 (make-fixnum 3))

    ;; CONSTANT instruction
    (let* ([asm (assemble `((movq ,(vm-register 'ac) 13)
                            (movq rax ,(vm-register 'ac))
                            (retq)))]
           [proc (u8-list->c-procedure asm)])
      (test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00
                                             #x48 #x8b #x47 #x08
                                             #xc3) asm)
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    ;; CONSTANT instruction
    (let* ([code (assemble (CONSTANT 13))]
           [proc (u8-list->c-procedure code)])
      (test-true (procedure? proc))
      (test-eq 3 (proc)))

    ;; REFER_LOCAL_PUSH_CONSTANT
#;    (let* ([code (assemble (REFER_LOCAL_PUSH_CONSTANT 0 (make-fixnum 3)))])
           (test-equal '(
                         #x48 #x8b #x47 #x30
                         #x48 #x8b #x4F #x28
;                          #x48 #x8b #x10
                         #x48 #xc7 #xc2 #x01 #x00 #x00 #x00
;                          #x48 #x83 #xc0 #x08
                          #x48 #x89 #x47 #x30
                          #x48 #x8b #x47 #x20
                          #x48 #xc1 #xfa #x02
                          #x48 #x63 #xd2
                          #x48 #x8b #x04 #xd0
                          #x48 #x89 #x01
                          #x48 #x8b #x57 #x30
                          #x48 #x83 #xc1 #x08
                          #x48 #x89 #x4f #x20
                          #x48 #x8b #x0a
                          #x48 #x8d #x42 #x08
                          #x48 #x89 #x47 #x28
                          #x48 #x89 #x4f #x08
                         ) code))

(display (vm-register 'sp))
    ;; REFER_LOCAL_PUSH_CONSTANT instruction
    (let* ([code1 (assemble (REFER_LOCAL_PUSH_CONSTANT 0 13))
#;            `(
                         #x48 #x8b #x47 #x30
                         #x48 #x8b #x4F #x28
                          #x48 #x8b #x10
                          #x48 #x83 #xc0 #x08
                          #x48 #x89 #x47 #x30
              #x48 #xc7 #xc0 #x0d 0 0 0 #;,@(FOREVER) #xc3
;                         #xcc
;;                           #x48 #x8b #x47 #x20
;;                           #x48 #xc1 #xfa #x02
;;                           #x48 #x63 #xd2
;;                           #x48 #x8b #x04 #xd0
;;                           #x48 #x89 #x01 
;;                           #x48 #x8b #x57 #x30 
;;                           #x48 #x83 #xc1 #x08
;;                           #x48 #x89 #x4f #x20
;;                           #x48 #x8b #x0a 
;;                           #x48 #x8d #x42 #x08
;;                           #x48 #x89 #x47 #x28 
;;                           #x48 #x89 #x4f #x08 ;,@(FOREVER) 

;;                          #x48 #x8b #x47 #x30
;;                          #x48 #x8b #x4F #x28 
;;                           #x48 #x8b #x10 
;;                           #x48 #x83 #xc0 #x08 
;;                           #x48 #x89 #x47 #x30 
;;                           #x48 #x8b #x47 #x20
;;                           #x48 #xc1 #xfa #x02
;;                           #x48 #x63 #xd2 
;;                           #x48 #x8b #x04 #xd0 
;;                           #x48 #x89 #x01 ; #xcc
;;                           #x48 #x8b #x57 #x30  
;;                           #x48 #x83 #xc1 #x08 ; 
;;                           #x48 #x89 #x4b #x28
;;                           #x48 #x8b #x0a
;;                           #x48 #x8d #x42 #x08
;;                           #x48 #x89 #x43 #x30
;;                           #x48 #x89 #x4b #x08 
                         )]
           [code2  (assemble (POP))
#;            `(72 139 79 40
              72 131 233 8  
              72 139 1 
              72 137 71 8 
                 #;#xcc  #;,@(FOREVER) #xc3  )]
           [proc (u8-list->c-procedure (append code1 #;'(#xeb #xfe) code2))])
      (display (append code1 code2))
      (display code2)
      (test-true (procedure? proc))
      (test-eq 1234 (proc 1234)))


    (test-equal '(movq rbx rsp) (gas->sassy "mov   %rsp,%rbx"))
    (test-equal '(movq rdx (& rbx #x30)) (gas->sassy "mov    0x30(%rbx),%rdx"))
    (test-equal '(addq rcx #x8) (gas->sassy "add    $0x8,%rcx"))
    (test-equal '(movq (& rbx #x28) rcx) (gas->sassy "mov    %rcx,0x28(%rbx)"))
    (test-equal '(movq rcx (& rdx)) (gas->sassy "mov    (%rdx),%rcx"))
    )
(display (gas->sassy "mov %rcx,0x8(%rbx)"))
)
