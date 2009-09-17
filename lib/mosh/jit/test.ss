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
    (test-true (register? 'rdi))
    (test-false (register? 'rrr))

    (receive (found index) (find-with-index even? '(1 3 4 5))
       (test-eq 4 found)
       (test-eq 2 index))

    (test-eq #xe3 (mod-r-m #b11 'rbx 'rsp))
    (test-eq #x5c (addr+disp8 'rsp 'rbx))

    (test-eq 4 (register->number 'rsp))

    ;; vm registers
    (test-equal '(& rdi 8) (vm-register 'ac))

    ;; assemble1
    (test-equal '(#x48 #x89 #xe3) (assemble1 '(movq rbx rsp)))
    (test-equal '(#x48 #x89 #xeb) (assemble1 '(movq rbx rbp)))
    (test-equal '(#x48 #x89 #xc4) (assemble1 '(movq rsp rax)))

    (test-equal '(#x48 #x8b #x5c #x24 #x50) (assemble1 '(movq rbx (& rsp 80))))
    (test-equal '(#x48 #x8b #x45 #x50) (assemble1 '(movq rax (& rbp 80))))
    (test-equal '(#x48 #x8d #x48 #x08) (assemble1 '(leaq rcx (& rax 8))))
    (test-equal '(#x48 #x8b #x10) (assemble1 '(movq rdx (& rax))))

    (test-equal '(#x48 #x89 #x4b #x30) (assemble1 '(movq (& rbx #x30) rcx)))
    (test-equal '(#x48 #x89 #x53 #x08) (assemble1 '(movq (& rbx #x08) rdx)))

    (test-equal '(movq #x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00) (assemble1 '(mov (& rdi 8) 13)))

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

;;     (let ([proc (u8-list->c-procedure (assemble `((movq ,(vm-register ac) 13
                                        
                                        
;;       (test-true (procedure? proc))
;;       (test-eq 3 (proc)))

;;  movq    48(%rbx), %rax ;; %rax = this->pc_
;;  .LBE14145:
;;  movq    (%rax), %rdx   ;; %rdx = *(this->pc_)
;;  .LBB14146:
;;  leaq    8(%rax), %rcx  ;; %rcx = this->pc_ + 8
;;  movq    %rcx, 48(%rbx) ;; this->pc_ = %rcx
;;  .loc 3 312 0
;;  movq    %rdx, 8(%rbx)  ;; this->ac_ = %rdx

    )
)
