; jit-assembler.scm - Tests for JIT assembler
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
(import (rnrs)
        (mosh)
        (mosh test)
        (mosh jit vm)
        (system)
        (only (mosh jit compiler) u8-list->c-procedure)
        (mosh jit assembler))

(define (u8-list->c-procedure+retq lst)
  (u8-list->c-procedure (append lst (assemble '((retq))))))

;; primitives
(test-true (r64? 'rdi))
(test-false (r64? 'rrr))
(test-eq #xe3 (mod-r-r/m #b11 (r64->number 'rsp) (r64->number 'rbx)))
(test-eq 4 (r64->number 'rsp))
(test-eq 'rdi (number->r64 (r64->number 'rdi)))

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
(test-equal '(#x49 #xc7 #xc0 #x78 #x56 #x34 #x12) (assemble1 '(movq r8 #x12345678)))
(test-equal '(#x4c #x63 #xe0) (assemble1 '(movslq r12 eax)))
(test-equal '(#x48 #x89 #x4b #x30) (assemble1 '(movq (& rbx #x30) rcx)))
(test-equal '(#x48 #x89 #x53 #x08) (assemble1 '(movq (& rbx #x08) rdx)))
(test-equal '(#x48 #xc7 #xc2 #x01 0 0 0) (assemble1 '(movq rdx 1)))
(test-equal '(#x48 #x8b #x14 #xd1) (assemble1 '(movq rdx (& rcx (* rdx 8)))))
;; negq
(test-equal '(#x48 #xf7 #xd8) (assemble1 '(negq rax)))

;; cltq
(test-equal '(#x48 #x98) (assemble1 '(cltq)))

;; conditinal move
(test-equal '(#x48 #x0f #x4c #xc2) (assemble1 '(cmovl rax rdx)))
(test-equal '(#x48 #x0f #x44 #xc2)  (assemble1 '(cmove rax rdx)))

;; cmp
(test-equal '(#x48 #x39 #xc3) (assemble1 '(cmpq rbx rax)))
(test-equal '(#x48 #x83 #x7e #x08 #x56) (assemble1 '(cmpq (& rsi 8) #x56)))
(test-equal '(#x48 #x3d #xff #xff #xff #x3f) (assemble1 '(cmpq rax #x3fffffff)))
(test-equal '(#x48 #x39 #x42 #x58) (assemble1 '(cmpq (& rdx 88) rax)))
(test-equal '(#x48 #x83 #xfa #x03) (assemble1 '(cmpq rdx 3)))
(test-equal '(#x48 #x3b #x0a) (assemble1 '(cmpq rcx (& rdx))))

;; test
(test-equal '(#x85 #xdb) (assemble1 '(testl ebx ebx)))
(test-equal '(#x85 #xd9) (assemble1 '(testl ecx ebx)))
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
(test-equal '(#x41 #x59) (assemble1 '(pop r9)))

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

;; cltq
(test-equal '(#x48 #x98) (assemble1 '(cltq)))

;; (movq dest-reg immediate)
(test-equal '(#x48 #xc7 #x47 #x08 #x0d #x00 #x00 #x00) (assemble1 '(movq (& rdi 8) 13)))

;; assemble
(test-equal '(#x48 #x89 #xe3 #x48 #x89 #xeb) (assemble '((movq rbx rsp) (movq rbx rbp))))

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
           [proc (u8-list->c-procedure+retq code1)])
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


(test-results)
