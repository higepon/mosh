; compiler.ss - JIT compiler for AMD64
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
(library (mosh jit compiler)
  (export
   (rename (jit-compiled? compiled?))
   compile
   disasm
   u8-list->c-procedure
   ;; exported for test
   macro-to-fixnum macro-make-fixnum
   REFER_LOCAL
   REFER_LOCAL_PUSH_CONSTANT
   BRANCH_NOT_LT
   CONSTANT FRAME
   RETURN NUMBER_SUB_PUSH
   REFER_GLOBAL
   CALL
   POP
   POP2
   PUSH
   PUSH_FRAME
   NUMBER_ADD
   RESTORE_REGISTERS
   pack-instruction
   )
  (import (rnrs)
          (system)
          (mosh jit vm)
          (mosh control)
          (match)
          (mosh jit assembler)
          (mosh)
          (rnrs mutable-pairs)
          (only (srfi private include) include/resolve))

(include/resolve ("mosh" "jit") "instructions.ss")



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

;; save argument registers
(define (call-prologue)
  `((push rdi)
    (push rsi)
    (push rdx)
    (push rcx)
    (push r8)
    (push r9)))

(define (call-epilogue)
  `((pop r9)
    (pop r8)
    (pop rcx)
    (pop rdx)
    (pop rsi)
    (pop rdi)))

(define (call0 c-func)
  `(,@(call-prologue)
    (movq rax ,c-func)
    (callq rax)
    ,@(call-epilogue)))

(define (call1 c-func arg1)
  `(,@(call-prologue)
    (movq rdi ,arg1)
    (movq rax ,c-func)
    (callq rax)
    ,@(call-epilogue)))

(define (trace-push! insn)
  `((push rax) ;; we discard the return value
    ,@(call1 (get-c-address 'jitStackPush) insn)
    (pop rax)))

(define (trace-reset!)
  `((push rax) ;; we discard the return value
    ,@(call0 (get-c-address 'jitStackReset))
    (pop rax)))

(define (PUSH_FRAME)
  `(,@(trace-push! $PUSH_FRAME)
    ,@(PUSH)
    ,@(FRAME)))

(define (CALL n)
  `(,@(trace-push! $CALL)
    (push rdi)                    ;; save registers
    (push rsi)
    (push rdx)
    (movq rax ,n)                 ;; argc
    (leaq rcx (& (* rax 8)))      ;; argc * 8
    (negq rcx)
    (addq rcx ,(vm-register 'sp)) ;; arg4 = sp - argc
    (movq ,(vm-register 'fp) rcx) ;; fp = sp - argc
    (movq rdx ,n)                 ;; arg3 = argc
    (movq rsi rdi)                ;; arg2 = VM
    (movq rax ,(vm-register 'ac))
    (movq rdi (& rax 8))          ;; arg1 CProcedure
    (movq rax ,(get-c-address 'CProcedure::call))
    (callq rax)
    (pop rdx)
    (pop rsi)
    (pop rdi)
    (push rax)
    ;; adjust VM stack
;    ,@(RESTORE-REGISTERS n)
    (pop rax)
    (movq ,(vm-register 'ac) rax)
    ))

(define (macro-is-raw-pointer reg not-case-label)
  `((testb ,(r64->8 reg)  3)
    (jne ,not-case-label)))

;; Discards rax, rdx
(define (macro-is-heap-object reg is-heap-object-case)
  (let ([not-case-label (gensym)])
    `(,@(macro-is-raw-pointer reg not-case-label)
      (movq rax (& ,reg))
      (movq rdx rax)
      (andl edx 3)
      (cmpq rdx 3)
      (je ,is-heap-object-case)
      (label ,not-case-label))))

(define (macro-is-gloc reg is-not-gloc-case)
  (let ([is-heap-object-case (gensym)])
    `(,@(macro-is-heap-object reg is-heap-object-case)
      (jmp ,is-not-gloc-case)
      (label ,is-heap-object-case)
      (cmpq rax 135) ;; rax = type, rax isGloc()?
      (jne ,is-not-gloc-case))))

(define (UNDEF)
  `(,@(trace-push! $CONSTANT_PUSH)
    (movq ,(vm-register 'ac) ,(obj->integer (if #f #f)))))

(define (CONSTANT_PUSH x)
  `(,@(trace-push! $CONSTANT_PUSH)
    ,@(CONSTANT x)
    ,@(PUSH)))

;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (movq rax (& rsi 48))
;; (movq rdx (& rax))
;; (leaq rcx (& rax 8))
;; (movq (& rsi 48) rcx)
;; (movq rcx (& rsi 32))
;; (sarq rdx 2)
;; (movslq edx,%rdx)
;; (cmpq (& rcx,%rdx,8) 6)
;; (je (label .L1134))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq (& rcx 8) 86)
;; (movq rdx (& rax 8))
;; (sarq rdx 2)
;; (subl edx 1)
;; (movslq edx,%rdx)
;; (leaq rax (& rax,%rdx,8 16))
;; (movq (& rcx 48) rax)
;; (label .L889)
;; (label .LBE9204)
(define (REFER_LOCAL_BRANCH_NOT_NULL index not-null-case)
  `(,@(trace-push! $REFER_LOCAL_BRANCH_NOT_NULL)
    ,@(REFER_LOCAL index)
    (movq rdx ,(vm-register 'ac))
    (cmpq rdx ,(obj->integer '()))
    (movq ,(vm-register 'ac) ,(obj->integer #f))
    (jne ,not-null-case)
    (movq ,(vm-register 'ac) ,(obj->integer #t))))

;; ;; rdx <- VM* | (movq rdx (& rsp 88))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq rax (& rdx 40))
;; (leaq rdx (& rax -8))
;; (movq (& rcx 40) rdx)
;; (movq rbx (& rax -8))
;; (cmpq (& rcx 8) rbx)
;; (je (label .L1084))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq rdx (& rcx 48))
;; (movq (& rcx 8) 86)
;; (movq rax (& rdx))
;; (sarq rax 2)
;; (subl eax 1)
;; (cltq)
;; (leaq rax (& rdx,%rax,8 8))
;; (movq (& rcx 48) rax)

;; todo : pop-to-reg
(define (BRANCH_NOT_EQ not-eq-case)
  `(,@(trace-push! $BRANCH_NOT_EQ)
    (movq rax ,(vm-register 'sp))
    (leaq rdx (& rax -8))
    (movq ,(vm-register 'sp) rdx)
    (movq rbx (& rax -8)) ;; rbx = pop()
    (cmpq ,(vm-register 'ac) rbx)
    (movq ,(vm-register 'ac) ,(obj->integer #f))
    (jne ,not-eq-case)
    (movq ,(vm-register 'ac) ,(obj->integer #t))))

;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (movq rax (& rsi 48))
;; (movq rbx (& rax))
;; (addq rax 8)
;; (movq (& rsi 48) rax)
;; (testb bl 3)
;; (jne (label .L206))
;; (movq rax (& rbx))
;; (movq rdx rax)
;; (andl edx 3)
;; (cmpq rdx 3)
;; (je (label .L1061))
;; (label .L206)
;; (movq rsi (& rsp 96))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq rdi (& rsp 96))
;; (movq rax (& rsi))
;; (movq rdx (& rcx 104))
;; (movq rsi rbx)
;; (call (& rax 24))
;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (cmpq (& rsi 104) rax)
;; (je (label .L1062))
;; (movq rdx (& rsi 8))
;; (movq rcx (& rax 8))
;; (movq (& rcx) rdx)
;; (movq rdx (& rsi 48))
;; (movq (& rdx -8) rax)
;; (label .L207)
;; (_ZN6scheme6Object5UndefE( rax rip))
;; (movl (& rsi 56) 1)
;; (movq (& rsi 8) rax)
;; (movq rax (& rsi 48))
;; (movq rdx (& rax))
;; (addq rax 8)
;; (movq (& rsi 48) rax)
;; (jmp (label .L1002))
;; (label .L208)
;; END
;; (label .L1061)
;; (cmpq rax 135)
;; (jne (label .L206))
;; (movq rdx (& rsi 8))
;; (movq rax (& rbx 8))
;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (movq (& rax) rdx)
;; (jmp (label .L207))

(define (ASSIGN_GLOBAL id)
  (let ([is-not-gloc-case (gensym)]
        [done (gensym)])
    `(,@(trace-push! $ASSIGN_GLOBAL)
      (movq rbx ,(obj->integer id))
      ,@(macro-is-gloc 'rbx is-not-gloc-case)
      (movq rdx ,(vm-register 'ac))
      (movq rax (& rbx 8))
      (movq (& rax) rdx)
      (movq ,(vm-register 'ac) ,(obj->integer (if #f #f)))
      (jmp ,done)
      ;; ID is not Gloc, never happen on JIT?
      (label ,is-not-gloc-case)
      ,@(DEBUGGER 4000)
      (label ,done))))

;; ;; rbx <- VM* | (movq rbx (& rsp 88))
;; (movq rax (& rbx 8))
;; (testb al 3)
;; (jne (label .L426))
;; (movq rdx (& rax))
;; (movl eax 70)
;; (andl edx 3)
;; (cmpq rdx 3)
;; (je (label .L426))
;; (label .L427)
;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (movq (& rsi 8) rax)
;; (movq rax (& rsi 48))
;; (movl (& rsi 56) 1)
;; (movq rdx (& rax))
;; (addq rax 8)
;; (movq (& rsi 48) rax)
;; (jmp (label .L1002))
;; END
;; (label .L426)
;; (movl eax 86)
;; (jmp (label .L427))
(define (PAIR_P)
  (let ([not-pair-case (gensym)]
        [done (gensym)])
    `(,@(trace-push! $PAIR_P)
      (movq rax ,(vm-register 'ac))
      (testb al 3)
      (jne ,not-pair-case)
      (movq rdx (& rax))
      (movl eax ,(obj->integer #t))
      (andl edx 3)
      (cmpq rdx 3)
      (movq ,(vm-register 'ac) rax)
      (jmp ,done)
      (je ,not-pair-case)
      (label ,not-pair-case)
      (movq ,(vm-register 'ac) ,(obj->integer #f))
      (label ,done))))

;; ;; rbx <- VM* | (movq rbx (& rsp 88))
;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (movq rax (& rbx 48))
;; (movq rbx (& rax))
;; (addq rax 8)
;; (movq (& rsi 48) rax)
;; (testb bl 3)
;; (jne (label .L488))
;; (movq rax (& rbx))
;; (movq rdx rax)
;; (andl edx 3)
;; (cmpq rdx 3)
;; (je (label .L1108))
;; (label .L488)
;; (movq rdx (& rsp 96))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq rsi rbx)
;; (movq rdi (& rsp 96))
;; (movq rax (& rdx))
;; (movq rdx (& rcx 104))
;; (call (& rax 24))
;; ;; rsi <- VM* | (movq rsi (& rsp 88))
;; (cmpq (& rsi 104) rax)
;; (je (label .L1109))
;; (movq rdx (& rax 8))
;; (movq rdx (& rdx))
;; (movq (& rsi 8) rdx)
;; (movq rdx (& rsi 48))
;; (movq (& rdx -8) rax)
;; ;; END
;; (label .L1108)
;; (cmpq rax 135)
;; (jne (label .L488))
;; (movq rax (& rbx 8))
;; (movq rax (& rax))
;; (movq (& rsi 8) rax)
;; (jmp (label .L489))
(define (REFER_GLOBAL id)
  (let ([not-found-case (gensym)]
        [is-not-gloc-case (gensym)]
        [get-gloc-value (gensym)])
    `(,@(trace-push! $REFER_GLOBAL)
       (movq rbx ,(obj->integer id))
       ,@(macro-is-gloc 'rbx is-not-gloc-case)
       ;; obj(rbx) is gloc
       (movq rax rbx)
       (jmp ,get-gloc-value)
       ;; END
       (label ,not-found-case)
       ,@(DEBUGGER 3199)
       (label ,is-not-gloc-case)
       ;; save arguments
       (push rdi)
       (push rsi)
       (push rdx)
       (movq rax ,(vm-register 'namespace)) ;; rax = namespace
       (movq rdx ,(vm-register 'not-found)) ;; arg3 not_found
       (movq rsi rbx)                       ;; arg2 id
       (movq rdi (& rax 8))                 ;; rdi = namespace.toEqHashTable
       (movq rax (& rdi))                   ;; rax EqHashTable::ref
       (callq (& rax 24))
       (pop rdx)
       (pop rsi)
       (pop rdi)
       (cmpq ,(vm-register 'not-found) rax)
       (je ,not-found-case)
       (label ,get-gloc-value)
       (movq rdx (& rax 8))   ;; rdx = gloc.value()
       (movq rdx (& rdx))
       (movq ,(vm-register 'ac) rdx))))

(define (REFER_GLOBAL_PUSH id)
  `(,@(trace-push! $REFER_GLOBAL_PUSH)
    ,@(REFER_GLOBAL id)
    ,@(PUSH)))

(define insn-dispatch-table
  (let1 table (make-vector insn-count)
    (do ([i 0 (+ i 1)])
        ((= i insn-count))
      (vector-set! table i (lambda x (error 'jit:compiler "not implemented instruction" (instruction->symbol (make-instruction i))))))
    table))



(define (register-insn-dispatch-table insn proc)
  (vector-set! insn-dispatch-table insn proc))


(define (macro-to-fixnum reg)
  `((sarq ,reg 2)                        ; reg = reg >> 2
   (movslq ,reg ,(r64->32 reg)))) ; reg = reg32 (with sign)

(define (macro-make-fixnum reg)
  `((leaq ,reg (& 1 (* ,reg 4)))))

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

(define (NUMBER_ADD)
  (let ([label1 (gensym)]
        [label2 (gensym)]
        [label3 (gensym)]
        )
  `(,@(trace-push! $NUMBER_ADD)
    (movq rax ,(vm-register 'sp)) ; rax = sp
    (leaq rdx (& rax -8))         ; rdx = sp - 8
    (movq ,(vm-register 'sp) rdx) ; sp = sp - 8
    (movq rbp (& rax -8))         ; rbp = *(sp - 8) == POP
    (movl eax ebp)                ; eax = (32bit)ebp
    (andl eax 3)                  ; eax.isFixnum
    (subb al 1)                   ;
    (je ,label1)
   ,@(DEBUGGER 3020) ;; stack arg is not Fixnum
    (label ,label2)
   ,@(DEBUGGER 3021) ;; ac arg is not Fixnum
    (label ,label3)
   ,@(DEBUGGER 3022) ;; add result is Bignum
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
;   ,@(DEBUGGER)
    (addl eax edx) ;; arg + ac
;   ,@(DEBUGGER)
 ;  ,@(DEBUGGER)
    (movslq r12 eax) ;; r12 = (32bit)arg1
;   ,@(DEBUGGER)
    (leaq rax (& r12 536870912)) ;; rax = r12 + max-fixnum
    (leaq rdx (& 1 (* r12 4))) ;; rdx = makeFixnum(r12)
    (cmpq rax 1073741823)
    (ja ,label3) ;; jump if rax > 1073741823 => Bignum
    (movq ,(vm-register 'ac) rdx) ;; ac = result of addition
    (movq rax rdx) ;; ac = result of addition
    )
))


(define (NUMBER_SUB_PUSH)
  (let ([label1 (gensym)]
        [label2 (gensym)]
        [label3 (gensym)]
        )
  `(,@(trace-push! $NUMBER_SUB_PUSH)
    (movq rax ,(vm-register 'sp)) ; rax = sp
    (leaq rdx (& rax -8))         ; rdx = sp - 8
    (movq ,(vm-register 'sp) rdx) ; sp = sp - 8
    (movq rbp (& rax -8))         ; rbp = *(sp - 8) == POP
    (movl eax ebp)                ; eax = (32bit)ebp
;    ,@(DEBUGGER 3029)
    (andl eax 3)                  ; eax.isFixnum
    (subb al 1)                   ;
    (je ,label1)
   ,@(DEBUGGER 3030) ;; stack arg is not Fixnum
    (label ,label2)
   ,@(DEBUGGER) ;; ac arg is not Fixnum
    (label ,label3)
   ,@(DEBUGGER 3031) ;; sub result is Bignum
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

(define TRUE (obj->integer #t))
(define FALSE (obj->integer #f))

;; EQ
;; ;; rax <- VM* | (movq rax (& rsp 88))
;; ;; rcx <- VM* | (movq rcx (& rsp 88))
;; (movq rdx (& rax 40))
;; (movq rbx (& rcx 8))
;; (leaq rax (& rdx -8))
;; (movq (& rcx 40) rax)
;; (cmpq (& rdx -8) rbx)
;; (movl eax 86)
;; (movl edx 70)
;; (cmove rax rdx)
;; (movq (& rcx 8) rax)
(define (EQ)
  `(,@(trace-push! $EQ)
    (movq rdx ,(vm-register 'sp))
    (movq rbx ,(vm-register 'ac))
    (leaq rax (& rdx -8))
    (movq ,(vm-register 'sp) rax)
    (cmpq (& rdx -8) rbx)
    (movl eax ,FALSE)
    (movl edx ,TRUE)
    (cmove rax rdx)
    (movq ,(vm-register 'ac) rax)))

;; REFER_FREE
;; ;; rdx <- VM* | (movq rdx (& rsp 88))
;; ;; rbx <- VM* | (movq rbx (& rsp 88))
;; (movq rax (& rdx 48))
;; (leaq rcx (& rax 8))
;; (movq rdx (& rax))
;; (movq (& rbx 48) rcx)
;; (movq rcx (& rbx 16))
;; (sarq rdx 2)
;; (movslq edx,%rdx)
;; (movq rcx (& rcx 8))
;; (movq rcx (& rcx 24))
;; (movq rdx (& rcx,%rdx,8))
;; (movq (& rbx 8) rdx)
(define (REFER_FREE index)
  `(,@(trace-push! $REFER_FREE)
    (movq rdx ,(obj->integer index))
    (movq rcx ,(vm-register 'dc))
    ,@(macro-to-fixnum 'rdx)
    (movq rcx (& rcx 8))
    (movq rcx (& rcx 24))
    (movq rdx (& rcx (* rdx 8)))
    (movq ,(vm-register 'ac) rdx)
    (movq rax rdx)))

;; REFER_LOCAL
;; (movq rbx (& rsp 88))
;; (movq rax ,(vm-register 'pc))
;; (movq rdx (& rax))
;; (leaq rcx (& rax 8))
;; (movq ,(vm-register 'pc) rcx)
;; (movq rcx ,(vm-register 'fp))
;; (sarq rdx 2)
;; (movslq edx,%rdx)
;; (movq rdx (& rcx,%rdx,8))
;; (movq ,(vm-register 'ac) rdx)
(define (REFER_LOCAL index)
  `(,@(trace-push! $REFER_LOCAL)
    (movq rcx ,(vm-register 'fp))
    (movq rdx ,(obj->integer index))
    ,@(macro-to-fixnum 'rdx)
    ,@(macro-refer-local 'rcx 'rcx 'rdx)
    (movq ,(vm-register 'ac) rcx)
    (movq rax ,(vm-register 'ac))))

;; REFER_LOCAL + PUSH
(define (REFER_LOCAL_PUSH index)
  `(,@(trace-push! $REFER_LOCAL_PUSH)
    ,@(REFER_LOCAL index)
    ,@(PUSH)))

;; REFER_LOCAL_PUSH + CONSTANT
(define (REFER_LOCAL_PUSH_CONSTANT index constant-value)
  `(,@(trace-push! $REFER_LOCAL_PUSH_CONSTANT)
    ,@(REFER_LOCAL_PUSH index)
    ,@(CONSTANT constant-value)))

(define (CLOSURE . x)
  `(,@(trace-push! $CLOSURE)))


;; pending: CALL for closure is not implemented.
(define (REFER_GLOBAL_CALL id n)
  `(,@(trace-push! $REFER_GLOBAL_CALL)
    ,@(REFER_GLOBAL id)
    ,@(CALL n)))



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

(define (FRAME) ;; label is not used. pc is
  `(
    ,@(trace-push! $FRAME)
    (movq rdx ,(vm-register 'sp))
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

;; This is not VM instruction, used for tests.
(define (RESTORE_REGISTERS n)
  `((movq rdx ,n)
    (leaq rdx (& (* rdx 8)))
    (negq rdx)
    (addq rdx ,(vm-register 'sp)) ; rdx = sp - n
    (movq rax (& rdx -8))         ; fp = (sp - 8)
;    ,@(DEBUGGER 1112)
    (movq ,(vm-register 'fp) rax)
    (movq rax (& rdx -16))
    (movq ,(vm-register 'cl) rax)
    (movq rax (& rdx -24))
    (movq ,(vm-register 'dc) rax)
    (movq rax (& rdx -32))
    (leaq rcx (& rdx -32))
    (movq ,(vm-register 'pc) rax)
    (movq ,(vm-register 'sp) rcx)))

(define (RETURN n)
  `(,@(trace-push! $RETURN)
    ,@(RESTORE_REGISTERS n)
    (movq rax ,(vm-register 'ac)) ;; we need this.
    (retq)))

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
  `(,@(trace-push! $BRANCH_NOT_LT)
    (movq rax ,(vm-register 'sp)) ; rax = sp
    (leaq rdx (& rax -8))         ; rdx = sp - 8
    (movq ,(vm-register 'sp) rdx) ; sp = sp - 8
    (movq rdx (& rax -8))         ; rax = *sp
;    ,@(DEBUGGER 5002)
    (movl eax edx)                ; eax = (32bit)(rdx)
;;                     (movq rdx 1234)
;;                    ,@(DEBUGGER 1235)
    (andl eax 3)                  ; n.isFixnum
;   ,@(DEBUGGER 3000)
    (subb al 1)                   ;

    (je ,label1)                  ;
    ;; ToDo : not Fixnum case here
   ,@(DEBUGGER 3001)
    (label ,label2)                  ; ac or n is not fixnum case
   ,@(DEBUGGER 3002)
    (label ,label1)
    (movq rcx ,(vm-register 'ac)) ; rcx = ac
    (movl eax ecx)                ; eax = (32bit)(rcx)
    (andl eax 3)                  ; ac.isFixnum
    (subb al 1)
    (jne ,label2)
    (sarq rdx 2)                  ; rdx.toFixnum()
    (sarq rcx 2)                   ; rcx.toFixnum()
;    ,@(DEBUGGER 5001)
    (movq rax ,(get-c-address 'Object::False)) ; eax = pointer to False
    (cmpq rdx rcx)                ; lt condition?
    (movq rdx ,(get-c-address 'Object::True)) ; edx = pointer to True
    (cmovl rax rdx)               ; if condition? then rax = rdx
    (movq rax (& rax))             ; rax = True or False
;   ,@(DEBUGGER)
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


;; CONSTANT
;; (movq rbx (& rsp 88))
;; (label .LVL362)
;; (movq rax (vm-register 'pc))
;; (label .LBE10902)
;; (movq rdx (& rax))
;; (label .LBB10903)
;; (leaq rcx (& rax 8))
;; (label .LVL363)
;; (movq (vm-register 'pc) rcx)
;; (movq (vm-register 'ac) rdx)
(define (CONSTANT val)
  `(,@(trace-push! $CONSTANT)
    (movq ,(vm-register 'ac) ,(obj->integer val))
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

(define (DEBUGGER . x)
  (if (pair? x)
      `((movq r8 ,(car x))
        (int 3))
      '((int 3))))

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




;; Instruction is just a serialized list
;; We convert the list to a list of instruction packet.
;; (CONST 3 PUSH FRAME 3) => (((CONST 3) . 0) ((PUSH) . 2) ((FRAME 3). 3))
(define (pack-instruction lst)
  (let loop ([lst lst]
             [accum '()]
             [packed '()])
    (cond
     [(null? lst)
      (let1 packed (reverse (cons (reverse accum) packed))
        (map-accum
         (lambda (x seed) (values (cons x seed) (+ seed (length x))))
         0
         packed))]
     [(instruction? (car lst))
      (loop (cdr lst)
            (cons (car lst) '())
            (if (null? accum) packed (cons (reverse accum) packed)))]
     [else
      (loop (cdr lst)
            (cons (car lst) accum)
            packed)])))

(define (map-accum proc seed lst)
  (let loop ([lst lst]
             [accum '()]
             [seed seed])
    (cond
     [(null? lst) (values (reverse accum) seed)]
     [else
      (let-values (([a s] (proc (car lst) seed)))
        (loop (cdr lst) (cons a accum) s))])))



;; TODO: Rewrote JIT compile using insert labels.
;; JIT compiler
(define (compile closure)
  (guard (c [#t
             (format #t "not implemented ~a\n" (condition-irritants (find irritants-condition? (simple-conditions c))))
             #f]) ;; JIT compile error returns #f to VM.
         (let* ([insn* (pack-instruction (closure->list closure))]
                [label* (collect-labels! insn*)])
           (let1 insn* (insert-labels insn* label*)
             (let1 asm* (map (lambda (insn)
                               (match (car insn)
                                 [('label . x) `((label . ,x))]
                                 [else
                                  (apply (vector-ref insn-dispatch-table (instruction->integer (caar insn)))
                                         (cdar insn))]))
                             insn*)
               (let1 compiled (u8-list->c-procedure (assemble (apply append (cons (trace-reset!) asm*))))
                 (set-jit-compiled! closure compiled)
                 closure))))))

;; N.B.
;;   this procedure will set-car! to the lst.
(define (collect-labels! lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (cond
     [(null? lst) (list-sort (lambda (x y) (> (cdr x) (cdr y))) ret)]
     [else
      (let1 insn (caar lst)
        (case (instruction->symbol (car insn))
          [(BRANCH_NOT_LT BRANCH_NOT_EQ)
           (let ([label `(label ,(gensym))]
                 [offset (cadr insn)])
             (set-car! (cdr insn) label) ;; ugly
             (loop (+ i (length (caar lst))) (cdr lst) (cons `(,label . ,(+ offset i 1)) ret)))]
          [(REFER_LOCAL_BRANCH_NOT_NULL)
           (let ([label `(label ,(gensym))]
                 [offset (caddr insn)])
             (set-car! (cddr insn) label) ;; ugly
             (loop (+ i (length (caar lst))) (cdr lst) (cons `(,label . ,(+ offset i 2)) ret)))]
          [else
            (loop (+ i (length (caar lst))) (cdr lst) ret)]))])))

(define (insert-labels lst labels)
  (format #t "insert-labels lst=~a labels=~a" lst labels)
  (let loop ([labels labels]
             [lst lst]
             [ret '()])
    (cond
     [(null? lst)
      (unless (null? labels)
        (error 'insert-labels "there are unresolved labels" labels))
        (reverse ret)]
     [(and (not (null? labels)) (= (cdar labels) (cdar lst)))
      (loop (cdr labels) (cdr lst) (append (list (car lst) (car labels)) ret))]
     [else
      (loop labels (cdr lst) (cons (car lst) ret))])))

(define (make-dispatch-table)
  (register-insn-dispatch-table $REFER_GLOBAL_PUSH REFER_GLOBAL_PUSH)
  (register-insn-dispatch-table $PAIR_P PAIR_P)
(register-insn-dispatch-table $BRANCH_NOT_EQ (lambda (label)
                                                 (match label
                                                   [('label dst)
                                                    (BRANCH_NOT_EQ dst)]
                                                   [else (error 'BRANCH_NOT_EQ "label expeced")])))
  (register-insn-dispatch-table $CONSTANT_PUSH CONSTANT_PUSH)
  (register-insn-dispatch-table $UNDEF UNDEF)
  (register-insn-dispatch-table $EQ EQ)
  (register-insn-dispatch-table $REFER_FREE REFER_FREE)
  (register-insn-dispatch-table $REFER_LOCAL REFER_LOCAL)
  (register-insn-dispatch-table $CLOSURE CLOSURE)
  (register-insn-dispatch-table $ASSIGN_GLOBAL ASSIGN_GLOBAL)
  (register-insn-dispatch-table $CONSTANT CONSTANT)
  (register-insn-dispatch-table $REFER_LOCAL_PUSH REFER_LOCAL_PUSH)
  (register-insn-dispatch-table $REFER_LOCAL_PUSH_CONSTANT REFER_LOCAL_PUSH_CONSTANT)
  (register-insn-dispatch-table $BRANCH_NOT_LT (lambda (label)
                                                 (match label
                                                   [('label dst)
                                                    (BRANCH_NOT_LT dst)]
                                                   [else (error 'BRANCH_NOT_LT "label expeced")])))
  (register-insn-dispatch-table $REFER_LOCAL_BRANCH_NOT_NULL
                                (lambda (index label)
                                  (match label
                                    [('label dst)
                                     (REFER_LOCAL_BRANCH_NOT_NULL index dst)]
                                    [else (error 'REFER_LOCAL_BRANCH_NOT_NULL "label expeced")])))

  (register-insn-dispatch-table $RETURN RETURN)
 (register-insn-dispatch-table $FRAME (lambda (x) (FRAME))) ;; discard offset
  (register-insn-dispatch-table $NUMBER_SUB_PUSH NUMBER_SUB_PUSH)
  (register-insn-dispatch-table $NUMBER_ADD NUMBER_ADD)
  (register-insn-dispatch-table $REFER_GLOBAL REFER_GLOBAL)
;  (register-insn-dispatch-table $REFER_GLOBAL_CALL REFER_GLOBAL_CALL)
  (register-insn-dispatch-table $CALL CALL)
  (register-insn-dispatch-table $PUSH_FRAME (lambda (x) (PUSH_FRAME)))
  (register-insn-dispatch-table $PUSH PUSH)
)

(make-dispatch-table)

;; Export jit compiler to VM.
(set-symbol-value! 'jit-compile compile)


;(assemble (EQ))
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
)
