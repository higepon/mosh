; jit-util.scm - Tests for JIT utility
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
        (mosh control)
        (match)
        (mosh file)
        (only (srfi :8) receive)
        (only (srfi :1) append-map)
        (mosh jit vm)
        (mosh jit util))

;; gas -> sassy
(test-equal '() (gas->sassy "\t.file\t\"VM.cpp\""))
(test-equal '() (gas->sassy "\t.section\t.debug_abbrev,\"\",@progbits"))
(test-equal '() (gas->sassy "\t.text"))
(test-equal '() (gas->sassy "\t.align 2"))
(test-equal '() (gas->sassy "\t.weak\t_ZN10gc_cleanup7cleanupEPvS0_"))
(test-equal '() (gas->sassy "\t.type\t_ZN10gc_cleanup7cleanupEPvS0_, @function"))
(test-equal '() (gas->sassy "\t.loc 1 339 0"))
(test-equal '(addq rdi rsi) (gas->sassy "\taddq\t%rsi, %rdi"))
(test-equal '(movq rax (& rdi)) (gas->sassy "\tmovq\t(%rdi), %rax"))
(test-equal '(jmp r11) (gas->sassy "\tjmp\t*%r11"))
(test-equal '() (gas->sassy "\t.size\t_ZN10gc_cleanup7cleanupEPvS0_, .-_ZN10gc_cleanup7cleanupEPvS0_"))
(test-equal '() (gas->sassy ".globl _ZNK6scheme2VM17mayBeStackPointerEPNS_6ObjectE"))
(test-equal '(jb (label ".L5")) (gas->sassy "\tjb\t.L5"))
(test-equal '(label ".Ldebug_abbrev0") (gas->sassy ".Ldebug_abbrev0:"))
(test-equal '(setbe al) (gas->sassy "\tsetbe\t%al"))
(test-equal '(rep) (gas->sassy "\trep"))
(test-equal '(ret) (gas->sassy "\tret"))
(test-equal '(call (& rax 24)) (gas->sassy "\tcall\t*24(%rax)"))
(test-equal '(cmpq (& rbx 104) rax) (gas->sassy "\tcmpq\t%rax, 104(%rbx)"))
(test-equal '(je (label ".L11")) (gas->sassy "\tje\t.L11"))
(test-equal '(movl (& rdi 56) 2) (gas->sassy "\tmovl\t$2, 56(%rdi)"))
(test-equal '(movq (& rcx) rdx) (gas->sassy "\tmovq\t%rdx, (%rcx)"))
(test-equal '(jne (label ".L37")) (gas->sassy "\tjne\t.L37"))
(test-equal '(jmp (label ".L30")) (gas->sassy "\tjmp\t.L30"))
(test-equal '(addq rax 8) (gas->sassy "\taddq\t$8, %rax"))
(test-equal '(jle (label ".L33")) (gas->sassy "\tjle\t.L33"))
(test-equal '(call "exit") (gas->sassy "\tcall\texit"))
(test-equal '() (gas->sassy "\t.string\t\"<\""))
(test-equal '(movl edi (label ".LC0")) (gas->sassy "\tmovl\t$.LC0, %edi"))
(test-equal '() (gas->sassy "\t.byte\t0xff"))
(test-equal '() (gas->sassy "\t.uleb128 .LLSDACSE3083-.LLSDACSB3083"))
(test-equal '(cltq) (gas->sassy "\tcltq"))
(test-equal '(ja (label ".L106")) (gas->sassy "\tja\t.L106"))
(test-equal '(movl esi "_ZN10gc_cleanup7cleanupEPvS0_") (gas->sassy "\tmovl\t$_ZN10gc_cleanup7cleanupEPvS0_, %esi"))
(test-equal '(jg (label ".L134")) (gas->sassy "\tjg\t.L134"))
(test-equal '(movq (& rsp -32) rbx) (gas->sassy "\tmovq\t%rbx, -32(%rsp)"))
(test-equal '(call "T.1760") (gas->sassy "\tcall\tT.1760"))
(test-equal '(jl (label ".L630")) (gas->sassy "\tjl\t.L630"))
(test-equal '(js (label ".L630")) (gas->sassy "\tjs\t.L630"))
(test-equal '(call r14) (gas->sassy "\tcall\t*%r14"))
(test-equal '() (gas->sassy "\t.quad\t0"))
(test-equal '() (gas->sassy "\t.long\t0"))
(test-equal '() (gas->sassy "\t.zero\t32"))
(test-equal '() (gas->sassy "\t.comm\t_ZGVZN6scheme2VM7compileENS_6ObjectEE4proc,8,8"))
(test-equal '() (gas->sassy "\t.value\t0x1"))
(test-equal '() (gas->sassy "\t.sleb128 16"))
(test-equal '() (gas->sassy "\t.ascii\t\"shta\""))
(test-equal '(call (& rax)) (gas->sassy "\tcall\t*(%rax)"))
(test-equal '(movq (& rdi 144) ".L161") (gas->sassy "\tmovq\t$.L161, 144(%rdi)"))
(test-equal '() (gas->sassy "#APP"))
(test-equal '(setne (& rbx 16)) (gas->sassy "\tsetne\t16(%rbx)"))
(test-equal '(movq (& rip "_ZN6scheme7ioErrorE+8") rax) (gas->sassy "\tmovq\t%rax, _ZN6scheme7ioErrorE+8(%rip)"))
(test-equal '() (gas->sassy "\t.data"))
(test-equal '(movq rbx (& rsp 88)) (gas->sassy "	movq	88(%rsp), %rbx"))

;; todo
(test-equal '(movq (& rdi) "_ZTV10gc_cleanup+24") (gas->sassy "\tmovq\t$_ZTV10gc_cleanup+24, (%rdi)"))
(test-equal '(cmpb (& rip "_ZGVZN6scheme2VM7compileENS_6ObjectEE4proc") 0) (gas->sassy "\tcmpb\t$0, _ZGVZN6scheme2VM7compileENS_6ObjectEE4proc(%rip)"))
(test-equal '(movq (& rip "_ZZN6scheme2VM7compileENS_6ObjectEE4proc") rax) (gas->sassy "\tmovq\t%rax, _ZZN6scheme2VM7compileENS_6ObjectEE4proc(%rip)"))

(test-equal ";; VM->pc++
;;   rbx <- VM* | (movq rbx (& rsp 88))
;;   rax <- pc_ | (movq rax (& rbx 48))
;;   rdx <- *pc_ | (movq rdx (& rax))
;;   rcx <- pc_ + 8 | (leaq rcx (& rax 8))
;;   pc_ <- rcx | (movq (& 48 rbx) rcx)"
            (asm*->jit-asm*
             '(
             (movq rbx (& rsp 88))
             (label a)
             (movq rax (& rbx 48))
             (label b)
             (movq rdx (& rax))
             (label c)
             (leaq rcx (& rax 8))
             (label d)
             (movq (& rbx 48) rcx))))


;; use jit-helper for VM-Run.S
(define (jit-helper gas-lines)
  (let loop ([asm* (remp null? (map gas->sassy gas-lines))])
    (cond
     [(null? asm*) '()]
     [else
      (receive (jit-asm* more) (asm*->jit-asm* asm*)
        (cond
         [jit-asm*
          (display jit-asm*)
          (newline)
          (loop more)]
         [else
          (display (car asm*))
          (newline)
          (loop (cdr asm*))]))])))

(define (print-vm-asm* asm-text)
  (for-each print (asm*->vm-asm* (string-split asm-text #\newline))))

(print-vm-asm* "# 0 \"\" 2
#NO_APP
.LBB10049:
.LBB10050:
	.loc 5 50 0
	movq	88(%rsp), %rbx
.LVL593:
	movq	48(%rbx), %rax
.LBE10050:
	movq	(%rax), %rdx
.LBB10051:
	leaq	8(%rax), %rcx
.LVL594:
	movq	%rcx, 48(%rbx)
	.loc 18 800 0
	movq	32(%rbx), %rcx
	sarq	$2, %rdx
	movslq	%edx,%rdx
	movq	(%rcx,%rdx,8), %rdx
	movq	%rdx, 8(%rbx)
.LBE10051:
.LBE10049:
	.loc 18 801 0
#APP
# 801 \"src/VM-Run.cpp\"")

#;(print-vm-asm* "	 	 # -- REFER_GLOBAL start
# 0 \"\" 2
#NO_APP
.LBB9989:
.LBB9990:
	.loc 5 50 0
	movq	88(%rsp), %rbx
.LVL580:
	movq	88(%rsp), %rsi
	movq	48(%rbx), %rax
.LBE9990:
	movq	(%rax), %rbx
.LBB9991:
	addq	$8, %rax
	movq	%rax, 48(%rsi)
.LBE9991:
.LBE9989:
.LBB9992:
.LBB9993:
.LBB9994:
.LBB9995:
	.loc 6 225 0
	testb	$3, %bl
	jne	.L488
	movq	(%rbx), %rax
	movq	%rax, %rdx
	andl	$3, %edx
	cmpq	$3, %rdx
	je	.L1115
.L488:
.LBE9995:
.LBE9994:
.LBE9993:
.LBE9992:
.LBB9997:
	.loc 18 737 0
	movq	96(%rsp), %rdx
	movq	88(%rsp), %rcx
.LVL581:
	movq	%rbx, %rsi
	movq	96(%rsp), %rdi
	movq	(%rdx), %rax
	movq	104(%rcx), %rdx
	call	*24(%rax)
.LVL582:
	.loc 18 738 0
	movq	88(%rsp), %rsi
	cmpq	104(%rsi), %rax
.LVL583:
	je	.L1116
	.loc 18 745 0
	movq	8(%rax), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, 8(%rsi)
	.loc 18 746 0
	movq	48(%rsi), %rdx
	movq	%rax, -8(%rdx)
.LVL584:
.L489:
.LBE9997:
	.loc 18 749 0
#APP
# 749 \"src/VM-Run.cpp\" 1
	 	 # -- REFER_GLOBAL end")


(test-equal '(movq rax (vm-register 'ac)) (asm->vm-asm 'rbx '(movq rax (& rbx 8))))
(test-equal '(movq rax (vm-register 'dc)) (asm->vm-asm 'rbx '(movq rax (& rbx 16))))
(test-equal '(movq rax (vm-register 'cl)) (asm->vm-asm 'rbx '(movq rax (& rbx 24))))
(test-equal '(movq rax (vm-register 'fp)) (asm->vm-asm 'rbx '(movq rax (& rbx 32))))
(test-equal '(movq rax (vm-register 'sp)) (asm->vm-asm 'rbx '(movq rax (& rbx 40))))
(test-equal '(movq rax (vm-register 'pc)) (asm->vm-asm 'rbx '(movq rax (& rbx 48))))
(test-equal '(movq rax (vm-register 'num-values)) (asm->vm-asm 'rbx '(movq rax (& rbx 56))))
(test-equal '(movq rax (vm-register 'values)) (asm->vm-asm 'rbx '(movq rax (& rbx 64))))

(test-equal '(movq rax (vm-register 'ac)) (asm->vm-asm 'rbx '(movq rax (& rbx 8))))
(test-equal '(movq rax (vm-register 'dc)) (asm->vm-asm 'rbx '(movq rax (& rbx 16))))
(test-equal '(movq rax (vm-register 'cl)) (asm->vm-asm 'rbx '(movq rax (& rbx 24))))
(test-equal '(movq rax (vm-register 'fp)) (asm->vm-asm 'rbx '(movq rax (& rbx 32))))
(test-equal '(movq rax (vm-register 'sp)) (asm->vm-asm 'rbx '(movq rax (& rbx 40))))
(test-equal '(movq rax (vm-register 'pc)) (asm->vm-asm 'rbx '(movq rax (& rbx 48))))
(test-equal '(movq rax (vm-register 'num-values)) (asm->vm-asm 'rbx '(movq rax (& rbx 56))))
(test-equal '(movq rax (vm-register 'values)) (asm->vm-asm 'rbx '(movq rax (& rbx 64))))

(test-equal '((movq rbx (& rsp 88)) rbx) (receive x (asm->vm-asm #f '(movq rbx (& rsp 88)))
              x))



(test-results)

(jit-helper (string-split
"	# -- SYMBOL_P start
	movq	88(%rsp), %rbx
	movq	8(%rbx), %rax
	testb	$3, %al
	jne	.L648
	movq	(%rax), %rdx
	movq	%rdx, %rax
	andl	$3, %eax
	cmpq	$3, %rax
	je	.L1093
.L648:
	movl	$86, %eax
.L649:
	movq	88(%rsp), %rsi
	movq	%rax, 8(%rsi)
	movq	48(%rsi), %rax
	movl	$1, 56(%rsi)
	movq	(%rax), %rdx
	addq	$8, %rax
	movq	%rax, 48(%rsi)
	jmp	.L1002"
 #\newline))
;; (let ([lst (remp null? (map gas->sassy (string-split "# 310 \"src/VM-Run.cpp\" 1
;; 	 	 # -- CONSTANT start
;; # 0 \"\" 2
;; #NO_APP
;; .LBB10901:
;; .LBB10902:
;; 	.loc 5 50 0
;; 	movq	88(%rsp), %rbx
;; .LVL362:
;; 	movq	48(%rbx), %rax
;; .LBE10902:
;; 	movq	(%rax), %rdx
;; .LBB10903:
;; 	leaq	8(%rax), %rcx
;; .LVL363:
;; 	movq	%rcx, 48(%rbx)
;; 	.loc 18 311 0
;; 	movq	%rdx, 8(%rbx)
;; .LBE10903:
;; .LBE10901:
;; 	.loc 18 312 0
;; #APP
;; # 312 \"src/VM-Run.cpp\" 1" #\newline)))])
;; (let loop ([lst lst])
;;   (cond
;;    [(null? lst) '()]
;;    [(display (asm*->jit-asm* lst))
;;     (loop (cdr lst))])))


;; (let ([lst (remp null? (map gas->sassy (file->list "/home/taro/Dropbox/VM-Run.S")))])
;; (let loop ([lst lst])
;;   (cond
;;    [(null? lst) '()]
;;    [(display (asm*->jit-asm* lst))
;;     (loop (cdr lst))])))

;; Following code was used for debug.
;; (call-with-input-file "/home/taro/Dropbox/VM-Run.S"
;;   (lambda (p)
;;     (do ([line (get-line p) (get-line p)]
;;          [i 0 (+ i 1)])
;;         [(eof-object? line)]
;;       (guard (c [#t
;;                  (format #t "line:~d" i)
;;                  (newline)
;;                  (format #t "(test-equal '() (gas->sassy ~s))" line)
;;                  (newline)
;;                  (raise c)])
;;              (display i)
;;              (newline)
;;                 (gas->sassy line)))))
