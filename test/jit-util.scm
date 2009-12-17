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
        (mosh jit util))

;; gas -> sassy
(test-equal '() (gas->sassy "\t.file\t\"VM.cpp\""))
(test-equal '() (gas->sassy "\t.section\t.debug_abbrev,\"\",@progbits"))
(test-equal '() (gas->sassy "\t.text"))
(test-equal '() (gas->sassy "\t.align 2"))
(test-equal '() (gas->sassy "\t.weak\t_ZN10gc_cleanup7cleanupEPvS0_"))
(test-equal '() (gas->sassy "\t.type\t_ZN10gc_cleanup7cleanupEPvS0_, @function"))
(test-equal '() (gas->sassy "\t.loc 1 339 0"))
(test-equal '("addq" "rdi" "rsi") (gas->sassy "\taddq\t%rsi, %rdi"))
(test-equal '("movq" "rax" (& "rdi")) (gas->sassy "\tmovq\t(%rdi), %rax"))
(test-equal '("jmp" "r11") (gas->sassy "\tjmp\t*%r11"))
(test-equal '() (gas->sassy "\t.size\t_ZN10gc_cleanup7cleanupEPvS0_, .-_ZN10gc_cleanup7cleanupEPvS0_"))
(test-equal '() (gas->sassy ".globl _ZNK6scheme2VM17mayBeStackPointerEPNS_6ObjectE"))
(test-equal '("jb" (label ".L5")) (gas->sassy "\tjb\t.L5"))
(test-equal '(label ".Ldebug_abbrev0") (gas->sassy ".Ldebug_abbrev0:"))
(test-equal '("setbe" "al") (gas->sassy "\tsetbe\t%al"))
(test-equal '("rep") (gas->sassy "\trep"))
(test-equal '("ret") (gas->sassy "\tret"))
(test-equal '("call" (& "rax" "24")) (gas->sassy "\tcall\t*24(%rax)"))
(test-equal '("cmpq" (& "rbx" "104") "rax") (gas->sassy "\tcmpq\t%rax, 104(%rbx)"))
(test-equal '("je" (label ".L11")) (gas->sassy "\tje\t.L11"))
(test-equal '("movl" (& "rdi" "56") "2") (gas->sassy "\tmovl\t$2, 56(%rdi)"))
(test-equal '("movq" (& "rcx") "rdx") (gas->sassy "\tmovq\t%rdx, (%rcx)"))
(test-equal '("jne" (label ".L37")) (gas->sassy "\tjne\t.L37"))
(test-equal '("jmp" (label ".L30")) (gas->sassy "\tjmp\t.L30"))
(test-equal '("addq" "rax" "8") (gas->sassy "\taddq\t$8, %rax"))
(test-equal '("jle" (label ".L33")) (gas->sassy "\tjle\t.L33"))
(test-equal '("call" "exit") (gas->sassy "\tcall\texit"))
(test-equal '() (gas->sassy "\t.string\t\"<\""))
(test-equal '("movl" "edi" (label ".LC0")) (gas->sassy "\tmovl\t$.LC0, %edi"))
(test-equal '() (gas->sassy "\t.byte\t0xff"))
(test-equal '() (gas->sassy "\t.uleb128 .LLSDACSE3083-.LLSDACSB3083"))

;; todo
(test-equal '("movq" (& "rdi") "_ZTV10gc_cleanup+24") (gas->sassy "\tmovq\t$_ZTV10gc_cleanup+24, (%rdi)"))



(test-results)

(call-with-input-file "/home/taro/Dropbox/VM.S"
  (lambda (p)
    (do ([line (get-line p) (get-line p)])
        [(eof-object? line)]
      (guard (c [#t
                 (format #t "(test-equal '() (gas->sassy ~s))" line)
                 (newline)
                 (raise c)])
                (gas->sassy line)))))


