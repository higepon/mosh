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
(test-equal '(movq rbx rsp) (gas->sassy "mov   %rsp,%rbx"))
(test-equal '(movq rdx (& rbx #x30)) (gas->sassy "mov    0x30(%rbx),%rdx"))
(test-equal '(addq rcx #x8) (gas->sassy "add    $0x8,%rcx"))
(test-equal '(movq (& rbx #x28) rcx) (gas->sassy "mov    %rcx,0x28(%rbx)"))
(test-equal '(movq rcx (& rdx)) (gas->sassy "mov    (%rdx),%rcx"))
(test-equal '(leaq rdx (& rax -8)) (gas->sassy "leaq    -8(%rax), %rdx"))
(test-equal '() (gas->sassy "\t.file\t\"VM.cpp\""))
(test-equal '() (gas->sassy "\t.section\t.debug_abbrev,\"\",@progbits"))
(test-equal '(label Ldebug_abbrev0) (gas->sassy ".Ldebug_abbrev0:"))
(test-equal '() (gas->sassy "\t.text"))
(test-equal '() (gas->sassy "\t.align 2"))
(test-equal '() (gas->sassy "\t.weak\t_ZN10gc_cleanup7cleanupEPvS0_"))
(test-equal '() (gas->sassy "\t.type\t_ZN10gc_cleanup7cleanupEPvS0_, @function"))
(test-equal '(label _ZN10gc_cleanup7cleanupEPvS0_) (gas->sassy "_ZN10gc_cleanup7cleanupEPvS0_:"))
(test-equal '() (gas->sassy "\t.loc 1 339 0"))
(test-equal '(addq rdi rsi) (gas->sassy "\taddq\t%rsi, %rdi"))
(call-with-input-file "/home/taro/Dropbox/VM.S"
  (lambda (p)
    (do ([line (get-line p) (get-line p)])
        [(eof-object? line)]
      (guard (c [#t
                 (format #t "(test-equal '() (gas->sassy ~s))" line)
                 (newline)
                 (raise c)])
                (gas->sassy line)))))

(test-results)
