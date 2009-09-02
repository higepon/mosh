/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

    .file   "ffi_stub_x86_64.s"

    .text

    .align  4,0x90

    .globl  c_callback_stub_intptr
    .globl  c_callback_stub_float
    .globl  c_callback_stub_double

c_callback_stub_double:
    movq        $c_callback_double, %r11
    jmp         callback_stub_common

c_callback_stub_float:
    movq        $c_callback_float, %r11
    jmp         callback_stub_common

c_callback_stub_intptr:
    movq        $c_callback_intptr, %r11
    jmp         callback_stub_common

callback_stub_common:
    pushq       %rbp
    movq        %rsp, %rbp
    subq        $112, %rsp

    movq        %rdi, (%rsp)
    movq        %rsi, 8(%rsp)
    movq        %rdx, 16(%rsp)
    movq        %rcx, 24(%rsp)
    movq        %r8, 32(%rsp)
    movq        %r9, 40(%rsp)

    testq       %rax, %rax
    jz          no_sse

    movsd       %xmm0, 48(%rsp)
    movsd       %xmm1, 56(%rsp)
    movsd       %xmm2, 64(%rsp)
    movsd       %xmm3, 72(%rsp)
    movsd       %xmm4, 80(%rsp)
    movsd       %xmm5, 88(%rsp)
    movsd       %xmm6, 96(%rsp)
    movsd       %xmm7, 104(%rsp)

no_sse:
    movq        (%r10), %rdi        # uid
    movq        8(%r10), %rsi       # argc
    movq        %rsp, %rdx          # reg
    leaq        16(%rbp), %rcx      # stack

    call        *%r11
    movq        %rbp, %rsp
    popq        %rbp
    ret

.section .note.GNU-stack,"",%progbits
