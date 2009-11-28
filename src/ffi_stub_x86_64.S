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
    .globl  c_func_stub_intptr_x64
    .globl  c_func_stub_double_x64

// c_func_stub_intptr_x64(intptr_t func<rdi>, intptr_t reg<rsi>, intptr_t stack<rdx>, intptr_t stack_count_bytes<rcx>, intptr_t xmm<r8>)
c_func_stub_double_x64:
c_func_stub_intptr_x64:
        push %r12 // r12-15 are callee saved
        push %r13
        push %r14
        push %r15
        pushq %rbp
        movq  %rsp, %rbp
        subq %rcx, %rsp
        movsd   (%r8), %xmm0
        movsd  8(%r8), %xmm1
        movsd 16(%r8), %xmm2
        movsd 24(%r8), %xmm3
        movsd 32(%r8), %xmm4
        movsd 40(%r8), %xmm5
        movsd 48(%r8), %xmm6
        movsd 56(%r8), %xmm7
        movq  %rdi, %rax
        addq  %rdx, %rcx // rcx = end of stack frame
        movq %rcx, %r11       // r11 = end of frame
        movq %rdx, %r12       // r12 = start of frame
        movq %rsi, %r10
        movq 0(%r10), %rdi    // register argument 1
        movq 8(%r10), %rsi    // register argument 2
        movq 16(%r10), %rdx   // register argument 3
        movq 24(%r10), %rcx   // register argument 4
        movq 32(%r10), %r8    // register argument 5
        movq 40(%r10), %r9    // register argument 6
        movq %rsp, %r13
arguments_loop:
        cmpq %r11, %r12
        je arguments_done
        movq 0(%r12), %r14
        movq %r14, 0(%r13)
        addq $8, %r12
        addq $8, %r13
        jmp arguments_loop
arguments_done:
        call *%rax
        movq %rbp, %rsp
        popq %rbp
        pop %r15
        pop %r14
        pop %r13
        pop %r12
        ret

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
