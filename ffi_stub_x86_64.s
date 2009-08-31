/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

    .file   "ffi_stub_linux64.s"

    .text

    .align  4,0x90

    .globl  c_func_stub_intptr_x64
    .globl  c_func_stub_float_x64
    .globl  c_func_stub_double_x64
    .globl  c_callback_stub_intptr_x64
    .globl  c_callback_stub_float_x64
    .globl  c_callback_stub_double_x64

c_func_stub_intptr_x64:
c_func_stub_float_x64:
c_func_stub_double_x64:

    pushq       %rbp
    movq        %rsp, %rbp

# stack arguments

    leaq        (, %rsi, 8), %rax
    andq        $-16, %rax
    subq        %rax, %rsp          # align to 16 byte
    movq        $0, %r10            # i = 0
loop:
    cmpq        %r10, %rsi
    je          done
    movq        (%rcx, %r10, 8), %rax
    movq        %rax, (%rsp, %r10, 8)
    addq        $1, %r10
    jmp         loop
done:
    leaq        (%rcx, %rsi, 8), %r10

# sse arguments

    testq       %rdx, %rdx
    jz          sse_done
    movq        (%r10), %rax
    testq       %rax, %rax
    js          sse_float
    jnz         sse_mixed

sse_double:
    movsd        8(%r10), %xmm0
    movsd       16(%r10), %xmm1
    movsd       24(%r10), %xmm2
    movsd       32(%r10), %xmm3
    movsd       40(%r10), %xmm4
    movsd       48(%r10), %xmm5
    movsd       56(%r10), %xmm6
    movsd       64(%r10), %xmm7
    jmp         sse_done

sse_float:
    cvtsd2ss     8(%r10), %xmm0
    cvtsd2ss    16(%r10), %xmm1
    cvtsd2ss    24(%r10), %xmm2
    cvtsd2ss    32(%r10), %xmm3
    cvtsd2ss    40(%r10), %xmm4
    cvtsd2ss    48(%r10), %xmm5
    cvtsd2ss    56(%r10), %xmm6
    cvtsd2ss    64(%r10), %xmm7
    jmp         sse_done

sse_mixed:
    testb       %al, %al
    jz          D0
    cvtsd2ss    8(%r10), %xmm0
    jmp         L1
D0:
    movsd       8(%r10), %xmm0

L1:
    shrq        $8, %rax
    testb       %al, %al
    jz          D1
    cvtsd2ss    16(%r10), %xmm1
    jmp         L2
D1:
    movsd       16(%r10), %xmm1

L2:
    shrq        $8, %rax
    testb       %al, %al
    jz          D2
    cvtsd2ss    24(%r10), %xmm2
    jmp         L3
D2:
    movsd       24(%r10), %xmm2

L3:
    shrq        $8, %rax
    testb       %al, %al
    jz          D3
    cvtsd2ss    32(%r10), %xmm3
    jmp         L4
D3:
    movsd       32(%r10), %xmm3

L4:
    shrq        $8, %rax
    testb       %al, %al
    jz          D4
    cvtsd2ss    40(%r10), %xmm4
    jmp         L5
D4:
    movsd       40(%r10), %xmm4

L5:
    shrq        $8, %rax
    testb       %al, %al
    jz          D5
    cvtsd2ss    48(%r10), %xmm5
    jmp         L6
D5:
    movsd       48(%r10), %xmm5

L6:
    shrq        $8, %rax
    testb       %al, %al
    jz          D6
    cvtsd2ss    56(%r10), %xmm6
    jmp         L7
D6:
    movsd       56(%r10), %xmm6

L7:
    shrq        $8, %rax
    testb       %al, %al
    jz          D7
    cvtsd2ss    64(%r10), %xmm7
    jmp         sse_done
D7:
    movsd       64(%r10), %xmm7

sse_done:
    leaq        72(%r10), %r10
    movq        %rdi, %r11
    movq        %rdx, %rax

# reg arguments

    movq          (%r10), %rdi
    movq         8(%r10), %rsi
    movq        16(%r10), %rdx
    movq        24(%r10), %rcx
    movq        32(%r10), %r8
    movq        40(%r10), %r9

    call        *%r11
    movq        %rbp, %rsp
    popq        %rbp
    ret

        /*
c_callback_stub_double_x64:
    movq        $c_callback_double_x64, %r11
    jmp         callback_stub_common

c_callback_stub_float_x64:
    movq        $c_callback_float_x64, %r11
    jmp         callback_stub_common
        */

c_callback_stub_intptr_x64:
    movq        $c_callback_intptr_x64, %r11
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
