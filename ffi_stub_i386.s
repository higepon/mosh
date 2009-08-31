/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

    .file   "ffi_stub_darwin.s"

    .text

    .align  4,0x90

    .globl  _c_func_stub_intptr
    .globl  _c_func_stub_int64
    .globl  _c_func_stub_double
    .globl  _c_callback_stub_intptr
/*    .globl  _c_callback_stub_int64
    .globl  _c_callback_stub_double*/

_c_func_stub_intptr:
_c_func_stub_int64:
_c_func_stub_double:

    pushl   %ebp
    movl    %esp, %ebp

    subl    $8, %esp
    movl    %edi, -8(%ebp)
    movl    %esi, -4(%ebp)

    movl    8(%ebp), %eax       # adrs
    movl    12(%ebp), %ecx      # argc
    movl    16(%ebp), %esi      # argv

    leal    15(,%ecx,4), %ecx   # align to 16 byte
    andl    $-16, %ecx

    movl    %esp, %edx
    subl    %ecx, %esp
    movl    %esp, %edi
    rep movsb

    call    *%eax

    movl    -8(%ebp), %edi
    movl    -4(%ebp), %esi
    movl    %ebp, %esp
    popl    %ebp
    ret

    .align  4,0x90

        /*
_c_callback_stub_double:
    movl    $_c_callback_double, %edx
    jmp     callback_stub_common

    .align  4,0x90

_c_callback_stub_int64:
    movl    $_c_callback_int64, %edx
    jmp     callback_stub_common

    .align  4,0x90
*/
_c_callback_stub_intptr:
    movl    $_c_callback_intptr, %edx
    jmp     callback_stub_common

    .align  4,0x90

callback_stub_common:
    pushl   %ebp
    movl    %esp, %ebp

    subl    $24, %esp           # 16 + 8

    movl    (%ecx), %eax        # uid

    movl    %eax, (%esp)

    movl    4(%ecx), %eax       # argc
    movl    %eax, 4(%esp)

    leal    8(%ebp), %eax       # base
    movl    %eax, 8(%esp)
    call    *%edx

    movl    %ebp, %esp
    popl    %ebp
    ret
