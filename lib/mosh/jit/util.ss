; util.ss - JIT utility
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
(library (mosh jit util)
  (export gas->sassy gas->jit-asm)
  (import (rnrs)
          (mosh))

(define (gas->jit-asm line)
  #f
)


(define (gas->sassy gas)
  (cond
   [(#/\s*#/ gas)
    '()]
   ;; movl $.LC0, %edi
   [(#/\s*([^\s]+)\s+\$(\.\w+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 3) (label ,(m 2))))]
   ;; movl $_ZN10gc_cleanup7cleanupEPvS0_, %esi
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 3) ,(m 2)))]
   ;; movq $_ZTV10gc_cleanup+24, (%rdi)
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 3)) ,(m 2)))]
   ;; setne 16(%rbx)
   [(#/^\s*([^\s]+)\s+(\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 3) ,(m 2))))]
   ;; movq $.L161, 144(%rdi)
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*(-?\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 4) ,(m 3)) ,(m 2)))]
   ;; call exit
   [(#/^\s*call\s+([\.\w]+)$/ gas) =>
    (lambda (m)
      `("call" ,(m 1)))]
   ;; addq $8, %rax
   [(#/\s*([^\s]+)\s+\$(\d+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 3) ,(m 2)))]
   ;; movq %rdx, (%rcx)
   [(#/\s*([^\s]+)\s+%([^\s]+),\s*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 3)) ,(m 2)))]
   ;; movl $2, 56(%rdi)
   [(#/\s*([^\s]+)\s+\$(\d+),\s*(\w+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 4) ,(m 3)) ,(m 2)))]
   ;; cmpq %rax, 104(%rbx)
   [(#/\s*([^\s]+)\s+%([^\s]+),\s*([\-\+\w]+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 4) ,(m 3)) ,(m 2)))]
   ;; call *24(%rax)
   [(#/\s*([^\s]+)\s+\*(\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 3) ,(m 2))))]
   ;; call *(%rax)
   [(#/\s*([^\s]+)\s+\*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(m 1) (& ,(m 2))))]
   ;; call *%rax
   [(#/\s*([^\s]+)\s+\*%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 2)))]
   ;; rep
   [(#/\s*(rep|ret|cltq)/ gas) =>
    (lambda (m)
      `(,(m 1)))]
   ;; setbe %al
   [(#/^\s*([^\s]+)\s+%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 2)))]
   ;; label
   [(#/\s*([^:]+):/ gas) =>
    (lambda (m)
      `(label ,(m 1)))]
   ;; jb .L5
   [(#/^\s*(jl|jns|js|ja|jge|jb|jbe|je|jg|jae|jne|jmp|jle)\s+([^%\*]+)/ gas) =>
    (lambda (m)
      `(,(m 1) (label ,(m 2))))]
   ;; jmp *%r11
   [(#/\s*jmp\s*\*%(.*)/ gas) =>
    (lambda (m)
      `("jmp" ,(m 1)))]
   ;; movq (%rdi), %rax
   [(#/\s*([^\s]+)\s*\(%([^\s]+)\),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 3) (& ,(m 2))))]
   ;; addq %rsi, %rdi
   [(#/\s*([^\s]+)\s*%([^\s]+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(m 1) ,(m 3) ,(m 2)))]
   [(#/\s+\.quad|data|long|zero|comm|value|sleb|ascii|file|section|text|align|weak|type|loc|cfi|size|globl|string|byte|\.uleb128/ gas) '()]
   [else
    (error 'gas->sassy "invalid form" gas)]
   ))
)
