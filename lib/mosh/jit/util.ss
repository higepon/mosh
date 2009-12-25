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
  (export gas->sassy asm*->jit-asm*)
  (import (rnrs)
          (match)
          (mosh))




;; ((label ".LBB10906") (testb bpl 3) (label ".LBE10906") (label ".LBE10905") (movq (& rsi 8) rbp) (label ".LBB10904") (label ".LBB10907") (jne (label ".L224")) (movq rax (& rbp)) (andl eax 3) (cmpq rax 3) (je (label ".L224")) (label ".LBE10907") (label ".LBE10904") (movq rax (& rbp 8)) (movq (& rsi 8) rax) (jmp (label ".L870")) (label ".LVL361") (label ".L234") (label ".LBB10901") (label ".LBB10902") 

;; (movq rbx (& rsp 88))
;; (label ".LVL362")
;; (movq rax (& rbx 48))
;; (label ".LBE10902")
;; (movq rdx (& rax))
;; (label ".LBB10903")
;; (leaq rcx (& rax 8))
;; (label ".LVL363")
;; (movq (& rbx 48) rcx) (movq (& rbx 8) rdx) (label ".LBE10903") (label ".LBE10901"))

(define (asm*->jit-asm* asm*)
  (match asm*
    [(('movq reg1 ('& 'rsp 88))
      ('label _a)
      ('movq reg2 ('& reg1 48))
      ('label _b)
      ('movq reg3 ('& reg2))
       ('label _c)
      ('leaq reg4 ('& 'rax 8))
      ('label _d)
      ('movq ('& reg1 48) reg4)
      . more)
     (values 
     (format
";; VM->pc++
;;   ~a <- VM* | (movq ~a (& rsp 88))
;;   ~a <- pc_ | (movq ~a (& ~a 48))
;;   ~a <- *pc_ | (movq ~a (& ~a))
;;   rcx <- pc_ + 8 | (leaq rcx (& rax 8))
;;   pc_ <- ~a | (movq (& 48 ~a) ~a)" reg1 reg1 reg2 reg2  reg1 reg3 reg3 reg2 reg4 reg1 reg4)
     more)]
    [(('movq reg1 ('& 'rsp 88))
      ('label _a)
      ('movq reg2 ('& reg1 48))
      ('label _b)
      ('movq reg3 ('& reg2))
      . more)
     (values
     (format
";; ~a <- VM* | (movq ~a (& rsp 88))
;; ~a <- pc_ | (movq ~a (& ~a 48))
;; ~a <- *pc_ | (movq ~a (& ~a))" reg1 reg1 reg2 reg2  reg1 reg3 reg3 reg2)
     more)]
    [(('movq reg1 ('& 'rsp 88))
      ('label _a)
      ('movq reg2 ('& reg1 48))
      . more)
     (values
     (format
";; ~a <- VM* | (movq ~a (& rsp 88))
;; ~a <- pc_ | (movq ~a (& ~a 48))" reg1 reg1 reg2 reg2  reg1)
     more)]
    [(('movq reg1 ('& 'rsp 88))
      . more)
     (values
     (format
";; ~a <- VM* | (movq ~a (& rsp 88))" reg1 reg1)
     more)]
    [else (values #f #f)]))


(define (gas->sassy gas)
  (let-syntax
      ([s (lambda (x)
            (syntax-case x ()
              [(_ val)
               #'(cond
                  [(string->number val 10) =>
                   (lambda (x) x)]
                  [else
                   (string->symbol val)])]))])
   (cond
   [(#/\s*#/ gas)
    '()]
   ;; movl $.LC0, %edi
   [(#/\s*([^\s]+)\s+\$(\.\w+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) (label ,(m 2))))]
   ;; movl $8, %esi
   [(#/\s*([^\s]+)\s+\$(\d+),\s*%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) ,(s (m 2))))]
   ;; movl $_ZN10gc_cleanup7cleanupEPvS0_, %esi
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) ,(m 2)))]
   ;; movq $2, (%rdi)
   [(#/\s*([^\s]+)\s+\$(\d+),\s*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 3))) ,(s (m 2))))]
   ;; movq $_ZTV10gc_cleanup+24, (%rdi)
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 3))) ,(m 2)))]
   ;; setne 16(%rbx)
   [(#/^\s*([^\s]+)\s+(\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 3)) ,(s (m 2)))))]
   ;; movq $2, 144(%rdi)
   [(#/\s*([^\s]+)\s+\$(\d+),\s*(-?\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(s (m 3))) ,(s (m 2))))]
   ;; movq $.L161, 144(%rdi)
   [(#/\s*([^\s]+)\s+\$([^\s]+),\s*(-?\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(s (m 3))) ,(m 2)))]
   ;; call exit
   [(#/^\s*call\s+([\.\w]+)$/ gas) =>
    (lambda (m)
      `(call ,(m 1)))]
   ;; addq $8, %rax
   [(#/\s*([^\s]+)\s+\$(\d+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) ,(s (m 2))))]
   ;; movq %rdx, (%rcx)
   [(#/\s*([^\s]+)\s+%([^\s]+),\s*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 3))) ,(s (m 2))))]
   ;; movl $2, 56(%rdi)
   [(#/\s*([^\s]+)\s+\$(\d+),\s*(\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(s (m 3))) ,(s (m 2))))]
   ;; movl $2, xxx(%rdi)
   [(#/\s*([^\s]+)\s+\$(\d+),\s*(\w+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(m 3)) ,(s (m 2))))]
   ;; cmpq %rax, 104(%rbx)
   [(#/\s*([^\s]+)\s+%([^\s]+),\s*([\-\+\d]+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(s (m 3))) ,(s (m 2))))]
   ;; cmpq %rax, zzz(%rbx)
   [(#/\s*([^\s]+)\s+%([^\s]+),\s*([\-\+\w]+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 4)) ,(m 3)) ,(s (m 2))))]
   ;; call *24(%rax)
   [(#/\s*([^\s]+)\s+\*(\d+)\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 3)) ,(s (m 2)))))]
   ;; call *(%rax)
   [(#/\s*([^\s]+)\s+\*\(%([^\s]+)\)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (& ,(s (m 2)))))]
   ;; call *%rax
   [(#/\s*([^\s]+)\s+\*%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 2))))]
   ;; rep
   [(#/\s*(rep|ret|cltq)/ gas) =>
    (lambda (m)
      `(,(s (m 1))))]
   ;; setbe %al
   [(#/^\s*([^\s]+)\s+%([^\s]+)$/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 2))))]
   ;; label
   [(#/\s*([^:]+):/ gas) =>
    (lambda (m)
      `(label ,(m 1)))]
   ;; jb .L5
   [(#/^\s*(jl|jns|js|ja|jge|jb|jbe|je|jg|jae|jne|jmp|jle)\s+([^%\*]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) (label ,(m 2))))]
   ;; jmp *%r11
   [(#/^\s*jmp\s*\*%(.*)/ gas) =>
    (lambda (m)
      `(jmp ,(s (m 1))))]
   ;; movq (%rdi), %rax
   [(#/^\s*([^\s]+)\s*\(%([^\s]+)\),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) (& ,(s (m 2)))))]
   ;; movq 88(%rdi), %rax
   [(#/\s*([^\s]+)\s*(\d+)\(%([^\s]+)\),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 4)) (& ,(s (m 3)) ,(s (m 2)))))]
   ;; addq %rsi, %rdi
   [(#/\s*([^\s]+)\s*%([^\s]+),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(s (m 1)) ,(s (m 3)) ,(s (m 2))))]
   [(#/\s+\.quad|data|long|zero|comm|value|sleb|ascii|file|section|text|align|weak|type|loc|cfi|size|globl|string|byte|\.uleb128/ gas) '()]
   [else
    (error 'gas->sassy "invalid form" gas)]
   )))
)
