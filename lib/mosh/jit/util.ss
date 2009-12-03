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
  (export gas->sassy)
  (import (rnrs)
          (mosh))

(define (gas->sassy gas)
  (cond
   ;; mov %rsp,%rbx
   [(#/([^\s]+)\s+%([^\s]+),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3)) ,(string->symbol (m 2))))]
   ;; mov 0x30(%rbx),%rdx
   [(#/([^\s]+)\s+0x([\d]+)\(%([^\s]+)\),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 4))
        (& ,(string->symbol (m 3)) , (string->number (m 2) 16))))]
   ;; leaq  -8(%rax), %rdx
   [(#/([^\s]+)\s+(-?[\d]+)\(%([^\s]+)\),\s*%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (m 1)) ,(string->symbol (m 4))
        (& ,(string->symbol (m 3)) , (string->number (m 2) 16))))]
   ;; mov (%rbx),%rdx
   [(#/([^\s]+)\s+\(%([^\s]+)\),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3))
        (& ,(string->symbol (m 2)))))]
   ;; mov    %rcx,0x28(%rbx)
   [(#/([^\s]+)\s+%([^\s]+),0x(\d+)\(%([^\s]+)\)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q"))
        (& ,(string->symbol (m 4)) ,(string->number (m 3) 16))
        ,(string->symbol (m 2))
        ))]
   ;; add    $0x8,%rcx
   [(#/([^\s]+)\s+\$0x(\d+),%([^\s]+)/ gas) =>
    (lambda (m)
      `(,(string->symbol (string-append (m 1) "q")) ,(string->symbol (m 3))
        ,(string->number (m 2) 16)))]
   [else
    (error 'gas->sassy "invalid form" gas)]
   ))
)
