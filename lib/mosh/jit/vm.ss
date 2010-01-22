; vm.ss - VM information.
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
(library (mosh jit vm)
  (export vm-register vm-make-fixnum get-c-address
          obj->integer asm->vm-asm asm*->vm-asm*)
  (import (rnrs)
          (mosh)
          (match)
          (srfi :8)
          (srfi :26)
          (mosh jit util)
          (system)
          (only (srfi private include) include/resolve))

;; VM register offset depends on your architecture.
(include/resolve ("mosh" "jit") "offset.ss")

(define vm-register* '(ac #;8 dc #;16 cl #;24 fp #;32 sp #; 40 pc num-values values _ _ _ namespace not-found))

(define (vm-register reg)
  `(& rdi ,(* (+ (receive (_ index) (find-with-index (cut eq? <> reg) vm-register*)
                   index) 1) vm-register-offset)))

(define (vm-make-fixnum n)
  (+ (bitwise-arithmetic-shift-left n 2) 1))

(define (find-with-index pred lst)
  (let loop ([i 0]
             [lst lst])
    (cond
     [(null? lst)
      (values #f #f)]
     [(pred (car lst))
      (values (car lst) i)]
     [else
      (loop (+ i 1) (cdr lst))])))


(define (asm->vm-asm vm* asm)
  (match asm
    ;; reg <- vm-reg
    [('movq dest ('& (? (lambda (x) (eq? vm* x))) (= (lambda (x) (list-ref vm-register* (- (/ x 8) 1))) vm-reg)))
     (values `(movq ,dest (vm-register ',vm-reg)) vm*)]
    ;; vm-reg <- reg
    [('movq ('& (? (lambda (x) (eq? vm* x))) (= (lambda (x) (list-ref vm-register* (- (/ x 8) 1))) vm-reg)) dest)
     (values `(movq (vm-register ',vm-reg) ,dest) vm*)]
    ;; reg <- vm*
    ;;   identify change vm* reg.
    [('movq reg ('& 'rsp 88))
     (values asm reg)]
    [else
     (values asm vm*)]))

(define (map-accum proc seed lst)
  (let loop ([lst lst]
             [accum '()]
             [seed seed])
    (cond
     [(null? lst) (values (reverse accum) seed)]
     [else
      (let-values (([a s] (proc (car lst) seed)))
        (loop (cdr lst) (cons a accum) s))])))

(define asm*->vm-asm*
  (case-lambda
   [(line*)
    (asm*->vm-asm* #f line*)]
   [(vm* line*)
    (map-accum (lambda (asm current-vm*) (asm->vm-asm current-vm* asm)) vm* (remp null? (map gas->sassy line*)))]))

)
