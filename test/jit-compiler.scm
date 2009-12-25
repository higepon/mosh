; jit-compiler.scm - Tests for JIT compiler
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
        (mosh control)
        (mosh test)
        (srfi :8)
        (mosh jit vm)
        (mosh jit assembler)
        (mosh jit compiler))

(define (u8*->c-procedure+retq lst)
  (u8-list->c-procedure (append lst (assemble '((retq))))))

(define (fib n) (if (< n 2) 1 (+ (fib (- n 2)) (fib (- n 1)))))

(define (asm*->procedure asm*)
  (u8*->c-procedure+retq (assemble asm*)))

;; c-function address
(test-true (number? (get-c-address 'Object::isNumber)))


;; CONSTANT instruction
(let* ([code (assemble (CONSTANT 3))]
       [proc (u8*->c-procedure+retq code)])
  (test-true (procedure? proc))
  (test-eq 3 (proc)))

;; REFER_LOCAL instruction
(let* ([code1 (assemble (REFER_LOCAL 0))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  (test-eq 1235 (proc 1235)))

;; REFER_LOCAL_PUSH_CONSTANT instruction
(let* ([code1 (assemble (REFER_LOCAL_PUSH_CONSTANT 0 13))]
       [code2  (assemble (POP))]
       [proc (u8*->c-procedure+retq (append code1 code2))])
  (test-true (procedure? proc))
  (test-eq 1234 (proc 1234)))

;; REFER_LOCAL_PUSH_CONSTANT
(let* ([code1 (assemble (REFER_LOCAL_PUSH_CONSTANT 1 13))]
       [code2  (assemble (POP))]
       [proc (u8*->c-procedure+retq (append code1 code2))])
  (test-true (procedure? proc))
  (test-eq 1235 (proc 1234 1235)))

;; BRANCH_NOT_LT
(let* ([label (gensym)]
       [code1 (assemble (append (REFER_LOCAL_PUSH_CONSTANT 0 1)
                                (BRANCH_NOT_LT label)
                                (CONSTANT 3)
                                `((label ,label))))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  ;; ac_ is result of compare
  (test-eq #f (proc 2)))

;; FRAME
(let* ([code1 (assemble
               (append
                (FRAME)
                (POP2)
                (POP2)
                (POP2)
                (POP2)
                (CONSTANT 3)))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  (test-eq 3 (proc)))

;; NUMBER_SUB_PUSH
(let* ([label (gensym)]
       [code1 (assemble
               (append
                (CONSTANT 10)
                (PUSH)
                (CONSTANT 2)
                (NUMBER_SUB_PUSH)
                (POP)))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  (test-eq 8 (proc)))

;; NUMBER_ADD
(let* ([label (gensym)]
       [code1 (assemble
               (append
                (CONSTANT 10)
                (PUSH)
                (CONSTANT 2)
                (NUMBER_ADD)
                ))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  (test-eq 12 (proc)))

;; REFER_GLOBAL
(let* ([code1 (assemble
               (append
                (REFER_GLOBAL 'zero?)
                (list `(movq rax ,(vm-register 'ac)))
                ))]
       [proc (u8*->c-procedure+retq code1)])
  (test-true (procedure? proc))
  (test-eq zero? (proc)))

;; RETURN
(let ([ERROR (gensym)])
  (define (save-vm-registers)
    `((movq rax ,(vm-register 'fp))
      (push rax)
      (movq rax ,(vm-register 'cl))
      (push rax)
      (movq rax ,(vm-register 'dc))
      (push rax)
      (movq rax ,(vm-register 'pc))
      (push rax)))
  (define (restore-vm-registers)
    `((pop rax)
      (movq ,(vm-register 'pc) rax)
      (pop rax)
      (movq ,(vm-register 'dc) rax)
      (pop rax)
      (movq ,(vm-register 'cl) rax)
      (pop rax)
      (movq ,(vm-register 'fp) rax)))
  (let* ([code1 (assemble
                 (append
                  (save-vm-registers)
                  ;; set test data to vm registers
                  `((movq ,(vm-register 'fp) 1)
                    (movq ,(vm-register 'cl) 2)
                    (movq ,(vm-register 'dc) 3)
                    (movq ,(vm-register 'pc) 4))
                  (FRAME)
                  ;; vm registers are dirty
                  `((movq ,(vm-register 'fp) 5)
                    (movq ,(vm-register 'cl) 6)
                    (movq ,(vm-register 'dc) 7)
                    (movq ,(vm-register 'pc) 8))
                  `((movq rdx ,(vm-register 'sp))
                    (movq rax ,(vm-make-fixnum 12)) ;; (push 12)
                    (movq (& rdx) rax)
                    (addq rdx 8)
                    (movq ,(vm-register 'sp) rdx))
                  (RESTORE_REGISTERS 1) ;; pseud RETURN
                  `(
                    ;; check restored fp
                    (movq rax ,(vm-register 'fp))
                    (movq rcx 1)
                    (cmpq rax rcx)
                    (jne ,ERROR)
                    ;; check restored cl
                    (movq rax ,(vm-register 'cl))
                    (movq rcx 2)
                    (cmpq rax rcx)
                    (jne ,ERROR)
                    ;; check restored dc
                    (movq rax ,(vm-register 'dc))
                    (movq rcx 3)
                    (cmpq rax rcx)
                    (jne ,ERROR)
                    ;; check restored pc
                    (movq rax ,(vm-register 'pc))
                    (movq rcx 4)
                    (cmpq rax rcx)
                    (jne ,ERROR)
                    ,@(restore-vm-registers))
                  (CONSTANT 10)
                  `((retq)
                    (label ,ERROR)
                    ,@(restore-vm-registers)
                    ,@(CONSTANT 11)
                    (retq))))]
         [proc (u8*->c-procedure+retq code1)])
    (test-true (procedure? proc))
    (test-eq 10 (proc))))

;;     ;; CALL
;;     (let* ([code1 (assemble
;;                    (append
;;                     (FRAME)
;;                     (CONSTANT (vm-make-fixnum 1))
;;                     (PUSH)
;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (obj->integer +)) ;; we know + is cprocedure
;;                     (CALL 2)
;;                     ))]
;;            [proc (u8*->c-procedure+retq code1)])
;;       (test-true (procedure? proc))
;;       (test-eq 1235 (proc 1234)))

;; ;;     ;; PUSH_FRAME
;; ;;     (let* ([code1 (assemble
;; ;;                    (append
;; ;;                     (PUSH_FRAME)
;; ;;                     (RESTORE_REGISTERS 0)
;; ;;                     (POP)
;; ;;                     (FRAME)
;; ;;                     (CONSTANT (vm-make-fixnum 1))
;; ;;                     (PUSH)
;; ;;                     (REFER_LOCAL_PUSH_CONSTANT 0 (obj->integer +)) ;; we know + is cprocedure
;; ;;                     (CALL 2)
;; ;;                     ))]
;; ;;            [proc (u8*->c-procedure+retq code1)])
;; ;;       (test-true (procedure? proc))
;; ;;       (test-eq 1235 (proc 1234)))

;; fib
#;(let ()
  (let* ([code1 (let ([label (gensym)])
                  (assemble
                   (append
                    (REFER_LOCAL_PUSH_CONSTANT 0 2)
                    (BRANCH_NOT_LT label)
                    (CONSTANT (vm-make-fixnum 1))
                    (RETURN 1)
                    `((label ,label))
                    (FRAME)
                    (REFER_LOCAL_PUSH_CONSTANT 0 2)
                    (NUMBER_SUB_PUSH)
                    (REFER_GLOBAL 'fib)
                    (CALL 1)
                    (PUSH_FRAME)
                    (REFER_LOCAL_PUSH_CONSTANT 0 1)
                    (NUMBER_SUB_PUSH)
                    (REFER_GLOBAL 'fib)
                    (CALL 1)
                    (NUMBER_ADD)
                    (RETURN 1)
                    )))]
         [proc (u8*->c-procedure+retq code1)])
    (test-true (procedure? proc))
    (set-symbol-value! 'fib proc)
    (test-eq 2 (proc 2))
    (test-eq 3 (proc 3))
    (test-eq 1346269 (proc 30))))

;; CPUID
(let* ([label (gensym)]
       [cpuid (asm*->procedure
               `((movq rax (& rdx))                  ;; arg0
                 ,@(macro-to-fixnum 'rax)
                 (cmpq (& rdx 8) 86)                 ;; arg1 isFalse
                 (je ,label)
                 (addq rax #x80000000)               ;; extended?
                 (label ,label)
                 (cpuid)
                 (movq r10 ,(vm-register 'values))   ;; values VM register
                 (movq ,(vm-register 'num-values) 4) ;; number of values
                 ,@(macro-make-fixnum 'rax)          ;; rax is result 1
                 (movq ,(vm-register 'ac) rax)
                 ,@(macro-make-fixnum 'rbx)          ;; rbx is result 2
                 (movq (& r10) rbx)
                 ,@(macro-make-fixnum 'rcx)          ;; rbx is result 2
                 (movq (& r10 8) rcx)
                 ,@(macro-make-fixnum 'rdx)          ;; rbx is result 2
                 (movq (& r10 16) rdx)
                 (movq rax ,(vm-register 'ac))))])
  (define (u32->string u32)
    (list->string (map integer->char
                       (list (bitwise-and u32 #xff)
                             (bitwise-and (bitwise-arithmetic-shift-right u32 8) #xff)
                             (bitwise-and (bitwise-arithmetic-shift-right u32 16) #xff)
                             (bitwise-and (bitwise-arithmetic-shift-right u32 24) #xff)))))
  (do ([i 2 (+ i 1)])
      [(= i 5)]
    (receive (rax rbx rcx rdx) (cpuid i #t)
      (format #;#t "~a~a~a~a" (u32->string rax) (u32->string rbx) (u32->string rcx) (u32->string rdx)))))

(define (a) 3)
(test-false (compiled? a))

(do ([i 0 (+ i 1)])
    [(= i 20)]
  (test-equal 3 (a)))

(test-true (compiled? a))

(test-results)
