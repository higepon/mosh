; assembler.ss - JIT assembler for AMD64
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
(library (mosh jit assembler)
  (export assemble assemble1 r64->32 r64->8
          ;; exported for test
          r64? mod-r-r/m r64->number number->r64)
  (import (rnrs)
          (mosh)
          (mosh control)
          (only (srfi :1) append-map drop-right)
          (match)
          (srfi :26)
          (srfi :8))

;; Memo
;;   Sassy style: http://home.earthlink.net/~krautj/sassy/sassy-Z-H-7.html#node_sec_5.1.1

(define r64* '(rax rcx rdx rbx rsp rbp rsi rdi r8  r9  r10 r11 r12 r13 r14 r15))
(define r32* '(eax ecx edx ebx esp ebp esi edi))

(define r8* '(al cl dl bl ah ch dh bh spl bpl sil dil))
(define (r64? obj) (memq obj r64*))
(define (r32? obj) (memq obj r32*))
(define (r8? obj) (memq obj r8*))

(define (find-index-eq x x*)
  (receive (_ index) (find-with-index (cut eq? <> x) x*)
    index))

(define (r64->number reg) (find-index-eq reg r64*))
(define (r32->number reg) (find-index-eq reg r32*))
(define (r8->number reg) (find-index-eq reg r8*))

(define (number->r8 n)
  (list-ref r8* n))
(define (number->r32 n)
  (list-ref r32* n))
(define (number->r64 n)
  (list-ref r64* n))

(define (r32->64 reg)
  (number->r64 (r32->number reg)))

(define (r64->32 reg)
  (number->r32 (r64->number reg)))

(define (r64->8 reg high?)
  (number->r8 (if high?
                  (+ 4 (r64->number reg))
                  (r64->number reg))))

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


;; See Table 2-1. 16-Bit Addressing Forms with the ModR/M Byte
(define mod.disp0    #b00)
(define mod.disp8    #b01)
(define mod.disp32   #b10)
(define mod.register #b11)

(define sib.scale1 #b00)
(define sib.scale4 #b10)
(define sib.scale8 #b11)

(define (mod-r-r/m mod reg-n r/m-n)
  (+ (bitwise-arithmetic-shift-left mod 6)
     (bitwise-arithmetic-shift-left (bitwise-and reg-n #b111) 3)
     (bitwise-and r/m-n #b111)))

(define (sib scale index base)
  (+ (bitwise-arithmetic-shift-left scale 6)
     (bitwise-arithmetic-shift-left (bitwise-and index #b111) 3)
     (bitwise-and base #b111)))

(define (compose-reg mod r64 r/m64)
  (values
   (rex-prefix #t (ext-reg? r64) #f (ext-reg? r/m64))
   (mod-r-r/m mod r64 r/m64)
   (cond
    ;; r/m = #b*100 (rsp, r12) requires sib except mod=#x11
    ;;   see http://www.mztn.org/lxasm64/amd05.html
    [(and (not (= mod mod.register)) (memq r/m64 '(#b0100 #b1100)))
     (let ([r64-index #b100])   ;; #b100 means "none", no index.
       (list (sib sib.scale1 r64-index r/m64)))]
    [else '()])))

(define (compose-reg+sib mod r64 scale index base)
  (values
   (rex-prefix #t (ext-reg? r64) (ext-reg? index) #f)
   (mod-r-r/m mod r64 #b100) ;; r/m64 = #b100 is SIB flag.
   (list (sib scale index base))))

(define (pack-op bits64? opcode mod r64 r/m64 scale index base disp0/8/32 imm8/32)
  (receive (rex-prefix modrr/m sib)
      (if scale (compose-reg+sib mod r64 scale index base) (compose-reg mod r64 r/m64))
    (values `(,@(if bits64? (list rex-prefix) '()) ,opcode ,modrr/m ,@sib ,@disp0/8/32 ,@imm8/32) #f)))

;; asm* : list of asm
;; asm  : (assembled-byte* addr-of-net-instruction label-to-fixup)
(define (assemble code*)
  (let loop ([code* code*]
             [addr 0]
             [asm* '()]
             [label* '()])
    (cond
     [(null? code*)
      (append-map (match-lambda
                      [(byte* addr #f) byte*]
                      [(byte* addr label-to-fixup)
                       (cond
                        [(assoc label-to-fixup label*) =>
                         (lambda (x)
                           (display x)
                           (let1 offset (- (cdr x) addr)
                             (cond
                              [(imm8? offset)
                               (format #t "<byte* ~a:~a>" byte* (append (drop-right byte* 1) (imm8->u8* offset)))
                               (append (drop-right byte* 1) (imm8->u8* offset))]
;; offset will be moved
;;                               [(imm32? offset)
;;                                ;; ugly
;;                                (format #t "<byte* ~a:~a>" byte* (append (list #x0F (+ (car byte*) #x10)) (imm32->u8* offset)))
;;                                (append (list #x0F (+ (car byte*) #x10)) (imm32->u8* offset))]
                              [else
                               (error 'assemble "offset out of range" offset)])))]
                        [else
                         (error 'assemble (format "BUG: label:~a not found on ~a" label-to-fixup label*))])])
                  (reverse asm*))]
     [else
      (match (car code*)
        [('label label)
         (loop (cdr code*)
               addr
               asm*
               (cons (cons label addr) label*))]
        [x
         (let-values (([asm label-to-fixup] (assemble1 x)))
           (loop (cdr code*)
                 (+ (length asm) addr)
                 (cons (list asm (+ (length asm) addr) label-to-fixup) asm*)
                 label*))])])))

(define (imm8? n)
  (and (integer? n) (<= -128 n 127)))

(define (imm16? n)
  (and (integer? n) (<= (- (expt 2 15)) n (- (expt 2 15) 1))))

(define (imm32? n)
;  (and (integer? n) (<= (- (expt 2 31)) n (- (expt 2 31) 1))))
  (and (integer? n) (<= 0 n (- (expt 2 32) 1))))

(define (imm64? n)
  (and (integer? n) (<= (- (expt 2 63)) n (- (expt 2 63) 1))))

(define (imm8->u8* n)
  (assert (imm8? n))
  (list (bitwise-and n #xff)))

(define (imm32->u8* n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)))

(define (imm64->u8* n)
  (list (bitwise-and n #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 8) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 16) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 24) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 32) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 40) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 48) #xff)
        (bitwise-and (bitwise-arithmetic-shift-right n 56) #xff)))

(define (ext-reg? reg)
  (>= reg 8))

;; REX prefix
;;    w - operand width.  #t - 64bit
;;    r - modrr/m reg register extra bit
;;    x - sib index register extra bit
;;    b - modrr/m r/m register extra bit
(define (rex-prefix w? r? x? b?)
  (+ #b01000000
     (if w? #b1000 0)
     (if r? #b100  0)
     (if x? #b10   0)
     (if b? #b1    0)))


;; (push r64)
(define ($r64 opcode r64)
  (if (ext-reg? r64)
      (values `(,(rex-prefix #f #f #f #t) ,(+ opcode (- r64 8))) #f)
      (values `(,(+ opcode r64)) #f)))


;; (oprand dest src)
;; returns (values byte* label-to-fixup)
(define (assemble1 code) ;; todo refactoring
  (define rex.w (rex-prefix #t #f #f #f))
  (match code
    ;; CPUID
    [('cpuid)
     (values '(#x0f #xa2) #f)]
    ;; NEG r/m64
    ;;   REX.W + F7 /3
    [('negq (? r64? (= r64->number r/m64)))
     (pack-op #t #xf7 mod.register 3 r/m64 #f #f #f '() '())]
    ;; CMOVC r64, r/m64
    ;;   REX.W + 0F 4C /r
    [('cmovl (? r64? (= r64->number r64)) (? r64? (= r64->number r/m64)))
     (values `(,(rex-prefix #t (ext-reg? r64) #f (ext-reg? r/m64)) #x0f #x4c ,(mod-r-r/m mod.register r64 r/m64)) #f)]
    ;; CMOVE r64, r/m64
    ;;   REX.W + 0F 44 /r
    [('cmove (? r64? (= r64->number r64)) (? r64? (= r64->number r/m64)))
     (values `(,(rex-prefix #t (ext-reg? r64) #f (ext-reg? r/m64)) #x0f #x44 ,(mod-r-r/m mod.register r64 r/m64)) #f)]
    ;; CDQE Valid N.E. RAX ← sign-extend of EAX.
    ;;   REX.W + 98
    [('cltq)
     (values `(,rex.w #x98) #f)]
    ;; TEST r/m8, imm8
    ;;   F6 /0 ib
    [('testb (? r8? (= r8->number r/m8)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #f #xf6 mod.register 0 r/m8 #f #f #f '() imm8)]
    ;; TEST r/m32, r32
    ;;   85 /r
    [('testl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (values `(#x85 ,(mod-r-r/m mod.register r32 r/m32)) #f)]
    ;; LEAVE
    ;;   C9
    [('leave) (values '(#xc9) #f)]
    ;; PUSH r64
    ;;   50+rd
    [('push (? r64? (= r64->number r64)))
     ($r64 #x50 r64)]
    [('pop (? r64? (= r64->number r64)))
     ($r64 #x58 r64)]
    [('je (? symbol? label))
     (values `(#x74 #x00) label)]
    [('ja (? symbol? label))
     (values `(#x77 #x00) label)]
    [('jmp (? symbol? label))
     (values `(#xeb #x00) label)]
    [('jne (? symbol? label))
     (values `(#x75 #x00) label)]
    [('js (? symbol? label))
     (values `(#x78 #x00) label)]
    [('jge (? symbol? label))
     (values `(#x7d #x00) label)]
    ;; CMP r/m64,r64
    ;;   REX.W + 39 /r
    [('cmpq (? r64? (= r64->number r/m64)) (? r64? (= r64->number r64)))
     (pack-op #t #x39 mod.register r64 r/m64 #f #f #f '() '())]
    [('cmpq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? r64? (= r64->number r64)))
     (pack-op #t #x39 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    [('cmpq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64))))
     (pack-op #t #x3b mod.disp0 r64 r/m64 #f #f #f '() '())]
    ;; CMP RAX, imm32 REX.W + 3D id
    [('cmpq 'rax (? imm32? (= imm32->u8* u8*)))
     (values `(,rex.w #x3d ,@u8*) #f)]
    ;; CMP r/m64, imm8
    ;;   REX.W + 83 /7 ib
    [('cmpq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.disp8 7 r/m64 #f #f #f disp8 imm8)]
    [('cmpq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 7 r/m64 #f #f #f '() imm8)]

    ;; CALL r/m64
    ;;   FF /2
    [('callq (? r64? (= r64->number r/m64)))
     (pack-op #f #xff mod.register 2 r/m64 #f #f #f '() '())]
    [('callq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #f #xff mod.disp8 2 r/m64 #f #f #f disp8 '())]
    ;; INT 3
    [('int 3) (values '(#xcc) #f)]
    ;; MOV r/m64, imm32 Valid
    ;;   REX.W + C7 /0
    [('movq (? r64? (= r64->number r/m64)) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.register 0 r/m64 #f #f #f '() imm32)]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq (? r64? (= r64->number r/m64)) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.register r64 r/m64 #f #f #f '() '())]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number base)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8b mod.disp0 r64 #b100 sib.scale8 index base '() '())]
    ;; MOV r64,r/m64
    ;;   REX.W + 8B /r
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) 0)) ;; special case for zero
     (pack-op #t #x8b mod.disp0 r64 r/m64 #f #f #f '() '())]
    [('movq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x8b mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    [('movq (? r64? r64) ('& (? r64? r/m64)))
     (assemble1 `(movq ,r64 (& ,r/m64 0)))]
    ;; MOV r/m64,r64
    ;;   REX.W + 89 /r
    [('movq ('& (? r64? (= r64->number r/m64)) 0) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp0 r64 r/m64 #f #f #f '() '())]
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    [('movq ('& (? r64? (= r64->number r/m64))) (? r64? (= r64->number r64)))
     (pack-op #t #x89 mod.disp0 r64 r/m64 #f #f #f '() '())]
    ;; MOV r/m32,r32
    ;;   89 /r
    [('movl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (pack-op #f #x89 mod.register r32 r/m32 #f #f #f '() '())]
    ;; MOV r32,imm32
    ;;   B8+ rd
    [('movl (? r32? (= r32->number r32)) (? imm32? (= imm32->u8* u8*)))
     (values `(,(+ #xb8 r32) ,@u8*) #f)]
    ;; AND r/m32, imm8
    ;;   83 /4 ib
    [('andl (? r32? (= r32->number r/m32)) (? imm8? (= imm8->u8* imm8)))
     (values `(,#x83 ,(mod-r-r/m mod.register 4 r/m32) ,@imm8) #f)]
    ;; SUB AL, imm8
    ;;   2C ib
    [('subb 'al (? imm8? (= imm8->u8* imm8)))
     (values `(,#x2c ,@imm8) #f)]
    ;; ADD r/m32, r32
    ;;   01 /r
    [('addl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (values `(,#x01 ,(mod-r-r/m mod.register r32 r/m32)) #f)]
    ;; SUB r/m32, r32
    ;;   29 /r
    [('subl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (values `(,#x29 ,(mod-r-r/m mod.register r32 r/m32)) #f)]
    ;; SUB r/m32, r32
    ;;   29 /r
    [('subl (? r32? (= r32->number r/m32)) (? r32? (= r32->number r32)))
     (values `(,#x29 ,(mod-r-r/m mod.register r32 r/m32)) #f)]

    ;; MOV r/m64, imm32
    ;;   REX.W + C7 /0
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.disp8 0 r/m64 #f #f #f disp8 imm32)]
    [('movq ('& (? r64? (= r64->number r/m64)) (? imm32? (= imm32->u8* disp32))) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #xc7 mod.disp32 0 r/m64 #f #f #f disp32 imm32)]
    ;; ADD r/m64, imm8
    ;;   REX.W + 83 /0 ib Valid N.E.
    [('addq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 0 r/m64 #f #f #f '() imm8)]
    ;; ADD r/m64, imm32
    ;;   REX.W + 81 /0 ib Valid N.E.
    [('addq (? r64? (= r64->number r/m64)) (? imm32? (= imm32->u8* imm32)))
     (pack-op #t #x81 mod.register 0 r/m64 #f #f #f '() imm32)]
    ;; ADD r64, r/m64
    ;;   REX.W + 03 /r
    [('addq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x03 mod.disp8 r64 r/m64 #f #f #f disp8 '())]
    ;; SUB r/m64, imm8
    ;;   REX.W + 83 /5 ib
    [('subq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #x83 mod.register 5 r/m64 #f #f #f '() imm8)]
    ;; RET
    ;;   C3
    [('retq)
     (values '(#xc3) #f)]
    ;; SAR r/m64, imm8
    ;;   REX.W + C1 /7 ib
    [('sarq (? r64? (= r64->number r/m64)) (? imm8? (= imm8->u8* imm8)))
     (pack-op #t #xc1 mod.register 7 r/m64 #f #f #f '() imm8)]
    ;; MOVSXD r64, r/m32
    ;;   REX.W** + 63 /r
    [('movslq (? r64? (= r64->number r64)) (? r32? (= r32->number r/m32)))
     (pack-op #t #x63 mod.register r64 r/m32 #f #f #f '() '())]
    ;; LEA r64,m
    ;;   REX.W + 8D /r
    [('leaq (? r64? (= r64->number r64)) ('& (? imm32? (= imm32->u8* disp32)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8d mod.disp0 r64 #b100 sib.scale8 index (r64->number 'rbp) disp32 '())] ;; base is omitted and disp32 => base = rbp
    [('leaq dest-reg ('& (? r64? src-reg)))
     (assemble1 `(leaq ,dest-reg (& ,src-reg 0)))]
    ;; (& displacement (* scale index-reg)) is disp32
    [('leaq (? r64? r64) ('& ('* (? r64? index-reg) 8)))
     (assemble1 `(leaq ,r64 (& 0 (* ,index-reg 8))))]
    [('leaq (? r64? (= r64->number r64)) ('& (? imm32? (= imm32->u8* disp32)) ('* (? r64? (= r64->number index)) 4)))
     (pack-op #t #x8d mod.disp0 r64 #b100 sib.scale4 index (r64->number 'rbp) disp32 '())] ;; base is omitted and disp32 => base = rbp
    [('leaq (? r64? (= r64->number r64)) ('& (? imm8? (= imm8->u8* disp8)) (? r64? (= r64->number base)) ('* (? r64? (= r64->number index)) 8)))
     (pack-op #t #x8d mod.disp8 r64 #b100 sib.scale8 index base disp8 '())]
    [('leaq (? r64? (= r64->number r64))  ('& (? r64? (= r64->number base)) 0))
     (pack-op #t #x8d mod.disp0 r64 base #f #f #f '() '())]
    [('leaq (? r64? (= r64->number r64))  ('& (? r64? (= r64->number base)) (? imm8? (= imm8->u8* disp8))))
     (pack-op #t #x8d mod.disp8 r64 base #f #f #f disp8 '())]
    [('leaq (? r64? (= r64->number r64)) ('& (? r64? (= r64->number base)) (? imm32? (= imm32->u8* disp32))))
     (pack-op #t #x8d mod.disp32 r64 base #f #f #f disp32 '())]
    [x
     (error 'assemble "assemble error: invalid syntax" x)]))
)
