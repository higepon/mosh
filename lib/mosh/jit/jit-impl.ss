;; http://home.earthlink.net/~krautj/sassy/sassy-Z-H-7.html#node_sec_5.1.1

;       movq    80(%rsp), %rbx

(define register* '(rax rcx rdx rbx rsp rbp rsi rdi
                    r8  r9  r10 r11 r12 r13 r14 r15))

(define (register? obj) (memq obj register*))

(define (register->number reg)
  (receive (_ index) (find-with-index (cut eq? <> reg) register*)
    index))

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
(define (mod-r-m dest src)
  (assert (for-all register? (list dest src)))
  (let ([mod #b11]
        [reg (register->number src)]
        [r/m (register->number dest)])
    (+ (bitwise-arithmetic-shift-left mod 6)
       (bitwise-arithmetic-shift-left reg 3)
       r/m)))

;; (oprand dest src)
(define (assemble code)
  (define rex.w #x48)
  (match code
    [('movq (? register? dest) (? register? src))
     ;; rex.w
     `(,rex.w #x89 ,(mod-r-m dest src))
     ]
    ;; (movq rbx (& rsp 80))

    ;;
    [('movq dest-reg ('& src-reg displacement))
     ;; REX.W + 8B /r MOV r64,r/m64 Valid N.E. Move r/m64 to r64.
     #t
     ]))
