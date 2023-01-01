;; Write Rust VM Ops as binary.
(import (except (rust_jump) run-tests))
(import (match))
(import (mosh control))
(import (mosh test))
(import (only (mosh) format regexp-replace-all rxmatch))
(import (only (rnrs) bytevector-s64-native-set! bytevector-u32-native-set! bytevector-u16-native-set! open-bytevector-output-port put-u8 put-bytevector))
(import (only (rnrs) open-string-output-port string-titlecase))
(import (only (srfi :1) list-ref))
(import (only (srfi :13) string-delete string-join))
(import (rnrs arithmetic fixnums))
(import (rust_sexp))
(import (scheme base))
(import (scheme case-lambda))
(import (scheme file))
(import (scheme process-context))
(import (scheme read))
(import (scheme write))

(define TAG_FIXNUM 0)
(define TAG_TRUE   1)
(define TAG_FALSE  2)
(define TAG_NIL    3)
(define TAG_CHAR   4)
(define TAG_SYMBOL 5)
(define TAG_STRING 6)
(define TAG_PAIR   7)

(define TAG_OP_CONSTANT 10)
(define TAG_OP_CLOSURE  11)
(define TAG_OP_REFER_LOCAL_BRANCH_NOT_NULL 12)
(define TAG_OP_REFER_LOCAL 13)
(define TAG_OP_RETURN 14)
(define TAG_OP_FRAME 15)
(define TAG_OP_CAR_PUSH 16)
(define TAG_OP_REFER_LOCAL_CALL 17)
(define TAG_OP_PUSH_FRAME 18)
(define TAG_OP_REFER_LOCAL_PUSH 19)
(define TAG_OP_CDR_PUSH 20)
(define TAG_OP_REFER_GLOBAL_CALL 21)
(define TAG_OP_CONS 22)
(define TAG_OP_DEFINE_GLOBAL 23)
(define TAG_OP_NOP 24)
(define TAG_OP_REFER_FREE_PUSH 25)
(define TAG_OP_REFER_GLOBAL 26)
(define TAG_OP_TAIL_CALL 27)
(define TAG_OP_LET_FRAME 28)
(define TAG_OP_DISPLAY 29)
(define TAG_OP_REFER_FREE_CALL 30)
(define TAG_OP_PUSH_ENTER 31)
(define TAG_OP_TEST 32)
(define TAG_OP_LOCAL_JMP 33)
(define TAG_OP_CONSTANT_PUSH 34)
(define TAG_OP_PUSH 35)
(define TAG_OP_LEAVE 36)
(define TAG_OP_PAIR_P 37)
(define TAG_OP_ENTER 38)
(define TAG_OP_REFER_FREE 39)
(define TAG_OP_SHIFTJ 40)
(define TAG_OP_HALT 41)

(define (insn->tag insn)
  (cond
    [(assoc insn `((FRAME . ,TAG_OP_FRAME)
                   (PUSH_FRAME . ,TAG_OP_PUSH_FRAME)
                   (REFER_LOCAL_PUSH . ,TAG_OP_REFER_LOCAL_PUSH)
                   (REFER_LOCAL_CALL . ,TAG_OP_REFER_LOCAL_CALL)
                   (REFER_GLOBAL_CALL . ,TAG_OP_REFER_GLOBAL_CALL)
                   (REFER_FREE_CALL . ,TAG_OP_REFER_FREE_CALL)
                   (REFER_FREE . ,TAG_OP_REFER_FREE)
                   (DEFINE_GLOBAL . ,TAG_OP_DEFINE_GLOBAL)
                   (REFER_GLOBAL . ,TAG_OP_REFER_GLOBAL)
                   (LOCAL_JMP . ,TAG_OP_LOCAL_JMP)
                   (CONS . ,TAG_OP_CONS)
                   (TEST . ,TAG_OP_TEST)
                   (HALT . ,TAG_OP_HALT)
                   (LEAVE . ,TAG_OP_LEAVE)
                   (SHIFTJ . ,TAG_OP_SHIFTJ)
                   (DISPLAY . ,TAG_OP_DISPLAY)
                   (PUSH_ENTER . ,TAG_OP_PUSH_ENTER)
                   (CONS . ,TAG_OP_CONS)
                   (CONSTANT_PUSH . ,TAG_OP_CONSTANT_PUSH)
                   (CONSTANT . ,TAG_OP_CONSTANT)                   
                   (ENTER . ,TAG_OP_ENTER)  
                   (REFER_LOCAL_BRANCH_NOT_NULL . ,TAG_OP_REFER_LOCAL_BRANCH_NOT_NULL)                 
                   (NOP . ,TAG_OP_NOP)
                   (PUSH . ,TAG_OP_PUSH)
                   (PAIR_P . ,TAG_OP_PAIR_P)
                   (LET_FRAME . ,TAG_OP_LET_FRAME)
                   (REFER_FREE_PUSH . ,TAG_OP_REFER_FREE_PUSH)
                   (TAIL_CALL . ,TAG_OP_TAIL_CALL)
                   (CAR_PUSH . ,TAG_OP_CAR_PUSH)
                   (CDR_PUSH . ,TAG_OP_CDR_PUSH)
                   (REFER_LOCAL . ,TAG_OP_REFER_LOCAL)
                   (RETURN . ,TAG_OP_RETURN))) => cdr]
    [else (error insn)]))

(define (put-s64 port n)
  (let1 bv (make-bytevector 8)
     (bytevector-s64-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u32 port n)
  (let1 bv (make-bytevector 4)
     (bytevector-u32-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u16 port n)
  (let1 bv (make-bytevector 2)
     (bytevector-u16-native-set! bv 0 n)
     (put-bytevector port bv)))

(define write-sexp
  (case-lambda
    [(port c)
      (match c
        [(? char? c)
          (put-u8 port TAG_CHAR)
          (put-u32 port (char->integer c))]
        [(? fixnum? n)
          (put-u8 port TAG_FIXNUM)
          (put-s64 port n)]
        [(? symbol? s)
          (put-u8 port TAG_SYMBOL)
          (let1 str (symbol->string s)
            (put-u16 port (string-length str))
            (for-each
              (lambda (c)
                (put-u32 port (char->integer c)))
              (string->list str)))]
        [(? string? str)
          (put-u8 port TAG_STRING)
          (put-u16 port (string-length str))
          (for-each
            (lambda (c)
              (put-u32 port (char->integer c)))
            (string->list str))]
        [(first . second)
          (put-u8 port TAG_PAIR)
          (write-sexp port first)
          (write-sexp port second)]
        [#t
          (put-u8 port TAG_TRUE)]
        [#f
          (put-u8 port TAG_FALSE)]
        [()
          (put-u8 port TAG_NIL)]
      )]
    [(c)
      (let-values ([(p get) (open-bytevector-output-port)])
        (write-sexp p c)
        (get))]))

(define (write-op port tag . args)
  (put-u8 port tag)
  (for-each
    (lambda (arg)
      (write-sexp port arg))
    args))

(define (write-op->bv tag . args)
  (let-values ([(port get) (open-bytevector-output-port)])
    (apply write-op port tag args)
    (get)))

(test-equal #vu8(0 3 0 0 0 0 0 0 0) (write-sexp 3))
(test-equal #vu8(1) (write-sexp #t))
(test-equal #vu8(2) (write-sexp #f))
(test-equal #vu8(3) (write-sexp '()))
(test-equal #vu8(4 97 0 0 0) (write-sexp #\a))
(test-equal #vu8(5 5 0 104 0 0 0 101 0 0 0 108 0 0 0 108 0 0 0 111 0 0 0) (write-sexp 'hello))
(test-equal #vu8(6 3 0 97 0 0 0 98 0 0 0 99 0 0 0) (write-sexp "abc"))
(test-equal #vu8(7 5 1 0 97 0 0 0 3) (write-sexp '(a)))

(test-equal #vu8(10 7 5 1 0 97 0 0 0 3) (write-op->bv TAG_OP_CONSTANT '(a)))
(test-equal #vu8(11 0 34 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 10 0 0 0 0 0 0 0) (write-op->bv TAG_OP_CLOSURE 34 2 #f 10))
(test-equal #vu8(12 0 2 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0) (write-refer-local-branch-not-null-op 2 5))
(test-equal #vu8(13 0 1 0 0 0 0 0 0 0) (write-op->bv TAG_OP_REFER_LOCAL 1))

(test-results)

;; Enable debug log.
(define debug? #f)
(define (log str . args)
  (when debug?
    (apply format (current-error-port) str args)))

(define (insn->string insn)
  (string-delete (lambda (c) (equal? c #\_)) (string-titlecase  (symbol->string insn))))

;; Instruction with 1 symbol argument.
(define (sym1-insn? insn)
  (memq insn '(ASSIGN_GLOBAL DEFINE_GLOBAL REFER_GLOBAL REFER_GLOBAL_PUSH)))

;; Constant instruction with 1 argument.
(define (const1-insn? insn)
  (memq insn '(CONSTANT CONSTANT_PUSH PUSH_CONSTANT)))

;; Constant instruction with 2 arguments.
(define (const2-insn? insn)
  (memq insn '(REFER_LOCAL_PUSH_CONSTANT)))

(define rewrite-insn*
  (case-lambda
   [(all-insn* insn*)
    (let-values ([(port get) (open-bytevector-output-port)])
      (rewrite-insn* all-insn* insn* 0 port)
      (get))]
   [(all-insn* insn* idx port)
     ;(log "insn*=~a idx=~a~n" (if (null? insn*) 'done (car insn*)) (if (null? insn*) 'done (list-ref all-insn* idx)))
     (let1 indent "            "
       (match insn*
          [('CLOSURE size arg-len optional? num-free-vars _stack-size _src . more*)
            (write-op port TAG_OP_CLOSURE (adjust-offset all-insn* idx) arg-len optional? num-free-vars)
            (rewrite-insn* all-insn* more* (+ idx 7) port)]
          ;; 0 arg instructions.
          [((? arg0-insn? insn) . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag)
              (rewrite-insn* all-insn* more*  (+ idx 1) port))]
          ;; 1 arg jump instruction.
          [((? jump1-insn? insn) offset . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag (adjust-offset all-insn* idx))
            (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; CONSTANT family with 1 arg.
          [((? const1-insn? insn) v . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag v)
              (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; GLOBAL family with 1 symbol argument.
          [((? sym1-insn? insn) (? symbol? n) . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag n)
              (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; Other 1 arg instructions.
          [((? arg1-insn? insn) n . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag n)
              (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; 2 args jump instructions.
          [((? jump2-insn? insn) m offset . more*)
            (let1 tag (insn->tag insn)          
              (write-op port tag m (adjust-offset all-insn* idx))
              (rewrite-insn* all-insn* more* (+ idx 3) port))]
          ;; CONSTANT family with 2 args.
          [((? const2-insn? insn) m v . more*)
                      (error (format "insn*=~a\n" insn*))
            (let1 var (gen v)
              (format port "~aOp::~a(~a, ~a),\n" indent (insn->string insn) m var)
              (rewrite-insn* all-insn* more* (+ idx 3) port))]
          [((and (or 'REFER_GLOBAL_CALL) insn) (? symbol? s) n . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag s n)
              (rewrite-insn* all-insn* more* (+ idx 3) port))]
          ;; Other 2 args insturctions.
          [((? arg2-insn? insn) m n . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag m n)
              (rewrite-insn* all-insn* more* (+ idx 3) port))]
          [((and (or 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE) insn) m v offset . more*)
                      (error (format "insn*=~a\n" insn*))
            (let1 var (gen v)
              (format port "~aOp::~a(~a, ~a, ~a),\n" indent (insn->string insn) m var (adjust-offset all-insn* idx))
              (rewrite-insn* all-insn* more* (+ idx 4) port))]
          ;; 3 arg jump instructions.
          ;;   Note that jump3-insn? should be evaluate first before arg3-insn.
          ;;   Because arg3-insn? include jump3-insn?
          [((? jump3-insn? insn) l m offset . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag l m (adjust-offset all-insn* idx))          
              (rewrite-insn* all-insn* more* (+ idx 4) port))]
          ;; Other 3 arg instructions.
          [((? arg3-insn? insn) l m n . more*)
            (let1 tag (insn->tag insn)
              (write-op port tag l m n)            
              (rewrite-insn* all-insn* more* (+ idx 4) port))]
          [() #f]
          [else (error "unknown insn" (car insn*) (cadr insn*))]))]))

(define (file->sexp* file)
  (call-with-input-file file
    (lambda (p)
      (let loop ([sexp (read p)]
                 [sexp* '()])
        (cond
         [(eof-object? sexp) (reverse sexp*)]
         [else
          (loop (read p) (cons sexp sexp*))])))))

(define (main args)
  (let-values ([(port get) (open-string-output-port)])
    (let* ([op-file (cadr args)]
           [sexp* (file->sexp* op-file)]
           [insn* (vector->list (car sexp*))]
           [insn-str (rewrite-insn* insn* insn*)]
           [decl-str (and (gen-code port) (get))])
      (format #t "
~a
        let ops = vec![\n~a];\n" decl-str  insn-str))))

(main (command-line))



