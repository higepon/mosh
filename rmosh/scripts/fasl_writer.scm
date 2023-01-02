;; Write Rust VM Ops as binary.
(import (except (rust_jump) run-tests))
(import (match))
(import (mosh control))
(import (mosh test))
(import (only (mosh) format regexp-replace-all rxmatch))
(import (only (rnrs) set! bytevector-s64-native-set! bytevector-u32-native-set! bytevector-u16-native-set! open-bytevector-output-port put-u8 put-bytevector))
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
(define TAG_OP_UNDEF 42)
(define TAG_OP_CAR 43)
(define TAG_OP_CDR 44)
(define TAG_OP_RECEIVE 45)
(define TAG_OP_NUMBER_EQUAL 46)
(define TAG_OP_REFER_LOCAL_PUSH_CONSTANT 47)
(define TAG_OP_NUMBER_GT 48)
(define TAG_OP_NUMBER_LT 49)
(define TAG_OP_BOX 50)
(define TAG_OP_CDAR 51)
(define TAG_OP_BRANCH_NOT_NULL 52)
(define TAG_OP_INDIRECT 53)
(define TAG_OP_LOCAL_TAIL_CALL 54)
(define TAG_OP_LOCAL_CALL 55)
(define TAG_OP_ASSIGN_LOCAL 56)
(define TAG_OP_CAAR 57)
(define TAG_OP_BRANCH_NOT_NUMBER_EQUAL 58)
(define TAG_OP_APPEND2 59)
(define TAG_OP_REFER_GLOBAL_PUSH 60)
(define TAG_OP_BRANCH_NOT_LT 61)
(define TAG_OP_NUMBER_ADD_PUSH 62)
(define TAG_OP_EQUAL 63)
(define TAG_OP_EQV 64)
(define TAG_OP_EQ 65)
(define TAG_OP_VALUES 66)
(define TAG_OP_BRANCH_NOT_GE 67)
(define TAG_OP_BRANCH_NOT_EQUAL 68)
(define TAG_OP_PUSH_CONSTANT 69)
(define TAG_OP_NUMBER_SUB_PUSH 70)
(define TAG_OP_REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE 71)
(define TAG_OP_BRANCH_NOT_GT 72)
(define TAG_OP_BRANCH_NOT_EQ 73)
(define TAG_OP_NUMBER_ADD 74)
(define TAG_OP_SET_CAR 75)
(define TAG_OP_SET_CDR 76)
(define TAG_OP_CDDR 77)
(define TAG_OP_NUMBER_MUL 78)
(define TAG_OP_NUMBER_DIV 79)
(define TAG_OP_MAKE_CONTINUATION 80)
(define TAG_OP_CALL 81)
(define TAG_OP_ASSIGN_GLOBAL 82)
(define TAG_OP_VECTOR_LENGTH 83)
(define TAG_OP_BRANCH_NOT_EQV 84)
(define TAG_OP_VECTOR_REF 85)
(define TAG_OP_CADR 86)
(define TAG_OP_MAKE_VECTOR 87)
(define TAG_OP_BRANCH_NOT_LE 88)
(define TAG_OP_VECTOR_SET 89)
(define TAG_OP_ASSIGN_FREE 90)
(define TAG_OP_NOT_TEST 91)
(define TAG_OP_READ_CHAR 92)
(define TAG_OP_NOT 93)
(define TAG_OP_NUMBER_SUB 94)
(define TAG_OP_NUMBER_GE 95)

(define (insn->tag insn)
  (cond
    [(assoc insn `(
                   (APPEND2 . ,TAG_OP_APPEND2)
                   (ASSIGN_FREE . ,TAG_OP_ASSIGN_FREE)
                   (ASSIGN_GLOBAL . ,TAG_OP_ASSIGN_GLOBAL)
                   (ASSIGN_LOCAL . ,TAG_OP_ASSIGN_LOCAL)
                   (BRANCH_NOT_EQUAL . ,TAG_OP_BRANCH_NOT_EQUAL)
                   (BRANCH_NOT_GE . ,TAG_OP_BRANCH_NOT_GE)
                   (BRANCH_NOT_GT . ,TAG_OP_BRANCH_NOT_GT)
                   (BRANCH_NOT_EQ . ,TAG_OP_BRANCH_NOT_EQ)
                   (BRANCH_NOT_EQV . ,TAG_OP_BRANCH_NOT_EQV)
                   (BRANCH_NOT_LE . ,TAG_OP_BRANCH_NOT_LE)
                   (BRANCH_NOT_LT . ,TAG_OP_BRANCH_NOT_LT)
                   (BRANCH_NOT_NULL . ,TAG_OP_BRANCH_NOT_NULL)
                   (BRANCH_NOT_NUMBER_EQUAL . ,TAG_OP_BRANCH_NOT_NUMBER_EQUAL)
                   (BOX . ,TAG_OP_BOX)
                   (CAAR . ,TAG_OP_CAAR)
                   (CADR . ,TAG_OP_CADR)
                   (CALL . ,TAG_OP_CALL)
                   (CAR . ,TAG_OP_CAR)
                   (CAR_PUSH . ,TAG_OP_CAR_PUSH)
                   (CDAR . ,TAG_OP_CDAR)
                   (CDDR . ,TAG_OP_CDDR)
                   (CDR . ,TAG_OP_CDR)
                   (CDR_PUSH . ,TAG_OP_CDR_PUSH)
                   (CONS . ,TAG_OP_CONS)
                   (CONS . ,TAG_OP_CONS)
                   (CONSTANT . ,TAG_OP_CONSTANT)
                   (CONSTANT_PUSH . ,TAG_OP_CONSTANT_PUSH)
                   (DEFINE_GLOBAL . ,TAG_OP_DEFINE_GLOBAL)
                   (DISPLAY . ,TAG_OP_DISPLAY)
                   (ENTER . ,TAG_OP_ENTER)
                   (EQ . ,TAG_OP_EQ)
                   (EQUAL . ,TAG_OP_EQUAL)
                   (EQV . ,TAG_OP_EQV)
                   (HALT . ,TAG_OP_HALT)
                   (INDIRECT . ,TAG_OP_INDIRECT)                   
                   (LEAVE . ,TAG_OP_LEAVE)
                   (LET_FRAME . ,TAG_OP_LET_FRAME)
                   (LOCAL_CALL . ,TAG_OP_LOCAL_CALL)
                   (LOCAL_JMP . ,TAG_OP_LOCAL_JMP)
                   (LOCAL_TAIL_CALL . ,TAG_OP_LOCAL_TAIL_CALL)
                   (MAKE_CONTINUATION . ,TAG_OP_MAKE_CONTINUATION)
                   (MAKE_VECTOR . ,TAG_OP_MAKE_VECTOR)
                   (NOP . ,TAG_OP_NOP)
                   (NOT . ,TAG_OP_NOT)
                   (NOT_TEST . ,TAG_OP_NOT_TEST)
                   (NUMBER_ADD . ,TAG_OP_NUMBER_ADD)
                   (NUMBER_ADD_PUSH . ,TAG_OP_NUMBER_ADD_PUSH)
                   (NUMBER_DIV . ,TAG_OP_NUMBER_DIV)
                   (NUMBER_EQUAL . ,TAG_OP_NUMBER_EQUAL)
                   (NUMBER_GE . ,TAG_OP_NUMBER_GE)
                   (NUMBER_GT . ,TAG_OP_NUMBER_GT)
                   (NUMBER_LT . ,TAG_OP_NUMBER_LT)
                   (NUMBER_MUL . ,TAG_OP_NUMBER_MUL)
                   (NUMBER_SUB . ,TAG_OP_NUMBER_SUB)
                   (NUMBER_SUB_PUSH . ,TAG_OP_NUMBER_SUB_PUSH)
                   (PAIR_P . ,TAG_OP_PAIR_P)
                   (PUSH . ,TAG_OP_PUSH)
                   (PUSH_CONSTANT . ,TAG_OP_PUSH_CONSTANT)
                   (PUSH_ENTER . ,TAG_OP_PUSH_ENTER)
                   (PUSH_FRAME . ,TAG_OP_PUSH_FRAME)
                   (READ_CHAR . ,TAG_OP_READ_CHAR)
                   (RECEIVE . ,TAG_OP_RECEIVE)
                   (REFER_FREE . ,TAG_OP_REFER_FREE)
                   (REFER_FREE_CALL . ,TAG_OP_REFER_FREE_CALL)
                   (REFER_FREE_PUSH . ,TAG_OP_REFER_FREE_PUSH)
                   (REFER_GLOBAL . ,TAG_OP_REFER_GLOBAL)
                   (REFER_GLOBAL_CALL . ,TAG_OP_REFER_GLOBAL_CALL)
                   (REFER_GLOBAL_PUSH . ,TAG_OP_REFER_GLOBAL_PUSH)
                   (REFER_LOCAL . ,TAG_OP_REFER_LOCAL)
                   (REFER_LOCAL_BRANCH_NOT_NULL . ,TAG_OP_REFER_LOCAL_BRANCH_NOT_NULL)
                   (REFER_LOCAL_CALL . ,TAG_OP_REFER_LOCAL_CALL)
                   (REFER_LOCAL_PUSH . ,TAG_OP_REFER_LOCAL_PUSH)
                   (REFER_LOCAL_PUSH_CONSTANT . ,TAG_OP_REFER_LOCAL_PUSH_CONSTANT)
                   (REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE . ,TAG_OP_REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE)
                   (RETURN . ,TAG_OP_RETURN)
                   (SET_CAR . ,TAG_OP_SET_CAR)
                   (SET_CDR . ,TAG_OP_SET_CDR)
                   (SHIFTJ . ,TAG_OP_SHIFTJ)
                   (TAIL_CALL . ,TAG_OP_TAIL_CALL)
                   (TEST . ,TAG_OP_TEST)
                   (UNDEF . ,TAG_OP_UNDEF)
                   (VALUES . ,TAG_OP_VALUES)
                   (VECTOR_LENGTH . ,TAG_OP_VECTOR_LENGTH)
                   (VECTOR_REF . ,TAG_OP_VECTOR_REF)
                   (VECTOR_SET . ,TAG_OP_VECTOR_SET)
                   (FRAME . ,TAG_OP_FRAME)
                   )) => cdr]
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
            (let1 tag (insn->tag insn)
              (write-op port tag m v)
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
            (let1 tag (insn->tag insn)
              (write-op port tag m v (adjust-offset all-insn* idx))          
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


(define i 10)
(for-each
  (lambda (x)
     (let1 insn (insn->string x)
       (format #t "~a = ~a, \n" insn i)
       (set! i (+ i 1))))
'(CONSTANT CLOSURE REFER_LOCAL_BRANCH_NOT_NULL REFER_LOCAL RETURN FRAME CAR_PUSH REFER_LOCAL_CALL PUSH_FRAME REFER_LOCAL_PUSH CDR_PUSH REFER_GLOBAL_CALL CONS DEFINE_GLOBAL NOP REFER_FREE_PUSH REFER_GLOBAL TAIL_CALL LET_FRAME DISPLAY REFER_FREE_CALL PUSH_ENTER TEST LOCAL_JMP CONSTANT_PUSH PUSH LEAVE PAIR_P ENTER REFER_FREE SHIFTJ HALT UNDEF CAR CDR RECEIVE NUMBER_EQUAL REFER_LOCAL_PUSH_CONSTANT NUMBER_GT NUMBER_LT BOX CDAR BRANCH_NOT_NULL INDIRECT LOCAL_TAIL_CALL LOCAL_CALL ASSIGN_LOCAL CAAR BRANCH_NOT_NUMBER_EQUAL APPEND2 REFER_GLOBAL_PUSH BRANCH_NOT_LT NUMBER_ADD_PUSH EQUAL EQV EQ VALUES BRANCH_NOT_GE BRANCH_NOT_EQUAL PUSH_CONSTANT NUMBER_SUB_PUSH REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE BRANCH_NOT_GT BRANCH_NOT_EQ NUMBER_ADD SET_CAR SET_CDR CDDR NUMBER_MUL NUMBER_DIV MAKE_CONTINUATION CALL ASSIGN_GLOBAL VECTOR_LENGTH BRANCH_NOT_EQV VECTOR_REF CADR MAKE_VECTOR BRANCH_NOT_LE VECTOR_SET ASSIGN_FREE NOT_TEST READ_CHAR NOT NUMBER_SUB NUMBER_GE))