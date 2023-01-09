(import (scheme base))
(import (scheme file))
(import (scheme read))
(import (scheme write))
(import (scheme process-context))
(import (scheme case-lambda))
(import (match))
(import (mosh control))
(import (rust_sexp))
(import (only (srfi :1) list-ref first second))
(import (only (srfi :13) string-delete string-join))
(import (only (mosh) format regexp-replace-all rxmatch))
(import (only (rnrs) open-string-output-port string-titlecase))

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

  ;; Instruction with no argument.
  (define (arg0-insn? insn)
    (memq insn '(APPEND2 CAAR CADR CAR CAR_PUSH CDAR CDDR
                 CDR CDR_PUSH CONS EQ EQUAL EQV HALT
                 INDIRECT MAKE_VECTOR NOP NOT NULL_P
                 NUMBER_ADD NUMBER_ADD_PUSH
                 NUMBER_DIV NUMBER_EQUAL NUMBER_GE NUMBER_GT
                 NUMBER_LE NUMBER_LT NUMBER_MUL NUMBER_SUB NUMBER_SUB_PUSH
                 PAIR_P
                 PUSH READ_CHAR SET_CAR SET_CDR
                 SIMPLE_STRUCT_REF SYMBOL_P UNDEF
                 VECTOR_LENGTH VECTOR_P VECTOR_REF VECTOR_SET)))

  ;; Instruction with 1 argument.
  (define (arg1-insn? insn)
    (or (jump1-insn? insn)
        (memq insn '(ASSIGN_FREE ASSIGN_GLOBAL ASSIGN_LOCAL
                     BOX CALL CONSTANT CONSTANT_PUSH
                     DISPLAY ENTER LEAVE LET_FRAME LIST
                     LOCAL_CALL MAKE_CONTINUATION PUSH_CONSTANT
                     PUSH_ENTER REFER_FREE REFER_FREE_PUSH
                     REFER_GLOBAL REFER_GLOBAL_PUSH
                     REFER_LOCAL REFER_LOCAL_PUSH RETURN VALUES VECTOR))))

  ;; Jump instuction with 1 argument.
  (define (jump1-insn? insn)
    (memq insn '(BRANCH_NOT_EQ BRANCH_NOT_EQUAL BRANCH_NOT_EQV BRANCH_NOT_GE
                 BRANCH_NOT_GT BRANCH_NOT_LE BRANCH_NOT_LT BRANCH_NOT_NULL
                 BRANCH_NOT_NUMBER_EQUAL FRAME LOCAL_JMP NOT_TEST
                 PUSH_FRAME TEST)))

  ;; Instruction with 2 arguments.
  (define (arg2-insn? insn)
    (or (jump2-insn? insn)
        (memq insn '(LOCAL_TAIL_CALL RECEIVE REFER_FREE_CALL REFER_GLOBAL_CALL
                     REFER_LOCAL_CALL REFER_LOCAL_PUSH_CONSTANT TAIL_CALL)))) 

  ;; Jump instuction with 2 arguments.
  (define (jump2-insn? insn)
    (memq insn '(REFER_LOCAL_BRANCH_NOT_LT REFER_LOCAL_BRANCH_NOT_NULL)))

  ;; Instruction with 3 arguments.
  (define (arg3-insn? insn)
    (or (jump3-insn? insn)
        (memq insn '(SHIFTJ))))

  ;; Jump instuction with 3 arguments.
  (define (jump3-insn? insn)
    (memq insn '(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE
                REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE)))      

(define rewrite-insn*
  (case-lambda
   [(all-insn* insn*)
    (let-values ([(port get) (open-string-output-port)])
      (rewrite-insn* all-insn* insn* 0 port)
      (get))]
   [(all-insn* insn* idx port)
     (log "insn*=~a idx=~a~n" (if (null? insn*) 'done (car insn*)) (if (null? insn*) 'done (list-ref all-insn* idx)))
     (let1 indent "            "
       (match insn*

          [('CLOSURE size arg-len optional? num-free-vars stack-size src . more*)
            (format port "~aObject::Instruction(Op::Closure),\n" indent)
            (format port "~a~a,\n" indent (gen size))
            (format port "~a~a,\n" indent (gen arg-len))
            (format port "~a~a,\n" indent (gen optional?))
            (format port "~a~a,\n" indent (gen num-free-vars))
            (format port "~a~a,\n" indent (gen stack-size))
            (format port "~a~a,\n" indent (gen src))
            (rewrite-insn* all-insn* more* (+ idx 7) port)]
          ;; 0 arg instructions.
          [((? arg0-insn? insn) . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (rewrite-insn* all-insn* more*  (+ idx 1) port)]
          ;; 1 arg jump instruction.
          [((? jump1-insn? insn) offset . more*)
            (let1 var (gen offset)
              (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
              (format port "~a~a,\n" indent var)
              (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; CONSTANT family with 1 arg.
          [((? const1-insn? insn) v . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen v))
            (rewrite-insn* all-insn* more* (+ idx 2) port)]
          ;; GLOBAL family with 1 symbol argument.
          [((? sym1-insn? insn) (? symbol? s) . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~avm.gc.symbol_intern(\"~a\"),\n" indent s)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]
          ;; Other 1 arg instructions.
          [((? arg1-insn? insn) n . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen n))
            (rewrite-insn* all-insn* more* (+ idx 2) port)]
          ;; 2 args jump instructions.
          [((? jump2-insn? insn) m offset . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen offset))
            (rewrite-insn* all-insn* more* (+ idx 3) port)]
          ;; CONSTANT family with 2 args.
          [((? const2-insn? insn) m v . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen v))
            (rewrite-insn* all-insn* more* (+ idx 3) port)]
          [((and (or 'REFER_GLOBAL_CALL) insn) (? symbol? s) n . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~avm.gc.symbol_intern(\"~a\"),\n" indent s)
            (format port "~a~a,\n" indent (gen n))
            (rewrite-insn* all-insn* more* (+ idx 3) port)]
          ;; Other 2 args insturctions.
          [((? arg2-insn? insn) m n . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen n))
            (rewrite-insn* all-insn* more* (+ idx 3) port)]
          [((and (or 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE) insn) m v offset . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen v))
            (format port "~a~a,\n" indent (gen offset))            
            (rewrite-insn* all-insn* more* (+ idx 4) port)]
          ;; 3 arg jump instructions.
          ;;   Note that jump3-insn? should be evaluate first before arg3-insn.
          ;;   Because arg3-insn? include jump3-insn?
          [((? jump3-insn? insn) l m offset . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen l))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen offset))            
            (rewrite-insn* all-insn* more* (+ idx 4) port)]
          ;; Other 3 arg instructions.
          [((? arg3-insn? insn) l m n . more*)
            (format port "~aObject::Instruction(Op::~a),\n" indent (insn->string insn))
            (format port "~a~a,\n" indent (gen l))
            (format port "~a~a,\n" indent (gen m))
            (format port "~a~a,\n" indent (gen n))    
            (rewrite-insn* all-insn* more* (+ idx 4) port)]
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

(define (test-header name scheme-expr)
  (format
"
    // ~s
    #[test]
    fn ~a() {
        let mut vm = Vm::new();
" scheme-expr name))

(define (main args)
  (let-values ([(port get) (open-string-output-port)])
    (let* ([op-file (cadr args)]
           [scm-file (regexp-replace-all #/\.op$/ op-file ".scm")]
           [test-name ((rxmatch #/([^\/]+)\.op$/ op-file) 1)]
           [raw-insn* (first (file->sexp* op-file))]
           [scheme-expr (first (file->sexp* scm-file))]
           [expected (second (file->sexp* scm-file))]
           [expected (gen expected)]
           [insn* (vector->list raw-insn*)]
           [insn-str (rewrite-insn* insn* insn*)]
           [decl-str (and (gen-code port "vm") (get))])
      (format #t "
~a
~a
        let ops = vec![\n~a
        ];
        let expected = ~a;
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * ~a + SIZE_OF_STRING * ~a);
    }\n
        " (test-header test-name scheme-expr) decl-str insn-str expected (num-sym*) (num-str*)))))

(main (command-line))