;; Convert Mosh style op into Rmosh style op.
;; Input: Instructions as Scheme vector.
;;   Example: (CLOSURE 10 0 #f 0 4 (((input string port) 2) a) CONSTANT 3 RETURN 0 DEFINE_GLOBAL a HALT NOP)
;;   Get the instructions by running `gosh vm.scm "compile-file" file-name`.

(import (scheme base))
(import (scheme file))
(import (scheme read))
(import (scheme write))
(import (scheme process-context))
(import (scheme case-lambda))
(import (match))
(import (mosh control))
(import (rust_sexp))
(import (except (rust_jump) run-tests))
(import (only (srfi :1) list-ref))
(import (only (srfi :13) string-delete string-join))
(import (only (mosh) format regexp-replace-all rxmatch))
(import (only (rnrs) open-string-output-port string-titlecase))

(define (insn->string insn)
  (string-delete (lambda (c) (equal? c #\_)) (string-titlecase  (symbol->string insn))))
            

(define rewrite-insn*
  (case-lambda
   [(all-insn* insn*)
    (let-values ([(port get) (open-string-output-port)])
      (rewrite-insn* all-insn* insn* 0 port)
      (get))]
   [(all-insn* insn* idx port)
           (format #t "insn*=~a idx=~a" (if (null? insn*) 'done (car insn*)) (if (null? insn*) 'done (list-ref all-insn* idx)))
           (newline)   
    (match insn*
           [('CLOSURE size arg-len optional? num-free-vars _stack-size _src . more*)
            (format port "            Op::Closure {size: ~a, arg_len: ~a, is_optional_arg: ~a, num_free_vars: ~a},\n"
                    0 arg-len (if optional? "true" "false") num-free-vars)
            (rewrite-insn* all-insn* more* (+ idx 7) port)]
           ;; Jump instuction with 1 argument.
           [((? jump1-insn? insn) offset . more*)
            (format port "            Op::~a(~a),\n" (insn->string insn) (adjust-offset all-insn* idx))
            (rewrite-insn* all-insn* more* (+ idx 2) port)] 
           [((and (or 'CONSTANT 'CONSTANT_PUSH 'PUSH_CONSTANT) insn) v . more*)
             (let1 var (gen v)
               (format port "            Op::~a(~a),\n" (insn->string insn) var)
               (rewrite-insn* all-insn* more* (+ idx 2) port))]
           ;; 3 args
           [((and (or 'SHIFTJ) insn) l m n . more*)
            (format port "            Op::~a(~a, ~a, ~a),\n" (insn->string insn) l m n)
            (rewrite-insn* all-insn* more* (+ idx 4) port)]
           [((and (or 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE) insn) l m offset . more*)
            (format port "            Op::~a(~a, ~a, ~a),\n" (insn->string insn) l m (adjust-offset all-insn* offset))
            (rewrite-insn* all-insn* more* (+ idx 4) port)]            
           ;; 2 args
           [((? arg2-insn? insn) m n . more*)
            (format port "            Op::~a(~a, ~a),\n" (insn->string insn) m n)
            (rewrite-insn* all-insn* more* (+ idx 3) port)]
            ;; 2 args jump. Please note that we increment offset there because jump source is the end of instruction.
           [((and (or 'REFER_LOCAL_BRANCH_NOT_NULL 'REFER_LOCAL_BRANCH_NOT_LT) insn) m offset . more*)
            (format port "            Op::~a(~a, ~a),\n" (insn->string insn) m (adjust-offset all-insn* idx))
            (rewrite-insn* all-insn* more* (+ idx 3) port)] 
           [((and (or 'REFER_GLOBAL_PUSH) insn) (? symbol? s) . more*)
            (format port "            Op::~a(vm.gc.intern(\"~a\"), ~a),\n" (insn->string insn) s)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]             
           [((and (or 'REFER_GLOBAL_CALL) insn) (? symbol? s) n . more*)
            (format port "            Op::~a(vm.gc.intern(\"~a\"), ~a),\n" (insn->string insn) s n)
            (rewrite-insn* all-insn* more* (+ idx 3) port)]                               
           [((and (or 'REFER_LOCAL_PUSH_CONSTANT) insn) n c . more*)
             (let1 var (gen c)
               (format port "            Op::~a(~a, ~a),\n" (insn->string insn) n var)
               (rewrite-insn* all-insn* more* (+ idx 3) port))]            
           [((and (or 'PUSH_ENTER 'ENTER 'BOX 'MAKE_CONTINUATION 'VALUES) insn) n . more*)
            (format port "            Op::~a(~a),\n" (insn->string insn) n)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]                
           [((and (or 'ASSIGN_GLOBAL 'DEFINE_GLOBAL 'REFER_GLOBAL) insn) (? symbol? n) . more*)
            (format port "            Op::~a(vm.gc.intern(\"~a\")),\n" (insn->string insn) n)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]
            ;; 1 arg          
           [((and (or 'LOCAL_CALL 'VECTOR 'CALL 'DISPLAY 'LEAVE 'LET_FRAME 'RETURN 'ASSIGN_FREE 'REFER_FREE 'REFER_FREE_PUSH 'REFER_LOCAL_PUSH 'REFER_LOCAL 'ASSIGN_LOCAL 'FRAME 'REFER_LOCAL) insn) n . more*)
            (format port "            Op::~a(~a),\n" (insn->string insn) n)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]
           [((and (or 'APPEND2 'MAKE_VECTOR 'VECTOR_LENGTH 'SIMPLE_STRUCT_REF 'NUMBER_ADD_PUSH 'VECTOR_REF 'CDAR 'CAAR 'CDR_PUSH 'VECTOR_SET 'VECTOR_REF 'READ_CHAR 'HALT 'SET_CAR 'SET_CDR 'CDDR 'EQ 'PAIR_P 'SYMBOL_P 'VECTOR_P 'CAR_PUSH 'NOT 'CAR 'CDR 'CADR 'CONS 'EQUAL 'EQV 'NUMBER_EQUAL 'NUMBER_DIV 'NUMBER_SUB 'NUMBER_SUB_PUSH 'NUMBER_GE 'NUMBER_GT 'NUMBER_LE 'NUMBER_LT 'NOP 'NULL_P 'INDIRECT 'PUSH 'NUMBER_ADD 'NUMBER_MUL 'UNDEF) insn) . more*)
            (format port "            Op::~a,\n" (insn->string insn))
            (rewrite-insn* all-insn* more*  (+ idx 1) port)]
           [() #f]
           [else (error "unknown insn" (car insn*) (cadr insn*))])]))

(define (file->sexp* file)
  (call-with-input-file file
    (lambda (p)
      (let loop ([sexp (read p)]
                 [sexp* '()])
        (cond
         [(eof-object? sexp) (reverse sexp*)]
         [else
          (loop (read p) (cons sexp sexp*))])))))

(define (expected->rust expected)
  (match expected
         [(? char? c)
          (format "Object::Char('~a')" c)]    
         [(? symbol? s)
          (format "vm.gc.symbol_intern(\"~a\")" s)]
         [(? string? s)
          (format "vm.gc.new_string(\"~a\")" s)]
         ['undef "Object::Undef"]
         [#t "Object::True"]
         [#f "Object::False"]
         [() "Object::Nil"]
         [(a . b)
          (format "Object::Pair(vm.gc.alloc(Pair::new(~a, ~a))" (expected->rust a) (expected->rust b))]
         [(? number? n) (format "Object::Number(~a)" n)]
         [else (error "expected->rust" expected)]))

(define (main args)
  (let* ([op-file (cadr args)]
         [sexp* (file->sexp* op-file)]
         [insn* (vector->list (car sexp*))])
    (let1 insn-str (rewrite-insn* insn* insn*)

          (let-values ([(port get) (open-string-output-port)])    
        (gen-code port)

                      (format port "

        let mut vm = Vm::new();
~a        
        let ops = vec![\n~a"   insn-str)        
            (display (get))))))

(main (command-line))