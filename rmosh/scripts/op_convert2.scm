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
  
          [('CLOSURE size arg-len optional? num-free-vars _stack-size _src . more*)
            (format port "~aOp::Closure {size: ~a, arg_len: ~a, is_optional_arg: ~a, num_free_vars: ~a},\n"
              indent 0 arg-len (if optional? "true" "false") num-free-vars)
            (rewrite-insn* all-insn* more* (+ idx 7) port)]
          ;; 0 arg instructions.  
          [((? arg0-insn? insn) . more*)
            (format port "~aOp::~a,\n" indent (insn->string insn))
            (rewrite-insn* all-insn* more*  (+ idx 1) port)]            
          ;; 1 arg jump instruction.
          [((? jump1-insn? insn) offset . more*)
            (format port "~aOp::~a(~a),\n" indent (insn->string insn) (adjust-offset all-insn* idx))
            (rewrite-insn* all-insn* more* (+ idx 2) port)] 
          ;; CONSTANT family with 1 arg.
          [((? const1-insn? insn) v . more*)
            (let1 var (gen v)
              (format port "~aOp::~a(~a),\n" indent (insn->string insn) var)
              (rewrite-insn* all-insn* more* (+ idx 2) port))]
          ;; GLOBAL family with 1 symbol argument.
          [((? sym1-insn? insn) (? symbol? n) . more*)
            (format port "~aOp::~a(vm.gc.intern(\"~a\")),\n" indent (insn->string insn) n)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]                
          ;; Other 1 arg instructions.    
          [((? arg1-insn? insn) n . more*)
            (format port "~aOp::~a(~a),\n" indent (insn->string insn) n)
            (rewrite-insn* all-insn* more* (+ idx 2) port)]                  
          ;; 3 arg jump instructions.
          ;;   Note that jump3-insn? should be evaluate first before arg3-insn.
          ;;   Because arg3-insn? include jump3-insn?
          [((? jump3-insn? insn) l m offset . more*)
          (format port "~aOp::~a(~a, ~a, ~a),\n" indent (insn->string insn) l m (adjust-offset all-insn* idx))
          (rewrite-insn* all-insn* more* (+ idx 4) port)]                
          [((? arg3-insn? insn) l m n . more*)
          (format port "~aOp::~a(~a, ~a, ~a),\n" indent (insn->string insn) l m n)
          (rewrite-insn* all-insn* more* (+ idx 4) port)]
          ;; 2 args jump. Please note that we increment offset there because jump source is the end of instruction.
          [((and (or 'REFER_LOCAL_BRANCH_NOT_NULL 'REFER_LOCAL_BRANCH_NOT_LT) insn) m offset . more*)
          (format port "~aOp::~a(~a, ~a),\n" indent (insn->string insn) m (adjust-offset all-insn* idx))
          (rewrite-insn* all-insn* more* (+ idx 3) port)]           
          ;; 2 args
          [((? arg2-insn? insn) m n . more*)
          (format port "~aOp::~a(~a, ~a),\n" indent (insn->string insn) m n)
          (rewrite-insn* all-insn* more* (+ idx 3) port)]

        
          [((and (or 'REFER_GLOBAL_CALL) insn) (? symbol? s) n . more*)
          (format port "~aOp::~a(vm.gc.intern(\"~a\"), ~a),\n" indent (insn->string insn) s n)
          (rewrite-insn* all-insn* more* (+ idx 3) port)]                               
          [((and (or 'REFER_LOCAL_PUSH_CONSTANT) insn) n c . more*)
            (let1 var (gen c)
              (format port "~aOp::~a(~a, ~a),\n" indent (insn->string insn) n var)
              (rewrite-insn* all-insn* more* (+ idx 3) port))]      

                 

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
        let mut vm = Vm::new();
~a        
        let ops = vec![\n~a];\n" decl-str  insn-str))))            

(main (command-line))