;; Convert Mosh style op into Rmosh style op.
;; Input: Instructions as Scheme vector.
;;   Example: (CLOSURE 10 0 #f 0 4 (((input string port) 2) a) CONSTANT 3 RETURN 0 DEFINE_GLOBAL a HALT NOP)
;;   Get the instructions by running `gosh vm.scm "compile-file" file-name`.

(import (scheme base) (scheme file) (scheme read) (scheme write) (scheme process-context) (scheme case-lambda
)
        (match) (mosh control) (only (srfi :13) string-delete) (only (mosh) format regexp-replace-all rxmatch) (only (rnrs) string-titlecase))

(define (insn->string insn)
    (string-delete (lambda (c) (equal? c #\_)) (string-titlecase  (symbol->string insn))))

(define (adjust-offset insn* start offset)
  (let loop ([insn* insn*]
             [cur-offset 0]
             [rust-offset 0])
    (format (current-error-port) "cur=~a rust=~a off=~a insn*=~a\n" cur-offset rust-offset offset insn*)
    (cond        
      [(= cur-offset (+ offset 1)) rust-offset]
      [else
        (match insn*
          [((or 'CALL 'CONSTANT 'DEFINE_GLOBAL 'FRAME 'LOCAL_JMP 'REFER_LOCAL 'RETURN 'TEST) _ . more*)
            (loop more* (+ cur-offset 2) (+ rust-offset 1))]
          [((or 'HALT 'NOP 'NUMBER_ADD 'PUSH) . more*)
            (loop more* (+ cur-offset 1) (+ rust-offset 1))]
          [('CLOSURE _a _b _c _d _e _f . more*) 
            (loop more* (+ cur-offset 7) (+ rust-offset 1))]
          [() (error "never reach here1")]
          [else (error "never reach here2" insn*)])])))

(define rewrite-insn*
  (case-lambda
    [(insn*) (rewrite-insn* insn* 0)]
    [(insn* idx)
      (match insn*
        [('CLOSURE size arg-len optional? num-free-vars _stack-size _src . more*)
          (format #t "            Op::Closure {size: ~a, arg_len: ~a, is_optional_arg: ~a, num_free_vars: ~a},\n"
             (adjust-offset insn* idx size) arg-len (if optional? "true" "false") num-free-vars)
           (rewrite-insn* more* (+ idx 7))]
        [((and (or 'FRAME 'TEST 'LOCAL_JMP) insn) offset . more*)
          (format #t "            Op::~a(~a),\n" (insn->string insn) (adjust-offset insn* idx offset))
          (rewrite-insn* more* (+ idx 2))] 
        [((and (or 'CONSTANT) insn) #f . more*)
          (format #t "            Op::~a(Object::False),\n" (insn->string insn))
          (rewrite-insn* more* (+ idx 2))]                    
        [((and (or 'CONSTANT) insn) n . more*)
          (format #t "            Op::~a(Object::Number(~a)),\n" (insn->string insn) n)
          (rewrite-insn* more* (+ idx 2))]      
        [((and (or 'CALL 'RETURN 'DEFINE_GLOBAL 'FRAME 'REFER_LOCAL) insn) n . more*)
          (format #t "            Op::~a(~a),\n" (insn->string insn) n)
          (rewrite-insn* more* (+ idx 2))]
        [((and (or 'HALT 'NOP 'PUSH 'NUMBER_ADD) insn) . more*)
          (format #t "            Op::~a,\n" (insn->string insn))
          (rewrite-insn* more*  (+ idx 1))]
        [() #f]
        [else (display insn*)])]))

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
  (let* ([op-file (cadr args)]
         [scm-file (regexp-replace-all #/\.op$/ op-file ".scm")]
         [test-name ((rxmatch #/([^\/]+)\.op$/ op-file) 1)]
         [expr* (file->sexp* scm-file)]
         [sexp* (file->sexp* op-file)])
  (format #t "
    #[test]
    fn test_~a() {
        let ops = vec![\n" test-name)         
    (match expr*
      [(expr expected size)
        (let1 insn* (vector->list (car sexp*))
          (rewrite-insn* insn*)
          (format #t "        ];
        test_ops_with_size(ops, Object::Number(~a), ~a);
    }\n" expected size))]
      [else (write sexp*)])))

(main (command-line))