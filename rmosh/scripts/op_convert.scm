;; Convert Mosh style op into Rmosh style op.
;; Input: Instructions as Scheme vector.
;;   Example: (CLOSURE 10 0 #f 0 4 (((input string port) 2) a) CONSTANT 3 RETURN 0 DEFINE_GLOBAL a HALT NOP)
;;   Get the instructions by running `gosh vm.scm "compile-file" file-name`.

(import (scheme base) (scheme file) (scheme read) (scheme write) (scheme process-context)
        (match) (only (srfi :13) string-delete) (only (mosh) format) (only (rnrs) string-titlecase))

(define (insn->string insn)
    (string-delete (lambda (c) (equal? c #\_)) (string-titlecase  (symbol->string insn))))

(define (rewrite-insn* insn*)
  (match insn*
    [('CLOSURE size arg-len optional? num-free-vars _stack-size _src . more*)
      (format #t "Op::Closure {size: ~a, arg_len: ~a, is_optional_arg: ~a, num_free_vars: ~a},\n"
        size arg-len (if optional? "true" "false") num-free-vars)
          (rewrite-insn* more*)]
    [((and (or 'CONSTANT 'CALL 'RETURN 'DEFINE_GLOBAL 'FRAME 'REFER_LOCAL) insn) n . more*)
      (format #t "Op::~a(~a)\n" (insn->string insn) n)
      (rewrite-insn* more*)]
    [((and (or 'HALT 'NOP 'PUSH 'NUMBER_ADD) insn) . more*)
      (format #t "Op::~a\n" (insn->string insn))
      (rewrite-insn* more*)]
    [() #f]
    [else (display insn*)]))

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
  (let ([insn* (vector->list (car (file->sexp* (cadr args))))])

    (rewrite-insn* insn*)))

(main (command-line))