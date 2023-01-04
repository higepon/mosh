;; Parse src/instruction.scm and generate instruction number.
(import (rnrs))
(import (only (srfi :1) second))
(import (match))
(import (only (mosh control) let1))
(import (only (mosh) format))
(import (only (srfi :13) string-delete string-join))
(import (mosh file))

(define (insn->string insn)
  (string-delete (lambda (c) (equal? c #\_)) (string-titlecase  (symbol->string insn))))

(define (main args)
  (let1 sexp* (file->sexp-list (second args))
    (let loop ([i 0]
               [sexp* sexp*])
      (match sexp*
        [() '()]
        [(('define-insn insn _) . more*)
          (format #t "(define TAG_OP_~a ~a)\n" insn i)
          (loop (+ i 1) more*)])))
  (let1 sexp* (file->sexp-list (second args))
    (let loop ([i 0]
               [sexp* sexp*])
      (match sexp*
        [() '()]
        [(('define-insn insn _) . more*)
          (format #t "~a = ~a,\n" (insn->string insn) i)
          (loop (+ i 1) more*)]))))

(main (command-line))