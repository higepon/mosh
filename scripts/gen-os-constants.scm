(import (rnrs)
        (mosh))

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (port)
      (let loop ([ret '()]
                 [sexp (read port)])
        (if (eof-object? sexp)
            (reverse ret)
            (loop (cons sexp ret) (read port)))))))

(define (main args)
  (format #t "// Do not edit, this file is generated from os-constants.scm\n")
  (for-each
   (lambda (constant)
    (format #t "#ifdef ~a\n" constant)
    (format #t "osConstants->set(Symbol::intern(UC(\"~a\")), Bignum::makeInteger((long int)~a));\n" constant constant)
    (format #t "#endif\n"))
  (map cadr (file->sexp-list "./os-constants.scm"))))

(main (command-line))



