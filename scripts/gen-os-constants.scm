(import (rnrs)
        (mosh)
        (match))

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
     (match constant
       [('define-c-defined-const val)
        (format #t "#ifdef ~a\n" val)
        (format #t "osConstants->set(Symbol::intern(UC(\"~a\")), Bignum::makeInteger((long int)~a));\n" val val)
        (format #t "#endif\n")]
       [('define-sizeof val)
        (format #t "osConstants->set(Symbol::intern(UC(\"sizeof:~a\")), Bignum::makeInteger(sizeof(~a)));\n" val val)]
       [('define-alignof val)
        (format #t "{\n    struct x { char y; ~a z; };\n    osConstants->set(Symbol::intern(UC(\"alignof:~a\")), Object::makeFixnum(offsetof(x, z)));\n}\n" val val)]
       [else #f]))
  (file->sexp-list "./os-constants.scm")))

(main (command-line))



