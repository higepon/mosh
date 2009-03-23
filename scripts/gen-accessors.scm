(import (rnrs)
        (srfi :1)
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
  (format #t "// Do not edit, this file is generated from accessors.scm\n")
  (for-each
   (lambda (type)
     (format #t "~a* to~a() const\n" type type)
     (format #t "{\n")
     (format #t "    MOSH_ASSERT(is~a());\n" type)
     (format #t "    return reinterpret_cast<~a*>(reinterpret_cast<HeapObject*>(val)->obj);\n" type)
     (format #t "}\n\n")
     (format #t "bool is~a() const\n" type)
     (format #t "{\n")
     (format #t "    return isHeapObject()\n")
     (format #t "    && reinterpret_cast<HeapObject*>(val)->type == HeapObject::~a;\n" type)
     (format #t "}\n\n"))
  (map second (file->sexp-list "./accessors.scm"))))

(main (command-line))
