(import (rnrs) (nmosh mosh-vm) (nmosh expander) (mosh pp))

(define baselib-src-sym
  '(core/primitives core/with-syntax core/syntax-rules
     core/let core/derived core/identifier-syntax core/quasisyntax core/quasiquote
     core/let-values
     rnrs/base rnrs/syntax-case rnrs/control rnrs/lists
     rnrs/records/procedural rnrs/records/inspection rnrs/records/syntactic
     rnrs/conditions rnrs/io/ports rnrs/io/simple rnrs/unicode
     rnrs/sorting rnrs/hashtables rnrs/arithmetic/fixnums
     rnrs/arithmetic/flonums rnrs/arithmetic/bitwise rnrs/programs
     rnrs/files rnrs/exceptions rnrs/enums rnrs/bytevectors 
     rnrs
     rnrs/mutable-pairs
     rnrs/mutable-strings rnrs/eval rnrs/r5rs rnrs/load rnrs/eval/reflection
     r5rs ; we need this for rnrs/r5rs
     ))

(define baselib-src 
  (apply append
	 (map (lambda (e) 
		(read-all (string-append "lib.rnrs/" (symbol->string e) ".ss")))
	      baselib-src-sym)))

(define syslib-src-sym
  '(mosh.generated system.generated nmosh/conditions 
		   nmosh/condition-printer.nmosh
     nmosh/runlib.nmosh nrepl/simple nmosh/startup.nmosh))

(define syslib-src 
  (apply append
	 (map (lambda (e) 
		(read-all (string-append "lib.boot/" (symbol->string e) ".ss")))
	      syslib-src-sym)))

(define runtimes 
  '("compat-mosh-run.scm"
    "mosh-utils5.scm"
    "runtime.scm"
    "runtime-cache.scm"))

(define runtime (apply append (map read-all runtimes)))

(define (output p)
  (display "load & expanding runtime..")(newline)
  (let* ((exp-src (read-all "expander.scm"))
	 (sysdef-src (read-all "mosh-exceptions.scm"))
	 (init-src (read-all "init.ss"))
	 (baselib (expand-sequence baselib-src))
	 (syslib (expand-sequence syslib-src))
	 (init5 (read-all "init5.scm"))
	 (init (expand-sequence init-src))
	 (expander (expand-sequence-r5rs exp-src (expander-environment '(rnrs base))))
	 (sysdef (expand-sequence-r5rs sysdef-src (expander-environment '(rnrs) '(nmosh conditions))))
	 (outfile (append runtime baselib syslib sysdef expander init5 init)))
    (dumpsrc outfile)
    (display "compile...")(newline)
    (let ((vec (compile-to-codevector/toplevel outfile)))
      ;(dumpvec vec)
      (display "writing boot image...")(newline)
      (write-cobj 'nmosh p (obj->fasl vec))
      (display "done.")(newline))))

(define (build p)
  (display "#define uint8_t unsigned char\n" p)
  (output p)
  (display "const unsigned char* nmosh_image_ptr = (unsigned char*)&nmosh_image;\n" p)
  (display "extern \"C\" const unsigned int nmosh_image_size = sizeof(nmosh_image); \n" p))


(define (dumpsrc l)
  (when (file-exists? "boot.scm")
    (delete-file "boot.scm"))
  (display "writing expanded boot image to boot.scm")(newline)
  (call-with-output-file
    "boot.scm"
    (lambda (p)
      (put-string p
		  (call-with-string-output-port
		    (lambda (p) (for-each (lambda (e) (pp e p)) l))))))
  (display "done.")(newline))

(define (dumpvec v)
  (when (file-exists? "bootvec.scm")
    (delete-file "bootvec.scm"))
  (display "writing compiled boot image to bootvec.scm")(newline)
  (call-with-output-file 
    "bootvec.scm" 
    (lambda (p) 
      (vector-for-each 
	(lambda (e) (write e p) (newline p))
	v)))
  (display "done.")(newline))

(when (file-exists? "nmosh_image.cpp")
  (delete-file "nmosh_image.cpp"))
(call-with-output-file "nmosh_image.cpp" build)
