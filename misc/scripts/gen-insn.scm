(import (rnrs) (mosh) (srfi :42) (match))

(define (output ip op)

  (define (for-each-with-index1 proc lst)
    (do-ec (: e (index i) lst)
	   (proc i e)))

  (define (print x)
    (display x op)
    (newline op))

  (define (print-header)
    (print "// Do not edit this file generated by gen-insn.scm")
    (print "#ifndef __SCHEME_INSTRUCTION_H__")
    (print "#define __SCHEME_INSTRUCTION_H__")
    (print "")
    (print "#include \"scheme.h\"")
    (print "")
    (print "namespace scheme {")
    (print "")
    (print "class Instruction EXTEND_GC")
    (print "{")
    (print "public:"))

  (define (print-enum lst)
    (print "    enum {")
    (for-each-with-index1 (lambda (i l)
			   (print (format "        ~a = ~d," l (+ (bitwise-arithmetic-shift i 5) 14)))) lst) ;; same as Object::makeInstruction2
    (format op "        INSTRUCTION_COUNT = ~d,\n" (length lst))
    (print "    };"))

  (define (print-to-string lst)
    (print "    static const ucs4char* toString(int val) {")
    (print "        switch(val) {")
    (for-each (lambda (insn)
		(format op "        case ~a:\n" insn)
		(format op "           return UC(\"~a\");\n"  insn)
		(format op "           break;\n")
		)
	      lst)
    (format op "        default:\n")
    (format op "           return UC(\"Unkown Instruction\");\n")
    (format op "           break;\n")
    (print "        }")
    (print "        return UC(\"\");")
    (print "    }")
    (print "\n"))

  (define (print-footer)
    (print "};")
    (print "}; // namespace scheme\n")
    (print "#endif // __SCHEME_INSTRUCTION_H__"))

  (define (read-input)
    (let loop ([obj (read ip)]
	       [ret '()])
      (if (eof-object? obj)
	(reverse ret)
	(match obj
	       [('define-insn name n)
		(loop (read ip) (cons name ret))]))))

  (print-header)
  (let ((lst (read-input)))
    (print-enum lst)
    (print-to-string lst))
  (print-footer)
  0)

(define (convert-file names k)
  (let ((in-name (car names))
	(out-name (cadr names)))
    (when (file-exists? out-name) (delete-file out-name))
    (call-with-input-file in-name 
      (lambda (ip)
	(call-with-output-file out-name
	  (lambda (op)
	    (k ip op)))))))

(convert-file (cdr (command-line)) output)
