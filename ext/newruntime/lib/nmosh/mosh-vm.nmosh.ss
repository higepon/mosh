(library (nmosh mosh-vm)
	 (export read-all
		 compile-to-codevector
		 compile-to-codevector/toplevel
		 obj->fasl
		 write-cobj)

	 (import (only (mosh) format) 
		 (rnrs) (srfi :8) (srfi :42) 
		 (primitives 
		   compile
		   compile-w/o-halt 
		   eval-compiled! 
		   fasl-read 
		   fasl-write!))

; from binary2c.scm
(define (do-write-cobj name p bv)
  (format p "static const uint8_t ~a_image[] = {" name)
  (do-ec (: i 0 (bytevector-length bv))
	 (begin (when (zero? (mod i 15)) (newline p)) 
		(format p "0x~a," (number->string (bytevector-u8-ref bv i) 16))))
  (display "\n};\n" p))

(define (write-cobj name p bv)
  (let ((s (call-with-string-output-port (lambda (port) (do-write-cobj name port bv)))))
    (put-string p s)))

(define (compile-to-codevector/toplevel l)
  (compile l))

(define (compile-to-codevector l)
  (compile-w/o-halt l))

(define (obj->fasl obj)
  (receive (port bv-proc) (open-bytevector-output-port)
    (fasl-write! obj port)
    (bv-proc)))

(define (read-all fn)
  (define (reader p)
    (define (itr cur)
      (let ((e (read p)))
        (if (eof-object? e)
          (reverse cur)
          (itr (cons e cur)))))
    (itr '()))
  (call-with-input-file fn reader))
)
