(library (nmosh bootstrap util)
	 (export read-all
		 compile-to-codevector-native
		 ;compile-to-codevector/toplevel
		 compile-to-codevector-native/toplevel
		 obj->fasl
		 fasl->obj
		 write-cobj)

	 (import (only (mosh) format make-instruction make-compiler-instruction) 
		 (rnrs) (srfi :8) (srfi :42) 
         #|
         (only 
           (nmosh boot compiler)
           compile compile-w/o-halt)
         |#
         ;(nmosh bootstrap stubs)
         ;(nmosh bootstrap compiler)
         (primitives
           compile compile-w/o-halt)
		 (primitives 
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


(define (extract obj)
  (cond
   [(vector? obj)
    (vector-map extract obj)]
   [(and (pair? obj) (eq? (car obj) '*insn*))
    (make-instruction (cadr obj))]
   [(and (pair? obj) (eq? (car obj) '*compiler-insn*))
    (make-compiler-instruction (cadr obj))]
   [(list? obj)
    (map extract obj)]
   [else obj]))

#|
(define (compile-to-codevector/toplevel l)
  (list->vector (extract (compile-to-sexp l))))
|#

(define (compile-to-codevector-native/toplevel l)
  (compile l))

(define (compile-to-codevector-native l)
  (compile-w/o-halt l))

(define (obj->fasl obj)
  (receive (port bv-proc) (open-bytevector-output-port)
    (fasl-write! obj port)
    (bv-proc)))

(define (fasl->obj fasl)
  (call-with-port (open-bytevector-input-port fasl) fasl-read))

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
