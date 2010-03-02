(import  (except (rnrs) identifier?)
	 (rnrs mutable-pairs)
	 (rnrs r5rs) (mosh) (except (system) mosh-cache-dir)
	 (srfi :98)
	 (only (psyntax system $all) interaction-environment))

(define %verbose #f)
(define %disable-acc #t)
(define %loadpath "lib.rnrs:lib.boot:../../../lib")

(define raise-syntax-violation (lambda e 
				 (display "SYNTAX-ERROR!!")(newline)
				 (write e)(newline)
				 (exit -1)))

(define fasl-write! fasl-write)

(define eval-compiled! 'eval-compiled!)
(define compile-w/o-halt 'compile-w/o-halt)

(define (eval-core l)
  (eval l (interaction-environment)))

(define core-prefix "lib.rnrs")
(define core-syms '(core/primitives
                     core/with-syntax core/syntax-rules
                     core/let core/derived
                     core/quasisyntax
                     core/quasiquote core/let-values core/identifier-syntax rnrs/base))

(define (makenames prefix l)
    (map (lambda (e) (string-append prefix "/" (symbol->string e) ".ss")) l))

(define core-names (makenames core-prefix core-syms))

(define (read-all/port p)
  (let ((r (read p)))
    (if (eof-object? r)
      '()
      (cons r (read-all/port p)))))

(define (read-all fn)
  (call-with-input-file fn read-all/port))

(define core-src (apply append (map read-all core-names)))
(define expander-src (read-all "expander.scm"))

