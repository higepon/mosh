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

(define %verbose #t)
(define %loadpath #f)

(define (raise-syntax-violation . e)
  (display 'SYNTAX-ERR!!)(newline)
  (display e)
  (exit -1))

(load "./bootstrap.gauche/mosh-stubs.scm")
(load "./bootstrap.gauche/compat-gauche.scm")
(load "./runtime.scm")
(load "./runtime-cache.scm")
(load "./mosh-utils5.scm")


(display "loading alexpander")(newline)

(load "./bootstrap.common/alexpander.scm")

(display "LOAD DONE")(newline)

(let* ((core (ex:expand-sequence core-src))
       (expander (ex:expand-sequence-r5rs expander-src (ex:environment '(rnrs base))))
       (code (append core expander)))
  (call-with-output-file "bootstrap1.exp"
			 (lambda (p)
			   (for-each 
			     (lambda (e)
			       (write e p)
			       (newline p))
			     code))))

