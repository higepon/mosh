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
(define %nmosh-portable-mode #f)
(define %nmosh-prefixless-mode #t)
(define %loadpath #f)

(define (raise-syntax-violation . e)
  (display 'SYNTAX-ERR!!)(newline)
  (write e)
  (newline)
  (exit -1))

(define expander-prog #f)

(when (file-exists? "bootstrap0.exp")
  (delete-file "bootstrap0.exp"))
(when (file-exists? "bootstrap1.exp")
  (delete-file "bootstrap1.exp"))

(display "loading nmosh runtime")(newline)

(load "compat-mosh-run.scm")
(load "runtime.scm")
(load "mosh-utils5.scm")

(set! top-level-macros '())
(display "macro disabled.")(newline)

(display "loading alexpander")(newline)

(load "bootstrap.common/alexpander.scm")

(display "expanding...")(newline)

(set! expander-prog (expand-program expander-src))

(display "dump(BOOT0.fasl)..")(newline)

(when (file-exists? "BOOT0.fasl")
 (delete-file "BOOT0.fasl"))

(call-with-port (open-file-output-port "BOOT0.fasl")
		(lambda (p)
		  (fasl-write! expander-prog p)))
