(library (nmosh minidebug)
	 (export load-symbols minidebug stacktrace-printer)
	 (import (rnrs) (nmosh condition-printer) (nmosh conditions)
		 (primitives dbg-files dbg-syms fasl-read %get-nmosh-dbg-image))


(define libsyms '())

(define intsyms '())

(define (list-dbgfile)
  (map cadr dbg-files))

(define (get-symfile fn)
  (guard
    (c (#t #f))
    (call-with-port (open-file-input-port fn) fasl-read)))

(define (load-symfiles)
  (define syms '())
  (define (addsym l)
    (set! syms (append syms l)))
  (define (step fn)
    (let ((r (get-symfile fn)))
      (when r
	(for-each 
	  (lambda (e)
	    (if (eq? 'DBG-SYMS (car e))
	      (addsym (cdr e))))
	  r))))
  (let ((f (list-dbgfile)))
    (for-each step f))
  syms)

(define (load-intsyms)
  (define syms '())
  (define (addsym l)
    (set! syms (append syms l)))
  (define (step l)
    (for-each
      (lambda (e)
	(if (eq? 'DBG-SYMS (car e))
	  (addsym (cdr e))))
      l))
  (for-each step (%get-nmosh-dbg-image))
  syms)

(define (fallback-trace-printer p trace)
  (define (cprocprint h)
    (let ((proc (car h)))
      (display "  cprc   " p)
      (display proc p)))
  (define (undec proc)
    (define (chopseqnum l) ; => symbol
      (define (step e cur)
	(if (char=? #\~ e)
	  '()
	  (cons e cur)))
      (string->symbol (list->string (fold-right step '() l))))
    (define (ungensym sym)
      (let ((l (string->list (symbol->string sym))))
	(if (char=? #\& (car l))
	  (chopseqnum (cdr l))
	  sym)))
    (define (do-undec sym)
      (if (symbol? sym)
	(ungensym sym)
	sym))
    (display (map do-undec proc) p))
  (define (decprint proc)
    (define (step cur e)
      (cond
	((assq e libsyms) => (lambda (p) (cons 'lib p)))
	((assq e dbg-syms) => (lambda (p) (cons 'dbg p)))
	((assq e intsyms) => (lambda (p) (cons 'int p)))
	(else cur)))
    (let ((dbg (fold-left step #f proc)))
      (cond
	(dbg
	  (case (car dbg)
	    ((lib) (display "==USRP== " p))
	    ((dbg) (display "==DBGP== " p))
	    ((int) (display "  nmsh   " p)))
	  (undec proc)
	  (display " @ " p)
	  (display (debug-format (cddr dbg)) p))
	(else
	  (display "  usrp   " p)
	  (undec proc)))))
  (define (procprint h)
    (let ((proc (car h))
	  (loc (cadr h)))
      (cond
	(loc
	  (display "  mosh   " p)
	  (display proc p)
	  (display " @ " p)
	  (display (debug-format loc) p))
	(else
	  (decprint proc)))))

  (define (numprint i)
    (when (< i 10)
      (display " " p))
    (when (< i 100)
      (display " " p))
    (display i p)
    (display " : "))

  (define (print num e)
    (let ((id (cadr e))
	  (param (cddr e)))
      (case id
	((*proc*) 
	 (numprint num)
	 (procprint param))
	((*cproc*) 
	 (numprint num)
	 (cprocprint param))
	(else 'ok)))
    (newline p))
  (define (printer t)
    (define (itr i cur)
      (when (pair? cur)
	(case (cadar cur)
	  ((*unknown-proc*)
	   (itr i (cdr cur)))
	  (else
	    (print i (car cur))
	    (itr (+ i 1) (cdr cur))))))
    (itr 1 t))

  (display "TRACE :\n" p)
  (printer (reverse trace)))

(define minidebug-key #f)

(define (stacktrace-printer trace p)
  (when (and (pair? trace) (pair? (cdr trace)))
    (fallback-trace-printer p (cdr trace))))

(define (load-symbols)
  (set! intsyms (load-intsyms))
  (set! libsyms (load-symfiles)))


(define (minidebug p c trace)
  (condition-printer c p)
  (when minidebug-key
    (display "!!! DOUBLE FAULT!\n" p)
    (exit -1))
  (set! minidebug-key #t)
  (load-symbols)
  (stacktrace-printer trace p)
  (exit -1))
)
