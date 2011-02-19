(library (nmosh debugger core)
	 (export debugger)
	 (import (rnrs) (nmosh global-flags)
                 (nmosh debugger condition-printer) 
                 (prefix (nmosh ui deco) deco:)
                 (nmosh conditions) 
                 (srfi :48)
                 (srfi :98)
		 (primitives 
                   dbg-files dbg-syms fasl-read %get-nmosh-dbg-image))

(define (guru-mode?)
  (get-global-flag '%nmosh-guru-mode))

(define color-output?
  (get-environment-variable "NMOSH_CLICOLOR"))


(define libsyms '())

(define intsyms '())

(define (list-dbgfile)
  (map cadr dbg-files))

(define (get-symfile fn)
  (guard
    (c (#t 
        (when (guru-mode?)
          (display (format "Cannot load ~a\n" fn) (current-error-port)))
        #f))
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

(define (ungensym sym)
  (define (chopseqnum l) ; => symbol
    (define (step e cur)
      (if (char=? #\~ e)
	'()
	(cons e cur)))
    (string->symbol (list->string (fold-right step '() l))))
  (let ((l (string->list (symbol->string sym))))
    (if (char=? #\& (car l))
      (chopseqnum (cdr l))
      sym)))

(define (disp l)
  (display l (current-error-port)))

(define deco/err
  (if color-output?
    deco:deco/err
    disp))

(define (fallback-trace-printer/deco trace)

  (define (cprocprint h)
    (let ((proc (car h)))
      (disp "  cprc   ")
      (deco/err proc)))
  (define (undec proc)
    (define (do-undec sym)
      (if (symbol? sym)
	(ungensym sym)
	sym))
    (deco/err (map do-undec proc)))
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
	    ((lib) (disp "==USRP== "))
	    ((dbg) (disp "==DBGP== "))
	    ((int) (disp "  nmsh   ")))
	  (undec proc)
	  (disp " @ ")
	  (deco/err (debug-format (cddr dbg))))
	(else
	  (disp "  usrp   ")
	  (undec proc)))))
  (define (procprint h)
    (let ((proc (car h))
	  (loc (cadr h)))
      (cond
	(loc
	  (disp "  mosh   ")
	  (deco/err proc)
	  (disp " @ ")
	  (deco/err (debug-format loc)))
	(else
	  (decprint proc)))))

  (define (numprint i)
    (when (< i 10)
      (disp " "))
    (when (< i 100)
      (disp " "))
    (deco/err i)
    (disp " : "))

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
    (disp "\n"))
  (define (user-code? e)
    (define (step cur e)
      (cond
	((assq e libsyms) #t)
	((assq e dbg-syms) #t)
	((assq e intsyms) #f)
	(else cur)))
    (define (find-dbginfo-for-user prc)
      (fold-left step #f prc))
    (define (user-proc? h)
      (let ((loc (cadr h)))
        (cond
          (loc #f) ; mosh baselib
          (else (find-dbginfo-for-user (car h))))))
    (let ((id (cadr e))
          (param (cddr e)))
      (case id
        ((*proc*)
         (user-proc? param))
        ((*cproc*) #f)
        (else #f))))
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
  (define (strip t)
    (define (do-strip cur)
      (if (pair? cur)
        (if (user-code? (car cur))
          cur
          (do-strip (cdr cur)))
        '()))
    (cond
      ((guru-mode?) t)
      (else
        (do-strip t))))
  (let* ((disp-a (reverse (strip trace)))
         (disp-t (if (pair? disp-a) (cdr disp-a) '())))
    (unless (= 0 (length disp-t))
      (disp "TRACE!! :\n")
      (printer disp-t))))

(define minidebug-key #f)

(define (stacktrace-printer/deco trace)
  (when (and (pair? trace) (pair? (cdr trace)))
    (fallback-trace-printer/deco (cdr trace))))

(define (load-symbols)
  (set! intsyms (load-intsyms))
  (set! libsyms (load-symfiles)))

(define (debugger c trace)
  (condition-printer/deco c)
  (when minidebug-key
    (display "!!! DOUBLE FAULT!\n" (current-error-port))
    (exit -1))
  (set! minidebug-key #t)
  (load-symbols)
  (stacktrace-printer/deco trace)
  (exit -1))

)
