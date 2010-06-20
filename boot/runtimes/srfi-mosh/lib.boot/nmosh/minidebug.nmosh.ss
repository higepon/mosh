(library (nmosh minidebug)
	 (export load-symbols minidebug stacktrace-printer show-profile)
	 (import (rnrs) (nmosh global-flags)
                 (nmosh condition-printer) (nmosh conditions) (only (mosh) format)
		 (primitives dbg-files dbg-syms fasl-read %get-nmosh-dbg-image
			     ; for show-profile
			     hashtable-map lpad rpad string-chop
			     get-closure-name summerize-samples
			     ))

(define (guru-mode?)
  (get-global-flag '%nmosh-guru-mode))

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

(define (fallback-trace-printer p trace)

  (define (cprocprint h)
    (let ((proc (car h)))
      (display "  cprc   " p)
      (display proc p)))
  (define (undec proc)
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
    (display " : " p))

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
        ((*cproc*) #t)
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
  (let ((disp-t (cdr (reverse (strip trace)))))
    (unless (= 0 (length disp-t))
      (display "TRACE :\n" p)
      (printer disp-t))))

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


; show-profile from baselib.scm


(define (cleanup-source src)
  (let ((src (map ungensym src)))
    (if (eq? (car src) 'lambda)
        `(lambda ,(cdr src) ...)
        src)))

(define (show-profile result)
  (define (search-nmosh-sym l)
    (define (step cur e)
      (cond
	((assq e libsyms) => cdr)
	((assq e dbg-syms) => cdr)
	((assq e intsyms) => cdr)
	(else cur)))
    (fold-left step #f l))

  (load-symbols) ; NMOSH
  (let* ([total (car result)]
         [calls-hash (cadr result)]
         [sample-closures  (cddr result)]
         [sample-table (summerize-samples calls-hash sample-closures)])
    (display "time%        msec      calls   name                              location\n")
    (for-each
     (lambda (x)
       (let* ([closure-or-src  (car x)]
              [name     (string-chop (format "~a" 
					     (if (procedure? closure-or-src) 
					       (get-closure-name closure-or-src) 
					       (cleanup-source (cdr closure-or-src)))) 
				     25 "...)")]
	      (nmosh-sym (search-nmosh-sym (if (procedure? closure-or-src) (list get-closure-name) closure-or-src)))
              [location (if (procedure? closure-or-src) #f (car closure-or-src) )]
              [file     (if (pair? location) (car location) #f)]
              [lineno   (if (pair? location) (cadr location) #f)]
              [count    (cadddr x)])
         (format #t " ~a   ~a ~a   ~a    ~a\n"
                 (lpad (caddr x) " " 3)
                 (lpad (* (cadr x) 10) " " 10)
                 (lpad count " " 10)
                 (rpad name " " 30)
		 (cond
		   (file (format "~a:~d" file lineno))
		   (nmosh-sym (debug-format nmosh-sym))
		   (else ""))
		 )))
     (list-sort
      (lambda (x y) (> (cadr x) (cadr y)))
      (hashtable-map
       (lambda (closure count*)
         (list closure (car count*) (exact (round (/ (* 100 (car count*)) total))) (cdr count*)))
       sample-table)))
    (let ((seen-syms (vector->list (hashtable-keys sample-table))))
    (format #t "  **   ~d         **   total\n" (lpad (* (* total 10)) " " 10)))))

)
