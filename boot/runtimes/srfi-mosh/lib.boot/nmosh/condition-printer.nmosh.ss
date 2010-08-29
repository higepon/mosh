(library (nmosh condition-printer)
	 (export condition-printer 
		 debug-format
		 with-condition-printer with-condition-printer/raise)
	 (import (rnrs) (nmosh conditions)
		 (only (mosh) format)
                 (nmosh global-flags)
                 (nmosh pathutils)
		 (primitives id-name id-debug id-maybe-library)) 


(define (guru-mode?)
  (get-global-flag '%nmosh-guru-mode))

; almost as syntax->datum but allows symbol in l
(define (strip l)
  (cond
    ((list? l) (map strip l))
    ((vector? l) (vector-map strip l))
    ((identifier? l) (id-name l))
    (else l)))

; -> (nmosh debug)
(define (debug-format debug)
  (define (extract-name name)
    (let ((l (string->list name)))
      (let* ((r (reverse l))
	     (a (car r))
	     (b (cadr r)))
	(if (char=? #\> a b) ; IT WON'T WORK (when supplied filename contains spc)
	  (let loop ((cur '())
		     (s (cddr r)))
	    (if (char=? #\space (car s))
	      (simplify-path (list->string cur))
	      (loop (cons (car s) cur) (cdr s))))
	  name))))
  (cond
    ((and (list? debug) (= 2 (length debug)))
     (let ((name (car debug))
	   (line (cadr debug)))
       (format "~a:~d" (if (string? name) 
			 (extract-name name)
			 "INVALID")
			 (- line 1))))
    (else #f)))

; STUB!
(define (id->debuginfo e)
  (let ((name (id-name e))
	(library (id-maybe-library e))
	(debug (id-debug e)))
    (if debug
      (debug-format debug)
      #f)))

(define (gather l)
  (define (next cur e)
    (if (member e cur)
      cur
      (if e (cons e cur) cur)))
  (define (itr cur e)
    (cond
      ((list? e) (fold-left itr cur e))
      ((vector? e) (fold-left itr cur (vector->list e)))
      ((identifier? e) (next cur (id->debuginfo e)))
      (else cur)))
  (if (list? l)
    (fold-left itr '() l)
    (itr '() l)))

(define-syntax with-condition-printer/raise
  (syntax-rules ()
    ((_ arg ...)
     (guard (e
	      (#t
	       (if (condition? e)
		 (begin 
		   (condition-printer e (current-error-port))
		   (raise e))
		 (display (list 'UNKNOWN-DATUM! e) (current-error-port)))))
	    arg ...))))

(define-syntax with-condition-printer
  (syntax-rules ()
    ((_ arg ...)
     (guard (e
	      (#t ;always handles
	       (if (condition? e)
		 (condition-printer e (current-error-port))
		 (display (list 'UNKNOWN-DATUM! e) (current-error-port)))))
	    arg ...))))

(define (safe-condition-who e) ; when who is #f... e.g.) eval (#f . #f)
  (guard (c (#t #f))
	 (condition-who e)))

(define (ptake x l) ;; permissive SRFI-1 take
  ;; (ptake N #f) => #f
  ;; (ptake N x) => '()
  (cond
    ((not l)
     #f)
    ((not (pair? x))
     '())
    ((= x 1)
     (car x))
    (else
      (cons (car x) (ptake (- x 1) (cdr l))))))

(define (syntax-trace-printer e port)
  (define (tab)
    (display "      " port))
  (let ((who (safe-condition-who e))
	(message (condition-message e))
	(form (syntax-violation-form e))
	(subform (syntax-violation-subform e))
	(trace (if (guru-mode?)
                 (condition-syntax-trace e)
                 (ptake 5 (condition-syntax-trace e))))
        (syntax-form (condition-syntax-form e))
        (syntax-subform (condition-syntax-subform e)))
    (display " Syntax error" port)
    (newline port)
    (display "      who : " port)
    (display who port)
    (newline port)
    (display "  message : " port)
    (display message port)
    (newline port)
    (cond
      ((and (not (guru-mode?))
            ;FIXME move the blacklist proper place..
            (member message 
                    '("Unbound export" "Attempt to export mutable variable"))) 
       'do-nothing)
      (else
        (when form
          (display "     form : " port)
          (display form port)
          (when syntax-form
            (let ((l (gather syntax-form)))
              (when (< 0 (length l))
                (display " " port)
                (display l port))))
          (newline port))))
    (when subform
      (display "  subform : " port)
      (display subform port)
      (when syntax-subform
        (let ((l (gather syntax-subform)))
          (when (< 0 (length l))
            (display " " port)
            (display l port))))
      (newline port))
    (when (pair? trace)
      (display "   around : " port)
      (for-each (lambda (e)
		  (display e port)
		  (newline port)
		  (display "            " port))
		(gather trace))
      (newline port)
      (display "    trace : \n" port)
      (for-each (lambda (e)
		  (tab)
		  (write (strip e) port)
		  (newline port)
		  (newline port)) trace))))

(define (condition-printer e port)
  (cond
    ((syntax-trace-condition? e)
     (syntax-trace-printer e port))
    (else (condition-printer/base e port))))


;-------------------------------------------------------------------------
; from mosh's psyntax (see lib/psyntax/psyntax/main.ss)
;-------------------------------------------------------------------------

(define (rpad str pad n)
  (let ([rest (- n (string-length (format "~a" str)))])
    (let loop ([rest rest]
	       [ret (format "~a" str)])
      (if (<= rest 0)
	ret
	(loop (- rest 1) (string-append ret pad))))))

(define (for-each-with-index proc lst)
  (do ((i 1 (+ i 1)) ; start with 1
       (lst lst (cdr lst)))
    ((null? lst))
    (proc i (car lst))))

(define (map-with-index proc lst)
  (let loop ([i 0]
	     [lst lst]
	     [ret '()])
    (if (null? lst)
      (reverse ret)
      (loop (+ i 1) (cdr lst) (cons (proc i (car lst)) ret)))))

(define (record->field-alist r)
  (define (ref rtd i x)
    (let ([val ((record-accessor rtd i) x)])
      val)); NMOSH: do not ungensym here
  (let loop ([ret '()]
	     [rtd (record-rtd r)])
    (cond
      [rtd
	(loop (append ret
		      (map-with-index
			(lambda (i field)
			  (cons field (ref rtd i r)))
			(vector->list (record-type-field-names rtd)))) (record-type-parent rtd))]
      [else ret])))

(define (condition-printer/base e port)
  (define max-condition-len
    (apply max (map (lambda (c) (string-length
				  (symbol->string (record-type-name (record-rtd c)))))
		    (simple-conditions e))))
  (display " Condition components:\n" port)
  (for-each-with-index
    (lambda (i x)
      (let ([rtd (record-rtd x)]
	    [fields-alist (record->field-alist x)])
	(format port " ~d. ~a" i (rpad (symbol->string (record-type-name rtd)) " " max-condition-len))
	(when (null? fields-alist)
	  (newline port))
	(let loop ([first #t]
		   [fields-alist fields-alist])
	  (cond
	    [(null? fields-alist) '()]
	    [else
	      (let ([field (car fields-alist)])
		(unless first
		  (display (rpad "" " " (+ 4 max-condition-len)) port))
		(display "       " port)
		;(display (car field) port)
		;(display ": " port)
		(write (cdr field) port)
		(newline port)
		(loop #f (cdr fields-alist)))
	      ]
	    ))))
    (simple-conditions e)))

)
