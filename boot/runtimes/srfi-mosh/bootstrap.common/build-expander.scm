
(define *SYMS* '())

(define (add-debug! fn r)
  (set! *SYMS* (cons (list
		       (cons 'DBG-SOURCE (car r))
		       (cons 'DBG-FILENAME fn)
		       (cons 'DBG-SYMS (cdr r))) *SYMS*))
  (car r))

(define deftable '())

(define invokers '())

(define (add-invoker! prog)
  (define (rip-sequence s)
    (define (step cur e)
      (case (car e)
	((values) cur)
	(else (cons e cur))))
    (fold-left step '() s))
  (define (rip prog)
    (if (and (pair? prog) (eq? 'lambda (car prog)))
      (rip-sequence (cddr prog))
      '()))
  (let ((r (rip prog)))
    (unless (null? r)
      (set! invokers (append r invokers)))))

(define (add-definition! l)
  (let ((name (car l))
	(vals (cdr l)))
    (unless (and (list? vals) (= 1 (length vals)) 
		 (let ((a (car vals)))
		   (or 
		     (not a)
		     (eq? 'ex:unspecified a))))
      (assertion-violation 'add-definition "unsupported.." l))
    (unless (memq name deftable)
      (set! deftable (cons name deftable)))))

(define (read-symfile)
  (cond
    ((file-exists? "symfile.scm")
     (call-with-input-file "symfile.scm" read))
    (else
      (call-with-output-file "symfile.scm"
			     (lambda (p)
			       (display "()" p)
			       (close-output-port p))) ; FIXME FIXME
      (read-symfile))))

(define (update-symfile c)
  (newline)
  (display ">>>>>>>>>>>>>>> UPDATE SYM!!")(newline)
  (display c)(newline)
  (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>")(newline)
  (cond
    ((file-exists? "symfile.scm")
     (let ((cur (read-symfile)))
       (delete-file "symfile.scm")
       (call-with-output-file "symfile.scm"
			      (lambda (p)
				(write (cons c cur) p)))))
    (else
      (call-with-output-file "symfile.scm"
			     (lambda (p)
			       (write (list c) p)
			       (close-output-port p)
			       )))))

(define (do-permissive-eval l)
  (define (permissive-eval-try)
    (eval l
	  (environment 
	    '(rnrs) ; FIXME!!
	    ;`(only (rnrs) 
	;	   quote
	;	   ,@(read-symfile))
	    '(mosh)
	    '(system)
	    '(primitives ca-load ca-load/disable-cache dbg-files symbol-value set-symbol-value! %nmosh-failproc)
	    '(rename (primitives my-register-macro!! 
				 my-syntax-rename 
				 my-register-library!!
				 my-identifier? 
				 my-generate-temporaries 
				 my-free-identifier=?
				 my-hooked-vector?
				 my-datum->syntax 
				 my-syntax->datum
				 my-generate-guid
				 my-dotted-length
				 my-dotted-last
				 my-dotted-butlast
				 my-uncompress
				 my-map-while
				 my-invalid-form
				 my-syntax-violation
				 my-make-library
				 my-unspecified
				 my-environment
				 my-id-name
				 my-id-colors
				 my-id-transformer-envs
				 my-id-maybe-library
				 my-id-debug
				 my-run-r6rs-sequence
				 my-eval-str
				 my-load
				 my-undefined)
		     (my-register-macro!! ex:register-macro!)
		     (my-register-library!! ex:register-library!)
		     (my-make-library ex:make-library)
		     (my-syntax-rename ex:syntax-rename)
		     (my-identifier? ex:identifier?)
		     (my-generate-temporaries ex:generate-temporaries)
		     (my-free-identifier=? ex:free-identifier=?)
		     (my-hooked-vector? hooked-vector?)
		     (my-datum->syntax ex:datum->syntax)
		     (my-syntax->datum ex:syntax->datum)
		     (my-generate-guid ex:generate-guid)
		     (my-dotted-length ex:dotted-length)
		     (my-dotted-last ex:dotted-last)
		     (my-dotted-butlast ex:dotted-butlast)
		     (my-uncompress ex:uncompress)
		     (my-map-while ex:map-while)
		     (my-invalid-form ex:invalid-form)
		     (my-syntax-violation ex:syntax-violation)
		     (my-unspecified ex:unspecified)
		     (my-environment ex:environment)
		     (my-id-name id-name)
		     (my-id-colors id-colors)
		     (my-id-transformer-envs id-transformer-envs)
		     (my-id-maybe-library id-maybe-library)
		     (my-id-debug id-debug)
		     (my-run-r6rs-sequence ex:run-r6rs-sequence)
		     (my-eval-str nm:eval-str)
		     (my-load ex:load)
		     (my-undefined ex:undefined))
	    )))
  (define (try k)
    (guard (c
	     ((and (syntax-violation? c)
		   (who-condition? c)
		   (string? (condition-who c))
		   (string=? (condition-who c) "invalid reference"))
	      (update-symfile (syntax-violation-form c))
	      (k (cons #f #f))))
	   (cons #t (permissive-eval-try))))
  (define (tryloop)
    (let ((r (call/cc try)))
      (let ((a (car r))
	    (d (cdr r)))
	(if a
	  d
	  (tryloop)))))
  (tryloop))

(define (permissive-eval l) ; eval with expanded library namespace
  ; (eval 'PROG) =>
  ; (eval (let ((var1 'var1) (var2 'var2)..)
  ;         (set! var1 INIT-BY-LIBRARY-INVOKER)
  ;         (set! var2 ...)
  ;         ...
  ;         (let ()
  ;           PROG)))
  (define (emittable)
    (map (lambda (e) `(,e (quote ,e))) deftable))
  (define (create-names l) 
    `(let ,(emittable) ,@(reverse invokers) (let () ,l)))
  (do-permissive-eval (create-names l)))

(define eval-core permissive-eval) ; used in expand.scm

(define (reglib! e)
  (define (makelib l)
    (add-invoker! (list-ref l 6)))
  (when (pair? e)
    (case (car e)
      ((begin)
       (for-each reglib! (cdr e)))
      ((ex:register-library!)
       (reglib! (cadr e)))
      ((ex:make-library)
       (makelib (cdr e)))
      ((values) 'ok) ; do nothing
      ((define) (add-definition! (cdr e))) 
      (else (assertion-violation 'reglib! "invalid expression" e)))))

(define (ex-6 fn)
  (define (foldprog l)
    (define (itr cur l)
      (if (pair? l)
	(if (eq? (caar l) 'library)
	  (itr (cons (car l) cur) (cdr l))
	  (reverse (cons (cons 'program l) cur)))
	(reverse cur)))
    (itr '() l))
  (define (register l)
    (let ((sig (car l))
	  (name (cadr l)))
      (case sig
	((library)
	 (display " library ")
	 (display name) (newline)
	 (let ((r (add-debug! fn (ex:expand-sequence/debug (list l)))))
	   (for-each reglib! r)
	   r))
	((program)
	 (display " program")(newline)
	 (add-debug! fn (ex:expand-sequence/debug (cdr l))))
	(else
	  (assertion-violation 'ex-6 "invalid expression" l)))))
  (display "expanding(R6RS) ")
  (display fn) (newline)
  (let ((l (foldprog (read-all fn))))
    (apply append (map register l))))

(define (expand-6 l)
  (apply append (map ex-6 l)))

(define (ex-5 fn . libs)
  (display "expanding(R5RS) ")
  (display fn)
  (unless (null? libs)
    (display " with ")
    (display libs))
  (newline)
  (add-debug! fn (ex:expand-sequence-r5rs/debug (read-all fn) (apply ex:environment libs))))
