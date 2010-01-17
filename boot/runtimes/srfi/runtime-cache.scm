; MOSH specific runtime appendices

; cached/serialized library entry point
(define (nm:register-source fn name imported)
  (define (fail) (nm:parachute 'nm:failure))
  (define current-libs (nm:dump-library-table name))
  (define (check name build)
    ;;assoc??
    (define (checkloop cur)
      (if (pair? cur)
	(begin
	  ;(display (list (list (caar cur) name) (list (cdar cur) build)))(newline)
	  (if (and (equal? (caar cur) name) (not (eqv? (cdar cur) build)))
	    #f
	    (checkloop (cdr cur))))
	#t))
    (checkloop current-libs))
  (define (audit cur)
    (if (pair? cur)
      (let ((libname (caar cur))
	    (build (cdar cur)))
	(if (check libname build)
	  (audit (cdr cur))
	  (fail)))))

  ;; Audit libraries 
  (audit imported))
  

