;;;!ypsilon

(import (rnrs)
  (for (proof) run expand))

(define-syntax with-record-fields
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ (((?field-name ...) ?record-name ?expr))
	?form0 ?form ...)
     (let-syntax
	 ((dummy (lambda (stx)

		   (define (vector-index item vec)
		     (let ((len (vector-length vec)))
		       (let loop ((i 0))
			 (and (< i len)
			      (if (eq? item (vector-ref vec i))
				  i
				(loop (+ i 1)))))))

		   (define (%record-field-accessor rtd-name rtd field-name)
		     (if rtd
			 (let ((idx (vector-index field-name (record-type-field-names rtd))))
			   (if idx
			       (record-accessor rtd idx)
			     (%record-field-accessor rtd-name (record-type-parent rtd) field-name)))
		       (assertion-violation #f
			 (string-append "unknown field name in record type hierarchy of \""
					(symbol->string rtd-name) "\"")
			 field-name)))

		   (define (%record-field-mutator rtd-name rtd field-name)
		     (if rtd
			 (let ((idx (vector-index field-name (record-type-field-names rtd))))
			   (cond ((not idx)
				  (%record-field-mutator rtd-name (record-type-parent rtd) field-name))
				 ((record-field-mutable? rtd idx)
				  (record-mutator rtd idx))
				 (else
				  (lambda args
				    (assertion-violation #f
				      (string-append "attempt to mutate immutable field for record \""
						     (symbol->string (record-type-name rtd)) "\"")
				      field-name)))))
		       (assertion-violation #f
			 (string-append "unknown field name in record type hierarchy of \""
					(symbol->string rtd-name) "\"")
			 field-name)))

		   (syntax-case stx ()
		     ((_ ?kontext)
		      (with-syntax
			  (((EXPR)	(datum->syntax #'?kontext '(?expr)))
			   ((FIELD (... ...))
			    (datum->syntax #'?kontext '(?field-name ...)))
			   ((ACCESSOR (... ...))
			    (datum->syntax #'?kontext
					   (list
					    (%record-field-accessor (quote ?record-name)
								    (record-type-descriptor ?record-name)
								    (quote ?field-name))
					    ...)))
			   ((MUTATOR (... ...))
			    (datum->syntax #'?kontext
					   (list
					    (%record-field-mutator  (quote ?record-name)
								    (record-type-descriptor ?record-name)
								    (quote ?field-name))
					    ...)))
			   ((FORMS (... ...))
			    (datum->syntax #'?kontext '(?form0 ?form ...))))
			(syntax (let ((the-record EXPR))
				  (let-syntax
				      ((FIELD (identifier-syntax
					       (_		('ACCESSOR the-record))
					       ((set! _ e)	('MUTATOR the-record e))))
				       (... ...))
				    FORMS (... ...))))))))))
       (dummy ?record-name)))

    ((_ (((?field-name0 ...) ?record-name0 ?expr0) ?bindings ...) ?form0 ?form ...)
     (with-record-fields (((?field-name0 ...) ?record-name0 ?expr0))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name0 ?record-name0 ?expr0) ?bindings ...) ?form0 ?form ...)
     (with-record-fields (((?field-name0) ?record-name0 ?expr0))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))))

(let ((o (make-<gamma> 1 2 3
			 4 5 6
			 7 8 9)))

  (with-record-fields ((a <gamma> o))
    a)

  (with-record-fields (((a) <gamma> o))
    a)

  (with-record-fields ((a <gamma> o)
		       (b <gamma> o)
		       (c <gamma> o)
		       (d <gamma> o)
		       (e <gamma> o)
		       (f <gamma> o)
		       (g <gamma> o)
		       (h <gamma> o)
		       (i <gamma> o))
    (list a b c d e f g h i))

    ;; (check
    ;; 	(with-record-fields (((a b c d e f g h i) <gamma> o))
    ;; 	  (list a b c d e f g h i))
    ;; 	=> '(1 2 3 4 5 6 7 8 9))

    ;; (check
    ;; 	(with-record-fields (((a b c) <gamma> o)
    ;; 			     (d <gamma> o)
    ;; 			     (e <gamma> o)
    ;; 			     ((f g) <gamma> o)
    ;; 			     (h <gamma> o)
    ;; 			     (i <gamma> o))
    ;; 	  (list a b c d e f g h i))
    ;; 	=> '(1 2 3 4 5 6 7 8 9))

    ;; (check
    ;; 	(with-record-fields (((a b c) <gamma> o)
    ;; 			     ((d e) <gamma> o)
    ;; 			     ((f g) <gamma> o)
    ;; 			     ((h i) <gamma> o))
    ;; 	  (set! a 10)
    ;; 	  (set! c 30)
    ;; 	  (set! d 40)
    ;; 	  (set! f 60)
    ;; 	  (set! g 70)
    ;; 	  (set! i 90)
    ;; 	  (list a b c d e f g h i))
    ;;   => '(10 2 30 40 5 60 70 8 90))

    )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
