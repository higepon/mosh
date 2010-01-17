(library (rnrs enums (6))
	 (export
	   make-enumeration
	   enum-set-universe
	   enum-set-indexer
	   enum-set-constructor
	   enum-set->list
	   enum-set-member?
	   enum-set-subset?
	   enum-set=?
	   enum-set-union
	   enum-set-intersection
	   enum-set-difference
	   enum-set-complement
	   enum-set-projection
	   define-enumeration
	   )
	 (import
	   (primitives
	     gensym
	   make-enumeration
	   enum-set-universe
	   enum-set-indexer
	   enum-set-constructor
	   enum-set->list
	   enum-set-member?
	   enum-set-subset?
	   enum-set=?
	   enum-set-union
	   enum-set-intersection
	   enum-set-difference
	   enum-set-complement
	   enum-set-projection)
	   (for (only (rnrs base) quote begin list lambda let if define define-syntax syntax-rules) run expand)
	   (for (only (rnrs lists) memq) run expand)
	   (for (rnrs syntax-case) run expand)
	   )

;FIXME report error
;based on Laceny's
(define-syntax defset
  (syntax-rules ()
    ((_ name endname endsname (sym ...))
     (begin
       (define endname (make-enumeration '(sym ...)))
       (define-syntax endsname
	 (syntax-rules ()
	  ((_ sym1 (... ...))
	   ((enum-set-constructor endname)
	    (list (name sym1) (... ...))))))))))

(define-syntax define-enumeration
  (syntax-rules ()
    ((_ name SYM syn)
     (begin
       (define-syntax name
	 (lambda (x)
	   (define (err)
	     (syntax-violation 'name "illegal symbol" x))
	   (syntax-case x ()
	     ((_ y)
	      (let ((sym1 (syntax->datum #'y)))
		(if (memq sym1 'SYM)
		  #''y
		  (err)))))))
       (defset name (gensym) syn SYM)))))


)
