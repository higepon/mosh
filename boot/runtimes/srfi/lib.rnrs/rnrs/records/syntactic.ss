;from Larceny's
;modified for mosh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (rnrs records syntactic)
;
; This implementation extends the R6RS define-record-type as
; outlined by William D Clinger in his essay on language design
; that accompanied his vote against ratification of the R6RS:
; http://www.r6rs.org/ratification/results.html#X101
;
; FIXME:  That means this implementation is R6RS-compatible but
; not R6RS-conforming.  An R6RS-conforming implementation would
; have to reject some perfectly sensible record type definitions
; that this implementation accepts.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs records syntactic helper)

  (export preferred-cd preferred-cd-set!)

  (import (for (core primitives) run expand)
          (for (primitives make-hashtable hashtable-ref hashtable-set!
                           symbol-hash record-type-name)
               run expand)
          (for (rnrs base) run expand)
          (rnrs records procedural))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; Preferred record-constructor descriptors.  Whoopee.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define preferred-cd-table 
    (make-hashtable (lambda (rtd) (symbol-hash (record-type-name rtd))) eqv?))

  (define (preferred-cd rtd)
    (let ((cd (hashtable-ref preferred-cd-table rtd #f)))
      (if cd
          cd
          (make-record-constructor-descriptor rtd #f #f))))

  (define (preferred-cd-set! rtd cd)
    (hashtable-set! preferred-cd-table rtd cd))
)

;MOSH: 
(library (rnrs records syntactic helper counter)
	 (export map/index1)
	 (import (only (rnrs base) null? reverse cons car cdr + define if quote))

      (define (map/index1 proc lst)
	(define (itr ind cur rest)
	  (if (null? rest)
	    (reverse cur)
	    (itr (+ ind 1) (cons (proc ind (car rest)) cur) (cdr rest))))
	(itr 0 '() lst))
)

(library (rnrs records syntactic (6))

  (export define-record-type fields mutable immutable
          parent protocol sealed opaque nongenerative parent-rtd
          record-type-descriptor record-constructor-descriptor)

  (import (for (core primitives) run expand)
          (for (rnrs base) run expand)
          (for (rnrs lists) run expand)
          (for (only (rnrs syntax-case) quasisyntax unsyntax) run expand)
          (rnrs records procedural)
          ;(err5rs records procedural) ;MOSH: Larceny's R6RS syntactic record library was built on ERR5RS procedural record
          (rnrs records syntactic helper)
          (for (primitives ex:generate-guid make-record-type-descriptor) run expand)
	  (for (rnrs records syntactic helper counter) expand) ; MOSH: Access fields by index number, not symbols.
	  )

;MOSH

(define-syntax define-aux-keywords
  (syntax-rules ()
    ((_ key)
     (define-syntax key (lambda (e) 
			  (syntax-violation 'key "Invalid expression" e))))
    ((_ key0 key1 ...)
     (begin 
       (define-aux-keywords key0)
       (define-aux-keywords key1 ...)))))

(define-aux-keywords 
  fields 
  mutable 
  immutable 
  parent 
  protocol 
  sealed 
  opaque 
  nongenerative 
  parent-rtd)

  (define-syntax define-record-type
    (syntax-rules ()
     ((_ (rtd-name constructor-name predicate-name) clause ...)
      (define-record-type-helper0
       #t rtd-name constructor-name predicate-name clause ...))
     ((_ rtd-name clause ...)
      (define-record-type-helper0
       #f rtd-name #f #f clause ...))))

  (define-syntax define-record-type-helper0
    (lambda (x)
      (define (construct-record-type-definitions
               rtd-name constructor-name predicate-name      ; syntax objects
               type-name field-specs
               protocol sealed? opaque? uid
               parent parent-cd)
        (let ()

          (define (frob x)
            (cond ((identifier? x)
                   x)
                  ((pair? x)
                   (cons (frob (car x)) (frob (cdr x))))
                  (else
                   (datum->syntax rtd-name x))))

          #`(#,(frob #'define-record-type-helper1)
             #,(frob rtd-name)
             #,(frob constructor-name)
             #,(frob predicate-name)
             #,(frob type-name)
             #,(frob field-specs)
             #,(frob protocol)
             #,(frob sealed?)
             #,(frob opaque?)
             #,(frob uid)
             #,(frob parent)
             #,(frob parent-cd)
	     )))

      ; Searches for a clause beginning with the given symbol,
      ; returning the entire clause (as a syntax object) if found
      ; or #f if no such clause is found.

      (define (clauses-assq sym clauses)
        (syntax-case clauses ()
         (((x1 x2 ...) y ...)
          (if (and (identifier? #'x1)
                   (eq? sym (syntax->datum #'x1)))
              #'(x1 x2 ...)
              (clauses-assq sym #'(y ...))))
         ((y0 y1 y2 ...)
          (clauses-assq sym #'(y1 y2 ...)))
         (x
          #f)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object for its first element.

      (define (syntax-car x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'x0)))

      ; Given a syntax object that represents a non-empty list,
      ; returns the syntax object obtained by omitting the first
      ; element of that list.

      (define (syntax-cdr x)
        (syntax-case x ()
         ((x0 x1 ...)
          #'(x1 ...))))

      (define (complain)
        (syntax-violation 'define-record-type "illegal syntax" x))

      (syntax-case x ()
       ((_ explicit? rtd-name constructor-name predicate-name clause ...)
        (let* ((type-name (syntax->datum #'rtd-name))

;              (ignored (begin (display "got to here okay") (newline)))

               (clauses #'(clause ...))
               (fields-clause (clauses-assq 'fields clauses))
               (parent-clause (clauses-assq 'parent clauses))
               (protocol-clause (clauses-assq 'protocol clauses))
               (sealed-clause (clauses-assq 'sealed clauses))
               (opaque-clause (clauses-assq 'opaque clauses))
               (nongenerative-clause (clauses-assq 'nongenerative clauses))
               (parent-rtd-clause (clauses-assq 'parent-rtd clauses))

               (okay?
                (let (
                      (clauses (syntax->datum clauses))
                      (fields-clause (syntax->datum fields-clause))
                      (parent-clause (syntax->datum parent-clause))
                      (protocol-clause (syntax->datum protocol-clause))
                      (sealed-clause (syntax->datum sealed-clause))
                      (opaque-clause (syntax->datum opaque-clause))
                      (nongenerative-clause
                       (syntax->datum nongenerative-clause))
                      (parent-rtd-clause (syntax->datum parent-rtd-clause))
                     )

                  (and (symbol? type-name)
                       (if (syntax->datum #'explicit?)
                           (and (symbol? (syntax->datum #'constructor-name))
                                (symbol? (syntax->datum #'predicate-name)))
                           #t)
                       (or (not fields-clause)
                           (and
                            (list? fields-clause)
                            (for-all (lambda (fspec)
                                       (or (symbol? fspec)
                                           (and (list? fspec)
                                                (>= (length fspec) 2)
                                                (memq (car fspec)
                                                      '(immutable mutable))
                                                (symbol? (cadr fspec))
                                                (case (length fspec)
                                                 ((2) #t)
                                                 ((3)
                                                  (and (eq? (car fspec)
                                                            'immutable)
                                                       (symbol?
                                                        (caddr fspec))))
                                                 ((4)
                                                  (and (eq? (car fspec)
                                                            'mutable)
                                                       (symbol? (caddr fspec))
                                                       (symbol?
                                                        (cadddr fspec))))
                                                 (else #f)))))
                                     (cdr fields-clause))))
                       (or (not parent-clause)
                           (and (list? parent-clause)
                                (= (length parent-clause) 2)))
                       (or (not protocol-clause)
                           (and (list? protocol-clause)
                                (= (length protocol-clause) 2)))
                       (or (not sealed-clause)
                           (and (list? sealed-clause)
                                (= (length sealed-clause) 2)
                                (boolean? (cadr sealed-clause))))
                       (or (not opaque-clause)
                           (and (list? opaque-clause)
                                (= (length opaque-clause) 2)
                                (boolean? (cadr opaque-clause))))
                       (or (not nongenerative-clause)
                           (and (list? nongenerative-clause)
                                (or (null? (cdr nongenerative-clause))
                                    (symbol? (cadr nongenerative-clause)))))
                       (or (not parent-rtd-clause)
                           (and (list? parent-rtd-clause)
                                (= (length parent-rtd-clause) 3))))))

               (type-name-string (symbol->string type-name))
               (cname
                (if (symbol? (syntax->datum #'constructor-name))
                    #'constructor-name
                    (datum->syntax
                     #'rtd-name
                     (string->symbol
                      (string-append "make-" type-name-string)))))
               (pname
                (if (symbol? (syntax->datum #'predicate-name))
                    #'predicate-name
                    (datum->syntax
                     #'rtd-name
                     (string->symbol
                      (string-append type-name-string "?")))))
               (make-accessor-name
                (lambda (field-name)
                  (string->symbol
                   (string-append type-name-string
                                  "-"
                                  (symbol->string field-name)))))
               (make-mutator-name
                (lambda (field-name)
                  (string->symbol
                   (string-append type-name-string
                                  "-"
                                  (symbol->string field-name)
                                  "-set!"))))

               (field-specs
                (map/index1 (lambda (count fspec)
			      (cons count ;MOSH: Append index number for field spec
				    (let ((fspec (if (identifier? fspec)
						   #`(immutable #,fspec)
						   fspec)))
				      (cond ((= (length fspec) 2)
					     (let ((accessor-name
						     (make-accessor-name
						       (syntax->datum (cadr fspec)))))
					       (case (syntax->datum (car fspec))
						 ((immutable)
						  #`(immutable
						      #,(cadr fspec)
						      #,accessor-name
						      #f
						      ))
						 ((mutable)
						  #`(mutable
						      #,(cadr fspec)
						      #,accessor-name
						      #,(make-mutator-name
							  (syntax->datum (cadr fspec)))
						      )))))
					    ((= (length fspec) 3)
					     #`(#,(car fspec)
						#,(cadr fspec)
						#,(caddr fspec)
						#f))
					    (else 
					      #`(#,(car fspec)
						 #,(cadr fspec)
						 #,(caddr fspec)
						 #,(cadddr fspec)
						 ))))))
                     (if fields-clause
                         (syntax-cdr fields-clause)
                         '()))))
          (if (not okay?)
              (complain))
          (construct-record-type-definitions
           #'rtd-name
           cname
           pname
           type-name
           field-specs

           (and protocol-clause (cadr (syntax->datum protocol-clause)))
           (and sealed-clause (cadr (syntax->datum sealed-clause)))
           (and opaque-clause (cadr (syntax->datum opaque-clause)))

           (cond ((eq? nongenerative-clause #f)
                  #f)
                 ((null? (cdr nongenerative-clause))
                  (ex:generate-guid 'uid))
                 (else
                  (cadr nongenerative-clause)))
           (cond (parent-clause (cadr parent-clause))
                 (parent-rtd-clause (cadr parent-rtd-clause))
                 (else #f))
           (and parent-rtd-clause (caddr parent-rtd-clause))
	   )
	  )))))

  (define-syntax define-record-type-helper1
    (syntax-rules ()
     ((_ rtd-name constructor-name predicate-name
         type-name ((count mutable? field-name accessor mutator) ...)
         protocol sealed? opaque? uid
         parent parent-cd)
      (begin (def-rtd-name rtd-name type-name ((mutable? field-name) ...)
                           parent sealed? opaque? uid)
             (def-cd rtd-name type-name parent parent-cd protocol)
             (def-constructor rtd-name constructor-name)
             (def-predicate rtd-name predicate-name)
             (def-accessor rtd-name count accessor) ;MOSH
             ...
             (def-mutator rtd-name count mutator)
             ...))))

;MOSH
  (define-syntax def-rtd-name
    (syntax-rules ()
     ((_ rtd-name type-name (fieldspec ...) parent sealed? opaque? #f)
      (define rtd-name (make-record-type-descriptor 'type-name parent #f sealed? opaque? '#(fieldspec ...))))
     ((_ rtd-name type-name (fieldspec ...) parent sealed? opaque? uid)
      (define rtd-name (make-record-type-descriptor 'type-name parent 'uid sealed? opaque? '#(fieldspec ...))))))

  (define-syntax def-cd
    (syntax-rules ()
     ((_ rtd-name type-name #f #f protocol)
      (define %%RECORD-SYNTAX-IGNORED (preferred-cd-set! rtd-name (make-record-constructor-descriptor rtd-name #f protocol))))
     ((_ rtd-name type-name parent #f protocol)
      (define %%RECORD-SYNTAX-IGNORED (preferred-cd-set! rtd-name (make-record-constructor-descriptor rtd-name (preferred-cd parent) protocol))))
     ((_ rtd-name type-name parent parent-cd protocol)
      (define %%RECORD-SYNTAX-IGNORED (preferred-cd-set! rtd-name (make-record-constructor-descriptor rtd-name parent-cd protocol))))))

  (define-syntax def-constructor
    (syntax-rules ()

     ((_ rtd-name #f)
      (begin))

     ((_ rtd-name constructor-name)
      (define constructor-name (record-constructor (preferred-cd rtd-name))))))

  (define-syntax def-predicate
    (syntax-rules ()

     ((_ rtd-name #f)
      (begin))

     ((_ rtd-name predicate-name)
      (define predicate-name (record-predicate rtd-name))))) ;was rtd

  (define-syntax def-accessor
    (syntax-rules ()

     ((_ rtd-name count #f)
      (begin))

     ((_ rtd-name count accessor)
      (define accessor (record-accessor rtd-name count))))) ;was rtd

  (define-syntax def-mutator
    (syntax-rules ()

     ((_ rtd-name count #f)
      (begin))

     ((_ rtd-name count mutator)
      (define mutator (record-mutator rtd-name count))))) ;was rtd

  (define (record-type-descriptor rtd)
    rtd)

  (define (record-constructor-descriptor rtd)
    (preferred-cd rtd))

  ) ; rnrs records syntactic


