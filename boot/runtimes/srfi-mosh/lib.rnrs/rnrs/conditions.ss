; based on mosh's original condition.ss by higepon
(library (rnrs conditions)
  (export

    &condition
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor

    &message
    make-message-condition
    message-condition?
    condition-message

    &warning
    make-warning
    warning?

    &serious
    make-serious-condition
    serious-condition?

    &error
    make-error
    error?

    &violation
    make-violation
    violation?

    &assertion
    make-assertion-violation
    assertion-violation?

    &irritants
    make-irritants-condition
    irritants-condition?
    condition-irritants

    &who
    make-who-condition
    who-condition?
    condition-who

    &non-continuable
    make-non-continuable-violation
    non-continuable-violation?

    &implementation-restriction
    make-implementation-restriction-violation
    implementation-restriction-violation?

    &lexical
    make-lexical-violation
    lexical-violation?

    &syntax
    make-syntax-violation
    syntax-violation?
    syntax-violation-form
    syntax-violation-subform

    &undefined
    make-undefined-violation
    undefined-violation?

    &i/o
    make-i/o-error
    i/o-error?

    &i/o-read
    make-i/o-read-error
    i/o-read-error?

    &i/o-write
    make-i/o-write-error
    i/o-write-error?

    &i/o-invalid-position
    make-i/o-invalid-position-error
    i/o-invalid-position-error?
    i/o-error-position

    &i/o-filename
    make-i/o-filename-error
    i/o-filename-error?
    i/o-error-filename

    &i/o-file-protection
    make-i/o-file-protection-error
    i/o-file-protection-error?

    &i/o-file-is-read-only
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?

    &i/o-file-already-exists
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?

    &i/o-file-does-not-exist
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?

    &i/o-port
    make-i/o-port-error
    i/o-port-error?
    i/o-error-port

    &i/o-decoding
    make-i/o-decoding-error
    i/o-decoding-error?

    &i/o-encoding
    make-i/o-encoding-error
    i/o-encoding-error?
    i/o-encoding-error-char

    &no-infinities
    make-no-infinities-violation
    no-infinities-violation?

    &no-nans
    make-no-nans-violation
    no-nans-violation?

    
    ;
    define-condition-type
    )
  (import
    (primitives condition-predicate condition simple-conditions condition? 
                condition-accessor

                record-type-rtd
                
                ;; for mosh 0.2.4 or later
                &condition
                condition
                simple-conditions
                condition?
                condition-predicate
                condition-accessor

                &message
                make-message-condition
                message-condition?
                condition-message

                &warning
                make-warning
                warning?

                &serious
                make-serious-condition
                serious-condition?

                &error
                make-error
                error?

                &violation
                make-violation
                violation?

                &assertion
                make-assertion-violation
                assertion-violation?

                &irritants
                make-irritants-condition
                irritants-condition?
                condition-irritants

                &who
                make-who-condition
                who-condition?
                condition-who

                &non-continuable
                make-non-continuable-violation
                non-continuable-violation?

                &implementation-restriction
                make-implementation-restriction-violation
                implementation-restriction-violation?

                &lexical
                make-lexical-violation
                lexical-violation?

                &syntax
                make-syntax-violation
                syntax-violation?
                syntax-violation-form
                syntax-violation-subform

                &undefined
                make-undefined-violation
                undefined-violation?

                &i/o
                make-i/o-error
                i/o-error?

                &i/o-read
                make-i/o-read-error
                i/o-read-error?

                &i/o-write
                make-i/o-write-error
                i/o-write-error?

                &i/o-invalid-position
                make-i/o-invalid-position-error
                i/o-invalid-position-error?
                i/o-error-position

                &i/o-filename
                make-i/o-filename-error
                i/o-filename-error?
                i/o-error-filename

                &i/o-file-protection
                make-i/o-file-protection-error
                i/o-file-protection-error?

                &i/o-file-is-read-only
                make-i/o-file-is-read-only-error
                i/o-file-is-read-only-error?

                &i/o-file-already-exists
                make-i/o-file-already-exists-error
                i/o-file-already-exists-error?

                &i/o-file-does-not-exist
                make-i/o-file-does-not-exist-error
                i/o-file-does-not-exist-error?

                &i/o-port
                make-i/o-port-error
                i/o-port-error?
                i/o-error-port

                &i/o-decoding
                make-i/o-decoding-error
                i/o-decoding-error?

                &i/o-encoding
                make-i/o-encoding-error
                i/o-encoding-error?
                i/o-encoding-error-char

                &no-infinities
                make-no-infinities-violation
                no-infinities-violation?

                &no-nans
                make-no-nans-violation
                no-nans-violation? )
    (for (rnrs records procedural) expand run)
    (for (rnrs records syntactic) expand run)
    (for (rnrs base) expand run)
    (for (rnrs syntax-case) expand run)
    (for (rnrs lists) expand run)
    )

  ; from psyntax
  (define-syntax define-condition-type
    (lambda (x)
      (define (mkname name suffix)
        (datum->syntax name
           (string->symbol
             (string-append
               (symbol->string (syntax->datum name))
               suffix))))
      (syntax-case x ()
        [(ctxt name super constructor predicate (field* accessor*) ...)
         (and (identifier? #'name)
              (identifier? #'super)
              (identifier? #'constructor)
              (identifier? #'predicate)
              (for-all identifier? #'(field* ...))
              (for-all identifier? #'(accessor* ...)))
         (with-syntax ([(aux-accessor* ...) (generate-temporaries #'(accessor* ...))])
            #'(begin
               (define-record-type (name constructor p?)
                  (parent (if super (record-type-rtd super) #f))
                  (fields (immutable field* aux-accessor*) ...)
                  (nongenerative)
                  (sealed #f) (opaque #f))
               (define predicate (condition-predicate (record-type-descriptor name)))
               (define accessor* (condition-accessor (record-type-descriptor name) aux-accessor*))
               ...
               ))])))

)
