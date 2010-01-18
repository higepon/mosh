;;;
;;; Runtime include file:
;;; Contains the minimal set of binding necessary
;;; for running a fully expanded program.
;;;


;;------------------------------------------------
;; syntactic object 
;;------------------------------------------------

;; MOSH: we use vectors for this purpose.


(define (make-identifier/debug name colors transformer-envs displacement maybe-library debug)
  (vector '*IDENTIFIER* name colors transformer-envs displacement maybe-library debug))

(define (make-identifier name colors transformer-envs displacement maybe-library)
  (make-identifier/debug name colors transformer-envs displacement maybe-library #f))

(define (identifier? x) 
  (and (true-vector? x) 
       (not (= (vector-length x) 0)) 
       (eq? '*IDENTIFIER* (vector-ref x 0))))
(define (id-name x) (vector-ref x 1))
(define (id-colors x) (vector-ref x 2))
(define (id-transformer-envs x) (vector-ref x 3))
(define (id-displacement x) (vector-ref x 4))
(define (id-maybe-library x) (vector-ref x 5))
(define (id-debug x) (vector-ref x 6)) ;NMOSH

;;------------------------------------------------
;; runtime library manager
;;------------------------------------------------

(define ex:unspecified (if #f #f))

(define (ex:make-library name envs exports imports builds visiter invoker build)
  ;(display (list 'I-M: name build))(newline)
  ;(display (list 'IMPORTS imports))(newline)
  ;(display (list 'BUILDS builds))(newline)
  (list name envs exports imports builds visiter invoker build #f #f))

(define (ex:library-name     lib) (car lib))
(define (ex:library-envs     lib) (cadr lib))
(define (ex:library-exports  lib) (caddr lib))
(define (ex:library-imports  lib) (cadddr lib))
(define (ex:library-builds   lib) (car (cddddr lib)))
(define (ex:library-visiter  lib) (car (cdr (cddddr lib))))
(define (ex:library-invoker  lib) (car (cdr (cdr (cddddr lib)))))
(define (ex:library-build    lib) (car (cdr (cdr (cdr (cddddr lib))))))
(define (ex:library-visited? lib) (car (cdr (cdr (cdr (cdr (cddddr lib)))))))
(define (ex:library-invoked? lib) (car (cdr (cdr (cdr (cdr (cdr (cddddr lib))))))))

(define (ex:library-visited?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cddddr lib))))) b))
(define (ex:library-invoked?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cdr (cddddr lib)))))) b))

(define ex:imported '())
(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (define (import-libraries imports builds phase)
    (for-each (lambda (import build)
                (let ((name   (car import))
                      (levels (cdr import)))
                  (for-each (lambda (level)
                              (import-library name build (+ phase level)))
                            levels)))
              imports
              builds)
    (values))
  (define (import-library name build phase)
    (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
        (let ((clibrary (ex:lookup-library name #f))) ;MOSH: pass recompile flag
	  (let ((library (if (and build (not (eq? build (ex:library-build clibrary))))
			   (assertion-violation 'import "Client was expanded against a different build of this library" name)
			   clibrary)) )
	    (import-libraries (ex:library-imports library) 
			      (ex:library-builds library)
			      phase)
	    (importer library phase ex:imported)
	    (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported))))))
  (import-libraries imports builds phase)
  )

(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           (lambda (library phase imported)
                             (if (and (= phase 0)
                                      (not (ex:library-invoked? library)))
                                 (begin 
                                   ((ex:library-invoker library))
                                   (ex:library-invoked?-set! library #t))))
                           'run))

(define ex:register-library! #f)
(define ex:lookup-library    #f)
(define nm:dump-library-table #f)
(let ((table '()))
  (set! ex:register-library! 
        (lambda (library)
          (set! table (cons library table))
          (set! ex:imported (filter (lambda (entry)
                                      (not (equal? (ex:library-name library) 
                                                   (car entry))))
                                    ex:imported))))
  (set! ex:lookup-library 
        (lambda (name recompile?)
          (let ((library (assoc name table)))
            (if (and (not recompile?) library)
                library
		(begin
		  (PCK 'LOOKING-UP... name)
		  (let ((fn (library-name->filename name)))
		    (cond ; we cannot use "=>" here..
		      (fn (ca-load fn recompile? name) ;MOSH: recompile flg
			  (ex:lookup-library name #f))
		      (else (assertion-violation 'lookup-library "Library not loaded" name)))))))))
  (set! nm:dump-library-table ; MOSH: for cached library
    (lambda (name)
      (map (lambda (e) (cons (ex:library-name e) (ex:library-build e))) table)
      ))
  )

;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (let ((error (lambda () 
                (assertion-violation 
                 'runtime.scm
                 "Attempt to use runtime instance of (core primitive-macros) for expansion.  Make sure expander.scm is loaded after runtime.scm."))))
   (ex:make-library
    '(core primitive-macros)
    ;; envs
    error
    ;; exports
    '()
    ;; imported-libraries
    '()
    ;; builds
    '()
    ;; visiter
    error
    ;; invoker
    (lambda () (values))
    ;; build
    'system)))
