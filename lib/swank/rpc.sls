;; swank-r6rs.sls --- Shareable code between swank-ikarus and swank-larceny
;;
;; Licence: public domain
;; Author: Helmut Eller
;;
;; This is a Swank server barely capable enough to process simple eval
;; requests from Emacs before dying.  No fancy features like
;; backtraces, module redefintion, M-. etc. are implemented.  Don't
;; even think about pc-to-source mapping.
;;
;; Despite standard modules, this file uses (swank os) and (swank sys)
;; which define implementation dependend functionality.  There are
;; multiple modules in this files, which is probably not standardized.
;;

;; Entry points for SLIME commands.
(library (swank rpc)
    (export connection-info interactive-eval
	    ;;compile-string-for-emacs 
	    throw-to-toplevel sldb-abort
	    operator-arglist buffer-first-change
	    create-repl listener-eval
            swank-require
            )
    (import (rnrs)
	    (rnrs eval)
	    (only (rnrs r5rs) scheme-report-environment)
	    (swank os)
	    (swank format)
	    (swank restarts)
	    (swank sys)
	    )
 
 (define (connection-info . _)
   `(,@'()
     :pid ,(getpid) 
     :package (:name ">" :prompt ">")
     :lisp-implementation (,@'() 
			   :name ,(implementation-name)
			   :type "R6RS-Scheme")))

 (define (interactive-eval string)
   (call-with-values 
       (lambda ()
	 (eval-in-interaction-environment (read-from-string string)))
     (case-lambda
      (() "; no value")
      ((value) (format "~s" value))
      (values (format "values: ~s" values)))))
 
 (define (throw-to-toplevel) (invoke-restart-by-name-or-nil 'toplevel))

 (define (sldb-abort) (invoke-restart-by-name-or-nil 'abort))
 
 (define (invoke-restart-by-name-or-nil name)
   (let ((r (find (lambda (r) (eq? (restart-name r) name))
		  (compute-restarts))))
     (if r (invoke-restart r) 'nil)))

 (define (create-repl target)
   (list "" ""))

 (define (listener-eval string)
   (call-with-values (lambda () (eval-region string))
     (lambda values `(:values ,@(map (lambda (v) (format "~s" v)) values)))))

 (define (eval-region string)
   (let ((sexp (read-from-string string)))
     (if (eof-object? exp)
	 (values)
	 (eval-in-interaction-environment sexp))))

 (define (read-from-string string)
   (call-with-port (open-string-input-port string) read))

 (define (operator-arglist . _) 'nil)
 (define (buffer-first-change . _) 'nil)

;; add
(define (swank-require sym) 'nil)

 )
