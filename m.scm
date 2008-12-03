;; ;; (define-macro (do2 . sexp)
;; ;;   (match sexp
;; ;;     [(((var) ...)
;; ;;          (test expr ...))
;; ;;      `(3)]
;; ;;     ))

;; ;; (display (do2 ((i))
;; ;;     ((= i 5) i)))

;; 	(define-macro (do2 . sexp)
;; 	  (match sexp
;; 	    [(((var init step ...) ...)
;; 	         (test expr ...)
;; 	       command ...)
;; 	     `(letrec
;; 	       ((loop
;; 	         (lambda (,@var)
;; 	           (if ,test
;; 	               (begin
;; 	                 #f ; avoid empty begin
;; 	                 ,@expr)
;; 	               (begin
;; 	                 ,@command
;; 	                 (loop ,@(map (lambda (v s) `(do2 "step" ,v ,@s)) var step)))))))
;; 	        (loop ,@init))]
;; 	    [("step" x)
;; 	     x]
;; 	    [("step" x y)
;; 	     y]
;; 	    [else
;; 	     (syntax-error "malformed do2 on mosh")]))
	
;; 	(display (do2 ((i 0 (+ i 1)))
;; 	    ((= i 5) i)))

(display (letrec ((a (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (a 0)))
