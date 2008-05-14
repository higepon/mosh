; Testcase accessors, as used by both SchemeDoc and the the Scheme documentation tool of the 
; Scheme Unit test framework.
; 
; Correspond to Emacs Lisp accessors in  
; emacs-support/scheme-interpreter-unit-testing/scheme-interpreter-unit-testing.el

; ------------------------------------------------------------------------------------------------
; Selectors. Originally programmed in schemedoc.scm. 

; Return the type of a testcase. Currently either functional-testcase or error-functional-testcase
(define (test-case-type-of testcase-struct)
  (car testcase-struct))

; Return the id of a testcase
(define (test-case-name-of testcase-struct)
  (cadr testcase-struct))

; Return the encoding of a testcase. A list of one or two symbols depending on the type of the testcase.
; The first gives the encoding of the expression. The last (if present) give the encoding of the value.
; Symbols: raw-text or number-encoded.
(define (test-case-encoding-of testcase-struct)
  (car (cdr (cdr testcase-struct))))

; Return example info (a list) of the testcase"
(define (test-case-example-info-of testcase-struct)
  (list-ref testcase-struct 3))

; Return the expression of a testcase
(define (test-case-expression-of testcase-struct)
  (cond ((eq? (test-case-type-of testcase-struct) 'functional-testcase)
           (list-ref testcase-struct 5))
        ((eq? (test-case-type-of testcase-struct) 'error-functional-testcase)
           (list-ref testcase-struct 4))
        (else (laml-error "test-case-expression-of: Invalid access to expression of testcase."))))

; Return the value of a functional testcase"
(define (test-case-result-of testcase-struct)
  (cond ((eq? (test-case-type-of testcase-struct) 'functional-testcase)
           (list-ref testcase-struct 6))
        (else (laml-error "test-case-result-of: Invalid access to result of testcase."))))

; Return the assertion of a functional testcase
(define (test-case-assertion-of testcase-struct)
  (cond ((eq? (test-case-type-of testcase-struct) 'functional-testcase)
           (list-ref testcase-struct 4))
        (else (laml-error "test-case-assertion-of: Invalid access to assertion of testcase."))))

; Return the kind-of-exception of an error functional testcase
(define (test-case-kind-of-exception-of testcase-struct)
  (cond ((eq? (test-case-type-of testcase-struct) 'error-functional-testcase)
           (list-ref testcase-struct 5))
        (else (laml-error "test-case-kind-of-exception-of: Invalid access to kind-of-exception."))))

; ------------------------------------------------------------------------
; Extended selectors - doing decoding if necessary. 
; Naming of the functions:  real-... versions.

; Return the expression of a testcase. Decoded if necessary."
(define (real-test-case-expression-of testcase-struct)
  (let ((enc (test-case-encoding-of testcase-struct)))
    (cond ((eq? (car enc) 'raw-text)
             (test-case-expression-of testcase-struct))
          ((eq? (car enc) 'number-encoded)
             (number-string-decode (test-case-expression-of testcase-struct)))
          (else (laml-error "real-test-case-expression-of: Unknown encoding.")))))

; Return the value of a functional testcase. Decoded if necessary"
(define (real-test-case-result-of testcase-struct)
  (if (eq? 'functional-testcase (test-case-type-of testcase-struct))
      (let ((enc (test-case-encoding-of testcase-struct)))
	(cond ((eq? (cadr enc) 'raw-text)
	       (test-case-result-of testcase-struct))
	      ((eq? (cadr enc) 'number-encoded)
	       (number-string-decode (test-case-result-of testcase-struct)))
	      (else (error "real-test-case-result-of: Unknown encoding."))))
      (laml-error "real-test-case-result-of applied on wrong type of test case")))

; Decode the (number-)encoded string encoded-str to a string and return it. 
; The result is a decoded string.
(define (number-string-decode encoded-str)
  (list->string (map (compose as-char as-number) (string-to-list encoded-str (list (as-char 32) #\( #\))))))

 