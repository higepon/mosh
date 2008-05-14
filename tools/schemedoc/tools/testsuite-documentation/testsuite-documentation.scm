; Testsuite documentation.
; Assumes that laml.scm is loaded on beforehand.
; The procedure document-testsuite is the entrance.

(laml-style "simple-xhtml1.0-transitional-validating")
(lib-load "xhtml1.0-convenience.scm")
(lib-load "scheme-pretty-printing.scm")
(lib-load "testcase-access.scm")


; ------------------------------------------------------------------------------------------------
(define laml-generation-meta (meta 'name "Generator" 'content "LAML"))
(define meta-props (list 'http-equiv "Content-Type" 'content "text/html; charset=iso-8859-1"))
(define html-props (list 'xmlns "http://www.w3.org/1999/xhtml"))

; ------------------------------------------------------------------------------------------------
; Constants

; The maximum length of simple, flat rendering of a test expression or test result.
; Longer expressions are pretty printed.
(define max-length-length-expression 50)


; ------------------------------------------------------------------------------------------------

(define inline-css-style-contribution (read-text-file (string-append laml-dir "tools/testsuite-documentation/" "stylesheets/testsuite-documentation-original.css")))
  

;; Write documentation about all testcases in the file, addressed by full-testcases-file-path, to 
;; a documentation.html file.
(define (document-testsuite full-testcases-file-path)
  (let* ((full-setup-file-path (string-append (file-name-initial-path full-testcases-file-path) "setup.scm"))
         (full-teardown-file-path (string-append (file-name-initial-path full-testcases-file-path) "teardown.scm"))
         (testcase-list (file-read-all full-testcases-file-path))
         (name-of-testsuite (name-of-testsuite-from-testcases-file-path full-testcases-file-path))
         (ttl (string-append "Documentation of the testsuite: " name-of-testsuite))
        )
    (write-xml '(raw prolog)
      (html html-props
        (head 
         (meta meta-props) laml-generation-meta
         (title ttl)
         (style 'type "text/css" inline-css-style-contribution)
        )
        (body 
          (h1 ttl)

          (b (font 'size 4 "setup.scm:")) (br)
          (indent-pixels 10
            (div 'css:font-size "80%" 
                 (pre (b 'css:color "blue" (read-text-file full-setup-file-path)))
                 (vertical-space 1)))

          (table 'border 0
           (map document-testcase testcase-list))

          (vertical-space 1) 

          (b (font 'size 4 "teardown.scm:")) (br)
          (indent-pixels 10
            (div 'css:font-size "80%"
                 (pre (b 'css:color "blue" (read-text-file full-teardown-file-path)))
                 ))

          (vertical-space 1)
          (font 'size 1 'color (rgb-color-encoding red) (when-generated))
        )
       )
       (string-append (file-name-initial-path full-testcases-file-path) "documentation.html"))))

(define (document-testcase testcase)
  (let ((testcase-type (test-case-type-of testcase)))
    (cond ((eq? testcase-type 'functional-testcase)
             (document-functional-testcase testcase))
          ((eq? testcase-type 'error-functional-testcase)
             (document-error-functional-testcase testcase))
          (else (laml-error "document-testcase: Unknown type of testcase" testcase-type)))))


(define (document-functional-testcase testcase)
  (let* ((id (test-case-name-of testcase))
         (expr (real-test-case-expression-of testcase))
         (long-expr (> (string-length expr) max-length-length-expression))
         (res (real-test-case-result-of testcase))
         (long-res (> (string-length res) max-length-length-expression))
         (assertion (test-case-assertion-of testcase))
         (example-info (test-case-example-info-of testcase))
         (example-message (if (and (list? example-info) (> (length example-info) 0) (eq? (first example-info) 'use-as-example))
                              ""
                             "Not used as example"))
         (time-seconds (emacs-lisp-time-stamp-to-second-count id)))
 
   ; Assignment of pretty printing variable:
   (set! prefered-maximum-width max-length-length-expression) 

   (list
    (tr 'class "testcase"
      (td 'width "70px" 'class "testcase-id" "Testcase" id)
      (td 'width "200px" 'class "testcase-time" (date-time-one-string time-seconds))
      (td 'width "600px" 'class "testcase-assertion" "Equality predicate: " assertion  (horizontal-space 5) (em example-message)))

    (tr 
       (td "")
       (if (and (not long-expr) (not long-res))
           (td 'colspan 2 
               (pre (span 'class "testcase-expression" expr)  
                    (horizontal-space 1) (b (font 'color (rgb-color-encoding 255 0 0) "=>")) (horizontal-space 1)
                    (span 'class "testcase-result" res)))
           (td 'colspan 2 
               (pre (span 'class "testcase-expression" (if long-expr (pretty-print-lisp-string expr) expr ))  
                    (horizontal-space 1) (b (font 'color (rgb-color-encoding 255 0 0) "=>")) (horizontal-space 1))
               (pre (span 'class "testcase-result" res))))

    )

    (tr 
      (td 'colspan 3 (horizontal-space 1)))

   )))

(define (document-error-functional-testcase testcase)
  (let* ((id (test-case-name-of testcase))
         (expr (real-test-case-expression-of testcase))
         (long-expr (> (string-length expr) max-length-length-expression))
         (example-info (test-case-example-info-of testcase))
         (example-message (if (and (list? example-info) (> (length example-info) 0) (eq? (first example-info) 'use-as-example))
                              ""
                             "Not used as example"))
         (time-seconds (emacs-lisp-time-stamp-to-second-count id)))
 
   ; Assignment of pretty printing variable:
   (set! prefered-maximum-width max-length-length-expression) 

   (list
    (tr 'class "testcase"
      (td 'width "70px" 'class "testcase-id" "Error testcase" id)
      (td 'width "200px" 'class "testcase-time" (date-time-one-string time-seconds))
      (td 'width "600px" 'class "testcase-assertion" (em example-message)))

    (tr 
       (td "")
       (if (not long-expr)
           (td 'colspan 2 
               (pre (span 'class "testcase-expression" expr)  
                    (horizontal-space 1) (b (font 'color (rgb-color-encoding 255 0 0) ":")) (horizontal-space 1)
                    (span 'class "testcase-result" "ERROR")))
           (td 'colspan 2 
               (pre (span 'class "testcase-expression" (if long-expr (pretty-print-lisp-string expr) expr ))  
                    (horizontal-space 1) (b (font 'color (rgb-color-encoding 255 0 0) ":")) (horizontal-space 1))
               (pre (span 'class "testcase-result" "ERROR"))))

    )

    (tr 
      (td 'colspan 3 (horizontal-space 1)))

   )))
   

(define (emacs-lisp-time-stamp-to-second-count el-time-string)
  (let* ((sp (split-point #\- el-time-string))
         (high (as-number (substring el-time-string 0 sp)))
         (low  (as-number (substring el-time-string (+ sp 1) (string-length el-time-string)))))
    (emacs-lisp-time-to-second-count (list high low))))


; The parameter testsuite-file-path is a full file path to the testcases file a testsuite directory.
; Given the naming conventions of such directories, return the name of the test suite.
(define (name-of-testsuite-from-testcases-file-path testcases-file-path)
  (let ((dir-name (directory-leave-name (file-name-initial-path testcases-file-path))))
     (substring dir-name 0 (- (string-length dir-name) 5))))

  
