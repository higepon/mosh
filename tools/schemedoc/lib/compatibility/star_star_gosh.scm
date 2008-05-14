(use file.util)

;; Return the current time in seconds
(define current-time sys-time)
(define current-seconds sys-time)
(define (current-process-milliseconds) 0)

;; Sort list using the comparison predicate
(define (sort-list list com) (sort list com))

; ---------------------------------------------------------------------------------------------------

;; Make a new directory, new-dir, in the directory path (first parameter).
;; The parameter in-directory-path ends in a slash.
(define (make-directory-in-directory in-directory-path new-dir)
  (make-directory* (string-append in-directory-path new-dir)))

(define directory-exists? file-exists?)
(define delete-file sys-unlink)

; ---------------------------------------------------------------------------------------------------

(define (eval-cur-env e) (eval e (interaction-environment)))

; -----------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.


;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght four or #f if no command line activation exists.
;; The first element must be the symbol laml (a tag).
;; Element number two must be the laml source file name (without extension and initial path).
;; Element number three must be a slash terminated, full directory path (with forward slashes), in which the source file resides.
;; Element number four must be a list of program parameters.
;; This function must be redefined in Scheme-system/OS/platform dependent compatibility file. 
(define (laml-canonical-command-line) `(laml ,@*argv* #f))

(define (fake-startup-parameters source-file startup-dir . optional-parameter-list) '())

(define-syntax load
  (syntax-rules()
    ((_ file)
     (call-with-input-file file load-from-port :encoding 'latin1))))



