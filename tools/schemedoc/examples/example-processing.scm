; LAML Process all files in lst
(define (process-list . lst)
 (for-each
   (lambda (f)
    (display f) (newline) (newline)
    (laml f)
   )
   lst))

; Return the list of all laml files, excluded the laml file of this script.
(define (all-laml-files . optional-parameter-list)
 (let* ((extension (optional-parameter 1 optional-parameter-list "laml"))
        (file-list (directory-list (startup-directory))))
  (filter (lambda (f) (not (equal? (source-filename-without-extension) (file-name-proper f))))
    (filter (lambda (f) (equal? (file-name-extension f) extension)) file-list))))

; Return the list of all leno files, excluded the laml file of this script.
(define (all-leno-files)
 (let ((file-list (directory-list (startup-directory))))
  (filter (lambda (f) (not (equal? (source-filename-without-extension) (file-name-proper f))))
    (filter (lambda (f) (equal? (file-name-extension f) "leno")) file-list))))




(define (process-part part)
  (laml-cd part)
  (display "----------------------------------------------------------------") (newline)
  (display part) (newline)
  (if (file-exists? (in-startup-directory "process-all.laml"))
      (laml "process-all")
      (laml-error "No process-all.laml file in" (startup-directory)))
  (laml-cd ".."))

