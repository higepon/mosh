(define *correct-count* 0)
(define *failed-count* 0)

(define-syntax check
  (syntax-rules (=> ==>)
    ((check ec => desired-result)
     (check ec => (equal?) desired-result))
    ((check ec ==> desired-result)
     (check ec => (n-r5rs=) desired-result))
    ((check ec => (equal?) desired-result)
     (begin
       (newline)
       (write (quote ec))
       (newline)
       (let ((actual-result ec))
         (display "  => ")
         (write actual-result)
         (if (equal? actual-result desired-result)
             (begin
               (display " ; correct")
               (set! *correct-count* (r5rs:+ *correct-count* 1)) )
             (begin
               (display " ; *** failed ***, desired result:")
               (newline)
               (display "  => ")
               (write desired-result)
               (set! *failed-count* (r5rs:+ *failed-count* 1)) ))
         (newline) )))))

(define-syntax numerical
  (syntax-rules ()
    ((numerical ?rator ?rand ...)
     (?rator (r5rs->number ?rand) ...))))