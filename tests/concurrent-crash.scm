(import (rnrs)
        (mosh)
        (mosh concurrent)
        (mosh test))

(let ([pid (spawn-link
            (lambda (arg) (car 3)) ;; this causes error
            '()
            '((rnrs) (mosh) (mosh concurrent)))])
  (receive
    [('exit why)
     (test-true (process-error? why))
     (let ([who (find who-condition? (simple-conditions (process-error why)))])
       (test-true who)
       (test-equal "car" (condition-who who)))]
    [x (fail (format "unexpected ~a\n" x))])
  (join! pid))

(let* ([pid (spawn-link
            (lambda (mutex)
              (sleep 300)
              (car 3)) ;; this causes error
            '()
            '((rnrs) (mosh) (mosh concurrent)))])
  (unlink pid)
  (test-eq 'timeout
    (receive
      [('exit why)
       (fail "not reached")]
      [after 50 'timeout]))
  (join! pid))

(test-results)
