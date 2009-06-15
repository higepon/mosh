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
              (define (sleep msec)
                (let ([mutex (make-mutex)])
                  (mutex-lock! mutex)
                  (condition-variable-wait! (make-condition-variable) (make-mutex) msec)
                  (mutex-unlock! mutex)))
              (sleep 600) (car 3)) ;; this causes error
            '()
            '((rnrs) (mosh) (mosh concurrent)))])
  (define (sleep msec)
    (let ([mutex (make-mutex)])
      (mutex-lock! mutex)
      (condition-variable-wait! (make-condition-variable) (make-mutex) msec)
      (mutex-unlock! mutex)))
  (unlink pid)
  (test-eq 'timeout
    (receive
      [('exit why)
       (fail "not reached")]
      [after 50 'timeout]))
  (join! pid))

(test-results)
