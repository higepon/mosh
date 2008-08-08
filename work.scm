(call/cc
 (lambda (k)
   (with-exception-handler (lambda (x)
                             (display "condition: ")
                             (write x)
                             (newline)
                             (k 'exception))
     (lambda ()
       (hashtable-clear! (hashtable-copy (make-hashtable zero? zero?) #f))
       ))))

(display "done")


(test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                        (make-expected-exception)])
                    expr)
             (make-expected-exception))

(test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                        (make-expected-exception)])
                    (hashtable-clear! (hashtable-copy (make-hashtable zero? zero?) #f)))
             (make-expected-exception))

(run-test 'expr
                   (catch-exns (lambda () expr))
                   expected)
(test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                        (make-expected-exception)])
                    (hashtable-clear! (hashtable-copy (make-hashtable zero? zero?) #f)))
             (make-expected-exception))
