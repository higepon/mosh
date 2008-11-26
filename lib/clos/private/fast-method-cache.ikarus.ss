
(library (clos private fast-method-cache)

  (export invalidate-method-caches!
          make-cached-dispatch)

  (import (ikarus)
          (ikarus system $pairs)
          (ikarus system $vectors)
          (ikarus system $fx)
          (clos introspection)
          (clos bootstrap standard-classes))

  (define *cache-token* (list 'token))

  (define (invalidate-method-caches!)
    (set! *cache-token* (list 'token)))

  (define *min-cache-size* 64)

  (define (max-specializer-count generic)
    (apply max (map (lambda (method)
                      (length (method-specializers method)))
                    (generic-methods generic))))

  (define (compute-start-index class)
    ($fxlogand (pointer-value class) ($fx- *min-cache-size* 1)))

  (define (make-cached-dispatch generic handle-cache-miss)
    (let* ((spec-count (max-specializer-count generic))
           (table-size (+ *min-cache-size* spec-count 1))
           (table      (make-vector table-size #f)))
      (lambda (args)
        (if (or (null? args)
                ($fx= spec-count 0))
            (let ((token ($vector-ref table 0))
                  (proc  ($vector-ref table 1)))
              (or (and (eq? token *cache-token*) (procedure? proc) proc)
                  (let ((proc (handle-cache-miss args)))
                    ($vector-set! table 0 *cache-token*)
                    ($vector-set! table 1 proc)
                    proc)))
            (let* ((class (class-of ($car args)))
                   (start (compute-start-index class)))
              (or (and (eq? ($vector-ref table start) *cache-token*)
                       (let loop ((table table)
                                  (limit ($fx+ start spec-count)) 
                                  (index ($fx+ start 1)) 
                                  (class class) 
                                  (tail  (cdr args)))
                         (and (eq? ($vector-ref table index) class)
                              (if (or ($fx= index limit)
                                      (null? tail))
                                  (let ((proc 
                                         (vector-ref table ($fx+ index 1))))
                                    (and (procedure? proc) proc))
                                  (loop table 
                                        limit 
                                        ($fx+ index 1) 
                                        (class-of ($car tail)) 
                                        ($cdr tail))))))
                  (let ((proc (handle-cache-miss args)))
                    (fix-table! table start ($fx+ start spec-count) args proc)
                    proc)))))))

  (define (fix-table! table start limit args proc)
    (let loop ((table table)
               (index start)
               (limit limit)
               (proc  proc)
               (value *cache-token*)
               (args  args))
      ($vector-set! table index value)
      (if (or (null? args)
              ($fx= index limit))
          ($vector-set! table ($fx+ index 1) proc)
          (loop table 
                ($fx+ index 1) 
                limit 
                proc 
                (class-of ($car args)) 
                ($cdr args)))))
   
  ) ;; (clos private fast-method-cache)
