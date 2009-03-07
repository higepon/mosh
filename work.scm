(import
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs io ports)
    (rnrs files)
    (rnrs records procedural)
    (rnrs records inspection)
    (only (rnrs programs) exit)
    (except (mosh) library-path);; for get-command-line
    (rnrs lists)
    (only (rnrs conditions) condition? condition make-non-continuable-violation make-who-condition make-message-condition make-irritants-condition serious-condition? who-condition? message-condition? violation? irritants-condition? condition-who condition-message condition-irritants simple-conditions)
    (only (rnrs exceptions) raise with-exception-handler guard))

(define (condition-printer e port)
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
         (format port "   ~d. ~a" i (record-type-name rtd))
         (for-each
          (lambda (field)
            (display "       " port)
            (display (car field) port)
            (display ": " port)
            (write (cdr field) port)
            (newline port))
          (record->field-alist x))))
     (simple-conditions e)))

(define (map-with-index proc lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (if (null? lst)
        (reverse ret)
        (loop (+ i 1) (cdr lst) (cons (proc i (car lst)) ret)))))

(define (record->field-alist r)
  (define (ref rtd i x)
    (let ([val ((record-accessor rtd i) x)])
      (if (symbol? val)
          (ungensym val)
          val)))
  (let loop ([ret '()]
             [rtd (record-rtd r)])
    (cond
     [rtd
      (loop (append ret
      (map-with-index
       (lambda (i field)
         (cons field (ref rtd i r)))
       (vector->list (record-type-field-names rtd)))) (record-type-parent rtd))]
     [else ret])))

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))



(condition-printer (make-i/o-decoding-error 3) (current-output-port))
