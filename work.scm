(import (rnrs)
        (mosh))

(define (map-with-index proc lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (if (null? lst)
        (reverse ret)
        (loop (+ i 1) (cdr lst) (cons (proc i (car lst)) ret)))))

(define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

(define (rpad str pad n)
  (let ([rest (- n (string-length (format "~a" str)))])
    (let loop ([rest rest]
               [ret (format "~a" str)])
      (if (<= rest 0)
          ret
          (loop (- rest 1) (string-append ret pad))))))

(define (condition-printer e port)
    (define max-condition-len (apply max (map (lambda (c) (string-length (symbol->string (record-type-name (record-rtd c))))) (simple-conditions e))))
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
        (format port "   ~d. ~a" i (rpad (symbol->string (record-type-name rtd)) " " max-condition-len))
         (for-each
          (lambda (field)
            (display "       " port)
           (display (car field) port)
           (display ": " port)
           (write (cdr field) port)
            (newline port))
          (record->field-alist x))))
     (simple-conditions e)))

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


(guard [c
        (#t (condition-printer c (current-error-port)))

        ]
(bytevector->string #vu8(97 #xff 98 99) (make-transcoder (utf-8-codec) (native-eol-style) (error-handling-mode raise))))
