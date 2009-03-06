(import (rnrs)
        (mosh))

(utf8->string #vu8(#xff))

;; (define (for-each-with-index proc lst)
;;     (do ((i 1 (+ i 1)) ; start with 1
;;          (lst lst (cdr lst)))
;;         ((null? lst))
;;       (proc i (car lst))))

;; (define (conditioon-printer e port)
;;     (define (ref rtd i x)
;;       (let ([val ((record-accessor rtd i) x)])
;;         (if (symbol? val)
;;             (ungensym val)
;;             val)))
;;     (display " Condition components:\n" port)
;;     (for-each-with-index
;;      (lambda (i x)
;;        (let ([rtd (record-rtd x)])
;;          (format port "   ~d. ~a" i (record-type-name rtd))
;;          (let ([v (record-type-field-names rtd)])
;;            (case (vector-length v)
;;              [(0) (newline port)]
;;              [(1)
;;               (display ": " port)
;;               (write (ref rtd 0 x) port)
;;               (newline port)]
;;              [else
;;               (display ":\n" port)
;;               (let f ([i 0])
;;                 (unless (= i (vector-length v))
;;                   (display "       " port)
;;                   (display (vector-ref v i) port)
;;                   (display ": " port)
;;                   (write (ref rtd i x) port)
;;                   (newline port)
;;                   (f (+ i 1))))]))))
;;      (simple-conditions e)))

;; (define (condition-printer e port)
;;     (display " Condition components:\n" port)
;;     (for-each-with-index
;;      (lambda (i x)
;;        (let ([rtd (record-rtd x)])
;;          (format port "   ~d. ~a" i (record-type-name rtd))
;;          (for-each
;;           (lambda (field)
;;             (display "       " port)
;;             (display (car field) port)
;;             (display ": " port)
;;             (write (cdr field) port)
;;             (newline port))
;;           (record->field-alist e))))
;;      (simple-conditions e)))

;; (define (map-with-index proc lst)
;;   (let loop ([i 0]
;;              [lst lst]
;;              [ret '()])
;;     (if (null? lst)
;;         (reverse ret)
;;         (loop (+ i 1) (cdr lst) (cons (proc i (car lst)) ret)))))

;; (define (record->field-alist r)
;;   (define (ref rtd i x)
;;     (let ([val ((record-accessor rtd i) x)])
;;       (if (symbol? val)
;;           (ungensym val)
;;           val)))
;;   (let loop ([ret '()]
;;              [rtd (record-rtd r)])
;;     (cond
;;      [rtd
;;       (loop (append ret
;;       (map-with-index
;;        (lambda (i field)
;;          (cons field (ref rtd i r)))
;;        (vector->list (record-type-field-names rtd)))) (record-type-parent rtd))]
;;      [else ret])))



;; (define c (make-i/o-decoding-error 3))

;; (record->field-alist c)

;; (condition-printer c (current-output-port))

;; ;(display (record-type-field-names  (record-rtd (make-i/o-decoding-error 3))))
