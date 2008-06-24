(define (make-code-builder)
  '(builder))

(define (code-builder-put! cb x)
  (append! cb (list x)))

(define (code-builder-put-list! cb lst)
  (for-each
   (lambda (x)
     (code-builder-put! cb x))
   lst))

(define (code-builder-emit cb)
  (list->vector (cdr cb)))

;; code-builder synonyms
(define-macro (cput! x) `(code-builder-put! cb ,x))
(define-macro (cputs! lst) `(code-builder-put-list! cb ,lst))

(let1 cb (make-code-builder)
  (cput! 'a)
  (cput! 'b)
  (cputs! '(c d e))
  (print (code-builder-emit cb)))

