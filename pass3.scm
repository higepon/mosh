(use util.match)
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
(define-macro (cput! cb x) `(code-builder-put! ,cb ,x))

;(define-macro (cputs! lst) `(code-builder-put-list! cb ,lst))

(define-macro (cput! cb . more)
  (match more
    [() '()]
    [(x . y)
     `(begin
        (code-builder-put! ,cb ,x)
        (cput! ,cb ,@y))]))

(let1 cb (make-code-builder)
  (cput! cb 'a)
  (cput! cb 'c 'd 'e)
  (print (code-builder-emit cb)))


(fold (lambda (a accum) (format #t "a=~a accum=~a\n" a accum) (+ a accum)) 0 '(1 2 3 4))
