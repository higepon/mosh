(library (yuni text config template)
         (export template-replace)
         (import (rnrs))

(define (split-template str)
  (define (itr mode cur acc rest)
    (if (pair? rest)
      (let ((chr (car rest))
            (next (cdr rest)))
        (case mode
          ((top)
           (cond
             ((char=? #\< chr)
              (itr 'templ (cons (list->string (reverse acc))
                                cur)
                   '()
                   next))
             (else
               (itr 'top cur (cons chr acc) next))))
          ((templ)
           (cond
             ((char=? #\> chr)
              (itr 'top (cons (list (list->string (reverse acc)))
                              cur)
                   '()
                   next))
             (else
               (itr 'templ cur (cons chr acc) next))))))
      (append
        (reverse cur)
        (if (pair? acc)
          (list (list->string (reverse acc)))
          '()))))
  (itr 'top '() '() (string->list str)))

(define (do-replace e env)
  (if (pair? e)
    (let ((name (car e)))
      (let ((b (assoc name env)))
        (if b (cdr b) #f)))
    e))

(define (template-replace str env)
  (define code (split-template str))
  (let ((l (map (lambda (e) (do-replace e env)) code)))
    (fold-left
      (lambda (cur e orig)
        (string-append cur e))
      "" l code)))

)
