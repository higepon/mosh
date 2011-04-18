(library (yuni text config reader)
         (export string-list->config)
         (import (rnrs))


;; looks silly, but we need this in very early stage..
(define (cleanup-line str)
  (define (char-comment? chr)
    (or (char=? #\# chr)
        (char=? #\; chr)))
  (define (itr mode cur rest)
    (if (pair? rest)
      (let ((chr (car rest))
            (next (cdr rest)))
        (case mode
          ((top)
           (cond
             ((char-whitespace? chr)
              (itr 'top '() next))
             ((char=? #\[ chr)
              (itr 'section (list chr) next))
             ((char-comment? chr)
              '())
             (else
               (itr 'key cur rest))))
          ((key)
           (cond
             ((char-whitespace? chr)
              (itr 'key cur next))
             (else
              (itr (if (char=? #\= chr) 'value-top 'key)
                   (cons chr cur) next))))
          ((value-top)
           (cond
             ((char-whitespace? chr)
              (itr 'value-top cur next))
             (else (itr 'value cur rest))))

          ((value)
           (cond
             ((char=? #\" chr)
              (itr 'value-string cur next))
             ((char-comment? chr)
              cur)
             (else
               (itr 'value (cons chr cur) next))))
          ((value-string)
           (cond
             ((char=? #\\ chr)
              (itr 'value-string-escape cur next))
             ((char=? #\" chr)
              (itr 'value cur next))
             (else
               (itr 'value-string (cons chr cur) next))))
          ((value-string-escape)
           (cond
             ((or (char=? #\" chr) (char=? #\\ chr))
              (itr 'value-string (cons chr cur) next))
             (else 
               ;; FIXME: Complain here..
               (itr 'value-string cur next))))
          ((section)
           (cond
             ((char=? #\" chr)
              (itr 'section-string cur next))
             ((char=? #\] chr)
              (cons chr cur))
             (else
               (itr 'section (cons chr cur) next))))
          ((section-string)
           (cond
             ((char=? #\\ chr)
              (itr 'section-string-escape cur next))
             ((char=? #\" chr)
              (itr 'section cur next))
             (else
               (itr 'section-string (cons chr cur) next))))
          ((value-section-escape)
           (cond
             ((or (char=? #\" chr) (char=? #\\ chr))
              (itr 'section-string (cons chr cur) next))
             (else 
               ;; FIXME: Complain here..
               (itr 'section-string cur next))))))
      cur))

  (list->string
    (reverse
      (itr 'top '() (string->list str)))))

(define (string-list->config l)
  (let ((lines (map cleanup-line l)))
    lines))

)
                 
