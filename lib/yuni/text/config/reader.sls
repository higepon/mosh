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

(define (phase1 x)
  (define (split-str chr x)
    (define (itr acc rest)
      (if (pair? rest)
        (let ((ch (car rest))
              (next (cdr rest)))
          (if (char=? ch chr)
            (cons (list->string (reverse acc))
                  (list->string next))
            (itr (cons ch acc) next)))
        (cons x '())))
    (itr '() (string->list x)))
  (let ((s (string->list x)))
    (cond
      ((not (pair? s))
       ;; An Empty Line..
       '())
      ((char=? #\[ (car s))
       ;; section
       (let* ((len (string-length x))
              (entry (substring x 1 (- len 1)))
              (spr (split-str #\space entry)))
         (let ((a (car spr))
               (b (cdr spr)))
           (if (null? b)
             (list a)
             (list a b)))))
      (else
        (split-str #\= x)))))

(define (string-list->config l)
  (define (valid? x)
    (and x
         (not (null? x))))
  (define (itr current acc cur rest)
    (if (pair? rest)
      (let ((e (car rest))
            (next (cdr rest)))
        (define (accum x)
          (if (null? x)
            acc
            (cons x acc)))
        (cond
          ((list? e)
           (if (valid? current)
             (itr e '() (cons (cons current (reverse acc))
                               cur)
                  next)
             (itr e '() cur next)))
          ((pair? e)
           (itr current (accum e) cur next))
          (else (itr current acc cur next))))
      (reverse (if (valid? current)
                 (cons (cons current (reverse acc))
                       cur)
                 cur))))
  (let ((lines (map phase1 (map cleanup-line l))))
    (itr #f '() '() lines)))

)
                 
