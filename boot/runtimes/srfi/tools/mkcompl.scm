(import (rnrs))

(define (read-all fn)
  (define (reader p)
    (define (itr cur)
      (let ((e (read p)))
        (if (eof-object? e)
          (reverse cur)
          (itr (cons e cur)))))
    (itr '()))
  (call-with-input-file fn reader))

(define (filter-exports e)
  (define (itr-renames cur rest)
    (if (pair? rest)
      (itr-renames (cons (cadar rest) cur) (cdr rest))
      cur))
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
	    (d (cdr rest)))
	(if (list? a)
	  (itr (itr-renames cur (cdr a)) d)
	  (itr (cons a cur) d)))
      cur))
  (itr '() e))



(define exports
  (fold-left (lambda (cur lib) (append (filter-exports (cdaddr lib)) cur)) '() (read-all "r6rs.scm")))

(define (writelist l)
  (for-each (lambda (e) (display e)(newline)) l))

(writelist exports)
