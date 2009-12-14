(import (rnrs)
        (match)
        (only (srfi :1) first second third))

;; Todo Replace with (mosh file)
(define (file->sexp-list fn)
  (define (proc)
    (define (itr cur)
      (let ((r (read)))
    (if (eof-object? r)
      (reverse cur)
      (itr (cons r cur)))))
    (itr '()))
  (with-input-from-file fn proc))

;; for better readability / grep friendliness
(define (write/newline l)
  (define (el e)
    (write e) (newline))
  (for-each el l))
;;

(define (main args)
  (let ((free-vars (cdar (file->sexp-list (second args)))))
    (display "(define *free-vars-decl* (quote (")(newline)
    (write/newline (map (lambda (free-var)
              (match free-var
                 [(proc proc-body) proc]
                 [else free-var]))
            free-vars))
    (display ") ) )")(newline)))

(main (command-line))
