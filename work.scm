(import (rnrs)
        (mosh control)
        (prefix (mosh cgi) cgi:)
        (srfi :48)
        (srfi :27)
        (srfi :26))

(define dictionary "./words.txt")
(define words (with-input-from-file dictionary read))

(define (register-word word)
  (define (write-dict words)
    (with-output-to-file dictionary (cut write words)))
  (if (member word words)
      '()
      (write-dict (cons word words))))

(define (redirect-to-alc word)
  (cgi:moved-temporarily-header (format "http://eow.alc.co.jp/~a/UTF-8/?ref=sa" word)))

(define (show-words words)
  (cgi:header)
  (random-source-randomize! default-random-source)
  (for-each (lambda (word) (format #t "<a href='http://eow.alc.co.jp/~a/UTF-8/?ref=sa'>~a</a><br>" word word)) (list-sort (lambda (x y) (even? (random-integer 100))) words)))

(let-values (([get-value method] (cgi:init)))
         (register-word (cgi:decode "word"))
  )
