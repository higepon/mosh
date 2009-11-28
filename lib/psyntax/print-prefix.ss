(import (rnrs)
        (mosh file))

;;; We use 'A', 'B' or 'C' for prefix.
(let ([prefix (call-with-port (open-input-file "./psyntax-prefix.txt") read)])
  (unless (and (symbol? prefix) (= (string-length (symbol->string prefix)) 1))
    (error 'prefix "prefix should be 1 character symbol" (list prefix)))
  (let* ([ch (+ (char->integer (string-ref (symbol->string prefix) 0)) 1)]
         [next-ch (if (< (char->integer #\C) ch) #\A (integer->char ch))])
    ;(call-with-port (open-output-file "./psyntax-prefix.txt") (lambda (x) (display next-ch x)))
    (write-to-file "./psyntax-prefix.txt" next-ch)
    (display prefix)))
