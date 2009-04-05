(import (rnrs))

(let ([prefix (call-with-port (open-input-file "./psyntax-prefix.txt") read)])
  (unless (and (symbol? prefix) (= (string-length (symbol->string prefix)) 1))
    (error 'prefix "prefix should be 1 character symbol" (list prefix)))
  (let* ([ch (+ (char->integer (string-ref (symbol->string prefix) 0)) 1)]
         [next-ch (if (< (char->integer #\z) ch) #\a (integer->char ch))])
    (call-with-port (open-output-file "./psyntax-prefix.txt") (lambda (x) (display next-ch x)))
    (display prefix)))
