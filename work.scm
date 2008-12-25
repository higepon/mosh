(import (rnrs)
        (mosh string))

(define (read-write-invariant? obj)
  (call-with-port (open-string-input-port
                   (call-with-string-output-port
                    (lambda (p) (write obj p))))
    (lambda (p) (equal? obj (read p)))))

(define-syntax test
  (syntax-rules ()
    ((_ obj)
     (begin
       (write obj)
       (display ": ")
       (display (read-write-invariant? obj))
       (newline)))))

;テスト結果、

(test #f)                               ; -> #f: #t
(test 1/2)                              ; -> 1/2: #t
(test -12)                              ; -> -12: #t
(test 2+5i)                             ; -> 2+5i #t
(test 3.141592653589793)                ; -> 3.141592653589793: #t
(test +inf.0)                           ; -> +inf.0: #t
(test +nan.0)                           ; -> +nan.0: #t
(test #\nul)                            ; -> #\nul: #t
(test #\linefeed)                       ; -> #\linefeed: #t
(test #\vtab)                           ; -> #\vtab: #t
(test #\page)                           ; -> #\page: #t
(test #\return)                         ; -> #\return: #t
(test #\esc)                            ; -> #\esc: #t
(test #\delete)                         ; -> #\delete: #t
(test "\a")                             ; -> "\a": #t
(test "\b")                             ; -> "\b": #t
(test "\t")                             ; -> "\t": #t
(test "\v")                             ; -> "\v": #t
(test "\r")                             ; -> "\r": #t
(test "ab\
 c")                                    ; -> "abc": #t
(test 'abc)                             ; -> abc: #t
(test '\x40;)                           ; -> \x40;: #t
(test 'a\x20;c)                         ; -> a\x20;c: #t
(test '(1 2 3))                         ; -> (1 2 3): #t
(test '(1 #\2 "3"))                     ; -> (1 #\2 "3"): #t
(test '#(a b c))                        ; -> #(a b c): #t
(test '#((a b) c))                      ; -> #((a b) c): #t
(test #vu8(0 128 255))                  ; -> #vu8(0 128 255): #t
