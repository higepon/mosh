 (library (cgi)
  (export init encode decode escape moved-temporarily-header header)
  (import (rnrs)
          (only (system) get-environment-variable)
          (only (mosh regexp) regexp-replace-all)
          (only (mosh bytevector) bytevector-for-each)
          (only (mosh string) string-split format call-with-string-io)
          (only (mosh number) digit->integer)
          (only (srfi :1) second)
          )

(define header-out? #f)
(define (escape text)
  (fold-right (lambda (x y) (regexp-replace-all (car x) y (cdr x)))
        text
        '((#/</ . "&lt;")
          (#/>/ . "&gt;")
          (#/\"/ . "&quot;")
          (#/[^\\]'/ . "'")
          (#/&/ . "&amp;")
          )))

(define (encode text)
  (call-with-string-output-port
   (lambda(out)
     (bytevector-for-each
      (call-with-bytevector-output-port
       (lambda (port)
         (display text port))
       (make-transcoder (utf-8-codec)))
      (lambda (b)
        (display "%" out)
        (display (number->string b 16) out))))))

(define (decode s)
  (call-with-string-io
   s
   (lambda (in out)
     (let ([p (transcoded-port
              (make-custom-binary-input-port
               "cgi decode"
               (lambda (bv start count)
                 (let ([read-byte (lambda ()
                                   (let ([c (read-char in)])
                                     (cond
                                      [(eof-object? c) (eof-object)]
                                      [(eq? #\+ c) 32]
                                      [(eq? #\% c)
                                       (let ([a (digit->integer (read-char in) 16)]
                                             [b (digit->integer (read-char in) 16)])
                                         (+ (* a 16) b))]
                                      [else
                                       (char->integer c)])))])
                   (let loop ([size 0]
                              [b (read-byte)])
                     (cond
                      [(eof-object? b) size]
                      [else
                       (bytevector-u8-set! bv (+ start size) b)
                       (if (>= (+ size 1) count)
                           '()
                           (loop (+ size 1) (read-byte)))]))))
               #f #f #f)
              (make-transcoder (utf-8-codec)))])
       (let loop ([c (read-char p)])
         (cond
          [(eof-object? c) '()]
          [else
           (display c out)
           (loop (read-char p))]))))))


(define (request-method)
  (and (get-environment-variable "REQUEST_METHOD")
       (if (string=? (get-environment-variable "REQUEST_METHOD") "GET") 'GET 'POST)))

(define (request-body method)
  (case method
    [(POST)
     (cond
      [(get-environment-variable "COMMAND_LINE") ;; for debug
       (get-environment-variable "QUERY_STRING")]
      [else
       (let* ([content-length (get-environment-variable "CONTENT_LENGTH")]
              [len            (if content-length (string->number content-length) 0)])
         (if (= 0 len)
             ""
             (utf8->string (get-bytevector-n (standard-input-port) len))))])]
    [else
     (get-environment-variable "QUERY_STRING")]))

(define (parse-query-string input)
  (if (or (not (string? input)) (string=? "" input)) '()
  (fold-right
   (lambda (a b)
     (let [(params (string-split a #\=))]
       (cons (list (car params) (cadr params)) b)))
   '()
   (string-split input #\&))))

(define (init . body)
  (let* ([content-body (if (pair? body) (car body)  (request-body (request-method)))]
         [parsed (parse-query-string content-body)])
    (values
     (lambda (key)
       (let ([value (assoc key parsed)])
         (if value
             (second value)
             #f)))
     request-method)))

(define (header)
  (unless header-out?
    (display "Status: 200 OK\nContent-type: text/html; charset=utf-8\n\n")
    (set! header-out? #t)))

(define (moved-temporarily-header url)
   (format #t "Status: 302 Moved Temporarily\nLocation: ~a\n\n" url)
   (set! header-out? #t))

)
