 (library (mosh cgi)
  (export init encode decode escape moved-temporarily-header header unauthorized-header call-with-cgi get-cookies not-found-header)
  (import (rnrs)
          (only (system) get-environment-variable)
          (shorten)
          (mosh control)
          (srfi :98)
          (irregex)
          (match)
          (only (mosh) digit->integer  string-split format call-with-string-io bytevector-for-each  regexp-replace-all assoc-ref)
          )

(define (call-with-cgi proc)
  (guard (e
          (#t
           (header)
           (write e)
           (raise e)))
         (let-values (([get-param request-method] (init)))
           (proc get-param request-method))))

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
      (lambda (b)
        (display "%" out)
        (display (number->string b 16) out))
      (call-with-bytevector-output-port
       (lambda (port)
         (display text port))
       (make-transcoder (utf-8-codec)))))))

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
                       (when (or (< b 0) (> b 255))
                         (error "cgi decode" "malformed encoded data" s))
                       (bytevector-u8-set! bv (+ start size) b)
                       (if (>= (+ size 1) count)
                           (+ size 1)
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
     (match-lambda*
      [(key)
       (let ([value (assoc (if (symbol? key) (symbol->string key) key) parsed)])
         (if value
             (cadr value)
             #f))]
      [else parsed])
     request-method)))

(define (header . alist*)
  (unless header-out?
    (display "Status: 200 OK\n")
    (for-each
     (^(key+value)
       (format #t "~a: ~a\n" (car key+value) (cdr key+value)))
     alist*)
    (display "Content-type: text/html; charset=utf-8\n\n")
    (set! header-out? #t)))

(define (moved-temporarily-header url . alist*)
   (format #t "Status: 302 Moved Temporarily\nLocation: ~a\n" url)
   (for-each
    (^(key+value)
      (format #t "~a: ~a\n" (car key+value) (cdr key+value)))
     alist*)
   (display "\n\n")
   (set! header-out? #t))

(define (unauthorized-header)
   (format #t "Status: 401 Unauthorized\n\n")
   (set! header-out? #t))

(define (not-found-header)
  (format #t "Status: 404 Not Found\n\n")
  (set! header-out? #t))

(define (get-cookies)
  (aif (assoc-ref (get-environment-variables) "HTTP_COOKIE")
       (let loop ([cookies-string it]
                  [cookies '()])

         (cond
          [(irregex-search "([^=]+)=([^;]+);?\\s?(.*)" cookies-string) =>
           (^m
            (loop (irregex-match-substring m 3)
                  (cons (cons (irregex-match-substring m 1) (irregex-match-substring m 2)) cookies)))]
           [else
            cookies]))
       '()))

)
