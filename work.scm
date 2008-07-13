;; これは動いた
;; (display (read-char (transcoded-port
;;             (make-custom-binary-input-port
;;              "cgi decode"
;;              (lambda (bv start count)  "***********")
;;              #f #f #f)
;;             (make-transcoder (utf-8-codec)))))
;;   (display 4)

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
                     (write b)
                     (cond
                      [(eof-object? b) (format #t "size=~a\n" size) size]
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
           (display "*****************")
           (display c out)
           (loop (read-char p))]))))))
;(display (cgi-encode "<<>>"))
(display (decode "%3c%3c"))
(display "END")
