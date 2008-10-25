(import (rnrs)
        (srfi :1)
        (mosh string))

(define (basename name)
  (let* ([lst (string-split name #\/)]
         [len (length lst)])
    (list-ref lst (- len 1))))

(define (filename->arrayname name)
  (list->string (map (lambda (ch) (if (char=? ch #\-) #\_ ch)) (string->list (first (string-split (basename name) #\.))))))

;; convert any files to binary array for include from C.
(define (main args)
  (call-with-port (open-output-file (third args))
    (lambda (out)
      (format out "static const uint8_t ~a_image[] = {" (filename->arrayname (second args)))
      (call-with-port (open-file-input-port (second args))
        (lambda (port)
          (let loop ([b (get-u8 port)]
                     [i 0])
            (cond
             [(eof-object? b)
              '()]
             [else
              (if (zero? (mod i 15))
                  (newline out))
              (format out "0x~a," (number->string b 16))
              (loop (get-u8 port) (+ i 1))]))))
      (display "\n};\n" out)))
  0)

(main (command-line))
