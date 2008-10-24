(import (rnrs)
        (srfi :1)
        (mosh string))

(define (main args)
  (format #t "static const uint8_t ~a[] = {" (second args))
  (call-with-port (open-file-input-port (third args))
    (lambda (port)
      (let loop ([b (get-u8 port)]
                 [i 0])
        (cond
         [(eof-object? b)
          '()]
         [else
          (if (zero? (mod i 15))
              (newline))
          (format #t "0x~a," (number->string b 16))
          (loop (get-u8 port) (+ i 1))]))))
  (display "\n};\n")
  0)

(main (command-line))
