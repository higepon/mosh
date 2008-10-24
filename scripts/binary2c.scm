(use srfi-1)
(use binary.io)

(define (main args)
  (format #t "static const uint8_t ~a[] = {" (second args))
  (with-input-from-file (third args)
    (lambda ()
      (let loop ([b (read-u8)]
                 [i 0])
        (cond
         [(eof-object? b)
          '()]
         [else
          (if (zero? (modulo i 15))
              (newline))
          (format #t "0x~2,'0x," b)
          (loop (read-u8) (+ i 1))]))))
  (display "\n};\n")
  0)


