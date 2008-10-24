(import (rnrs)
        (srfi-1)
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
(display (command-line))
(main (command-line))

;; (use srfi-1)
;; (use binary.io)

;; (define (main args)
;;   (format #t "static const uint8_t ~a[] = {" (second args))
;;   (with-input-from-file (third args)
;;     (lambda ()
;;       (let loop ([b (read-u8)]
;;                  [i 0])
;;         (cond
;;          [(eof-object? b)
;;           '()]
;;          [else
;;           (if (zero? (modulo i 15))
;;               (newline))
;;           (format #t "0x~2,'0x," b)
;;           (loop (read-u8) (+ i 1))]))))
;;   (display "\n};\n")
;;   0)


