(import (rnrs)
        (mosh))

(define (basename name)
  (let* ([lst (string-split name #\/)]
         [len (length lst)])
    (list-ref lst (- len 1))))

(define (filename->arrayname name)
  (list->string (map (lambda (ch) (if (char=? ch #\-) #\_ ch)) (string->list (car (string-split (basename name) #\.))))))

;; convert any files to binary array for include from C.
(define (main args)
  (call-with-port (open-file-output-port (caddr args) (file-options no-fail) (buffer-mode none) (native-transcoder))
    (lambda (out)
      (display "#include <stdint.h>\n" out)
      (format out "const uint8_t ~a[] = {" (filename->arrayname (cadr args)))
      (call-with-port (open-file-input-port (cadr args) (file-options) (buffer-mode none))
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
      (display "\n};\n" out)
      (format out "const unsigned int ~a_size = sizeof(~a);\n" (filename->arrayname (cadr args)) (filename->arrayname (cadr args)))))
  0)

(main (command-line))
