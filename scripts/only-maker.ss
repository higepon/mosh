(import (rnrs)
        (mosh)
        (mosh process))

;; Read file, find (only (...) ...) and replace it to (only (...) ... sym)
(define (add-only-sym! file regex sym)
  (let ([lst (map (lambda (line)
       (if (regex line)
           (call-with-port (open-string-input-port line)
             (lambda (p)
               (let ([only* (read p)])
                 (cond
                  [(and (pair? only*) (>= (length only*) 2))
                   (cons* (car only*) (cadr only*) (append (cddr only*) (list sym)))]
                  [else
                   (error 'add-symbol "malformed (only)")]))))
           line))
                  (file->list file))])
    (with-output-to-file file
      (lambda ()
        (for-each print lst)))))

(define (spawn->string cmd)
  (let-values ([(in out) (pipe)])
    (define (port->string p)
      (let loop ([ret '()][c (read-char p)])
        (if (eof-object? c)
            (list->string (reverse ret))
            (loop (cons c ret) (read-char p)))))
    (let-values ([(pid cin cout cerr) (spawn cmd '() (list #f out #f))])
      (close-port out)
      (let ([x (port->string (transcoded-port in (make-transcoder (utf-8-codec))))])
        (close-port in)
        (waitpid pid)
        x))))

(define (chop s)
  (substring s 0 (- (string-length s) 1)))

(define (main args)
  (let loop ([unbound-sym (spawn->string "./scripts/only.sh")])
    (if (string=? "" unbound-sym)
        '()
        (begin (add-only-sym! (cadr args) (string->regexp (caddr args)) (string->symbol (chop unbound-sym)))
               (loop (spawn->string "./scripts/only.sh"))))))

(main (command-line))
