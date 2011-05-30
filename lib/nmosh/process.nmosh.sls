(library (nmosh process)
         (export process->stdout+stderr
                 process->stdout
                 process->stderr
                 process->string
                 format-process-string)
         (import (nmosh io core)
                 (srfi :8)
                 (rnrs)
                 (shorten))

;; FIXME: support quotation
(define (format-process-string x)
  (define (itr l acc cur)
    (if (pair? cur)
      (if (char-whitespace? (car cur))
        (itr (cons (list->string (reverse acc))
                   l)
             '()
             (cdr cur))
        (itr l (cons (car cur) acc) (cdr cur)))
      (if (= 0 (length acc))
        (reverse l)
        (reverse (cons (list->string (reverse acc)) l)))))
  (itr '() '() (string->list x)))

(define (process->stdout+stderr . args)
  (io-dispatch-sync return
                    (launch! (exec . args)
                             ((finish stdout stderr)
                              return))))

(define (process->stdout . args)
  (receive (status stdout stderr) (apply process->stdout+stderr args)
    (values status stdout)))

(define (process->stderr . args)
  (receive (status stdout stderr) (apply process->stdout+stderr args)
    (values status stderr)))

(define (process->string . args)
  (receive (status stdout) (apply process->stdout args)
    (if (= status 0)
      stdout
      #f)))

)
