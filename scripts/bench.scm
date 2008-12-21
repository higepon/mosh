#!/usr/bin/env gosh
(use gauche.process)
(use srfi-1)
(use util.match)
(use gauche.parseopt)

;; execute benchmarks and format them

(define (show-flonum num n)
  (receive (q r)
     (quotient&remainder (inexact->exact (round (* num (expt 10 n))))
                                         (expt 10 n))
    (format "~d.~v,'0d" q n r)))

(define (get-process-error-line lst)
  (let* ((process (apply run-process `(,@lst :error :pipe)))
         (line (read-line (process-error process) #t)))
    (process-wait process)
    line))

(define (exec-with-time command)
  (string->number (get-process-error-line  `("time" "-f" "%e" ,@command))))

(define (ntimes-map proc n)
  (let loop ([i 0]
             [ret '()])
    (if (>= i n)
        ret
        (loop (+ i 1) (append ret (list (proc)))))))

(define (count-insns command)
  (let1 process (apply run-process `("./scripts/detect-repeat.sh" ,@command :output :pipe))
    (let loop ([obj (read (process-output process))])
      (cond
       [(eof-object? obj) '()]
       [else
        (match obj
          [('total total) (format (current-error-port) "|~d|" total)]
          [(insn times percentage)
           (format (current-error-port) "~a(~dtimes, ~d%)|" insn times percentage)])
        (loop (read (process-output process)))]))
    (process-wait process)
    ))


(define (benchmark command verbose?)
  (sys-system "make")
  (let ([our-results (apply min (ntimes-map (lambda () (exec-with-time `("./mosh -5" ,@command))) 10))]
        [gosh-results (apply min (ntimes-map (lambda () (exec-with-time `("gosh" ,@command))) 10))])
    (format (current-error-port) "\n|*~a|~a|~a|" (first command) gosh-results our-results)
    (if verbose? (count-insns command))))

(define (main args)
  (let-args (cdr args)
      [(verbose? "v|verbose" #f)]
    (format (current-error-port) "|*benchmark|*gosh sec|*our sec|~a" (if verbose? "count|insn1|insn2|insn3|insn4|insn5|" ""))
    (for-each (cut benchmark <> verbose?)
              '(
  ;                        ("./bench/hoge.scm")
              ("./bench/fib.scm")
               ("./bench/case.scm")
              ("./bench/let.scm")
               ("./bench/takl.scm")
;                                                               ("./bench/triangl.scm")
              ("./bench/cpstak.scm")
              ("./bench/sum.scm")
               ("./bench/tak.scm")
             ("./bench/array1.scm")

)))
  0)

