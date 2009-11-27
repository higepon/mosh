#!/usr/bin/env gosh
(use gauche.process)
(use srfi-1)
(use util.match)

;; test of compilers
;; compare the compile code of compiler on Gauche and VM on Gauche.

;(define compiler-on-gauche       '("gosh" "-f" "no-inline" "./compiler-gauche.scm"))
(define compiler-on-gauche       '("gosh" "./compiler-gauche.scm"))
;(define compiler-on-vm-on-gauche '("gosh" "./vm.scm" "./compiler-vm-outer.scm"))
(define compiler-on-new-vm       '("./mosh" "-c"))

(define *insn* (with-input-from-file "./instruction.scm"
                 (lambda ()
                   (let loop ([i 0] [obj (read)] [ret '()])
                     (match obj
                       [('define-insn name num)
                        (loop (+ i 1) (read) (cons (list i name num) ret))]
                       [else (reverse ret)])))))

(define (num-insn->insn s)
  (with-string-io s
    (lambda ()
      (let loop ([i 0] [next 0] [v (read)] (ret '()))
        (cond
         [(>= i (vector-length v))
          (write (list->vector ret))]
         [(= i next)
          (let1 found (find (lambda (x) (eq? (first x) (vector-ref v i))) *insn*)
            (if found
                (loop (+ i 1) (+ i (third found) 1) v (append ret (list (second found))))
                (error "insn not found" (vector-ref v i) ret)))]
         [else
          ;; remove source-info
          (match (vector-ref v i)
            [(_ '<proc> . _)
             (set-car! (vector-ref v i) #f)]
            [else '()])
          (loop (+ i 1) next v (append ret (list (vector-ref v i))))])))))


(define (compile compiler str)
  (let* ((process (apply run-process `(,@compiler ,str :output :pipe)))
         (line (read-line (process-output process) #t)))
    (process-wait process)
    line))

(define (compare-compile str current total)
  (print (format "compiling (~d/~d) : ~a" current total str))
  (let ([gauche (compile compiler-on-gauche str)]
;        [vm (compile compiler-on-vm-on-gauche str)]
        [vm-new (num-insn->insn (compile compiler-on-new-vm str))])
    (cond [(and (string=? gauche vm-new)); (string=? vm vm-new))
           (print " ==> ok " gauche)]
          [else
           (print " ==> NG")
           (print (format " [Gauche output ] : ~s" gauche))
;           (print (format " [VM output     ] : ~s" vm))
           (print (format " [VM(new) output] : ~s" vm-new))
           (error "Stopped.")])))

(define (read-tests file)
  (with-input-from-file file
    (lambda ()
      (let loop ([ret '()]
                 [obj (read)])
        (match obj
          [('lib . body) ;; ignore library test
           (loop ret (read))]
          [else
           (if (eof-object? obj)
               ret
               (loop (append ret (list obj)) (read)))])))))

(define (compare from-index)
  (let* ([tests (read-tests "./test-data.scm")]
         [tests-num (length tests)])
      (let loop1 ([tests tests]
                  [i 0])
        (cond [(null? tests) '()]
              [(and (pair? tests) (>= (length tests) 2))
               (let loop2 ([lst (cdar tests)])
                 (if (null? lst)
                     (loop1 (cdr tests) (+ i 1))
                     (begin
                       (if (>= i from-index)
                           (compare-compile (format "~s" (car lst)) i tests-num))
                       (loop2 (cdr lst)))))]
              [else
               ]))))

(define (main args)
  (compare (if (> (length args) 1) (string->number (cadr args)) 0))
  0)
