;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.


;;; modified for Mosh by Higepon.
(library (psyntax main)
  (export)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs programs)
    (rnrs exceptions)
    (rnrs records inspection)
    (rnrs records procedural)
    (mosh condition)
    (mosh string)
    (psyntax compat)
    (psyntax library-manager)
    (rename (psyntax expander) (eval psyntax:eval)))

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

  (define (load-r6rs-top-level filename)
    (let ((x*
           (with-input-from-file filename
             (lambda ()
               (let f ()
                 (let ((x (read)))
                   (if (eof-object? x)
                       '()
                       (cons x (f)))))))))
      (eval-r6rs-top-level x*)))

;; (define (repl)
;;   (define (rec)
;;     (display "mosh>")
;;     (guard (e
;;             [e
;;              (display e)
;;              (rec)])
;;            (let ([obj (read (current-input-port))])
;;              (if (eof-object? obj)
;;                  (exit)
;;                  (display (eval-r6rs-top-level (list obj))))))
;;     (rec))
;;   (rec))
(define (repl . x)
  (define (rec)
    (display "mosh>")
    (guard (e
;;            [(who-condition? e)
;;             (format #t "    &who: ~a\n" (condition-who e))]
;;            [(message-condition? e)
;;             (format #t "    &message: ~s\n" (condition-message e))]
;;            [(violation? e)
;;             (format #t "    ~a\n" (record-type-name (record-rtd e)))]
;;            [(irritants-condition? e)
;;             (format #t "    &irritants: ~s\n" (condition-irritants e))]
;;            [else
;;             (format #t "    ~a\n"  (record-type-name (record-rtd e)))])
            (#t
       (for-each-with-index
        (lambda (i x)
          (cond
           [(who-condition? x)
            (format #t "   ~d. &who: ~a\n" i (condition-who x))]
           [(message-condition? x)
            (format #t "   ~d. &message: ~s\n" i (condition-message x))]
           [(violation? x)
            (format #t "   ~d. ~a\n" i (record-type-name (record-rtd x)))]
           [(irritants-condition? x)
            (format #t "   ~d. &irritants: ~s\n" i (condition-irritants x))]
           [else
            (format #t "   ~d. ~a\n" i (record-type-name (record-rtd x)))]))
        (simple-conditions e))))
           (let ([obj (read (current-input-port))])
             (if (eof-object? obj)
                 (exit)
                 (display (eval-top-level obj)))))
;                 (display (eval-top-level '(display 345))))))
    (rec))
  (rec))

;(set-symbol-value! 'remp eval-top-level)


  (let ([args (command-line)]
        [port (current-error-port)])
    (with-exception-handler
     (lambda (c)
       (display " Condition components:\n" port)
       (for-each-with-index
        (lambda (i x)
          (cond
           [(who-condition? x)
            (format port "   ~d. &who: ~a\n" i (condition-who x))]
           [(message-condition? x)
            (format port "   ~d. &message: ~s\n" i (condition-message x))]
           [(violation? x)
            (format port "   ~d. ~a\n" i (record-type-name (record-rtd x)))]
           [(irritants-condition? x)
            (format port "   ~d. &irritants: ~s\n" i (condition-irritants x))]
           [else
            (format port "   ~d. ~a\n" i (record-type-name (record-rtd x)))]))
        (simple-conditions c)))
     (lambda ()
;;      (repl)))))
       (load-r6rs-top-level (car args))))))
