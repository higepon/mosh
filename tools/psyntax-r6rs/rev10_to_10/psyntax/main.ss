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

(library (psyntax main)
  (export
    trace-printer
    command-line
    load
    load/args
    ironscheme-build
    compile
    compile-system-libraries
    compile->closure)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs io simple)
    (rnrs records procedural)
    ;(rename (rnrs programs) (command-line get-command-line))
    (mosh) ;; for get-command-line
    (rnrs lists)
    (only (rnrs conditions) serious-condition? who-condition? message-condition? violation? irritants-condition? condition-who condition-message condition-irritants simple-conditions)
    (only (rnrs exceptions) raise with-exception-handler guard)
    (rnrs records inspection)
    (psyntax compat)
    (psyntax internal)
    (psyntax library-manager)
    (psyntax expander)
    (psyntax config)
    (mosh string)
    (mosh file)
;    (only (ironscheme core) get-command-line format)
;    (ironscheme files)
;    (ironscheme library)
)

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))


  (define trace-printer (make-parameter write))

  (define command-line (make-parameter (get-command-line)))

;;   (define (local-library-path filename)
;;     (cons (get-directory-name filename) (library-path)))

  (define (local-library-path filename)
    (cons "." (library-path)))


  (define (load/args filename . args)
    (apply load-r6rs-top-level filename 'load args)
    (void))

  (define (load filename)
    (apply load-r6rs-top-level filename 'load (command-line))
    (void))

  (define (ironscheme-build)
    (load "ironscheme-buildscript.ss"))

  (define (eval-top-level x)
    (eval x (interaction-environment)))

  (define (compile-system-libraries)
    (eval-top-level
      `(begin
         (include "system-libraries.ss")
         (compile "system-libraries.ss"))))

  (define (compile filename)
    (load-r6rs-top-level filename 'compile))

  (define (compile->closure filename)
    (load-r6rs-top-level filename 'closure))

  (define (load-r6rs-top-level filename how . args)
    (parameterize ([library-path (local-library-path filename)])
      (let ((x*
             (with-input-from-file filename
               (lambda ()
                 (let f ()
                   (let ((x (read-annotated)))
                     (if (eof-object? x)
                         '()
                         (cons x (f)))))))))
        (case how
          ((closure)   (pre-compile-r6rs-top-level x*))
          ((load)
            (parameterize ([command-line (cons filename (map (lambda (x) (format "~a" x)) args))])
              ((compile-r6rs-top-level x*))))
          ((compile)
              (begin
                          (compile-r6rs-top-level x*) ; i assume this is needed
                          (serialize-all serialize-library compile-core-expr)))))))

(define (write-record record port)
  (let ([rtd (record-rtd record)])
    (format port "    ~a" (record-type-name rtd))
    (let ([v (record-type-field-names rtd)])
      (case (vector-length v)
        [(0) (newline)]
        [(1)
         (display ": " port)
         (write ((record-accessor rtd 0) record) port)
         (newline port)]
        [else
         (display ":\n")
         (let f ([i 0])
           (unless (= i (vector-length v))
             (display "       " port)
             (display (vector-ref v i) port)
             (display ": " port)
             (write ((record-accessor rtd i) record) port)
             (newline port)
             (f (+ i 1))))]))))

;;   (define (for-each-with-index proc lst)
;;     (do ((i 1 (+ i 1)) ; start with 1
;;          (lst lst (cdr lst)))
;;         ((null? lst))
;;       (proc i (car lst))))


  (current-precompiled-library-loader load-serialized-library)

;;   (set-symbol-value! 'default-exception-handler
;;     (lambda (ex)
;;       (cond
;;         [(serious-condition? ex) (raise ex)]
;;         [else
;;           (display ex)
;;           (newline)])))

  (set-symbol-value! 'load load)
;;   (set-symbol-value! 'compile compile)
;;   (set-symbol-value! 'compile->closure compile->closure)
  (set-symbol-value! 'eval-r6rs eval-top-level)
  (set-symbol-value! 'int-env-syms interaction-environment-symbols)
  (set-symbol-value! 'expanded2core expanded->core)

  (set-symbol-value! 'trace-printer trace-printer)
  (set-symbol-value! 'compile-r6rs-top-level 'compile-r6rs-top-level)


;  (library-path (get-library-paths))
;  (library-path '("." "/tmp/"))
  (library-path (list (string-append (current-directory) "/lib")
                  (string-append (standard-library-path) "/lib")))

;;   (define (print-record x)
;;     (display "hige")
;;     (let ([rtd (record-rtd x)])
;;       (format #t "    ~a" (record-type-name rtd))
;;       (let ([v (record-type-field-names rtd)])
;;         (case (vector-length v)
;;           [(0) (newline)]
;;           [(1)
;;            (display ": ")
;;            (write ((record-accessor rtd 0) x))
;;            (newline)]
;;           [else
;;            (display ":\n")
;;            (let f ([i 0])
;;              (unless (= i (vector-length v))
;;                (display "       ")
;;                (display (vector-ref v i))
;;                (display ": ")
;;                (write ((record-accessor rtd i) x))
;;                (newline)
;;                (f (+ i 1))))]))))

  (let ([args (command-line)]
        [port (current-error-port)])
    (with-exception-handler
     (lambda (c)
       (display " Condition components:\n" port)
       (for-each-with-index
        (lambda (i x)
          (cond
;;            [(who-condition? x)
;;             (format port "   ~d. &who: ~a\n" i (condition-who x))]
;;            [(message-condition? x)
;;             (format port "   ~d. &message: ~s\n" i (condition-message x))]
;;            [(violation? x)
;;             (format port "   ~d. ~a\n" i (record-type-name (record-rtd x)))]
;;            [(irritants-condition? x)
;;             (format port "   ~d. &irritants: ~s\n" i (condition-irritants x))]
           [else
            (let ([rtd (record-rtd x)])
              (format port "   ~d. ~a" i (record-type-name rtd))
              (let ([v (record-type-field-names rtd)])
                (case (vector-length v)
                  [(0) (newline)]
                  [(1)
                   (display ": ")
                   (write ((record-accessor rtd 0) x))
                   (newline)]
                  [else
                   (display ":\n")
                   (let f ([i 0])
                     (unless (= i (vector-length v))
                       (display "       ")
                       (display (vector-ref v i))
                       (display ": ")
                       (write ((record-accessor rtd i) x))
                       (newline)
                       (f (+ i 1))))])))]
            ))
        (simple-conditions c)))
     (lambda ()
      (load-r6rs-top-level (car args) 'load))))


;;   (display "r6rs psyntax ready\n")
;;   (let ((args (command-line)))
;;     (unless (= (length args) 2)
;;       (display "provide a script name argument\n")
;;     )
;;     (let ((script-name (car args)) (args (cdr args)))
;;       (load-r6rs-top-level (car args) 'load)))

  )
