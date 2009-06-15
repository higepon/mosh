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
    (rnrs io ports)
    (rnrs files)
    (rnrs records procedural)
    ;(rename (rnrs programs) (command-line get-command-line))
    (only (rnrs programs) exit)
    (except (mosh) library-path);; for get-command-line
    (rnrs lists)
    (only (rnrs conditions) condition? condition make-non-continuable-violation make-who-condition make-message-condition make-irritants-condition serious-condition? who-condition? message-condition? violation? irritants-condition? condition-who condition-message condition-irritants simple-conditions)
    (only (rnrs exceptions) raise with-exception-handler guard)
    (rnrs records inspection)
    (psyntax compat)
    (psyntax internal)
    (psyntax library-manager)
    (psyntax expander)
    (psyntax config)
    (only (system) get-environment-variable); get-environment-variable
)

  (define (add-library-path! path)
    (library-path (append (library-path) (list path))))

  (define (parse-and-add-library-path paths message)
    (cond [paths
           => (lambda (paths)
                (for-each
                 (lambda (path)
                   (cond [(file-exists? path)
                          (add-library-path! (expand-path path))]
                         [else
                          (format (current-error-port) message path)]))
                (string-split paths #\:)))]))

  (define (for-each-with-index proc lst)
    (do ((i 1 (+ i 1)) ; start with 1
         (lst lst (cdr lst)))
        ((null? lst))
      (proc i (car lst))))

#;  (define (conditioon-printer e port)
    (define (ref rtd i x)
      (let ([val ((record-accessor rtd i) x)])
        (if (symbol? val)
            (ungensym val)
            val)))
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
         (format port "   ~d. ~a" i (record-type-name rtd))
         (let ([v (record-type-field-names rtd)])
           (case (vector-length v)
             [(0) (newline port)]
             [(1)
              (display ": " port)
              (write (ref rtd 0 x) port)
              (newline port)]
             [else
              (display ":\n" port)
              (let f ([i 0])
                (unless (= i (vector-length v))
                  (display "       " port)
                  (display (vector-ref v i) port)
                  (display ": " port)
                  (write (ref rtd i x) port)
                  (newline port)
                  (f (+ i 1))))]))))
     (simple-conditions e)))

(define (rpad str pad n)
  (let ([rest (- n (string-length (format "~a" str)))])
    (let loop ([rest rest]
               [ret (format "~a" str)])
      (if (<= rest 0)
          ret
          (loop (- rest 1) (string-append ret pad))))))

(define (condition-printer e port)
    (define max-condition-len (apply max (map (lambda (c) (string-length (symbol->string (record-type-name (record-rtd c))))) (simple-conditions e))))
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)]
             [fields-alist (record->field-alist x)])
        (format port " ~d. ~a" i (rpad (symbol->string (record-type-name rtd)) " " max-condition-len))
        (when (null? fields-alist)
          (newline port))
         (let loop ([first #t]
                    [fields-alist fields-alist])
           (cond
            [(null? fields-alist) '()]
            [else
             (let ([field (car fields-alist)])
               (unless first
                 (display (rpad "" " " (+ 4 max-condition-len)) port))
             (display "       " port)
             (display (car field) port)
             (display ": " port)
             (write (cdr field) port)
             (newline port)
             (loop #f (cdr fields-alist)))
             ]
          ))))
     (simple-conditions e)))

;; このコードを使いたいが使うと vm_test の $? が 1 になりテスト失敗する
#;(define (condition-printer e port)
    (display " Condition components:\n" port)
    (for-each-with-index
     (lambda (i x)
       (let ([rtd (record-rtd x)])
        (format port "   ~d. ~a" i (record-type-name rtd))
         (for-each
          (lambda (field)
            (display "       " port)
           (display (car field) port)
           (display ": " port)
           (write (cdr field) port)
            (newline port))
          (record->field-alist x))))
     (simple-conditions e)))

(define (record->field-alist r)
  (define (ref rtd i x)
    (let ([val ((record-accessor rtd i) x)])
      (if (symbol? val)
          (ungensym val)
          val)))
  (let loop ([ret '()]
             [rtd (record-rtd r)])
    (cond
     [rtd
      (loop (append ret
      (map-with-index
       (lambda (i field)
         (cons field (ref rtd i r)))
       (vector->list (record-type-field-names rtd)))) (record-type-parent rtd))]
     [else ret])))

(define (map-with-index proc lst)
  (let loop ([i 0]
             [lst lst]
             [ret '()])
    (if (null? lst)
        (reverse ret)
        (loop (+ i 1) (cdr lst) (cons (proc i (car lst)) ret)))))

  (define (repl . x)
    (define (rec)
      (display "mosh>")
      (guard (e
              (#t
               (display "\nUnhandled exception:\n\n" (current-error-port))
               (if (condition? e)
                   (condition-printer e (current-error-port))
                   (format (current-error-port) "  Non-condition object:\n     ~a\n" e))))
             (let loop ([line (get-line (current-input-port))]
                        [accum ""])
               (define (parentheses-ok? text)
                 (let loop ([chars (string->list text)]
                            [p0 0]
                            [p1 0])
                   (if (null? chars)
                       (= 0 p0 p1)
                       (case (car chars)
                         [(#\()
                          (loop (cdr chars) (+ p0 1) p1)]
                         [(#\))
                          (loop (cdr chars) (- p0 1) p1)]
                         [(#\[)
                          (loop (cdr chars) p0 (+ p1 1))]
                         [(#\])
                          (loop (cdr chars) p0 (- p1 1))]
                         [else
                          (loop (cdr chars) p0 p1)]))))
               (define (eval-string-print text)
                 (unless (or (string=? "\n" text) (= 0 (string-length text)))
                   (write (eval-top-level (call-with-port (open-string-input-port text) read)))))
               (if (eof-object? line)
                   (begin
                     (eval-string-print accum)
                     (exit))
                   (let ([current (string-append accum line)])
                     (if (parentheses-ok? current)
                         (eval-string-print current)
                         (loop (get-line (current-input-port)) current))))))
      (newline)
      (rec))
    (rec))



  (define trace-printer (make-parameter write))

  (define command-line (make-parameter (get-command-line)))

  (define (local-library-path filename)
    (library-path))


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

  (define (pre-compile-r6rs-file filename)
    (load-r6rs-top-level filename 'compile))

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
           (parameterize ([command-line (cons filename (car args))])
             ((compile-r6rs-top-level x*))))
          ((compile)
           (begin
             (compile-r6rs-top-level x*) ; i assume this is needed
             (serialize-all serialize-library compile-core-expr)))))))

   ;; mosh-only
  (define (load-r6rs-top-level-sexp import-spec thunk)
    (parameterize ([library-path (local-library-path "")])
      (parameterize ([command-line '()])
;        (display `((import ,@import-spec) (,thunk)))
        ((compile-r6rs-top-level `((import ,@import-spec) (,thunk)))))))


  (current-precompiled-library-loader load-serialized-library)


  (set-symbol-value! 'load load)
                                        ;  (set-symbol-value! 'load-r6rs-top-level load-r6rs-top-level)
  (set-symbol-value! 'pre-compile-r6rs-file pre-compile-r6rs-file)
  ;;   (set-symbol-value! 'compile compile)
  ;;   (set-symbol-value! 'compile->closure compile->closure)
  (set-symbol-value! 'eval-r6rs eval-top-level)
  (set-symbol-value! 'int-env-syms interaction-environment-symbols)
  (set-symbol-value! 'expanded2core expanded->core)

  (set-symbol-value! 'trace-printer trace-printer)
  (set-symbol-value! 'compile-r6rs-top-level 'compile-r6rs-top-level)

  (set-symbol-value! 'create-non-continuable-violation (lambda (c)
                                                         (condition (make-non-continuable-violation)
                                                                    (make-who-condition 'raise)
                                                                    (make-message-condition "returned from non-continuable exception")
                                                                    (make-irritants-condition (list c)))))


                                        ;  (library-path (get-library-paths))
                                        ;  (library-path '("." "/tmp/")


  ;; (cond [(get-environment-variable "MOSH_LOADPATH")
;;          => (lambda (paths)
;;               (for-each
;;                (lambda (path)
;;                  (cond [(file-exists? path)
;;                         (add-library-path! path)]
;;                        [else
;;                         (format (current-error-port) "** ERROR in environment variable 'MOSH_LOADPATH': directory ~s not exist~%" path)]))
;;                      (reverse (string-split paths #\:))))])

  (parse-and-add-library-path
   (get-environment-variable "MOSH_LOADPATH")
   "** WARN in environment variable 'MOSH_LOADPATH': directory ~s not exist\n")

  (parse-and-add-library-path
   (symbol-value '%loadpath)
   "** WARN in command-line option '--loadpath': directory ~s not exist\n")


  ;; (cond [(symbol-value '%loadpath) ;; command line option --loadpath
;;          => (lambda (paths)
;;               (for-each
;;                (lambda (path)
;;                  (cond [(file-exists? path)
;;                         (add-library-path! path)]
;;                        (else
;;                         (format (current-error-port) "** ERROR in command line option  '--loadpath': directory ~s not exist~%" path))))
;;                      (reverse (string-split paths #\:))))])


#;  (library-path (append (library-path) (list (string-append (current-directory) "/lib")
                      (string-append (standard-library-path) "/lib")
                      )))

  (if (mosh-executable-path)
      (when (file-exists? (string-append (mosh-executable-path) "/lib"))
          (add-library-path! (string-append (mosh-executable-path) "/lib")))
      (when (file-exists? (string-append (current-directory) "/lib"))
        (add-library-path! (string-append (current-directory) "/lib"))))

  (when (file-exists? (string-append (standard-library-path) "/lib"))
    (add-library-path! (string-append (standard-library-path) "/lib")))

  (let ([prefix
           (lambda (ext ls)
             (append (map (lambda (x) (string-append ext x)) ls) ls))])
      (library-extensions
          (prefix ".mosh"
            (library-extensions))))

  (let ([args (command-line)]
        [port (current-error-port)])
    (define (ref rtd i x)
      (let ([val ((record-accessor rtd i) x)])
        (if (symbol? val)
            (ungensym val)
            val)))
    (with-exception-handler
     (lambda (c)
       (if (condition? c)
           (condition-printer c (current-error-port))
           (format (current-error-port) "\n Non-condition object:\n     ~a\n" c)))
     (lambda ()
       (cond
        ;; mulitple vm
        [(guard [c (#t #f)] (symbol-value '%vm-import-spec))
         (load-r6rs-top-level-sexp (symbol-value '%vm-import-spec) (symbol-value '%vm-thunk))]
        ;; REPL
        [(null? args) (repl)]
        ;; load file
        [else
           (load-r6rs-top-level (car args) 'load (cdr args))]))))


  ;;   (display "r6rs psyntax ready\n")
  ;;   (let ((args (command-line)))
  ;;     (unless (= (length args) 2)
  ;;       (display "provide a script name argument\n")
  ;;     )
  ;;     (let ((script-name (car args)) (args (cdr args)))
  ;;       (load-r6rs-top-level (car args) 'load)))

  )
