(import (rnrs) 
	(rnrs r5rs)
	(irregex) 
	(match)
	(only (srfi :13) string-contains)
	(srfi :64)
	(rename (srfi :48) (format sprintf)))

(define call-with-output-string call-with-string-output-port) ;MOSH: R6RS
(define (call-with-input-string str proc)
  (call-with-port (open-string-input-port str) proc))

;from: http://srfi.schemers.org/srfi-13/mail-archive/msg00110.html
(define (string-split s delimiter TRUE)
  (let ((r 
   (let ((sl (string-length s))
	 (dl (string-length delimiter)))
      (let unfold ((index 0) (strings '()))
	 (let ((start (string-contains s delimiter index)))
	    (if start
		(unfold (+ start dl)
			(cons (substring s index start) strings))
		(reverse (cons (substring s index sl) strings))
		))))))
    ;(display r)(newline) ;;SHOW PROGRESS
    r))
; to stderr?
(define (warning desc datum)
  (display (list "WARNING:" desc datum))(newline))

(define-syntax test
  (syntax-rules ()
    ((_ test-name expected test-expr) (test-equal test-name expected test-expr))
    ((_ expected test-expr) (test-equal "[NO-NAME]" expected test-expr))))

(define (port-for-each proc sym)
  (let ((l (get-line (current-input-port))))
    (cond
      ((eof-object? l) 'ok)
      (else
	(proc l)
	(port-for-each proc 'none)))))
(define read-line 'ok)

(define (intersperse l e)
  (define (comp c i) (cons i (cons e c)))
  (let ((f (car l))
	(d (cdr l)))
    (if (null? d)
      f
      (reverse (fold-left comp (list f) d)))))
(define (string-intersperse l e)
  (apply string-append (intersperse l e)))



;;;;;;;;;;;;;;;;;;;;
;;; orig
(define (subst-matches matches subst)
  (define (submatch n)
    (if (vector? matches)
        (irregex-match-substring matches n)
        (list-ref matches n)))
  (and
   matches
   (call-with-output-string
     (lambda (out)
       (call-with-input-string subst
         (lambda (in)
           (let lp ()
             (let ((c (read-char in)))
               (cond
                ((not (eof-object? c))
                 (case c
                   ((#\&)
                    (display (or (submatch 0) "") out))
                   ((#\\)
                    (let ((c (read-char in)))
                      (if (char-numeric? c)
                          (let lp ((res (list c)))
                            (if (and (char? (peek-char in))
                                     (char-numeric? (peek-char in)))
                                (lp (cons (read-char in) res))
                                (display
                                 (or (submatch (string->number
                                                (list->string (reverse res))))
                                     "")
                                 out)))
                          (write-char c out))))
                   (else
                    (write-char c out)))
                 (lp)))))))))))

(define (test-re matcher line)
  (match (string-split line "\t" #t)
    ((pattern input result subst output)
     (let ((name (sprintf "~A  ~A  ~A  ~A" pattern input result subst)))
       (cond
        ((equal? "c" result)
         (test-error name (matcher pattern input)))
        ((equal? "n" result)
         (test-assert name (not (matcher pattern input))))
        (else
         (test name output
           (subst-matches (matcher pattern input) subst))))))
    (else
     (warning "invalid regex test line" line))))

(test-begin "irregex") ;MOSH: srfi-64



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic irregex

(for-each
 (lambda (opts)
   (test-group (sprintf "irregex - ~S" opts)
     (with-input-from-file "re-tests.txt"
       (lambda ()
         (port-for-each
          (lambda (line)
            (test-re (lambda (pat str)
                       (irregex-search (apply irregex pat opts) str))
                     line))
          read-line)))))
 '((backtrack)
   (fast)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chunked irregex

(define (rope . args)
  (map (lambda (x) (if (pair? x) x (list x 0 (string-length x)))) args))

(define rope-chunker
  (make-irregex-chunker
   (lambda (x) (and (pair? (cdr x)) (cdr x)))
   caar
   cadar
   caddar
   (lambda (src1 i src2 j)
     (if (eq? src1 src2)
         (substring (caar src1) i j)
         (let lp ((src (cdr src1))
                  (res (list (substring (caar src1) i (caddar src1)))))
           (if (eq? src src2)
               (string-intersperse
                (reverse (cons (substring (caar src2) (cadar src2) j) res))
                "")
               (lp (cdr src)
                   (cons (substring (caar src) (cadar src) (caddar src))
                         res))))))))

(define (make-ropes str)
  (let ((len (string-length str)))
    (case len
      ((0 1)
       (list (rope str)))
      ((2)
       (list (rope str)
             (rope (substring str 0 1) (substring str 1 2))))
      ((3)
       (list (rope str)
             (rope (substring str 0 1) (substring str 1 3))
             (rope (substring str 0 2) (substring str 2 3))
             (rope (substring str 0 1)
                   (substring str 1 2)
                   (substring str 2 3))))
      (else
       (let ((mid (quotient (+ len 1) 2)))
         (list (rope str)
               (rope (substring str 0 1) (substring str 1 len))
               (rope (substring str 0 mid) (substring str mid len))
               (rope (substring str 0 (- len 1))
                     (substring str (- len 1) len))
               (rope (substring str 0 1)
                     (substring str 1 mid)
                     (substring str mid len))
               ))))))

(define (make-shared-ropes str)
  (let ((len (string-length str)))
    (case len
      ((0 1)
       '())
      ((2)
       (list (list (list str 0 1) (list str 1 2))))
      ((3)
       (list (list (list str 0 1) (list str 1 3))
             (list (list str 0 2) (list str 2 3))
             (list (list str 0 1) (list str 1 2) (list str 2 3))))
      (else
       (let ((mid (quotient (+ len 1) 2)))
         (list (list (list str 0 1) (list str 1 len))
               (list (list str 0 mid) (list str mid len))
               (list (list str 0 (- len 1))
                     (list str (- len 1) len))
               (list (list str 0 1) (list str 1 mid) (list str mid len))
               ))))))

(for-each
 (lambda (opts)
   (test-group (sprintf "irregex/chunked - ~S" opts)
     (with-input-from-file "re-tests.txt"
       (lambda ()
         (port-for-each
          (lambda (line)
            (match (string-split line "\t" #t)
              ((pattern input result subst output)
               (let ((name
                      (sprintf "~A  ~A  ~A  ~A" pattern input result subst)))
                 (cond
                  ((equal? "c" result))
                  ((equal? "n" result)
                   (for-each
                    (lambda (rope)
                      (test-assert name
                        (not (irregex-search/chunked pattern
                                                     rope-chunker
                                                     rope))))
                    (append (make-ropes input)
                            (make-shared-ropes input))))
                  (else
                   (for-each
                    (lambda (rope)
                      (test name output
                        (subst-matches (irregex-search/chunked pattern
                                                               rope-chunker
                                                               rope)
                                       subst)))
                    (append (make-ropes input)
                            (make-shared-ropes input)))))))
              (else
               (warning "invalid regex test line" line)))
            )
          read-line)))))
 '((backtrack)
   (fast)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp

'(test-group "pregexp"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re pregexp-match line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default regex (PCRE)

'(test-group "regex"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re string-search line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "unmatchable patterns"
  (test-assert (not (irregex-search '(or) "abc")))
  (test-assert (not (irregex-search '(: "ab" (or)) "abc")))
  (test-assert (not (irregex-search '(submatch "ab" (or)) "abc")))
  (test-assert (not (irregex-search '(: "ab" (submatch (or))) "abc")))
  (test-assert (not (irregex-search '(/) "abc")))
  (test-assert (not (irregex-search '(: "ab" (/)) "abc")))
  (test-assert (not (irregex-search '(~ any) "abc")))
  (test-assert (not (irregex-search '(: "ab" (~ any)) "abc")))
  (test-assert (not (irregex-search '("") "abc")))
  (test-assert (not (irregex-search '(: "ab" ("")) "abc")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "API"
  (test-assert (irregex? (irregex "a.*b")))
  (test-assert (irregex? (irregex '(: "a" (* any) "b"))))
  (test-assert (not (irregex? (vector '*irregex-tag* #f #f #f #f #f #f #f))))
  (test-assert (not (irregex? (vector #f #f #f #f #f #f #f #f #f))))
  (test-assert (irregex-match-data? (irregex-search "a.*b" "axxxb")))
  (test-assert (irregex-match-data? (irregex-match "a.*b" "axxxb")))
  (test-assert (not (irregex-match-data? (vector '*irregex-match-tag* #f #f #f #f #f #f #f #f #f))))
  (test-assert (not (irregex-match-data? (vector #f #f #f #f #f #f #f #f #f #f #f))))
  (test 0 (irregex-num-submatches (irregex "a.*b")))
  (test 1 (irregex-num-submatches (irregex "a(.*)b")))
  (test 2 (irregex-num-submatches (irregex "(a(.*))b")))
  (test 2 (irregex-num-submatches (irregex "a(.*)(b)")))
  (test 10 (irregex-num-submatches (irregex "((((((((((a))))))))))")))
  (test 0 (irregex-match-num-submatches (irregex-search "a.*b" "axxxb")))
  (test 1 (irregex-match-num-submatches (irregex-search "a(.*)b" "axxxb")))
  (test 2 (irregex-match-num-submatches (irregex-search "(a(.*))b" "axxxb")))
  (test 2 (irregex-match-num-submatches (irregex-search "a(.*)(b)" "axxxb")))
  (test 10 (irregex-match-num-submatches (irregex-search "((((((((((a))))))))))" "a")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "utils"
  (test "h*llo world"
      (irregex-replace "[aeiou]" "hello world" "*"))
  (test "h*ll* w*rld"
      (irregex-replace/all "[aeiou]" "hello world" "*"))
  (test '("bob@test.com" "fred@example.com")
      (irregex-fold 'email
                    (lambda (i m s) (cons (irregex-match-substring m) s))
                    '()
                    "bob@test.com and fred@example.com"
                    (lambda (i s) (reverse s))))
  (test '("bob@test.com" "fred@example.com")
      (irregex-fold/chunked
       'email
       (lambda (src i m s) (cons (irregex-match-substring m) s))
       '()
       rope-chunker
       (rope "bob@test.com and fred@example.com")
       (lambda (src i s) (reverse s))))
  )

(test-end)

