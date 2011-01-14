;;;; irregex.scm -- IrRegular Expressions
(library (irregex)
         (export
           ;from irregex-chicken

           irregex string->irregex sre->irregex
           string->sre maybe-string->sre                                                 
           irregex? irregex-match-data?
           irregex-new-matches irregex-reset-matches!
           irregex-search irregex-search/matches irregex-match
           irregex-search/chunked irregex-match/chunked make-irregex-chunker
           irregex-match-substring irregex-match-subchunk
           irregex-match-start-chunk irregex-match-start-index
           irregex-match-end-chunk irregex-match-end-index
           irregex-match-num-submatches irregex-match-names
           irregex-match-valid-index?
           irregex-fold irregex-replace irregex-replace/all
           irregex-dfa irregex-dfa/search irregex-dfa/extract
           irregex-nfa irregex-flags irregex-lengths irregex-names
           irregex-num-submatches irregex-extract irregex-split

           ;; add
           irregex-fold/chunked
           )
         (import (rnrs)
                 (only (rnrs r5rs (6)) modulo remainder quotient)
                 (rnrs mutable-strings)
                 (rnrs mutable-pairs))

;; based on rev:2e55ccfbba

;;;; irregex.scm -- IrRegular Expressions
;;
;; Copyright (c) 2005-2010 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At this moment there was a loud ring at the bell, and I could
;; hear Mrs. Hudson, our landlady, raising her voice in a wail of
;; expostulation and dismay.
;;
;; "By heaven, Holmes," I said, half rising, "I believe that
;; they are really after us."
;;
;; "No, it's not quite so bad as that.  It is the unofficial
;; force, -- the Baker Street irregulars."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Notes
;;
;; This code should not require any porting - it should work out of
;; the box in any R[45]RS Scheme implementation.  Slight modifications
;; are needed for R6RS (a separate R6RS-compatible version is included
;; in the distribution as irregex-r6rs.scm).
;;
;; The goal of portability makes this code a little clumsy and
;; inefficient.  Future versions will include both cleanup and
;; performance tuning, but you can only go so far while staying
;; portable.  AND-LET*, SRFI-9 records and custom macros would've been
;; nice.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; History
;;
;; 0.8.2: 2010/08/28 - (...)? submatch extraction fix and alternate
;;                     named submatches from Peter Bex
;;                     Added irregex-split, irregex-extract,
;;                     irregex-match-names and irregex-match-valid-index?
;;                     to Chicken and Guile module export lists and made
;;                     the latter accept named submatches.  The procedures
;;                     irregex-match-{start,end}-{index,chunk} now also
;;                     accept named submatches, with the index argument
;;                     made optional.  Improved argument type checks.
;;                     Disallow negative submatch index.
;;                     Improve performance of backtracking matcher.
;;                     Refactor charset handling into a consistent API
;; 0.8.1: 2010/03/09 - backtracking irregex-match fix and other small fixes
;; 0.8.0: 2010/01/20 - optimizing DFA compilation, adding SRE escapes
;;                     inside PCREs, adding utility SREs
;; 0.7.5: 2009/08/31 - adding irregex-extract and irregex-split
;;                     *-fold copies match data (use *-fold/fast for speed)
;;                     irregex-opt now returns an SRE
;; 0.7.4: 2009/05/14 - empty alternates (or) and empty csets always fail,
;;                     bugfix in default finalizer for irregex-fold/chunked
;; 0.7.3: 2009/04/14 - adding irregex-fold/chunked, minor doc fixes
;; 0.7.2: 2009/02/11 - some bugfixes, much improved documentation
;; 0.7.1: 2008/10/30 - several bugfixes (thanks to Derick Eddington)
;; 0.7.0: 2008/10/20 - support abstract chunked strings
;; 0.6.2: 2008/07/26 - minor bugfixes, allow global disabling of utf8 mode,
;;                     friendlier error messages in parsing, \Q..\E support
;; 0.6.1: 2008/07/21 - added utf8 mode, more utils, bugfixes
;;   0.6: 2008/05/01 - most of PCRE supported
;;   0.5: 2008/04/24 - fully portable R4RS, many PCRE features implemented
;;   0.4: 2008/04/17 - rewriting NFA to use efficient closure compilation,
;;                     normal strings only, but all of the spencer tests pass
;;   0.3: 2008/03/10 - adding DFA converter (normal strings only)
;;   0.2: 2005/09/27 - adding irregex-opt (like elisp's regexp-opt) utility
;;   0.1: 2005/08/18 - simple NFA interpreter over abstract chunked strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Data Structures

(define irregex-tag '*irregex-tag*)

(define (make-irregex dfa dfa/search dfa/extract nfa flags
                      submatches lengths names)
  (vector irregex-tag dfa dfa/search dfa/extract nfa flags
          submatches lengths names))

(define (irregex? obj)
  (and (vector? obj)
       (= 9 (vector-length obj))
       (eq? irregex-tag (vector-ref obj 0))))

(define (irregex-dfa x) (vector-ref x 1))
(define (irregex-dfa/search x) (vector-ref x 2))
(define (irregex-dfa/extract x) (vector-ref x 3))
(define (irregex-nfa x) (vector-ref x 4))
(define (irregex-flags x) (vector-ref x 5))
(define (irregex-num-submatches x) (vector-ref x 6))
(define (irregex-lengths x) (vector-ref x 7))
(define (irregex-names x) (vector-ref x 8))

(define (irregex-new-matches irx)
  (make-irregex-match (irregex-num-submatches irx) (irregex-names irx)))

(define (irregex-reset-matches! m)
  (do ((i (- (vector-length m) 1) (- i 1)))
      ((<= i 3) m)
    (vector-set! m i #f)))

(define (irregex-copy-matches m)
  (and (vector? m)
       (let ((r (make-vector (vector-length m))))
         (do ((i (- (vector-length m) 1) (- i 1)))
             ((< i 0) r)
           (vector-set! r i (vector-ref m i))))))

(define irregex-match-tag '*irregex-match-tag*)

(define (irregex-match-data? obj)
  (and (vector? obj)
       (>= (vector-length obj) 11)
       (eq? irregex-match-tag (vector-ref obj 0))))

(define (make-irregex-match count names)
  (let ((res (make-vector (+ (* 4 (+ 2 count)) 3) #f)))
    (vector-set! res 0 irregex-match-tag)
    (vector-set! res 2 names)
    res))

(define (irregex-match-num-submatches m)
  (- (quotient (- (vector-length m) 3) 4) 2))

(define (irregex-match-chunker m)
  (vector-ref m 1))
(define (irregex-match-names m)
  (vector-ref m 2))
(define (irregex-match-chunker-set! m str)
  (vector-set! m 1 str))

(define (%irregex-match-start-chunk m n) (vector-ref m (+ 3 (* n 4))))
(define (%irregex-match-start-index m n) (vector-ref m (+ 4 (* n 4))))
(define (%irregex-match-end-chunk m n)   (vector-ref m (+ 5 (* n 4))))
(define (%irregex-match-end-index m n)   (vector-ref m (+ 6 (* n 4))))

(define (%irregex-match-fail m)
  (vector-ref m (- (vector-length m) 1)))
(define (%irregex-match-fail-set! m x)
  (vector-set! m (- (vector-length m) 1) x))

;; public interface with error checking
(define (irregex-match-start-chunk m . opt)
  (let ((n (irregex-match-numeric-index "irregex-match-start-chunk" m opt)))
    (and n (%irregex-match-start-chunk m n))))
(define (irregex-match-start-index m . opt)
  (let ((n (irregex-match-numeric-index "irregex-match-start-index" m opt)))
    (and n (%irregex-match-start-index m n))))
(define (irregex-match-end-chunk m . opt)
  (let ((n (irregex-match-numeric-index "irregex-match-end-chunk" m opt)))
    (and n (%irregex-match-end-chunk m n))))
(define (irregex-match-end-index m . opt)
  (let ((n (irregex-match-numeric-index "irregex-match-end-index" m opt)))
    (and n (%irregex-match-end-index m n))))

(define (irregex-match-start-chunk-set! m n start)
  (vector-set! m (+ 3 (* n 4)) start))
(define (irregex-match-start-index-set! m n start)
  (vector-set! m (+ 4 (* n 4)) start))
(define (irregex-match-end-chunk-set! m n end)
  (vector-set! m (+ 5 (* n 4)) end))
(define (irregex-match-end-index-set! m n end)
  (vector-set! m (+ 6 (* n 4)) end))

;; Helper procedure to convert any type of index from a rest args list
;; to a numeric index.  Named submatches are converted to their corresponding
;; numeric index, and numeric submatches are checked for validity.
;; An error is raised for invalid numeric or named indices, #f is returned
;; for defined but nonmatching indices.
(define (irregex-match-numeric-index location m opt)
  (cond
   ((not (irregex-match-data? m))
    (error (string-append location ": not match data") m))
   ((not (pair? opt)) 0)
   ((pair? (cdr opt))
    (apply error (string-append location ": too many arguments") m opt))
   (else
    (let ((n (car opt)))
      (if (number? n)
          (if (and (integer? n) (exact? n))
              (if (irregex-match-valid-numeric-index? m n)
                  (and (irregex-match-matched-numeric-index? m n) n)
                  (error (string-append location ": not a valid index")
                         m n))
              (error (string-append location ": not an exact integer") n))
          (let lp ((ls (irregex-match-names m))
                   (unknown? #t))
            (cond
             ((null? ls)
              (and unknown?
                   (error (string-append location ": unknown match name") n)))
             ((eq? n (caar ls))
              (if (%irregex-match-start-chunk m (cdar ls))
                  (cdar ls)
                  (lp (cdr ls) #f)))
             (else (lp (cdr ls) unknown?)))))))))

(define (irregex-match-valid-numeric-index? m n)
  (and (>= n 0) (< (+ 3 (* n 4)) (- (vector-length m) 4))))

(define (irregex-match-matched-numeric-index? m n)
  (and (vector-ref m (+ 4 (* n 4)))
       #t))

(define (irregex-match-valid-named-index? m n)
  (and (assq n (irregex-match-names m))
       #t))

(define (irregex-match-valid-index? m n)
  (if (not (irregex-match-data? m))
      (error "irregex-match-valid-index?: not match data" m))
  (if (integer? n)
      (if (not (exact? n))
          (error "irregex-match-valid-index?: not an exact integer" n)
          (irregex-match-valid-numeric-index? m n))
      (irregex-match-valid-named-index? m n)))

(define (irregex-match-substring m . opt)
  (let* ((n (irregex-match-numeric-index "irregex-match-substring" m opt))
         (cnk (irregex-match-chunker m)))
    (and n
         ((chunker-get-substring cnk)
          (%irregex-match-start-chunk m n)
          (%irregex-match-start-index m n)
          (%irregex-match-end-chunk m n)
          (%irregex-match-end-index m n)))))

(define (irregex-match-subchunk m . opt)
  (let* ((n (irregex-match-numeric-index "irregex-match-subchunk" m opt))
         (cnk (irregex-match-chunker m))
         (get-subchunk (chunker-get-subchunk cnk)))
    (if (not get-subchunk)
        (error "this chunk type does not support match subchunks")
        (and n (get-subchunk
                (%irregex-match-start-chunk m n)
                (%irregex-match-start-index m n)
                (%irregex-match-end-chunk m n)
                (%irregex-match-end-index m n))))))

;; chunkers tell us how to navigate through chained chunks of strings

(define (make-irregex-chunker get-next get-str . o)
  (let* ((get-start (or (and (pair? o) (car o)) (lambda (cnk) 0)))
         (o (if (pair? o) (cdr o) o))
         (get-end (or (and (pair? o) (car o))
                      (lambda (cnk) (string-length (get-str cnk)))))
         (o (if (pair? o) (cdr o) o))
         (get-substr
          (or (and (pair? o) (car o))
              (lambda (cnk1 start cnk2 end)
                (if (eq? cnk1 cnk2)
                    (substring (get-str cnk1) start end)
                    (let loop ((cnk (get-next cnk1))
                               (res (list (substring (get-str cnk1)
                                                     start
                                                     (get-end cnk1)))))
                      (if (eq? cnk cnk2)
                          (string-cat-reverse
                           (cons (substring (get-str cnk)
                                            (get-start cnk)
                                            end)
                                 res))
                          (loop (get-next cnk)
                                (cons (substring (get-str cnk)
                                                 (get-start cnk)
                                                 (get-end cnk))
                                      res))))))))
         (o (if (pair? o) (cdr o) o))
         (get-subchunk (and (pair? o) (car o))))
    (if (not (and (procedure? get-next) (procedure? get-str)
                  (procedure? get-start) (procedure? get-substr)))
        (error "make-irregex-chunker: expected a procdure"))
    (vector get-next get-str get-start get-end get-substr get-subchunk)))

(define (chunker-get-next cnk) (vector-ref cnk 0))
(define (chunker-get-str cnk) (vector-ref cnk 1))
(define (chunker-get-start cnk) (vector-ref cnk 2))
(define (chunker-get-end cnk) (vector-ref cnk 3))
(define (chunker-get-substring cnk) (vector-ref cnk 4))
(define (chunker-get-subchunk cnk) (vector-ref cnk 5))

(define (chunker-prev-chunk cnk start end)
  (if (eq? start end)
      #f
      (let ((get-next (chunker-get-next cnk)))
        (let lp ((start start))
          (let ((next (get-next start)))
            (if (eq? next end)
                start
                (and next (lp next))))))))

(define (chunker-prev-char cnk start end)
  (let ((prev (chunker-prev-chunk cnk start end)))
    (and prev
         (string-ref ((chunker-get-str cnk) prev)
                     (- ((chunker-get-end cnk) prev) 1)))))

(define (chunker-next-char cnk src)
  (let ((next ((chunker-get-next cnk) src)))
    (and next
         (string-ref ((chunker-get-str cnk) next)
                     ((chunker-get-start cnk) next)))))

(define (chunk-before? cnk a b)
  (and (not (eq? a b))
       (let ((next ((chunker-get-next cnk) a)))
         (and next
              (if (eq? next b)
                  #t
                  (chunk-before? cnk next b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; String Utilities

;; Unicode version (skip surrogates)
(define *all-chars*
  `(/ ,(integer->char 0) ,(integer->char #xD7FF)
      ,(integer->char #xE000) ,(integer->char #x10FFFF)))

;; ASCII version, offset to not assume 0-255
;; (define *all-chars* `(/ ,(integer->char (- (char->integer #\space) 32)) ,(integer->char (+ (char->integer #\space) 223))))

;; set to #f to ignore even an explicit request for utf8 handling
(define *allow-utf8-mode?* #f)

;; (define *named-char-properties* '())

(define (string-scan-char str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (string-scan-char-escape str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            ((eqv? c #\\) (scan (+ i 2)))
            (else (scan (+ i 1)))))))

(define (string-scan-pred str pred . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((pred (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (string-split-char str c)
  (let ((end (string-length str)))
    (let lp ((i 0) (from 0) (res '()))
      (define (collect) (cons (substring str from i) res))
      (cond ((>= i end) (reverse (collect)))
            ((eqv? c (string-ref str i)) (lp (+ i 1) (+ i 1) (collect)))
            (else (lp (+ i 1) from res))))))

(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

(define (%substring=? a b start1 start2 len)
  (let lp ((i 0))
    (cond ((>= i len)
           #t)
          ((char=? (string-ref a (+ start1 i)) (string-ref b (+ start2 i)))
           (lp (+ i 1)))
          (else
           #f))))

;; SRFI-13 extracts

(define (%%string-copy! to tstart from fstart fend)
  (do ((i fstart (+ i 1))
       (j tstart (+ j 1)))
      ((>= i fend))
    (string-set! to j (string-ref from i))))

(define (string-cat-reverse string-list)
  (string-cat-reverse/aux
   (fold (lambda (s a) (+ (string-length s) a)) 0 string-list)
   string-list))

(define (string-cat-reverse/aux len string-list)
  (let ((res (make-string len)))
    (let lp ((i len) (ls string-list))
      (if (pair? ls)
          (let* ((s (car ls))
                 (slen (string-length s))
                 (i (- i slen)))
            (%%string-copy! res i s 0 slen)
            (lp i (cdr ls)))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; List Utilities

;; like the one-arg IOTA case
(define (zero-to n)
  (if (<= n 0)
      '()
      (let lp ((i (- n 1)) (res '()))
        (if (zero? i) (cons 0 res) (lp (- i 1) (cons i res))))))

;; SRFI-1 extracts (simplified 1-ary versions)

(define (find-tail pred ls)
  (let lp ((ls ls))
    (cond ((null? ls) #f)
          ((pred (car ls)) ls)
          (else (lp (cdr ls))))))

(define (last ls)
  (if (not (pair? ls))
      (error "can't take last of empty list" ls)
      (let lp ((ls ls))
        (if (pair? (cdr ls))
            (lp (cdr ls))
            (car ls)))))

(define (any pred ls)
  (and (pair? ls)
       (let lp ((head (car ls)) (tail (cdr ls)))
         (if (null? tail)
             (pred head)
             (or (pred head) (lp (car tail) (cdr tail)))))))

(define (every pred ls)
  (or (null? ls)
      (let lp ((head (car ls))  (tail (cdr ls)))
        (if (null? tail)
            (pred head)
            (and (pred head) (lp (car tail) (cdr tail)))))))

(define (fold kons knil ls)
  (let lp ((ls ls) (res knil))
    (if (null? ls)
        res
        (lp (cdr ls) (kons (car ls) res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Flags

(define (bit-shr n i)
  (quotient n (expt 2 i)))

(define (bit-shl n i)
  (* n (expt 2 i)))

(define (bit-not n) (- #xFFFF n))

(define (bit-ior a b)
  (cond
   ((zero? a) b)
   ((zero? b) a)
   (else
    (+ (if (or (odd? a) (odd? b)) 1 0)
       (* 2 (bit-ior (quotient a 2) (quotient b 2)))))))

(define (bit-and a b)
  (cond
   ((zero? a) 0)
   ((zero? b) 0)
   (else
    (+ (if (and (odd? a) (odd? b)) 1 0)
       (* 2 (bit-and (quotient a 2) (quotient b 2)))))))

(define (integer-log n)
  (define (b8 n r)
    (if (>= n (bit-shl 1 8)) (b4 (bit-shr n 8) (+ r 8)) (b4 n r)))
  (define (b4 n r)
    (if (>= n (bit-shl 1 4)) (b2 (bit-shr n 4) (+ r 4)) (b2 n r)))
  (define (b2 n r)
    (if (>= n (bit-shl 1 2)) (b1 (bit-shr n 2) (+ r 2)) (b1 n r)))
  (define (b1 n r) (if (>= n (bit-shl 1 1)) (+ r 1) r))
  (if (>= n (bit-shl 1 16)) (b8 (bit-shr n 16) 16) (b8 n 0)))

(define (flag-set? flags i)
  (= i (bit-and flags i)))
(define (flag-join a b)
  (if b (bit-ior a b) a))
(define (flag-clear a b)
  (bit-and a (bit-not b)))

(define ~none 0)
(define ~searcher? 1)
(define ~consumer? 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing Embedded SREs in PCRE Strings

;; (define (with-read-from-string str i proc)
;;   (define (port-size in)
;;     (let lp ((i 0)) (if (eof-object? (read-char in)) i (lp (+ i 1)))))
;;   (let* ((len (string-length str))
;;          (tail-len (- len i))
;;          (in (open-input-string (substring str i len)))
;;          (sre (read in))
;;          (unused-len (port-size in)))
;;     (close-input-port in)
;;     (proc sre (- tail-len unused-len))))

(define close-token (list 'close))
(define dot-token (string->symbol "."))

(define (with-read-from-string str i proc)
  (define end (string-length str))
  (define (read i k)
    (cond
     ((>= i end) (error "unterminated embedded SRE" str))
     (else
      (case (string-ref str i)
        ((#\()
         (let lp ((i (+ i 1)) (ls '()))
           (read
            i
            (lambda (x j)
              (cond
               ((eq? x close-token)
                (k (reverse ls) j))
               ((eq? x dot-token)
                (if (null? ls)
                    (error "bad dotted form" str)
                    (read j (lambda (y j2)
                              (read j2 (lambda (z j3)
                                         (if (not (eq? z close-token))
                                             (error "bad dotted form" str)
                                             (k (append (reverse (cdr ls))
                                                        (cons (car ls) y))
                                                j3))))))))
               (else
                (lp j (cons x ls))))))))
        ((#\))
         (k close-token (+ i 1)))
        ((#\;)
         (let skip ((i (+ i 1)))
           (if (or (>= i end) (eqv? #\newline (string-ref str i)))
               (read (+ i 1) k)
               (skip (+ i 1)))))
        ((#\' #\`)
         (read (+ i 1)
           (lambda (sexp j)
             (let ((q (if (eqv? #\' (string-ref str i)) 'quote 'quasiquote)))
               (k (list q sexp) j)))))
        ((#\,)
         (let* ((at? (and (< (+ i 1) end) (eqv? #\@ (string-ref str (+ i 1)))))
                (u (if at? 'uquote-splicing 'unquote))
                (j (if at? (+ i 2) (+ i 1))))
           (read j (lambda (sexp j) (k (list u sexp) j)))))
        ((#\")
         (let scan ((from (+ i 1)) (i (+ i 1)) (res '()))
           (define (collect)
             (if (= from i) res (cons (substring str from i) res)))
           (if (>= i end)
               (error "unterminated string in embeded SRE" str)
               (case (string-ref str i)
                 ((#\") (k (string-cat-reverse (collect)) (+ i 1)))
                 ((#\\) (scan (+ i 1) (+ i 2) (collect)))
                 (else (scan from (+ i 1) res))))))
        ((#\#)
         (case (string-ref str (+ i 1))
           ((#\;)
            (read (+ i 2) (lambda (sexp j) (read j k))))
           ((#\\)
            (read (+ i 2)
              (lambda (sexp j)
                (k (case sexp
                     ((space) #\space)
                     ((newline) #\newline)
                     (else (let ((s (if (number? sexp)
                                        (number->string sexp)
                                        (symbol->string sexp))))
                             (string-ref s 0))))
                   j))))
           ((#\t #\f)
            (k (eqv? #\t (string-ref str (+ i 1))) (+ i 2)))
           (else
            (error "bad # syntax in simplified SRE" i))))
        (else
         (cond
          ((char-whitespace? (string-ref str i))
           (read (+ i 1) k))
          (else ;; symbol/number
           (let scan ((j (+ i 1)))
             (cond
              ((or (>= j end)
                   (let ((c (string-ref str j)))
                     (or (char-whitespace? c)
                         (memv c '(#\; #\( #\) #\" #\# #\\)))))
               (let ((str2 (substring str i j)))
                 (k (or (string->number str2) (string->symbol str2)) j)))
              (else (scan (+ j 1))))))))))))
  (read i (lambda (res j)
            (if (eq? res 'close-token)
                (error "unexpected ')' in SRE" str j)
                (proc res j)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing PCRE Strings

(define ~save? 1)
(define ~case-insensitive? 2)
(define ~multi-line? 4)
(define ~single-line? 8)
(define ~ignore-space? 16)
(define ~utf8? 32)

(define (symbol-list->flags ls)
  (let lp ((ls ls) (res ~none))
    (if (not (pair? ls))
        res
        (lp (cdr ls)
            (flag-join
             res
             (case (car ls)
               ((i ci case-insensitive) ~case-insensitive?)
               ((m multi-line) ~multi-line?)
               ((s single-line) ~single-line?)
               ((x ignore-space) ~ignore-space?)
               ((u utf8) (if *allow-utf8-mode?* ~utf8? ~none))
               (else #f)))))))

(define (maybe-string->sre obj)
  (if (string? obj) (string->sre obj) obj))

(define (string->sre str . o)
  (if (not (string? str)) (error "string->sre: expected a string" str))
  (let ((end (string-length str))
        (flags (symbol-list->flags o)))

    (let lp ((i 0) (from 0) (flags flags) (res '()) (st '()))

      ;; handle case sensitivity at the literal char/string level
      (define (cased-char ch)
        (if (and (flag-set? flags ~case-insensitive?)
                 (char-alphabetic? ch))
            `(or ,ch ,(char-altcase ch))
            ch))
      (define (cased-string str)
        (if (flag-set? flags ~case-insensitive?)
            (sre-sequence (map cased-char (string->list str)))
            str))
      ;; accumulate the substring from..i as literal text
      (define (collect)
        (if (= i from) res (cons (cased-string (substring str from i)) res)))
      ;; like collect but breaks off the last single character when
      ;; collecting literal data, as the argument to ?/*/+ etc.
      (define (collect/single)
        (let* ((utf8? (flag-set? flags ~utf8?))
               (j (if (and utf8? (> i 1))
                      (utf8-backup-to-initial-char str (- i 1))
                      (- i 1))))
          (cond
           ((< j from)
            res)
           (else
            (let ((c (cased-char (if utf8?
                                     (utf8-string-ref str j (- i j))
                                     (string-ref str j)))))
              (cond
               ((= j from)
                (cons c res))
               (else
                (cons c
                      (cons (cased-string (substring str from j))
                            res)))))))))
      ;; collects for use as a result, reversing and grouping OR
      ;; terms, and some ugly tweaking of `function-like' groups and
      ;; conditionals
      (define (collect/terms)
        (let* ((ls (collect))
               (func
                (and (pair? ls)
                     (memq (last ls)
                           '(atomic if look-ahead neg-look-ahead
                                    look-behind neg-look-behind
                                    => submatch-named
                                    w/utf8 w/noutf8))))
               (prefix (if (and func (memq (car func) '(=> submatch-named)))
                           (list 'submatch-named (cadr (reverse ls)))
                           (and func (list (car func)))))
               (ls (if func
                       (if (memq (car func) '(=> submatch-named))
                           (reverse (cddr (reverse ls)))
                           (reverse (cdr (reverse ls))))
                       ls)))
          (let lp ((ls ls) (term '()) (res '()))
            (define (shift)
              (cons (sre-sequence term) res))
            (cond
             ((null? ls)
              (let* ((res (sre-alternate (shift)))
                     (res (if (flag-set? flags ~save?)
                              (list 'submatch res)
                              res)))
                (if prefix
                    (if (eq? 'if (car prefix))
                        (cond
                         ((not (pair? res))
                          'epsilon)
                         ((memq (car res)
                                '(look-ahead neg-look-ahead
                                             look-behind neg-look-behind))
                          res)
                         ((eq? 'seq (car res))
                          `(if ,(cadr res)
                               ,(if (pair? (cdr res))
                                    (sre-sequence (cddr res))
                                    'epsilon)))
                         (else
                          `(if ,(cadadr res)
                               ,(if (pair? (cdr res))
                                    (sre-sequence (cddadr res))
                                    'epsilon)
                               ,(sre-alternate
                                 (if (pair? (cdr res)) (cddr res) '())))))
                        `(,@prefix ,res))
                    res)))
             ((eq? 'or (car ls)) (lp (cdr ls) '() (shift)))
             (else (lp (cdr ls) (cons (car ls) term) res))))))
      (define (save)
        (cons (cons flags (collect)) st))

      ;; main parsing
      (if (>= i end)
          (if (pair? st)
              (error "unterminated parenthesis in regexp" str)
              (collect/terms))
          (let ((c (string-ref str i)))
            (case c
              ((#\.)
               (lp (+ i 1) (+ i 1) flags
                   (cons (if (flag-set? flags ~single-line?) 'any 'nonl)
                         (collect))
                   st))
              ((#\?)
               (let ((res (collect/single)))
                 (if (null? res)
                     (error "? can't follow empty pattern" str res)
                     (let ((x (car res)))
                       (lp (+ i 1)
                           (+ i 1)
                           flags
                           (cons
                            (if (pair? x)
                                (case (car x)
                                  ((*)  `(*? ,@(cdr x)))
                                  ((+)  `(**? 1 #f ,@(cdr x)))
                                  ((?)  `(?? ,@(cdr x)))
                                  ((**) `(**? ,@(cdr x)))
                                  ((=)  `(**? ,(cadr x) ,@(cdr x)))
                                  ((>=)  `(**? ,(cadr x) #f ,@(cddr x)))
                                  (else `(? ,x)))
                                `(? ,x))
                            (cdr res))
                           st)))))
              ((#\+ #\*)
               (let* ((res (collect/single))
                      (x (if (pair? res) (car res) 'epsilon))
                      (op (string->symbol (string c))))
                 (cond
                  ((sre-repeater? x)
                   (error "duplicate repetition (e.g. **) in pattern" str res))
                  ((sre-empty? x)
                   (error "can't repeat empty pattern (e.g. ()*)" str res))
                  (else
                   (lp (+ i 1) (+ i 1) flags
                       (cons (list op x) (cdr res))
                       st)))))
              ((#\()
               (cond
                ((>= (+ i 1) end)
                 (error "unterminated parenthesis in regexp" str))
                ((not (memv (string-ref str (+ i 1)) '(#\? #\*))) ; normal case
                 (lp (+ i 1) (+ i 1) (flag-join flags ~save?) '() (save)))
                ((>= (+ i 2) end)
                 (error "unterminated parenthesis in regexp" str))
                ((eqv? (string-ref str (+ i 1)) #\*)
                 (if (eqv? #\' (string-ref str (+ i 2)))
                     (with-read-from-string str (+ i 3)
                       (lambda (sre j)
                         (if (or (>= j end) (not (eqv? #\) (string-ref str j))))
                             (error "unterminated (*'...) SRE escape" str)
                             (lp (+ j 1) (+ j 1) flags (cons sre (collect)) st))))
                     (error "bad regexp syntax: (*FOO) not supported" str)))
                (else                   ;; (?...) case
                 (case (string-ref str (+ i 2))
                   ((#\#)
                    (let ((j (string-scan-char str #\) (+ i 3))))
                      (lp (+ j i) (+ j 1) flags (collect) st)))
                   ((#\:)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?) '() (save)))
                   ((#\=)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(look-ahead) (save)))
                   ((#\!)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(neg-look-ahead) (save)))
                   ((#\<)
                    (cond
                     ((>= (+ i 3) end)
                      (error "unterminated parenthesis in regexp" str))
                     (else
                      (case (string-ref str (+ i 3))
                        ((#\=)
                         (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                             '(look-behind) (save)))
                        ((#\!)
                         (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                             '(neg-look-behind) (save)))
                        (else
                         (let ((j (and (char-alphabetic?
                                        (string-ref str (+ i 3)))
                                       (string-scan-char str #\> (+ i 4)))))
                           (if j
                               (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                                   `(,(string->symbol (substring str (+ i 3) j))
                                     submatch-named)
                                   (save))
                               (error "invalid (?< sequence" str))))))))
                   ((#\>)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(atomic) (save)))
                   ;;((#\' #\P) ; named subpatterns
                   ;; )
                   ;;((#\R) ; recursion
                   ;; )
                   ((#\()
                    (cond
                     ((>= (+ i 3) end)
                      (error "unterminated parenthesis in regexp" str))
                     ((char-numeric? (string-ref str (+ i 3)))
                      (let* ((j (string-scan-char str #\) (+ i 3)))
                             (n (string->number (substring str (+ i 3) j))))
                        (if (not n)
                            (error "invalid conditional reference" str)
                            (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                                `(,n if) (save)))))
                     ((char-alphabetic? (string-ref str (+ i 3)))
                      (let* ((j (string-scan-char str #\) (+ i 3)))
                             (s (string->symbol (substring str (+ i 3) j))))
                        (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                            `(,s if) (save))))
                     (else
                      (lp (+ i 2) (+ i 2) (flag-clear flags ~save?)
                          '(if) (save)))))
                   ((#\{)
                    (error "unsupported Perl-style cluster" str))
                   (else
                    (let ((old-flags flags))
                      (let lp2 ((j (+ i 2)) (flags flags) (invert? #f))
                        (define (join x)
                          ((if invert? flag-clear flag-join) flags x))
                        (define (new-res res)
                          (let ((before (flag-set? old-flags ~utf8?))
                                (after (flag-set? flags ~utf8?)))
                            (if (eq? before after)
                                res
                                (cons (if after 'w/utf8 'w/noutf8) res))))
                        (cond
                         ((>= j end)
                          (error "incomplete cluster" str i))
                         (else
                          (case (string-ref str j)
                            ((#\i)
                             (lp2 (+ j 1) (join ~case-insensitive?) invert?))
                            ((#\m)
                             (lp2 (+ j 1) (join ~multi-line?) invert?))
                            ((#\x)
                             (lp2 (+ j 1) (join ~ignore-space?) invert?))
                            ((#\u)
                             (if *allow-utf8-mode?*
                                 (lp2 (+ j 1) (join ~utf8?) invert?)
                                 (lp2 (+ j 1) flags invert?)))
                            ((#\-)
                             (lp2 (+ j 1) flags (not invert?)))
                            ((#\))
                             (lp (+ j 1) (+ j 1) flags (new-res (collect))
                                 st))
                            ((#\:)
                             (lp (+ j 1) (+ j 1) flags (new-res '())
                                 (cons (cons old-flags (collect)) st)))
                            (else
                             (error "unknown regex cluster modifier" str)
                             )))))))))))
              ((#\))
               (if (null? st)
                   (error "too many )'s in regexp" str)
                   (lp (+ i 1)
                       (+ i 1)
                       (caar st)
                       (cons (collect/terms) (cdar st))
                       (cdr st))))
              ((#\[)
               (apply
                (lambda (sre j)
                  (lp (+ j 1) (+ j 1) flags (cons sre (collect)) st))
                (string-parse-cset str (+ i 1) flags)))
              ((#\{)
               (cond
                ((or (>= (+ i 1) end)
                     (not (or (char-numeric? (string-ref str (+ i 1)))
                              (eqv? #\, (string-ref str (+ i 1))))))
                 (lp (+ i 1) from flags res st))
                (else
                 (let ((res (collect/single)))
                   (cond
                    ((null? res)
                     (error "{ can't follow empty pattern"))
                    (else
                     (let* ((x (car res))
                            (tail (cdr res))
                            (j (string-scan-char str #\} (+ i 1)))
                            (s2 (string-split-char (substring str (+ i 1) j)
                                                   #\,))
                            (n (string->number (car s2)))
                            (m (and (pair? (cdr s2))
                                    (string->number (cadr s2)))))
                       (cond
                        ((or (not n)
                             (and (pair? (cdr s2))
                                  (not (equal? "" (cadr s2)))
                                  (not m)))
                         (error "invalid {n} repetition syntax" s2))
                        ((null? (cdr s2))
                         (lp (+ j 1) (+ j 1) flags `((= ,n ,x) ,@tail) st))
                        (m
                         (lp (+ j 1) (+ j 1) flags `((** ,n ,m ,x) ,@tail) st))
                        (else
                         (lp (+ j 1) (+ j 1) flags `((>= ,n ,x) ,@tail) st)
                         )))))))))
              ((#\\)
               (cond
                ((>= (+ i 1) end)
                 (error "incomplete escape sequence" str))
                (else
                 (let ((c (string-ref str (+ i 1))))
                   (case c
                     ((#\d)
                      (lp (+ i 2) (+ i 2) flags `(numeric ,@(collect)) st))
                     ((#\D)
                      (lp (+ i 2) (+ i 2) flags `((~ numeric) ,@(collect)) st))
                     ((#\s)
                      (lp (+ i 2) (+ i 2) flags `(space ,@(collect)) st))
                     ((#\S)
                      (lp (+ i 2) (+ i 2) flags `((~ space) ,@(collect)) st))
                     ((#\w)
                      (lp (+ i 2) (+ i 2) flags
                          `((or alphanumeric ("_")) ,@(collect)) st))
                     ((#\W)
                      (lp (+ i 2) (+ i 2) flags
                          `((~ (or alphanumeric ("_"))) ,@(collect)) st))
                     ((#\b)
                      (lp (+ i 2) (+ i 2) flags
                          `((or bow eow) ,@(collect)) st))
                     ((#\B)
                      (lp (+ i 2) (+ i 2) flags `(nwb ,@(collect)) st))
                     ((#\A)
                      (lp (+ i 2) (+ i 2) flags `(bos ,@(collect)) st))
                     ((#\Z)
                      (lp (+ i 2) (+ i 2) flags
                          `((? #\newline) eos ,@(collect)) st))
                     ((#\z)
                      (lp (+ i 2) (+ i 2) flags `(eos ,@(collect)) st))
                     ((#\R)
                      (lp (+ i 2) (+ i 2) flags `(newline ,@(collect)) st))
                     ((#\K)
                      (lp (+ i 2) (+ i 2) flags `(reset ,@(collect)) st))
                     ;; these two are from Emacs and TRE, but not in PCRE
                     ((#\<)
                      (lp (+ i 2) (+ i 2) flags `(bow ,@(collect)) st))
                     ((#\>)
                      (lp (+ i 2) (+ i 2) flags `(eow ,@(collect)) st))
                     ((#\x)
                      (apply
                       (lambda (ch j)
                         (lp (+ j 1) (+ j 1) flags `(,ch ,@(collect)) st))
                       (string-parse-hex-escape str (+ i 2) end)))
                     ((#\k)
                      (let ((c (string-ref str (+ i 2))))
                        (if (not (memv c '(#\< #\{ #\')))
                            (error "bad \\k usage, expected \\k<...>" str)
                            (let* ((terminal (char-mirror c))
                                   (j (string-scan-char str terminal (+ i 2)))
                                   (s (and j (substring str (+ i 3) j)))
                                   (backref
                                    (if (flag-set? flags ~case-insensitive?)
                                        'backref-ci
                                        'backref)))
                              (if (not j)
                                  (error "unterminated named backref" str)
                                  (lp (+ j 1) (+ j 1) flags
                                      `((,backref ,(string->symbol s))
                                        ,@(collect))
                                      st))))))
                     ((#\Q)  ;; \Q..\E escapes
                      (let ((res (collect)))
                        (let lp2 ((j (+ i 2)))
                          (cond
                           ((>= j end)
                            (lp j (+ i 2) flags res st))
                           ((eqv? #\\ (string-ref str j))
                            (cond
                             ((>= (+ j 1) end)
                              (lp (+ j 1) (+ i 2) flags res st))
                             ((eqv? #\E (string-ref str (+ j 1)))
                              (lp (+ j 2) (+ j 2) flags
                                  (cons (substring str (+ i 2) j) res) st))
                             (else
                              (lp2 (+ j 2)))))
                           (else
                            (lp2 (+ j 1)))))))
                     ((#\')
                      (with-read-from-string str (+ i 2)
                       (lambda (sre j)
                         (lp j j flags (cons sre (collect)) st))))
                     ;;((#\p)  ; XXXX unicode properties
                     ;; )
                     ;;((#\P)
                     ;; )
                     (else
                      (cond
                       ((char-numeric? c)
                        (let* ((j (or (string-scan-pred
                                       str
                                       (lambda (c) (not (char-numeric? c)))
                                       (+ i 2))
                                      end))
                               (backref
                                (if (flag-set? flags ~case-insensitive?)
                                    'backref-ci
                                    'backref))
                               (res `((,backref ,(string->number
                                                  (substring str (+ i 1) j)))
                                      ,@(collect))))
                          (lp j j flags res st)))
                       ((char-alphabetic? c)
                        (let ((cell (assv c posix-escape-sequences)))
                          (if cell
                              (lp (+ i 2) (+ i 2) flags
                                  (cons (cdr cell) (collect)) st)
                              (error "unknown escape sequence" str c))))
                       (else
                        (lp (+ i 2) (+ i 1) flags (collect) st)))))))))
              ((#\|)
               (lp (+ i 1) (+ i 1) flags (cons 'or (collect)) st))
              ((#\^)
               (let ((sym (if (flag-set? flags ~multi-line?) 'bol 'bos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
              ((#\$)
               (let ((sym (if (flag-set? flags ~multi-line?) 'eol 'eos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
              ((#\space)
               (if (flag-set? flags ~ignore-space?)
                   (lp (+ i 1) (+ i 1) flags (collect) st)
                   (lp (+ i 1) from flags res st)))
              ((#\#)
               (if (flag-set? flags ~ignore-space?)
                   (let ((j (or (string-scan-char str #\newline (+ i 1))
                                (- end 1))))
                     (lp (+ j 1) (+ j 1) flags (collect) st))
                   (lp (+ i 1) from flags res st)))
              (else
               (lp (+ i 1) from flags res st))))))))

(define posix-escape-sequences
  `((#\n . #\newline)
    (#\r . ,(integer->char (+ (char->integer #\newline) 3)))
    (#\t . ,(integer->char (- (char->integer #\newline) 1)))
    (#\a . ,(integer->char (- (char->integer #\newline) 3)))
    (#\e . ,(integer->char (+ (char->integer #\newline) #x11)))
    (#\f . ,(integer->char (+ (char->integer #\newline) 2)))
    ))

(define (char-altcase c)
  (if (char-upper-case? c) (char-downcase c) (char-upcase c)))

(define (char-mirror c)
  (case c ((#\<) #\>) ((#\{) #\}) ((#\() #\)) ((#\[) #\]) (else c)))

(define (string-parse-hex-escape str i end)
  (cond
   ((>= i end)
    (error "incomplete hex escape" str i))
   ((eqv? #\{ (string-ref str i))
    (let ((j (string-scan-char-escape str #\} (+ i 1))))
      (if (not j)
          (error "incomplete hex brace escape" str i)
          (let* ((s (substring str (+ i 1) j))
                 (n (string->number s 16)))
            (if n
                (list (integer->char n) j)
                (error "bad hex brace escape" s))))))
   ((>= (+ i 1) end)
    (error "incomplete hex escape" str i))
   (else
    (let* ((s (substring str i (+ i 2)))
           (n (string->number s 16)))
      (if n
          (list (integer->char n) (+ i 2))
          (error "bad hex escape" s))))))

(define (string-parse-cset str start flags)
  (let* ((end (string-length str))
         (invert? (and (< start end) (eqv? #\^ (string-ref str start))))
         (utf8? (flag-set? flags ~utf8?)))
    (define (go i prev-char cset)
      (if (>= i end)
          (error "incomplete char set" str i end)
          (let ((c (string-ref str i)))
            (case c
              ((#\])
               (if (cset-empty? cset)
                   (go (+ i 1) #\] (cset-adjoin cset #\]))
                   (let ((ci? (flag-set? flags ~case-insensitive?)))
                     (list
                      (let ((res (if ci? (cset-case-insensitive cset) cset)))
                        (cset->sre (if invert? (cset-complement res) res)))
                      i))))
              ((#\-)
               (cond
                ((or (= i start)
                     (and (= i (+ start 1)) (eqv? #\^ (string-ref str start)))
                     (eqv? #\] (string-ref str (+ i 1))))
                 (go (+ i 1) c (cset-adjoin cset c)))
                ((not prev-char)
                 (error "bad char-set"))
                (else
                 (let ((char (string-ref str (+ i 1))))
                   (apply
                    (lambda (c j)
                      (if (char<? c prev-char)
                          (error "inverted range in char-set" prev-char c)
                          (go j #f (cset-union cset (range->cset prev-char c)))))
                    (cond
                     ((and (eqv? #\\ char) (assv char posix-escape-sequences))
                      => (lambda (x) (list (cdr x) (+ i 3))))
                     ((and (eqv? #\\ char)
                           (eqv? (string-ref str (+ i 2)) #\x))
                      (string-parse-hex-escape str (+ i 3) end))
                     ((and utf8? (<= #x80 (char->integer char) #xFF))
                      (let ((len (utf8-start-char->length char)))
                        (list (utf8-string-ref str (+ i 1) len) (+ i 1 len))))
                     (else
                      (list char (+ i 2)))))))))
              ((#\[)
               (let* ((inv? (eqv? #\^ (string-ref str (+ i 1))))
                      (i2 (if inv? (+ i 2) (+ i 1))))
                 (case (string-ref str i2)
                   ((#\:)
                    (let ((j (string-scan-char str #\: (+ i2 1))))
                      (if (or (not j) (not (eqv? #\] (string-ref str (+ j 1)))))
                          (error "incomplete character class" str)
                          (let* ((class (sre->cset
                                         (string->symbol
                                          (substring str (+ i2 1) j))))
                                 (class (if inv? (cset-complement class) class)))
                            (go (+ j 2) #f (cset-union cset class))))))
                   ((#\= #\.)
                    (error "collating sequences not supported" str))
                   (else
                    (go (+ i 1) #\[ (cset-adjoin cset #\[))))))
              ((#\\)
               (let ((c (string-ref str (+ i 1))))
                 (case c
                   ((#\d #\D #\s #\S #\w #\W)
                    (go (+ i 2) #f
                        (cset-union cset
                                    (sre->cset (string->sre (string #\\ c))))))
                   ((#\x)
                    (apply
                     (lambda (ch j)
                       (go j ch (cset-adjoin cset ch)))
                     (string-parse-hex-escape str (+ i 2) end)))
                   (else
                    (let ((c (cond ((assv c posix-escape-sequences) => cdr)
                                   (else c))))
                      (go (+ i 2) c (cset-adjoin cset c)))))))
              (else
               (if (and utf8? (<= #x80 (char->integer c) #xFF))
                   (let ((len (utf8-start-char->length c)))
                     (go (+ i len)
                         (utf8-string-ref str i len)
                         (cset-adjoin cset (utf8-string-ref str i len))))
                   (go (+ i 1) c (cset-adjoin cset c))))))))
    (if invert?
        (go (+ start 1)
            #f
            (if (flag-set? flags ~multi-line?)
                (char->cset #\newline)
                (make-cset)))
        (go start #f (make-cset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTF-8 Utilities

;; Here are some hairy optimizations that need to be documented
;; better.  Thanks to these, we never do any utf8 processing once the
;; regexp is compiled.

;; two chars: ab..ef
;;            a[b..xFF]|[b-d][x80..xFF]|e[x80..xFF]

;; three chars: abc..ghi
;;              ab[c..xFF]|a[d..xFF][x80..xFF]|
;;              [b..f][x80..xFF][x80..xFF]|
;;              g[x80..g][x80..xFF]|gh[x80..i]

;; four chars: abcd..ghij
;;             abc[d..xFF]|ab[d..xFF][x80..xFF]|a[c..xFF][x80..xFF][x80..xFF]|
;;             [b..f][x80..xFF][x80..xFF][x80..xFF]|
;;             g[x80..g][x80..xFF][x80..xFF]|gh[x80..h][x80..xFF]|ghi[x80..j]

(define (high-char? c) (<= #x80 (char->integer c)))

;; number of total bytes in a utf8 char given the 1st byte

(define utf8-start-char->length
  (let ((table '#(
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 0x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 1x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 2x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 3x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 4x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 5x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 6x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 7x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 8x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 9x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; ax
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; bx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; cx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; dx
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 ; ex
4 4 4 4 4 4 4 4 5 5 5 5 6 6 0 0 ; fx
)))
    (lambda (c) (vector-ref table (char->integer c)))))

(define (utf8-string-ref str i len) str)

(define (utf8-backup-to-initial-char str i) str)

(define (utf8-lowest-digit-of-length len)
  (case len
    ((1) 0) ((2) #xC0) ((3) #xE0) ((4) #xF0)
    (else (error "invalid utf8 length" len))))

(define (utf8-highest-digit-of-length len)
  (case len
    ((1) #x7F) ((2) #xDF) ((3) #xEF) ((4) #xF7)
    (else (error "invalid utf8 length" len))))

#| ;; NMOSH: we don't need this one
;; Maybe this should just modify the input?
(define (cset->utf8-pattern cset)
  (let lp ((ls (cset->plist cset)) (alts '()) (lo-cset '()))
    (if (null? ls)
        (sre-alternate (append (reverse alts)
                               (if (null? lo-cset)
                                   '()
                                   (list (cons '/ (reverse lo-cset))))))
        (if (or (high-char? (car ls))  (high-char? (cadr ls)))
            (lp (cddr ls)
                (cons (unicode-range->utf8-pattern (car ls) (cadr ls)) alts)
                lo-cset)
            (lp (cddr ls) alts (cons (cadr ls) (cons (car ls) lo-cset)))))))
|#

(define (sre-adjust-utf8 sre flags) sre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compilation

(define (irregex x . o)
  (cond
   ((irregex? x) x)
   ((string? x) (apply string->irregex x o))
   (else (apply sre->irregex x o))))

(define (string->irregex str . o)
  (apply sre->irregex (apply string->sre str o) o))

(define (sre->irregex sre . o)
  (let* ((pat-flags (symbol-list->flags o))
         (sre (if *allow-utf8-mode?*
                  (sre-adjust-utf8 sre pat-flags)
                  sre))
         (searcher? (sre-searcher? sre))
         (sre-dfa (if searcher? (sre-remove-initial-bos sre) sre))
         (dfa-limit (cond ((memq 'small o) 1) ((memq 'fast o) 50) (else 10)))
         (dfa/search
          (cond ((memq 'backtrack o) #f)
                (searcher? #t)
                ((sre->nfa `(seq (* any) ,sre-dfa) pat-flags)
                 => (lambda (nfa)
                      (nfa->dfa nfa (* dfa-limit (nfa-num-states nfa)))))
                (else #f)))
         (dfa (cond ((and dfa/search (sre->nfa sre-dfa pat-flags))
                     => (lambda (nfa)
                          (nfa->dfa nfa (* dfa-limit (nfa-num-states nfa)))))
                    (else #f)))
         (submatches (sre-count-submatches sre-dfa))
         (extractor
          (and dfa dfa/search (sre-match-extractor sre-dfa submatches)))
         (names (sre-names sre-dfa 1 '()))
         (lens (sre-length-ranges sre-dfa names))
         (flags (flag-join
                 (flag-join ~none (and searcher? ~searcher?))
                 (and (sre-consumer? sre) ~consumer?))))
    (cond
     (dfa
      (make-irregex dfa dfa/search extractor #f flags submatches lens names))
     (else
      (let ((f (sre->procedure sre pat-flags names)))
        (make-irregex #f #f #f f flags submatches lens names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SRE Analysis

;; returns #t if the sre can ever be empty
(define (sre-empty? sre)
  (if (pair? sre)
      (case (car sre)
        ((* ? look-ahead look-behind neg-look-ahead neg-look-behind) #t)
        ((**) (or (not (number? (cadr sre))) (zero? (cadr sre))))
        ((or) (any sre-empty? (cdr sre)))
        ((: seq $ submatch => submatch-named + atomic)
         (every sre-empty? (cdr sre)))
        (else #f))
      (memq sre '(epsilon bos eos bol eol bow eow commit))))

(define (sre-any? sre)
  (or (eq? sre 'any)
      (and (pair? sre)
           (case (car sre)
             ((seq : $ submatch => submatch-named)
              (and (pair? (cdr sre)) (null? (cddr sre)) (sre-any? (cadr sre))))
             ((or) (every sre-any? (cdr sre)))
             (else #f)))))

(define (sre-repeater? sre)
  (and (pair? sre)
       (or (memq (car sre) '(* +))
           (and (memq (car sre) '($ submatch => submatch-named seq :))
                (pair? (cdr sre))
                (null? (cddr sre))
                (sre-repeater? (cadr sre))))))

(define (sre-searcher? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : $ submatch => submatch-named)
         (and (pair? (cdr sre)) (sre-searcher? (cadr sre))))
        ((or) (every sre-searcher? (cdr sre)))
        (else #f))
      (eq? 'bos sre)))

(define (sre-consumer? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : $ submatch => submatch-named)
         (and (pair? (cdr sre)) (sre-consumer? (last sre))))
        ((or) (every sre-consumer? (cdr sre)))
        (else #f))
      (eq? 'eos sre)))

(define (sre-has-submatches? sre)
  (and (pair? sre)
       (or (memq (car sre) '($ submatch => submatch-named))
           (if (eq? 'posix-string (car sre))
               (sre-has-submatches? (string->sre (cadr sre)))
               (any sre-has-submatches? (cdr sre))))))

(define (sre-count-submatches sre)
  (let count ((sre sre) (sum 0))
    (if (pair? sre)
        (fold count
              (+ sum (case (car sre)
                       (($ submatch => submatch-named) 1)
                       ((dsm) (+ (cadr sre) (caddr sre)))
                       ((posix-string)
                        (sre-count-submatches (string->sre (cadr sre))))
                       (else 0)))
              (cdr sre))
        sum)))

(define (sre-length-ranges sre . o)
  (let ((names (if (pair? o) (car o) (sre-names sre 1 '())))
        (sublens (make-vector (+ 1 (sre-count-submatches sre)) #f)))
    (vector-set!
     sublens
     0
     (let lp ((sre sre) (n 1) (lo 0) (hi 0) (return cons))
       (define (grow i) (return (+ lo i) (and hi (+ hi i))))
       (cond
        ((pair? sre)
         (if (string? (car sre))
             (grow 1)
             (case (car sre)
               ((/ ~ & -)
                (grow 1))
               ((posix-string)
                (lp (string->sre (cadr sre)) n lo hi return))
               ((seq : w/case w/nocase atomic)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 0) (hi2 0))
                  (if (null? ls)
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatches (car ls)))
                                 (+ lo2 lo3)
                                 (and hi2 hi3 (+ hi2 hi3))))))))
               ((or)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 #f) (hi2 0))
                  (if (null? ls)
                      (return (+ lo (or lo2 1)) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatches (car ls)))
                                 (if lo2 (min lo2 lo3) lo3)
                                 (and hi2 hi3 (max hi2 hi3))))))))
               ((if)
                (cond
                 ((or (null? (cdr sre)) (null? (cddr sre)))
                  (return lo hi))
                 (else
                  (let ((n1 (sre-count-submatches (car sre)))
                        (n2 (sre-count-submatches (cadr sre))))
                    (lp (if (or (number? (cadr sre)) (symbol? (cadr sre)))
                            'epsilon
                            (cadr sre))
                        n lo hi
                        (lambda (lo2 hi2)
                          (lp (caddr sre) (+ n n1) 0 0
                              (lambda (lo3 hi3)
                                (lp (if (pair? (cdddr sre))
                                        (cadddr sre)
                                        'epsilon)
                                    (+ n n1 n2) 0 0
                                    (lambda (lo4 hi4)
                                      (return (+ lo2 (min lo3 lo4))
                                              (and hi2 hi3 hi4
                                                   (+ hi2 (max hi3 hi4))
                                                   ))))))))))))
               ((dsm)
                (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) lo hi return))
               (($ submatch => submatch-named)
                (lp (sre-sequence
                     (if (eq? 'submatch (car sre)) (cdr sre) (cddr sre)))
                    (+ n 1) lo hi
                    (lambda (lo2 hi2)
                      (vector-set! sublens n (cons lo2 hi2))
                      (return lo2 hi2))))
               ((backref backref-ci)
                (let ((n (cond
                          ((number? (cadr sre)) (cadr sre))
                          ((assq (cadr sre) names) => cdr)
                          (else (error "unknown backreference" (cadr sre))))))
                  (cond
                   ((or (not (integer? n))
                        (not (< 0 n (vector-length sublens))))
                    (error "sre-length: invalid backreference" sre))
                   ((not (vector-ref sublens n))
                    (error "sre-length: invalid forward backreference" sre))
                   (else
                    (let ((lo2 (car (vector-ref sublens n)))
                          (hi2 (cdr (vector-ref sublens n))))
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2))))))))
               ((* *?)
                (lp (sre-sequence (cdr sre)) n lo hi (lambda (lo hi) #f))
                (return lo #f))
               ((** **?)
                (cond
                 ((or (and (number? (cadr sre))
                           (number? (caddr sre))
                           (> (cadr sre) (caddr sre)))
                      (and (not (cadr sre)) (caddr sre)))
                  (return lo hi))
                 (else
                  (if (caddr sre)
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2))
                                    (and hi hi2 (+ hi (* (caddr sre) hi2))))))
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2)) #f)))))))
               ((+)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return (+ lo lo2) #f))))
               ((? ??)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return lo (and hi hi2 (+ hi hi2))))))
               ((= =? >= >=?)
                (lp `(** ,(cadr sre)
                         ,(if (memq (car sre) '(>= >=?)) #f (cadr sre))
                         ,@(cddr sre))
                    n lo hi return))
               ((look-ahead neg-look-ahead look-behind neg-look-behind)
                (return lo hi))
               (else
                (cond
                 ((assq (car sre) sre-named-definitions)
                  => (lambda (cell)
                       (lp (apply (cdr cell) (cdr sre)) n lo hi return)))
                 (else
                  (error "sre-length-ranges: unknown sre operator" sre)))))))
        ((char? sre)
         (grow 1))
        ((string? sre)
         (grow (string-length sre)))
        ((memq sre '(any nonl))
         (grow 1))
        ((memq sre '(epsilon bos eos bol eol bow eow nwb commit))
         (return lo hi))
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (lp (if (procedure? (cdr cell)) ((cdr cell)) (cdr cell))
                   n lo hi return)
               (error "sre-length-ranges: unknown sre" sre)))))))
    sublens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SRE Manipulation

;; build a (seq ls ...) sre from a list
(define (sre-sequence ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'seq ls))))

;; build a (or ls ...) sre from a list
(define (sre-alternate ls)
  (cond
   ((null? ls) '(or))
   ((null? (cdr ls)) (car ls))
   (else (cons 'or ls))))

;; returns an equivalent SRE without any match information
(define (sre-strip-submatches sre)
  (if (not (pair? sre))
      sre
      (case (car sre)
        (($ submatch) (sre-strip-submatches (sre-sequence (cdr sre))))
        ((=> submatch-named) (sre-strip-submatches (sre-sequence (cddr sre))))
        ((dsm) (sre-strip-submatches (sre-sequence (cdddr sre))))
        (else (map sre-strip-submatches sre)))))

;; given a char-set list of chars and strings, flattens them into
;; chars only
(define (sre-flatten-ranges ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((string? (car ls))
      (lp (append (string->list (car ls)) (cdr ls)) res))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define (sre-names sre n names)
  (if (not (pair? sre))
      names
      (case (car sre)
        (($ submatch)
         (sre-names (sre-sequence (cdr sre)) (+ n 1) names))
        ((=> submatch-named)
         (sre-names (sre-sequence (cddr sre))
                    (+ n 1)
                    (cons (cons (cadr sre) n) names)))
        ((dsm)
         (sre-names (sre-sequence (cdddr sre)) (+ n (cadr sre)) names))
        ((seq : or * + ? *? ?? w/case w/nocase atomic
          look-ahead look-behind neg-look-ahead neg-look-behind)
         (sre-sequence-names (cdr sre) n names))
        ((= >=)
         (sre-sequence-names (cddr sre) n names))
        ((** **?)
         (sre-sequence-names (cdddr sre) n names))
        (else
         names))))

(define (sre-sequence-names ls n names)
  (if (null? ls)
      names
      (sre-sequence-names (cdr ls)
                          (+ n (sre-count-submatches (car ls)))
                          (sre-names (car ls) n names))))

(define (sre-remove-initial-bos sre)
  (cond
   ((pair? sre)
    (case (car sre)
      ((seq : $ submatch => submatch-named * +)
       (cond
        ((not (pair? (cdr sre)))
         sre)
        ((eq? 'bos (cadr sre))
         (cons (car sre) (cddr sre)))
        (else
         (cons (car sre)
               (cons (sre-remove-initial-bos (cadr sre)) (cddr sre))))))
      ((or)
       (sre-alternate (map sre-remove-initial-bos (cdr sre))))
      (else
       sre)))
   (else
    sre)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Basic Matching

(define irregex-basic-string-chunker
  (make-irregex-chunker (lambda (x) #f)
                        car
                        cadr
                        caddr
                        (lambda (src1 i src2 j)
                          (substring (car src1) i j))))

(define (irregex-search x str . o)
  (if (not (string? str)) (error "irregex-search: not a string" str))
  (let ((start (or (and (pair? o) (car o)) 0))
        (end (or (and (pair? o) (pair? (cdr o)) (cadr o)) (string-length str))))
    (if (not (and (integer? start) (exact? start)))
        (error "irregex-search: not an exact integer" start))
    (if (not (and (integer? end) (exact? end)))
        (error "irregex-search: not an exact integer" end))
    (irregex-search/chunked x
                            irregex-basic-string-chunker
                            (list str start end)
                            start)))

(define (irregex-search/chunked x cnk src . o)
  (let* ((irx (irregex x))
         (matches (irregex-new-matches irx))
         (i (if (pair? o) (car o) ((chunker-get-start cnk) src))))
    (irregex-match-chunker-set! matches cnk)
    (irregex-search/matches irx cnk src i matches)))

;; internal routine, can be used in loops to avoid reallocating the
;; match vector
(define (irregex-search/matches irx cnk src i matches)
  (cond
   ((irregex-dfa irx)
    (cond
     ((flag-set? (irregex-flags irx) ~searcher?)
      (cond
       ((dfa-match/longest (irregex-dfa irx) cnk src i #f #f matches 0)
        (irregex-match-start-chunk-set! matches 0 src)
        (irregex-match-start-index-set! matches 0 i)
        ((irregex-dfa/extract irx)
         cnk src i
         (%irregex-match-end-chunk matches 0)
         (%irregex-match-end-index matches 0)
         matches)
        matches)
       (else
        #f)))
     ((dfa-match/shortest
       (irregex-dfa/search irx) cnk src i matches 0)
      (let ((dfa (irregex-dfa irx))
            (get-start (chunker-get-start cnk))
            (get-end (chunker-get-end cnk))
            (get-next (chunker-get-next cnk)))
        (let lp1 ((src src) (i i))
          (let ((end (get-end src)))
            (let lp2 ((i i))
              (cond
               ((dfa-match/longest dfa cnk src i #f #f matches 0)
                (irregex-match-start-chunk-set! matches 0 src)
                (irregex-match-start-index-set! matches 0 i)
                ((irregex-dfa/extract irx)
                 cnk src i
                 (%irregex-match-end-chunk matches 0)
                 (%irregex-match-end-index matches 0)
                 matches)
                matches)
               ((>= i end)
                (let ((next (get-next src)))
                  (and next (lp1 next (get-start next)))))
               (else
                (lp2 (+ i 1)))))))))
     (else
      #f)))
   (else
    (let ((res (irregex-search/backtrack irx cnk src i matches)))
      (if res (%irregex-match-fail-set! res #f))
      res))))

(define (irregex-search/backtrack irx cnk src i matches)
  (let ((matcher (irregex-nfa irx))
        (str ((chunker-get-str cnk) src))
        (end ((chunker-get-end cnk) src))
        (get-next (chunker-get-next cnk))
        (init (cons src i)))
    (if (flag-set? (irregex-flags irx) ~searcher?)
        (matcher cnk init src str i end matches (lambda () #f))
        (let lp ((src2 src)
                 (str str)
                 (i i)
                 (end end))
          (cond
           ((matcher cnk init src2 str i end matches (lambda () #f))
            (irregex-match-start-chunk-set! matches 0 src2)
            (irregex-match-start-index-set! matches 0 i)
            matches)
           ((< i end)
            (lp src2 str (+ i 1) end))
           (else
            (let ((src2 (get-next src2)))
              (if src2
                  (lp src2
                      ((chunker-get-str cnk) src2)
                      ((chunker-get-start cnk) src2)
                      ((chunker-get-end cnk) src2))
                  #f))))))))

(define (irregex-match irx str . o)
  (if (not (string? str)) (error "irregex-match: not a string" str))
  (let ((start (or (and (pair? o) (car o)) 0))
        (end (or (and (pair? o) (pair? (cdr o)) (cadr o)) (string-length str))))
    (if (not (and (integer? start) (exact? start)))
        (error "irregex-match: not an exact integer" start))
    (if (not (and (integer? end) (exact? end)))
        (error "irregex-match: not an exact integer" end))
    (irregex-match/chunked irx
                           irregex-basic-string-chunker
                           (list str start end))))

(define (irregex-match/chunked irx cnk src)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx)))
    (irregex-match-chunker-set! matches cnk)
    (cond
     ((irregex-dfa irx)
      (and
       (dfa-match/longest
        (irregex-dfa irx) cnk src ((chunker-get-start cnk) src) #f #f matches 0)
       (= ((chunker-get-end cnk) (%irregex-match-end-chunk matches 0))
          (%irregex-match-end-index matches 0))
       (begin
         (irregex-match-start-chunk-set! matches 0 src)
         (irregex-match-start-index-set! matches
                                         0
                                         ((chunker-get-start cnk) src))
         ((irregex-dfa/extract irx)
          cnk src ((chunker-get-start cnk) src)
          (%irregex-match-end-chunk matches 0)
          (%irregex-match-end-index matches 0)
          matches)
         matches)))
     (else
      (let* ((matcher (irregex-nfa irx))
             (str ((chunker-get-str cnk) src))
             (i ((chunker-get-start cnk) src))
             (end ((chunker-get-end cnk) src))
             (init (cons src i)))
        (let lp ((m (matcher cnk init src str i end matches (lambda () #f))))
          (and m
               (cond
                ((and (not ((chunker-get-next cnk)
                            (%irregex-match-end-chunk m 0)))
                      (= ((chunker-get-end cnk)
                          (%irregex-match-end-chunk m 0))
                         (%irregex-match-end-index m 0)))
                 (%irregex-match-fail-set! m #f)
                 m)
                ((%irregex-match-fail m)
                 (lp ((%irregex-match-fail m))))
                (else
                 #f)))))))))

(define (irregex-match? . args)
  (and (apply irregex-match args) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DFA Matching

;; inline these
(define (dfa-init-state dfa)
  (vector-ref dfa 0))
(define (dfa-next-state dfa node)
  (vector-ref dfa (cdr node)))
(define (dfa-final-state? dfa state)
  (car state))

;; this searches for the first end index for which a match is possible
(define (dfa-match/shortest dfa cnk src start matches index)
  (let ((get-str (chunker-get-str cnk))
        (get-start (chunker-get-start cnk))
        (get-end (chunker-get-end cnk))
        (get-next (chunker-get-next cnk)))
    (let lp1 ((src src) (start start) (state (dfa-init-state dfa)))
      (and
       src
       (let ((str (get-str src))
             (end (get-end src)))
         (let lp2 ((i start) (state state))
           (cond
            ((dfa-final-state? dfa state)
             (cond
              (index
               (irregex-match-end-chunk-set! matches index src)
               (irregex-match-end-index-set! matches index i)))
             #t)
            ((< i end)
             (let* ((ch (string-ref str i))
                    (next (find (lambda (x)
                                  (or (eqv? ch (car x))
                                      (and (not (char? (car x)))
                                           (cset-contains? (car x) ch))))
                                (cdr state))))
               (and next (lp2 (+ i 1) (dfa-next-state dfa next)))))
            (else
             (let ((next (get-next src)))
               (and next (lp1 next (get-start next) state)))))))))))

;; this finds the longest match starting at a given index
(define (dfa-match/longest dfa cnk src start end-src end matches index)
  (let ((get-str (chunker-get-str cnk))
        (get-start (chunker-get-start cnk))
        (get-end (chunker-get-end cnk))
        (get-next (chunker-get-next cnk))
        (start-is-final? (dfa-final-state? dfa (dfa-init-state dfa))))
    (cond
     (index
      (irregex-match-end-chunk-set! matches index #f)
      (irregex-match-end-index-set! matches index #f)))
    (let lp1 ((src src)
              (start start)
              (state (dfa-init-state dfa))
              (res-src (and start-is-final? src))
              (res-index (and start-is-final? start)))
      (let ((str (get-str src))
            (end (if (eq? src end-src) end (get-end src))))
        (let lp2 ((i start)
                  (state state)
                  (res-src res-src)
                  (res-index res-index))
          (cond
           ((>= i end)
            (cond
             ((and index res-src)
              (irregex-match-end-chunk-set! matches index res-src)
              (irregex-match-end-index-set! matches index res-index)))
            (let ((next (and (not (eq? src end-src)) (get-next src))))
              (if next
                  (lp1 next (get-start next) state res-src res-index)
                  (and index
                       (%irregex-match-end-chunk matches index)
                       #t))))
           (else
            (let* ((ch (string-ref str i))
                   (cell (find (lambda (x)
                                 (or (eqv? ch (car x))
                                     (and (not (char? (car x)))
                                          (cset-contains? (car x) ch))))
                               (cdr state))))
              (cond
               (cell
                (let ((next (dfa-next-state dfa cell)))
                  (if (dfa-final-state? dfa next)
                      (lp2 (+ i 1) next src (+ i 1))
                      (lp2 (+ i 1) next res-src res-index))))
               (res-src
                (cond
                 (index
                  (irregex-match-end-chunk-set! matches index res-src)
                  (irregex-match-end-index-set! matches index res-index)))
                #t)
               ((and index (%irregex-match-end-chunk matches index))
                #t)
               (else
                #f))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Named Definitions

(define sre-named-definitions
  `((any . ,*all-chars*)
    (nonl . (- ,*all-chars* (,(string #\newline))))
    (alphabetic . (/ #\a #\z #\A #\Z))
    (alpha . alphabetic)
    (alphanumeric . (/ #\a #\z #\A #\Z #\0 #\9))
    (alphanum . alphanumeric)
    (alnum . alphanumeric)
    (lower-case . (/ #\a #\z))
    (lower . lower-case)
    (upper-case . (/ #\A #\Z))
    (upper . upper-case)
    (numeric . (/ #\0 #\9))
    (num . numeric)
    (digit . numeric)
    (punctuation . (or #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
                       #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}))
    (punct . punctuation)
    (graphic
     . (or alphanumeric punctuation #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~))
    (graph . graphic)
    (blank . (or #\space ,(integer->char (- (char->integer #\space) 23))))
    (whitespace . (or blank #\newline))
    (space . whitespace)
    (white . whitespace)
    (printing or graphic whitespace)
    (print . printing)

    ;; XXXX we assume a (possibly shifted) ASCII-based ordering
    (control . (/ ,(integer->char (- (char->integer #\space) 32))
                  ,(integer->char (- (char->integer #\space) 1))))
    (cntrl . control)
    (hex-digit . (or numeric (/ #\a #\f #\A #\F)))
    (xdigit . hex-digit)
    (ascii . (/ ,(integer->char (- (char->integer #\space) 32))
                ,(integer->char (+ (char->integer #\space) 95))))
    (ascii-nonl . (/ ,(integer->char (- (char->integer #\space) 32))
                     ,(integer->char (- (char->integer #\newline) 1))
                     ,(integer->char (+ (char->integer #\newline) 1))
                     ,(integer->char (+ (char->integer #\space) 95))))
    (newline . (or (seq ,(integer->char (+ (char->integer #\newline) 3))
                        #\newline)
                   (/ #\newline
                      ,(integer->char (+ (char->integer #\newline) 3)))))

    ;; ... it's really annoying to support old Scheme48
    (word . (seq bow (+ (or alphanumeric #\_)) eow))
    (utf8-tail-char . (/ ,(integer->char (+ (char->integer #\space) #x60))
                         ,(integer->char (+ (char->integer #\space) #xA1))))
    (utf8-2-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xA2))
                           ,(integer->char (+ (char->integer #\space) #xBF)))
                        utf8-tail-char))
    (utf8-3-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xC0))
                           ,(integer->char (+ (char->integer #\space) #xCF)))
                        utf8-tail-char
                        utf8-tail-char))
    (utf8-4-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xD0))
                           ,(integer->char (+ (char->integer #\space) #xD7)))
                        utf8-tail-char
                        utf8-tail-char
                        utf8-tail-char))
    (utf8-any . (or ascii utf8-2-char utf8-3-char utf8-4-char))
    (utf8-nonl . (or ascii-nonl utf8-2-char utf8-3-char utf8-4-char))

    ;; extended library patterns
    (integer . (seq (? (or #\+ #\-)) (+ numeric)))
    (real . (seq (+ numeric) (? #\. (+ numeric)) (? (or #\e #\E) integer)))
    ;; slightly more lax than R5RS, allow ->foo, etc.
    (symbol-initial . (or alpha ("!$%&*/:<=>?^_~")))
    (symbol-subsequent . (or symbol-initial digit ("+-.@")))
    (symbol . (or (seq symbol-initial (* symbol-subsequent))
                  (seq ("+-") (? symbol-initial (* symbol-subsequent)))
                  (seq ".." (* "."))))
    (sexp-space . (seq (* (* space) ";" (* nonl) newline) (+ space)))
    (string . (seq #\" (escape #\\ #\") #\"))
    (escape . ,(lambda (esc . o) `(* (or (~ ,esc ,@o) (seq ,esc any)))))

    (ipv4-digit . (seq (? (/ "12")) (? numeric) numeric))
    (ipv4-address . (seq ipv4-digit (= 3 #\. ipv4-digit)))
    ;; XXXX lax, allows multiple double-colons or < 8 terms w/o a ::
    (ipv6-address . (seq (** 0 4 hex-digit)
                         (** 1 7 #\: (? #\:) (** 0 4 hex-digit))))
    (ip-address . (or ipv4-address ipv6-address))
    (domain-atom . (+ (or alphanumeric #\_ #\-)))
    (domain . (seq domain-atom (+ #\. domain-atom)))
    ;; XXXX now anything can be a top-level domain, but this is still handy
    (top-level-domain . (w/nocase (or "arpa" "com" "gov" "mil" "net" "org"
                                      "aero" "biz" "coop" "info" "museum"
                                      "name" "pro" (= 2 alpha))))
    (domain/common . (seq (+ domain-atom #\.) top-level-domain))
    ;;(email-local-part . (seq (+ (or (~ #\") string))))
    (email-local-part . (+ (or alphanumeric #\_ #\- #\. #\+)))
    (email . (seq email-local-part #\@ domain))
    (url-char . (or alnum #\_ #\- #\+ #\\ #\= #\~ #\. #\, #\& #\;
                    (seq "%" hex-digit hex-digit)))
    (url-final-char . (or alnum #\_ #\- #\+ #\\ #\= #\~ #\&
                          (seq "%" hex-digit hex-digit)))
    (http-url . (w/nocase
                 "http" (? "s") "://"
                 (or domain/common ipv4-address) ;; (seq "[" ipv6-address "]")
                 (? ":" (+ numeric)) ;; port
                 ;; path
                 (? "/" (* url-char)
                    (? "?" (* url-char))                      ;; query
                    (? "#" (? (* url-char) url-final-char)) ;; fragment
                    )))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SRE->NFA compilation
;;
;; An NFA state is a numbered node with a list of pattern->number
;; transitions, where pattern is character set range, or epsilon
;; (indicating an empty transition).
;; There may be overlapping ranges - since it's an NFA we process it
;; by considering all possible transitions.

(define *nfa-presize* 128)  ;; constant
(define *nfa-num-fields* 4) ;; constant

(define (nfa-num-states nfa) (quotient (vector-length nfa) *nfa-num-fields*))
(define (nfa-start-state nfa) (- (nfa-num-states nfa) 1))

(define (nfa-get-state-trans nfa i)
  (vector-ref nfa (* i *nfa-num-fields*)))
(define (nfa-set-state-trans! nfa i x)
  (vector-set! nfa (* i *nfa-num-fields*) x))

(define (nfa-get-epsilons nfa i)
  (vector-ref nfa (+ (* i *nfa-num-fields*) 1)))
(define (nfa-set-epsilons! nfa i x)
  (vector-set! nfa (+ (* i *nfa-num-fields*) 1) x))
(define (nfa-add-epsilon! nfa i x)
  (let ((eps (nfa-get-epsilons nfa i)))
    (if (not (memq x eps))
        (nfa-set-epsilons! nfa i (cons x eps)))))

(define (nfa-get-state-closure nfa i)
  (vector-ref nfa (+ (* i *nfa-num-fields*) 2)))
(define (nfa-set-state-closure! nfa i x)
  (vector-set! nfa (+ (* i *nfa-num-fields*) 2) x))

(define (nfa-get-closure nfa mst)
  (cond ((assoc mst
                (vector-ref nfa (+ (* (nfa-multi-state-hash nfa mst)
                                      *nfa-num-fields*)
                                   (- *nfa-num-fields* 1))))
         => cdr)
        (else #f)))
(define (nfa-add-closure! nfa mst x)
  (let ((i (+ (* (nfa-multi-state-hash nfa mst) *nfa-num-fields*)
              (- *nfa-num-fields* 1))))
    (vector-set! nfa i (cons (cons mst x) (vector-ref nfa i)))))

;; Compile and return the vector of NFA states (in groups of
;; *nfa-num-fields* packed elements).  The start state will be the
;; last element(s) of the vector, and all remaining states will be in
;; descending numeric order, with state 0 being the unique accepting
;; state.
(define (sre->nfa sre init-flags)
  (let ((buf (make-vector (* *nfa-presize* *nfa-num-fields*) '())))
    ;; we loop over an implicit sequence list
    (define (lp ls n flags next)
      (define (new-state-number state)
        (max n (+ 1 state)))
      (define (add-state! n2 trans-ls)
        (if (>= (* n2 *nfa-num-fields*) (vector-length buf))
            (let ((tmp (make-vector (* 2 (vector-length buf)) '())))
              (do ((i (- (vector-length buf) 1) (- i 1)))
                  ((< i 0))
                (vector-set! tmp i (vector-ref buf i)))
              (set! buf tmp)))
        (nfa-set-state-trans! buf n2 trans-ls)
        n2)
      (define (extend-state! next trans-cs)
        (and next
             (add-state! (new-state-number next) (cons trans-cs next))))
      (define (add-char-state! next ch)
        (let ((alt (char-altcase ch)))
          (if (flag-set? flags ~case-insensitive?)
              (extend-state! next (cset-union (char->cset ch) (char->cset alt)))
              (extend-state! next (char->cset ch)))))
      (if (null? ls)
          next
          (cond
           ((or (eq? 'epsilon (car ls)) (equal? "" (car ls)))
            ;; chars and epsilons go directly into the transition table
            (let ((next (lp (cdr ls) n flags next)))
              (and next
                   (let ((new (add-state! (new-state-number next) '())))
                     (nfa-add-epsilon! buf new next)
                     new))))
           ((string? (car ls))
            ;; process literal strings a char at a time
            (let ((next (lp (cdr ls) n flags next)))
              (and next
                   (let lp2 ((i (- (string-length (car ls)) 1))
                             (next next))
                     (if (< i 0)
                         next
                         (lp2 (- i 1)
                              (add-char-state! next (string-ref (car ls) i))))
                     ))))
           ((char? (car ls))
            (add-char-state! (lp (cdr ls) n flags next) (car ls)))
           ((symbol? (car ls))
            (let ((cell (assq (car ls) sre-named-definitions)))
              (and cell
                   (lp (cons (if (procedure? (cdr cell))
                                 ((cdr cell))
                                 (cdr cell))
                             (cdr ls))
                       n
                       flags
                       next))))
           ((pair? (car ls))
            (cond
             ((string? (caar ls))       ; Enumerated character set
              (let ((set (if (flag-set? flags ~case-insensitive?)
                             (cset-case-insensitive (string->cset (caar ls)))
                             (string->cset (caar ls)))))
               (extend-state! (lp (cdr ls) n flags next) set)))
             (else
              (case (caar ls)
                ((seq :)
                 ;; for an explicit sequence, just append to the list
                 (lp (append (cdar ls) (cdr ls)) n flags next))
                ((w/case w/nocase w/utf8 w/noutf8)
                 (let* ((next (lp (cdr ls) n flags next))
                        (flags ((if (memq (caar ls) '(w/case w/utf8))
                                    flag-clear
                                    flag-join)
                                flags
                                (if (memq (caar ls) '(w/case w/nocase))
                                    ~case-insensitive?
                                    ~utf8?))))
                   (and next
                        (lp (cdar ls) (new-state-number next) flags next))))
                ((/ - & ~)
                 (let ((range (sre->cset (car ls)
                                         (flag-set? flags ~case-insensitive?))))
                   (extend-state! (lp (cdr ls) n flags next)
                                  range)))
                ((or)
                 (let ((next (lp (cdr ls) n flags next)))
                   (and
                    next
                    (if (null? (cdar ls))
                        ;; empty (or) always fails
                        (add-state! (new-state-number next) '())
                        ;; compile both branches and insert epsilon
                        ;; transitions to either
                        (let* ((b (lp (list (sre-alternate (cddar ls)))
                                      (new-state-number next)
                                      flags
                                      next))
                               (a (and b
                                       (lp (list (cadar ls))
                                           (new-state-number (max b next))
                                           flags
                                           next))))
                          (and a
                               (let ((c (add-state! (new-state-number a)
                                                    '())))
                                 (nfa-add-epsilon! buf c a)
                                 (nfa-add-epsilon! buf c b)
                                 c)))))))
                ((?)
                 (let ((next (lp (cdr ls) n flags next)))
                   ;; insert an epsilon transition directly to next
                   (and
                    next
                    (let ((a (lp (cdar ls) (new-state-number next) flags next)))
                      (if a
                          (nfa-add-epsilon! buf a next))
                      a))))
                ((+ *)
                 (let ((next (lp (cdr ls) n flags next)))
                   (and
                    next
                    (let* ((new (lp '(epsilon)
                                    (new-state-number next)
                                    flags
                                    next))
                           (a (lp (cdar ls) (new-state-number new) flags new)))
                      (cond
                       (a
                        ;; for *, insert an epsilon transition as in ? above
                        (if (eq? '* (caar ls))
                            (nfa-add-epsilon! buf a new))
                        ;; for both, insert a loop back to self
                        (nfa-add-epsilon! buf new a)))
                      a))))
                ;; need to add these to the match extractor first,
                ;; but they tend to generate large DFAs
                ;;((=)
                ;; (lp (append (vector->list
                ;;              (make-vector (cadar ls)
                ;;                           (sre-sequence (cddar ls))))
                ;;             (cdr ls))
                ;;     n flags next))
                ;;((>=)
                ;; (lp (append (vector->list
                ;;              (make-vector (- (cadar ls) 1)
                ;;                           (sre-sequence (cddar ls))))
                ;;             (cons `(+ ,@(cddar ls)) (cdr ls)))
                ;;     n flags next))
                ;;((**)
                ;; (lp (append (vector->list
                ;;              (make-vector (cadar ls)
                ;;                           (sre-sequence (cdddar ls))))
                ;;             (map
                ;;              (lambda (x) `(? ,x))
                ;;              (vector->list
                ;;               (make-vector (- (caddar ls) (cadar ls))
                ;;                            (sre-sequence (cdddar ls)))))
                ;;             (cdr ls))
                ;;     n flags next))
                ;; ignore submatches altogether
                (($ submatch)
                 (lp (cons (sre-sequence (cdar ls)) (cdr ls)) n flags next))
                ((=> submatch-named)
                 (lp (cons (sre-sequence (cddar ls)) (cdr ls)) n flags next))
                (else
                 (cond
                  ((assq (caar ls) sre-named-definitions)
                   => (lambda (cell)
                        (if (procedure? (cdr cell))
                            (lp (cons (apply (cdr cell) (cdar ls)) (cdr ls))
                                n flags next)
                            (error "non-procedure in op position" (caar ls)))))
                  (else #f)))))))
           (else
            #f))))
    (let ((len (lp (list sre) 1 init-flags 0)))
      (and len
           (let ((nfa (make-vector (* *nfa-num-fields* (+ len 1)))))
             (do ((i (- (vector-length nfa) 1) (- i 1)))
                 ((< i 0))
               (vector-set! nfa i (vector-ref buf i)))
             nfa)))))

;; We don't really want to use this, we use the closure compilation
;; below instead, but this is included for reference and testing the
;; sre->nfa conversion.

;; (define (nfa-match nfa str)
;;   (let lp ((ls (string->list str)) (state (car nfa)) (epsilons '()))
;;     (if (null? ls)
;;         (zero? (car state))
;;         (any (lambda (m)
;;                (if (eq? 'epsilon (car m))
;;                    (and (not (memv (cdr m) epsilons))
;;                         (lp ls (assv (cdr m) nfa) (cons (cdr m) epsilons)))
;;                    (and (or (eqv? (car m) (car ls))
;;                             (and (pair? (car m))
;;                                  (char<=? (caar m) (car ls))
;;                                  (char<=? (car ls) (cdar m))))
;;                         (lp (cdr ls) (assv (cdr m) nfa) '()))))
;;              (cdr state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NFA multi-state representation

;; Cache closures in a simple hash-table keyed on the smallest state
;; (define (nfa-multi-state-hash nfa mst)
;;   (car mst))

;; Original sorted list-based representation

;; (define (make-nfa-multi-state nfa)
;;   '())

;; (define (nfa-state->multi-state nfa state)
;;   (list state))

;; (define (nfa-multi-state-copy mst)
;;   (map (lambda (x) x) mst))

;; (define (list->nfa-multi-state nfa ls)
;;   (nfa-multi-state-copy ls))

;; (define (nfa-multi-state-contains? mst i)
;;   (memq i mst))

;; (define (nfa-multi-state-fold mst kons knil)
;;   (fold kons knil mst))

;; (define (nfa-multi-state-add! mst i)
;;   (insert-sorted i mst))

;; (define (nfa-multi-state-add mst i)
;;   (insert-sorted i mst))

;; (define (nfa-multi-state-union a b)
;;   (merge-sorted a b))

;; Sorted List Utilities

;; (define (insert-sorted n ls)
;;   (cond
;;    ((null? ls)
;;     (cons n '()))
;;    ((<= n (car ls))
;;     (if (= n (car ls))
;;         ls
;;         (cons n ls)))
;;    (else
;;     (cons (car ls) (insert-sorted n (cdr ls))))))

;; (define (insert-sorted! n ls)
;;   (cond
;;    ((null? ls)
;;     (cons n '()))
;;    ((<= n (car ls))
;;     (if (= n (car ls))
;;         ls
;;         (cons n ls)))
;;    (else
;;     (let lp ((head ls) (tail (cdr ls)))
;;       (cond ((or (null? tail) (< n (car tail)))
;;              (set-cdr! head (cons n tail)))
;;             ((> n (car tail))
;;              (lp tail (cdr tail)))))
;;     ls)))

;; (define (merge-sorted a b)
;;   (cond ((null? a) b)
;;         ((null? b) a)
;;         ((< (car a) (car b))
;;          (cons (car a) (merge-sorted (cdr a) b)))
;;         ((> (car a) (car b))
;;          (cons (car b) (merge-sorted a (cdr b))))
;;         (else (merge-sorted (cdr a) b))))

;; ========================================================= ;;

;; Presized bit-vector based

(define (nfa-multi-state-hash nfa mst)
  (modulo (vector-ref mst 0) (nfa-num-states nfa)))

(define (make-nfa-multi-state nfa)
  (make-vector (quotient (+ (nfa-num-states nfa) 24 -1) 24) 0))

(define (nfa-state->multi-state nfa state)
  (nfa-multi-state-add! (make-nfa-multi-state nfa) state))

(define (nfa-multi-state-copy mst)
  (let ((res (make-vector (vector-length mst))))
    (do ((i (- (vector-length mst) 1) (- i 1)))
        ((< i 0) res)
      (vector-set! res i (vector-ref mst i)))))

(define (nfa-multi-state-contains? mst i)
  (let ((cell (quotient i 24))
        (bit (remainder i 24)))
    (not (zero? (bit-and (vector-ref mst cell) (bit-shl 1 bit))))))

(define (nfa-multi-state-add! mst i)
  (let ((cell (quotient i 24))
        (bit (remainder i 24)))
    (vector-set! mst cell (bit-ior (vector-ref mst cell) (bit-shl 1 bit)))
    mst))

(define (nfa-multi-state-add mst i)
  (nfa-multi-state-add! (nfa-multi-state-copy mst) i))

(define (nfa-multi-state-union! a b)
  (do ((i (- (vector-length a) 1) (- i 1)))
      ((< i 0) a)
    (vector-set! a i (bit-ior (vector-ref a i) (vector-ref b i)))))

(define (nfa-multi-state-union a b)
  (nfa-multi-state-union! (nfa-multi-state-copy a) b))

(define (nfa-multi-state-fold mst kons knil)
  (let ((limit (vector-length mst)))
    (let lp1 ((i 0)
              (acc knil))
      (if (>= i limit)
          acc
          (let lp2 ((n (vector-ref mst i))
                    (acc acc))
            (if (zero? n)
                (lp1 (+ i 1) acc)
                (let* ((n2 (bit-and n (- n 1)))
                       (n-tail (- n n2))
                       (bit (+ (* i 24) (integer-log n-tail))))
                  (lp2 n2 (kons bit acc)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NFA->DFA compilation
;;
;; During processing, the DFA is a list of the form:
;;
;;   ((NFA-states ...) accepting-state? transitions ...)
;;
;; where the transitions are as in the NFA, except there are no
;; epsilons, duplicate characters or overlapping char-set ranges, and
;; the states moved to are closures (sets of NFA states).  Multiple
;; DFA states may be accepting states.

(define (nfa->dfa nfa . o)
  (let ((max-states (and (pair? o) (car o))))
    (let lp ((ls (list (nfa-cache-state-closure! nfa (nfa-start-state nfa))))
             (i 0)
             (res '()))
      (cond
       ((null? ls)
        (dfa-renumber nfa (reverse res)))
       ((assoc (car ls) res) ;; already seen this combination of states
        (lp (cdr ls) i res))
       ((and max-states (> i max-states)) ;; too many DFA states
        #f)
       (else
        (let* ((states (car ls))
               (trans (nfa-state-transitions nfa states))
               (accept? (and (nfa-multi-state-contains? states 0) #t)))
          (lp (append (map cdr trans) (cdr ls))
              (+ i 1)
              `((,states ,accept? ,@trans) ,@res))))))))

;; When the conversion is complete we renumber the DFA sets-of-states
;; in order and convert the result to a vector for fast lookup.
;; Charsets containing single characters are converted to those characters
;; for quick matching of the literal parts in a regex.
(define (dfa-renumber nfa dfa)
  (let* ((len (length dfa))
         (states (make-vector (nfa-num-states nfa) '()))
         (res (make-vector len)))
    (define (renumber mst)
      (cdr (assoc mst (vector-ref states (nfa-multi-state-hash nfa mst)))))
    (let lp ((ls dfa) (i 0))
      (cond ((pair? ls)
             (let ((j (nfa-multi-state-hash nfa (caar ls))))
               (vector-set! states j (cons (cons (caar ls) i)
                                           (vector-ref states j))))
             (lp (cdr ls) (+ i 1)))))
    (let lp ((ls dfa) (i 0))
      (cond ((pair? ls)
             (for-each
              (lambda (x)
                (set-car! x (maybe-cset->char (car x)))
                (set-cdr! x (renumber (cdr x))))
              (cddar ls))
             (vector-set! res i (cdar ls))
             (lp (cdr ls) (+ i 1)))))
    res))

;; Extract all distinct ranges and the potential states they can transition
;; to from a given set of states.  Any ranges that would overlap with
;; distinct characters are split accordingly.
(define (nfa-state-transitions nfa states)
  (let ((res (nfa-multi-state-fold
              states
              (lambda (st res)
                (let ((trans (nfa-get-state-trans nfa st)))
                  (if (null? trans)
                      res
                      (nfa-join-transitions! nfa res (car trans) (cdr trans)))))
              '())))
    (for-each (lambda (x) (set-cdr! x (nfa-closure nfa (cdr x)))) res)
    res))

(define (nfa-join-transitions! nfa existing elt state)
  (define (csets-intersect? a b)
    (let ((i (cset-intersection a b)))
      (and (not (cset-empty? i)) i)))
  (let lp ((ls existing) (res '()))
    (cond
     ((null? ls)
      (cond       ; First try to find a group that includes this state
       ((find (lambda (x) (nfa-multi-state-contains? (cdr x) state)) existing) =>
        (lambda (existing-state)    ; If found, merge charsets with it
          (set-car! existing-state (cset-union (car existing-state) elt))
          existing))
       ;; State not seen yet?  Add a new state transition
       (else (cons (cons elt (nfa-state->multi-state nfa state)) existing))))
     ((cset=? elt (caar ls)) ; Add state to existing set for this charset
      (set-cdr! (car ls) (nfa-multi-state-add! (cdar ls) state))
      existing)
     ((csets-intersect? elt (caar ls)) => ; overlapping charset, but diff state
      (lambda (intersection)
        (let* ((only-in-old (cset-difference (caar ls) elt))
               (states-for-old (and (not (cset-empty? only-in-old))
                                    (nfa-multi-state-copy (cdar ls))))
               (result (if states-for-old
                           (cons (cons only-in-old states-for-old)
                                 (append res (cdr ls)))
                           (append res (cdr ls))))
               (only-in-new (cset-difference elt (caar ls))))
          ;; Add this state to the states already here and restrict to
          ;; the overlapping charset
          (set-car! (car ls) intersection)
          (set-cdr! (car ls) (nfa-multi-state-add! (cdar ls) state))
          ;; Continue with the remaining subset of the new cset (if nonempty)
          (cons (car ls)
                (if (cset-empty? only-in-new)
                    result
                    (nfa-join-transitions! nfa result only-in-new state))))))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define (nfa-cache-state-closure! nfa state)
  (let ((cached (nfa-get-state-closure nfa state)))
    (cond
     ((not (null? cached))
      cached)
     (else
      (let ((res (nfa-state-closure-internal nfa state)))
        (nfa-set-state-closure! nfa state res)
        res)))))

;; The `closure' of a list of NFA states - all states that can be
;; reached from any of them using any number of epsilon transitions.
(define (nfa-state-closure-internal nfa state)
  (let lp ((ls (list state))
           (res (make-nfa-multi-state nfa)))
    (cond
     ((null? ls)
      res)
     ((nfa-multi-state-contains? res (car ls))
      (lp (cdr ls) res))
     (else
      (lp (append (nfa-get-epsilons nfa (car ls)) (cdr ls))
          (nfa-multi-state-add! res (car ls)))))))

(define (nfa-closure-internal nfa states)
  (nfa-multi-state-fold
   states
   (lambda (st res)
     (nfa-multi-state-union! res (nfa-cache-state-closure! nfa st)))
   (make-nfa-multi-state nfa)))

(define (nfa-closure nfa states)
  (or (nfa-get-closure nfa states)
      (let ((res (nfa-closure-internal nfa states)))
        (nfa-add-closure! nfa states res)
        res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Match Extraction
;;
;; DFAs don't give us match information, so once we match and
;; determine the start and end, we need to recursively break the
;; problem into smaller DFAs to get each submatch.
;;
;; See http://compilers.iecc.com/comparch/article/07-10-026

(define (sre-match-extractor sre num-submatches)
  (let* ((tmp (+ num-submatches 1))
         (tmp-end-src-offset (+ 5 (* tmp 4)))
         (tmp-end-index-offset (+ 6 (* tmp 4))))
    (let lp ((sre sre) (n 1) (submatch-deps? #f))
      (cond
       ((not (sre-has-submatches? sre))
        (if (not submatch-deps?)
            (lambda (cnk start i end j matches) #t)
            (let ((dfa (nfa->dfa (sre->nfa sre ~none))))
              (lambda (cnk start i end j matches)
                (dfa-match/longest dfa cnk start i end j matches tmp)))))
       ((pair? sre)
        (case (car sre)
          ((: seq)
           (let* ((right (sre-sequence (cddr sre)))
                  (match-left (lp (cadr sre) n #t))
                  (match-right
                   (lp right (+ n (sre-count-submatches (cadr sre))) #t)))
             (lambda (cnk start i end j matches)
               (let lp1 ((end2 end) (j2 j) (best-src #f) (best-index #f))
                 (let ((limit (if (eq? start end2)
                                  i
                                  ((chunker-get-start cnk) end2))))
                   (let lp2 ((k j2) (best-src best-src) (best-index best-index))
                     (if (< k limit)
                         (cond
                          ((not (eq? start end2))
                           (let ((prev (chunker-prev-chunk cnk start end2)))
                             (lp1 prev
                                  ((chunker-get-end cnk) prev)
                                  best-src
                                  best-index)))
                          (best-src
                           (vector-set! matches tmp-end-src-offset best-src)
                           (vector-set! matches tmp-end-index-offset best-index)
                           #t)
                          (else
                           #f))
                         (if (and (match-left cnk start i end2 k matches)
                                  (eq? end2 (vector-ref matches
                                                        tmp-end-src-offset))
                                  (eqv? k (vector-ref matches
                                                      tmp-end-index-offset))
                                  (match-right cnk end2 k end j matches))
                             (let ((right-src
                                    (vector-ref matches tmp-end-src-offset))
                                   (right
                                    (vector-ref matches tmp-end-index-offset)))
                               (cond
                                ((and (eq? end right-src) (eqv? j right))
                                 (vector-set! matches tmp-end-src-offset end)
                                 (vector-set! matches tmp-end-index-offset j)
                                 #t)
                                ((or (not best-src)
                                     (if (eq? best-src right-src)
                                         (> right best-index)
                                         (chunk-before? cnk
                                                        best-src
                                                        right-src)))
                                 (lp2 (- k 1) right-src right))
                                (else
                                 (lp2 (- k 1) best-src best-index))))
                             (lp2 (- k 1) best-src best-index)))))))))
          ((or)
           (if (null? (cdr sre))
               (lambda (cnk start i end j matches) #f)
               (let* ((rest (sre-alternate (cddr sre)))
                      (match-first
                       (lp (cadr sre) n #t))
                      (match-rest
                       (lp rest
                           (+ n (sre-count-submatches (cadr sre)))
                           submatch-deps?)))
                 (lambda (cnk start i end j matches)
                   (or (and (match-first cnk start i end j matches)
                            (eq? end (vector-ref matches tmp-end-src-offset))
                            (eqv? j (vector-ref matches tmp-end-index-offset)))
                       (match-rest cnk start i end j matches))))))
          ((* +)
           (letrec ((match-once
                     (lp (sre-sequence (cdr sre)) n #t))
                    (match-all
                     (lambda (cnk start i end j matches)
                       (if (match-once cnk start i end j matches)
                           (let ((src (vector-ref matches tmp-end-src-offset))
                                 (k (vector-ref matches tmp-end-index-offset)))
                             (if (and src (or (not (eq? start src)) (< i k)))
                                 (match-all cnk src k end j matches)
                                 #t))
                           (begin
                             (vector-set! matches tmp-end-src-offset start)
                             (vector-set! matches tmp-end-index-offset i)
                             #t)))))
             (if (eq? '* (car sre))
                 match-all
                 (lambda (cnk start i end j matches)
                   (and (match-once cnk start i end j matches)
                        (let ((src (vector-ref matches tmp-end-src-offset))
                              (k (vector-ref matches tmp-end-index-offset)))
                          (match-all cnk src k end j matches)))))))
          ((?)
           (let ((match-once (lp (sre-sequence (cdr sre)) n #t)))
             (lambda (cnk start i end j matches)
               (cond
                ((match-once cnk start i end j matches)
                 #t)
                (else
                 (vector-set! matches tmp-end-src-offset start)
                 (vector-set! matches tmp-end-index-offset i)
                 #t)))))
          (($ submatch => submatch-named)
           (let ((match-one
                  (lp (sre-sequence (if (memq (car sre) '($ submatch))
                                        (cdr sre)
                                        (cddr sre)))
                      (+ n 1)
                      #t))
                 (start-src-offset (+ 3 (* n 4)))
                 (start-index-offset (+ 4 (* n 4)))
                 (end-src-offset (+ 5 (* n 4)))
                 (end-index-offset (+ 6 (* n 4))))
             (lambda (cnk start i end j matches)
               (cond
                ((match-one cnk start i end j matches)
                 (vector-set! matches start-src-offset start)
                 (vector-set! matches start-index-offset i)
                 (vector-set! matches end-src-offset
                              (vector-ref matches tmp-end-src-offset))
                 (vector-set! matches end-index-offset
                              (vector-ref matches tmp-end-index-offset))
                 #t)
                (else
                 #f)))))
          (else
           (error "unknown regexp operator" (car sre)))))
       (else
        (error "unknown regexp" sre))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closure Compilation
;;
;; We use this for non-regular expressions instead of an interpreted
;; NFA matcher.  We use backtracking anyway, but this gives us more
;; freedom of implementation, allowing us to support patterns that
;; can't be represented in the above NFA representation.

(define (sre->procedure sre . o)
  (define names
    (if (and (pair? o) (pair? (cdr o))) (cadr o) (sre-names sre 1 '())))
  (let lp ((sre sre)
           (n 1)
           (flags (if (pair? o) (car o) ~none))
           (next (lambda (cnk init src str i end matches fail)
                   (irregex-match-start-chunk-set! matches 0 (car init))
                   (irregex-match-start-index-set! matches 0 (cdr init))
                   (irregex-match-end-chunk-set! matches 0 src)
                   (irregex-match-end-index-set! matches 0 i)
                   (%irregex-match-fail-set! matches fail)
                   matches)))
    ;; XXXX this should be inlined
    (define (rec sre) (lp sre n flags next))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (sre-cset->procedure
           (sre->cset (car sre) (flag-set? flags ~case-insensitive?))
           next)
          (case (car sre)
            ((~ - & /)
             (sre-cset->procedure
              (sre->cset sre (flag-set? flags ~case-insensitive?))
              next))
            ((or)
             (case (length (cdr sre))
               ((0) (lambda (cnk init src str i end matches fail) (fail)))
               ((1) (rec (cadr sre)))
               (else
                (let* ((first (rec (cadr sre)))
                       (rest (lp (sre-alternate (cddr sre))
                                 (+ n (sre-count-submatches (cadr sre)))
                                 flags
                                 next)))
                  (lambda (cnk init src str i end matches fail)
                    (first cnk init src str i end matches
                           (lambda ()
                             (rest cnk init src str i end matches fail))))))))
            ((w/case)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-clear flags ~case-insensitive?)
                 next))
            ((w/nocase)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-join flags ~case-insensitive?)
                 next))
            ((w/utf8)
             (lp (sre-sequence (cdr sre)) n (flag-join flags ~utf8?) next))
            ((w/noutf8)
             (lp (sre-sequence (cdr sre)) n (flag-clear flags ~utf8?) next))
            ((seq :)
             (case (length (cdr sre))
               ((0) next)
               ((1) (rec (cadr sre)))
               (else
                (let ((rest (lp (sre-sequence (cddr sre))
                                (+ n (sre-count-submatches (cadr sre)))
                                flags
                                next)))
                  (lp (cadr sre) n flags rest)))))
            ((?)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (cnk init src str i end matches fail)
                 (body cnk init src str i end matches
                       (lambda () (next cnk init src str i end matches fail))))))
            ((??)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (cnk init src str i end matches fail)
                 (next cnk init src str i end matches
                       (lambda () (body cnk init src str i end matches fail))))))
            ((*)
             (cond
              ((sre-empty? (sre-sequence (cdr sre)))
               (error "invalid sre: empty *" sre))
              (else
               (letrec
                   ((body
                     (lp (sre-sequence (cdr sre))
                         n
                         flags
                         (lambda (cnk init src str i end matches fail)
                           (body cnk init src str i end matches
                                 (lambda ()
                                   (next cnk init src str i end matches fail)
                                   ))))))
                 (lambda (cnk init src str i end matches fail)
                   (body cnk init src str i end matches
                         (lambda ()
                           (next cnk init src str i end matches fail))))))))
            ((*?)
             (cond
              ((sre-empty? (sre-sequence (cdr sre)))
               (error "invalid sre: empty *?" sre))
              (else
               (letrec
                   ((body
                     (lp (sre-sequence (cdr sre))
                         n
                         flags
                         (lambda (cnk init src str i end matches fail)
                           (next cnk init src str i end matches
                                 (lambda ()
                                   (body cnk init src str i end matches fail)
                                   ))))))
                 (lambda (cnk init src str i end matches fail)
                   (next cnk init src str i end matches
                         (lambda ()
                           (body cnk init src str i end matches fail))))))))
            ((+)
             (lp (sre-sequence (cdr sre))
                 n
                 flags
                 (rec (list '* (sre-sequence (cdr sre))))))
            ((=)
             (rec `(** ,(cadr sre) ,(cadr sre) ,@(cddr sre))))
            ((>=)
             (rec `(** ,(cadr sre) #f ,@(cddr sre))))
            ((** **?)
             (cond
              ((or (and (number? (cadr sre))
                        (number? (caddr sre))
                        (> (cadr sre) (caddr sre)))
                   (and (not (cadr sre)) (caddr sre)))
               (lambda (cnk init src str i end matches fail) (fail)))
              (else
               (let* ((from (cadr sre))
                      (to (caddr sre))
                      (? (if (eq? '** (car sre)) '? '??))
                      (* (if (eq? '** (car sre)) '* '*?))
                      (sre (sre-sequence (cdddr sre)))
                      (x-sre (sre-strip-submatches sre))
                      (next (if to
                                (if (= from to)
                                    next
                                    (fold (lambda (x next)
                                            (lp `(,? ,sre) n flags next))
                                          next
                                          (zero-to (- to from))))
                                (rec `(,* ,sre)))))
                 (if (zero? from)
                     next
                     (lp `(seq ,@(map (lambda (x) x-sre) (zero-to (- from 1)))
                               ,sre)
                         n
                         flags
                         next))))))
            ((word)
             (rec `(seq bow ,@(cdr sre) eow)))
            ((word+)
             (rec `(seq bow (+ (& (or alphanumeric "_")
                                  (or ,@(cdr sre)))) eow)))
            ((posix-string)
             (rec (string->sre (cadr sre))))
            ((look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (if (check cnk init src str i end matches (lambda () #f))
                     (next cnk init src str i end matches fail)
                     (fail)))))
            ((neg-look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (if (check cnk init src str i end matches (lambda () #f))
                     (fail)
                     (next cnk init src str i end matches fail)))))
            ((look-behind neg-look-behind)
             (let ((check
                    (lp (sre-sequence
                         (cons '(* any) (append (cdr sre) '(eos))))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (let* ((prev ((chunker-get-substring cnk)
                               (car init)
                               (cdr init)
                               src
                               i))
                        (len (string-length prev))
                        (src2 (list prev 0 len)))
                   (if ((if (eq? (car sre) 'look-behind) (lambda (x) x) not)
                        (check irregex-basic-string-chunker
                               (cons src2 0) src2 prev 0 len matches (lambda () #f)))
                       (next cnk init src str i end matches fail)
                       (fail))))))
            ((atomic)
             (let ((once
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (cnk init src str i end matches fail) i))))
               (lambda (cnk init src str i end matches fail)
                 (let ((j (once cnk init src str i end matches (lambda () #f))))
                   (if j
                       (next cnk init src str j end matches fail)
                       (fail))))))
            ((if)
             (let* ((test-submatches (sre-count-submatches (cadr sre)))
                    (pass (lp (caddr sre) flags (+ n test-submatches) next))
                    (fail (if (pair? (cdddr sre))
                              (lp (cadddr sre)
                                  (+ n test-submatches
                                     (sre-count-submatches (caddr sre)))
                                  flags
                                  next)
                              (lambda (cnk init src str i end matches fail)
                                (fail)))))
               (cond
                ((or (number? (cadr sre)) (symbol? (cadr sre)))
                 (let ((index
                        (if (symbol? (cadr sre))
                            (cond
                             ((assq (cadr sre) names) => cdr)
                             (else
                              (error "unknown named backref in SRE IF" sre)))
                            (cadr sre))))
                   (lambda (cnk init src str i end matches fail2)
                     (if (%irregex-match-end-chunk matches index)
                         (pass cnk init src str i end matches fail2)
                         (fail cnk init src str i end matches fail2)))))
                (else
                 (let ((test (lp (cadr sre) n flags pass)))
                   (lambda (cnk init src str i end matches fail2)
                     (test cnk init src str i end matches
                           (lambda () (fail cnk init src str i end matches fail2)))
                     ))))))
            ((backref backref-ci)
             (let ((n (cond ((number? (cadr sre)) (cadr sre))
                            ((assq (cadr sre) names) => cdr)
                            (else (error "unknown backreference" (cadr sre)))))
                   (compare (if (or (eq? (car sre) 'backref-ci)
                                    (flag-set? flags ~case-insensitive?))
                                string-ci=?
                                string=?)))
               (lambda (cnk init src str i end matches fail)
                 (let ((s (irregex-match-substring matches n)))
                   (if (not s)
                       (fail)
                       ;; XXXX create an abstract subchunk-compare
                       (let lp ((src src)
                                (str str)
                                (i i)
                                (end end)
                                (j 0)
                                (len (string-length s)))
                         (cond
                          ((<= len (- end i))
                           (cond
                            ((compare (substring s j (string-length s))
                                      (substring str i (+ i len)))
                             (next cnk init src str (+ i len) end matches fail))
                            (else
                             (fail))))
                          (else
                           (cond
                            ((compare (substring s j (+ j (- end i)))
                                      (substring str i end))
                             (let ((src2 ((chunker-get-next cnk) src)))
                               (if src2
                                   (lp src2
                                       ((chunker-get-str cnk) src2)
                                       ((chunker-get-start cnk) src2)
                                       ((chunker-get-end cnk) src2)
                                       (+ j (- end i))
                                       (- len (- end i)))
                                   (fail))))
                            (else
                             (fail)))))))))))
            ((dsm)
             (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) flags next))
            (($ submatch)
             (let ((body
                    (lp (sre-sequence (cdr sre))
                        (+ n 1)
                        flags
                        (lambda (cnk init src str i end matches fail)
                          (let ((old-source
                                 (%irregex-match-end-chunk matches n))
                                (old-index
                                 (%irregex-match-end-index matches n)))
                            (irregex-match-end-chunk-set! matches n src)
                            (irregex-match-end-index-set! matches n i)
                            (next cnk init src str i end matches
                                  (lambda ()
                                    (irregex-match-end-chunk-set!
                                     matches n old-source)
                                    (irregex-match-end-index-set!
                                     matches n old-index)
                                    (fail))))))))
               (lambda (cnk init src str i end matches fail)
                 (let ((old-source (%irregex-match-start-chunk matches n))
                       (old-index (%irregex-match-start-index matches n)))
                   (irregex-match-start-chunk-set! matches n src)
                   (irregex-match-start-index-set! matches n i)
                   (body cnk init src str i end matches
                         (lambda ()
                           (irregex-match-start-chunk-set!
                            matches n old-source)
                           (irregex-match-start-index-set!
                            matches n old-index)
                           (fail)))))))
            ((=> submatch-named)
             (rec `(submatch ,@(cddr sre))))
            (else
             (error "unknown regexp operator" sre)))))
     ((symbol? sre)
      (case sre
        ((any)
         (lambda (cnk init src str i end matches fail)
           (if (< i end)
               (next cnk init src str (+ i 1) end matches fail)
               (let ((src2 ((chunker-get-next cnk) src)))
                 (if src2
                     (let ((str2 ((chunker-get-str cnk) src2))
                           (i2 ((chunker-get-start cnk) src2))
                           (end2 ((chunker-get-end cnk) src2)))
                       (next cnk init src2 str2 (+ i2 1) end2 matches fail))
                     (fail))))))
        ((nonl)
         (lambda (cnk init src str i end matches fail)
           (if (< i end)
               (if (not (eqv? #\newline (string-ref str i)))
                   (next cnk init src str (+ i 1) end matches fail)
                   (fail))
               (let ((src2 ((chunker-get-next cnk) src)))
                 (if src2
                     (let ((str2 ((chunker-get-str cnk) src2))
                           (i2 ((chunker-get-start cnk) src2))
                           (end2 ((chunker-get-end cnk) src2)))
                       (if (not (eqv? #\newline (string-ref str2 i2)))
                           (next cnk init src2 str2 (+ i2 1) end2 matches fail)
                           (fail)))
                     (fail))))))
        ((bos)
         (lambda (cnk init src str i end matches fail)
           (if (and (eq? src (car init)) (eqv? i (cdr init)))
               (next cnk init src str i end matches fail)
               (fail))))
        ((bol)
         (lambda (cnk init src str i end matches fail)
           (if (or (and (eq? src (car init)) (eqv? i (cdr init)))
                   (and (> i ((chunker-get-start cnk) src))
                        (eqv? #\newline (string-ref str (- i 1)))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((bow)
         (lambda (cnk init src str i end matches fail)
           (if (and (or (if (> i ((chunker-get-start cnk) src))
                            (not (char-alphanumeric? (string-ref str (- i 1))))
                            (let ((ch (chunker-prev-char cnk src end)))
                              (and ch (not (char-alphanumeric? ch)))))
                        (and (eq? src (car init)) (eqv? i (cdr init))))
                    (if (< i end)
                        (char-alphanumeric? (string-ref str i))
                        (let ((next ((chunker-get-next cnk) src)))
                          (and next
                               (char-alphanumeric?
                                (string-ref ((chunker-get-str cnk) next)
                                            ((chunker-get-start cnk) next)))))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eos)
         (lambda (cnk init src str i end matches fail)
           (if (and (>= i end) (not ((chunker-get-next cnk) src)))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eol)
         (lambda (cnk init src str i end matches fail)
           (if (if (< i end)
                   (eqv? #\newline (string-ref str i))
                   (let ((src2 ((chunker-get-next cnk) src)))
                     (if (not src2)
                         #t
                         (eqv? #\newline
                               (string-ref ((chunker-get-str cnk) src2)
                                           ((chunker-get-start cnk) src2))))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((eow)
         (lambda (cnk init src str i end matches fail)
           (if (and (if (< i end)
                        (not (char-alphanumeric? (string-ref str i)))
                        (let ((ch (chunker-next-char cnk src)))
                          (or (not ch) (not (char-alphanumeric? ch)))))
                    (if (> i ((chunker-get-start cnk) src))
                        (char-alphanumeric? (string-ref str (- i 1)))
                        (let ((prev (chunker-prev-char cnk init src)))
                          (or (not prev) (char-alphanumeric? prev)))))
               (next cnk init src str i end matches fail)
               (fail))))
        ((nwb)  ;; non-word-boundary
         (lambda (cnk init src str i end matches fail)
           (let ((c1 (if (< i end)
                         (string-ref str i)
                         (chunker-next-char cnk src)))
                 (c2 (if (> i ((chunker-get-start cnk) src))
                         (string-ref str (- i 1))
                         (chunker-prev-char cnk init src))))
             (if (and c1 c2
                      (if (char-alphanumeric? c1)
                          (char-alphanumeric? c2)
                          (not (char-alphanumeric? c2))))
                 (next cnk init src str i end matches fail)
                 (fail)))))
        ((epsilon)
         next)
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (rec (cdr cell))
               (error "unknown regexp" sre))))))
     ((char? sre)
      (if (flag-set? flags ~case-insensitive?)
          ;; case-insensitive
          (lambda (cnk init src str i end matches fail)
            (if (>= i end)
                (let lp ((src2 ((chunker-get-next cnk) src)))
                  (if src2
                      (let ((str2 ((chunker-get-str cnk) src2))
                            (i2 ((chunker-get-start cnk) src2))
                            (end2 ((chunker-get-end cnk) src2)))
                        (if (>= i2 end2)
                            (lp ((chunker-get-next cnk) src2))
                            (if (char-ci=? sre (string-ref str2 i2))
                                (next cnk init src2 str2 (+ i2 1) end2
                                      matches fail)
                                (fail))))
                      (fail)))
                (if (char-ci=? sre (string-ref str i))
                    (next cnk init src str (+ i 1) end matches fail)
                    (fail))))
          ;; case-sensitive
          (lambda (cnk init src str i end matches fail)
            (if (>= i end)
                (let lp ((src2 ((chunker-get-next cnk) src)))
                  (if src2
                      (let ((str2 ((chunker-get-str cnk) src2))
                            (i2 ((chunker-get-start cnk) src2))
                            (end2 ((chunker-get-end cnk) src2)))
                        (if (>= i2 end2)
                            (lp ((chunker-get-next cnk) src2))
                            (if (char=? sre (string-ref str2 i2))
                                (next cnk init src2 str2 (+ i2 1) end2
                                      matches fail)
                                (fail))))
                      (fail)))
                (if (char=? sre (string-ref str i))
                    (next cnk init src str (+ i 1) end matches fail)
                    (fail))))
          ))
     ((string? sre)
      (rec (sre-sequence (string->list sre)))
;; XXXX reintroduce faster string matching on chunks
;;       (if (flag-set? flags ~case-insensitive?)
;;           (rec (sre-sequence (string->list sre)))
;;           (let ((len (string-length sre)))
;;             (lambda (cnk init src str i end matches fail)
;;               (if (and (<= (+ i len) end)
;;                        (%substring=? sre str 0 i len))
;;                   (next str (+ i len) matches fail)
;;                   (fail)))))
      )
     (else
      (error "unknown regexp" sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Character Sets
;;
;; Simple character sets as lists of ranges, as used in the NFA/DFA
;; compilation.  This is not especially efficient, but is portable and
;; scalable for any range of character sets.

(define (sre-cset->procedure cset next)
  (lambda (cnk init src str i end matches fail)
    (if (< i end)
        (if (cset-contains? cset (string-ref str i))
            (next cnk init src str (+ i 1) end matches fail)
            (fail))
        (let ((src2 ((chunker-get-next cnk) src)))
          (if src2
              (let ((str2 ((chunker-get-str cnk) src2))
                    (i2 ((chunker-get-start cnk) src2))
                    (end2 ((chunker-get-end cnk) src2)))
                (if (cset-contains? cset (string-ref str2 i2))
                    (next cnk init src2 str2 (+ i2 1) end2 matches fail)
                    (fail)))
              (fail))))))

(define (make-cset) (vector))
(define (range->cset from to) (vector (cons from to)))
(define (char->cset ch) (vector (cons ch ch)))
(define (cset-empty? cs) (zero? (vector-length cs)))
(define (maybe-cset->char cs)
  (if (and (= (vector-length cs) 1)
           (char=? (car (vector-ref cs 0)) (cdr (vector-ref cs 0))))
      (car (vector-ref cs 0))
      cs))

;; Since csets are sorted, there's only one possible representation of any cset
(define cset=? equal?)

(define (cset-size cs)
  (let ((len (vector-length cs)))
   (let lp ((i 0) (size 0))
     (if (= i len)
         size
         (lp (+ i 1) (+ size 1
                        (- (char->integer (cdr (vector-ref cs i)))
                           (char->integer (car (vector-ref cs i))))))))))

(define (cset->plist cs)
  (let lp ((i (- (vector-length cs) 1))
           (res '()))
    (if (= i -1)
        res
        (lp (- i 1) (cons (car (vector-ref cs i))
                          (cons (cdr (vector-ref cs i)) res))))))

(define (plist->cset ls)
  (let lp ((ls ls) (res (make-cset)))
    (if (null? ls)
        res
        (lp (cddr ls) (cset-union (range->cset (car ls) (cadr ls)) res)))))

(define (string->cset s)
  (fold (lambda (ch cs)
          (cset-adjoin cs ch))
        (make-cset)
        (string->list s)))

(define (sre->cset sre . o)
  (let lp ((sre sre) (ci? (and (pair? o) (car o))))
    (define (rec sre) (lp sre ci?))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (if ci?
              (cset-case-insensitive (string->cset (car sre)))
              (string->cset (car sre)))
          (case (car sre)
            ((~)
             (cset-complement
              (fold cset-union (rec (cadr sre)) (map rec (cddr sre)))))
            ((&)
             (fold cset-intersection (rec (cadr sre)) (map rec (cddr sre))))
            ((-)
             (fold (lambda (x res) (cset-difference res x))
                   (rec (cadr sre))
                   (map rec (cddr sre))))
            ((/)
             (let ((res (plist->cset (sre-flatten-ranges (cdr sre)))))
               (if ci?
                   (cset-case-insensitive res)
                   res)))
            ((or)
             (fold cset-union (rec (cadr sre)) (map rec (cddr sre))))
            ((w/case)
             (lp (sre-alternate (cdr sre)) #f))
            ((w/nocase)
             (lp (sre-alternate (cdr sre)) #t))
            (else
             (error "not a valid sre char-set operator" sre)))))
     ((char? sre) (if ci?
                      (cset-case-insensitive (range->cset sre sre))
                      (range->cset sre sre)))
     ((string? sre) (rec (list sre)))
     (else
      (let ((cell (assq sre sre-named-definitions)))
        (if cell
            (rec (cdr cell))
            (error "not a valid sre char-set" sre)))))))

(define (cset->sre cset)
  (sre-alternate
   (map (lambda (x) (list '/ (car x) (cdr x)))
        (vector->list cset))))

(define (cset-contains? cset ch)
  (let ((len (vector-length cset)))
    (case len
      ((0) #f)
      ((1) (let ((range (vector-ref cset 0)))
             (and (char<=? ch (cdr range)) (char<=? (car range) ch))))
      (else (let lp ((lower 0) (upper len))
              (let* ((middle (quotient (+ upper lower) 2))
                     (range (vector-ref cset middle)))
                (cond ((char<? (cdr range) ch)
                       (let ((next (+ middle 1)))
                         (and (< next upper) (lp next upper))))
                      ((char<? ch (car range))
                       (and (< lower middle) (lp lower middle)))
                      (else #t))))))))

(define (char-ranges-union a b)
  (cons (if (char<=? (car a) (car b)) (car a) (car b))
        (if (char>=? (cdr a) (cdr b)) (cdr a) (cdr b))))

(define (cset-union a b)
  (let union-range ((a (vector->list a))
                    (b (vector->list b))
                    (res '()))
    (cond
     ((null? a) (list->vector (reverse (append (reverse b) res))))
     ((null? b) (list->vector (reverse (append (reverse a) res))))
     (else
      (let ((a-range (car a))
            (b-range (car b)))
        (cond
         ((i/char<? (next-char-idx (cdr a-range)) (car b-range))
          (union-range (cdr a) b (cons a-range res)))
         ((i/char>? (car a-range) (next-char-idx (cdr b-range)))
          (union-range (cdr b) a (cons b-range res)))
         ((char>=? (cdr a-range) (car b-range))
          (union-range (cons (char-ranges-union a-range b-range) (cdr a))
                       (cdr b)
                       res))
         (else (union-range (cdr a)
                            (cons (char-ranges-union a-range b-range) (cdr b))
                            res))))))))

(define (cset-adjoin cs ch) (cset-union cs (char->cset ch)))

(define (char-idx obj)
  (if (char? obj)
    (char->integer obj)
    obj))
(define (i/char<? x y)
  (< (char-idx x) 
     (char-idx y)))
(define (i/char>? x y)
  (> (char-idx x)
     (char-idx y)))


(define (next-char c)
  (integer->char (+ (char->integer c) 1)))

(define (prev-char c)
  (integer->char (- (char->integer c) 1)))

;; NMOSH: we need this because R6RS doesn't allow (+ #x10ffff 1)
;;        char index.
(define (next-char-idx c)
  (+ (char->integer c) 1))

(define (cset-difference a b)
  (let diff ((a (vector->list a))
             (b (vector->list b))
             (res '()))
    (cond ((null? a) (list->vector (reverse res)))
          ((null? b) (list->vector (append (reverse res) a)))
          (else
           (let ((a-range (car a))
                 (b-range (car b)))
             (cond
              ((char<? (cdr a-range) (car b-range))
               (diff (cdr a) b (cons a-range res)))
              ((char>? (car a-range) (cdr b-range))
               (diff a (cdr b) res))
              ((and (char<=? (car b-range) (car a-range))
                    (char>=? (cdr b-range) (cdr a-range)))
               (diff (cdr a) b res))
              (else (let ((left (and (char<? (car a-range) (car b-range))
                                     (cons (car a-range)
                                           (prev-char (car b-range)))))
                          (right (and (char>? (cdr a-range) (cdr b-range))
                                      (cons (next-char (cdr b-range))
                                            (cdr a-range)))))
                      (diff (if right (cons right (cdr a)) (cdr a))
                            b
                            (if left (cons left res) res))))))))))

(define (min-char a b)
  (if (char<? a b) a b))

(define (max-char a b)
  (if (char<? a b) b a))

(define (cset-intersection a b)
  (let intersect ((a (vector->list a))
                  (b (vector->list b))
                  (res '()))
    (if (or (null? a) (null? b))
        (list->vector (reverse res))
        (let ((a-range (car a))
              (b-range (car b)))
          (cond
           ((char<? (cdr a-range) (car b-range))
            (intersect (cdr a) b res))
           ((char>? (car a-range) (cdr b-range))
            (intersect a (cdr b) res))
           (else
            (let ((result (cons (max-char (car b-range) (car a-range))
                                (min-char (cdr a-range) (cdr b-range)))))
              (intersect (if (char>? (cdr a-range) (cdr result))
                             a (cdr a))
                         (if (char>? (cdr b-range) (cdr result))
                             b (cdr b))
                         (cons result res)))))))))

(define (cset-complement a)
  (cset-difference (sre->cset *all-chars*) a))

;; This could use some optimization :)
(define (cset-case-insensitive a)
  (let lp ((ls (vector->list a)) (res '()))
    (cond ((null? ls) (list->vector (reverse res)))
          ((and (char-alphabetic? (caar ls))
                (char-alphabetic? (cdar ls)))
           (lp (cdr ls)
               (reverse
                (vector->list
                 (cset-union (cset-union (list->vector (reverse res))
                                         (vector (car ls)))
                             (range->cset (char-altcase (caar ls))
                                          (char-altcase (cdar ls))))))))
          (else (lp (cdr ls) (reverse (vector->list
                                       (cset-union (list->vector (reverse res))
                                                   (vector (car ls))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Match and Replace Utilities

(define (irregex-fold/fast irx kons knil str . o)
  (if (not (string? str)) (error "irregex-fold: not a string" str))
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (finish (or (and (pair? o) (car o)) (lambda (i acc) acc)))
         (start (if (and (pair? o) (pair? (cdr o))) (cadr o) 0))
         (end (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                  (caddr o)
                  (string-length str))))
    (if (not (and (integer? start) (exact? start)))
        (error "irregex-fold: not an exact integer" start))
    (if (not (and (integer? end) (exact? end)))
        (error "irregex-fold: not an exact integer" end))
    (irregex-match-chunker-set! matches irregex-basic-string-chunker)
    (let lp ((i start) (acc knil))
      (if (>= i end)
          (finish i acc)
          (let ((m (irregex-search/matches
                    irx
                    irregex-basic-string-chunker
                    (list str i end)
                    i
                    matches)))
            (if (not m)
                (finish i acc)
                (let* ((end (%irregex-match-end-index m 0))
                       (acc (kons i m acc)))
                  (irregex-reset-matches! matches)
                  (lp end acc))))))))

(define (irregex-fold irx kons . args)
  (if (not (procedure? kons)) (error "irregex-fold: not a procedure" kons))
  (let ((kons2 (lambda (i m acc) (kons i (irregex-copy-matches m) acc))))
    (apply irregex-fold/fast irx kons2 args)))

(define (irregex-fold/chunked/fast irx kons knil cnk start . o)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (finish (or (and (pair? o) (car o)) (lambda (src i acc) acc)))
         (i (if (and (pair? o) (pair? (cdr o)))
                (cadr o)
                ((chunker-get-start cnk) start))))
    (if (not (integer? i)) (error "irregex-fold/chunked: not an integer" i))
    (irregex-match-chunker-set! matches cnk)
    (let lp ((start start) (i i) (acc knil))
      (if (not start)
          (finish start i acc)
          (let ((m (irregex-search/matches irx cnk start i matches)))
            (if (not m)
                (finish start i acc)
                (let* ((acc (kons start i m acc))
                       (end-src (%irregex-match-end-chunk m 0))
                       (end-index (%irregex-match-end-index m 0)))
                  (irregex-reset-matches! matches)
                  (lp end-src end-index acc))))))))

(define (irregex-fold/chunked irx kons . args)
  (if (not (procedure? kons)) (error "irregex-fold/chunked: not a procedure" kons))
  (let ((kons2 (lambda (s i m acc) (kons s i (irregex-copy-matches m) acc))))
    (apply irregex-fold/chunked/fast irx kons2 args)))

(define (irregex-replace irx str . o)
  (if (not (string? str)) (error "irregex-replace: not a string" str))
  (let ((m (irregex-search irx str)))
    (and
     m
     (string-cat-reverse
      (cons (substring str (%irregex-match-end-index m 0) (string-length str))
            (append (irregex-apply-match m o)
                    (list (substring str 0 (%irregex-match-start-index m 0)))
                    ))))))

(define (irregex-replace/all irx str . o)
  (if (not (string? str)) (error "irregex-replace/all: not a string" str))
  (irregex-fold/fast
   irx
   (lambda (i m acc)
     (let ((m-start (%irregex-match-start-index m 0)))
       (append (irregex-apply-match m o)
               (if (>= i m-start)
                   acc
                   (cons (substring str i m-start) acc)))))
   '()
   str
   (lambda (i acc)
     (let ((end (string-length str)))
       (string-cat-reverse (if (>= i end)
                               acc
                               (cons (substring str i end) acc)))))))

(define (irregex-apply-match m ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        res
        (cond
         ((integer? (car ls))
          (lp (cdr ls)
              (cons (or (irregex-match-substring m (car ls)) "") res)))
         ((procedure? (car ls))
          (lp (cdr ls) (cons ((car ls) m) res)))
         ((symbol? (car ls))
          (case (car ls)
            ((pre)
             (lp (cdr ls)
                 (cons (substring (car (%irregex-match-start-chunk m 0))
                                  0
                                  (%irregex-match-start-index m 0))
                       res)))
            ((post)
             (let ((str (car (%irregex-match-start-chunk m 0))))
               (lp (cdr ls)
                   (cons (substring str
                                    (%irregex-match-end-index m 0)
                                    (string-length str))
                         res))))
            (else
             (cond
              ((assq (car ls) (irregex-match-names m))
               => (lambda (x) (lp (cons (cdr x) (cdr ls)) res)))
              (else
               (error "unknown match replacement" (car ls)))))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))

(define (irregex-extract irx str . o)
  (if (not (string? str)) (error "irregex-extract: not a string" str))
  (apply irregex-fold/fast
         irx
         (lambda (i m a) (cons (irregex-match-substring m) a))
         '()
         str
         (lambda (i a) (reverse a))
         o))

(define (irregex-split irx str . o)
  (if (not (string? str)) (error "irregex-split: not a string" str))
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (irregex-fold/fast
     irx
     (lambda (i m a)
       (if (= i (%irregex-match-start-index m 0))
           a
           (cons (substring str i (%irregex-match-start-index m 0)) a)))
     '()
     str
     (lambda (i a)
       (reverse (if (= i end) a (cons (substring str i end) a))))
     start
     end)))
)
