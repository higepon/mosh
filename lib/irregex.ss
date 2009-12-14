;; libheader added by hand
;; MOSH: mark code changes made

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
		;MOSH: check them...
		;irregex-match-start-source 
		;irregex-match-start-index
		;irregex-match-end-source 
		;irregex-match-end-index
		irregex-match-num-submatches
		irregex-fold irregex-replace irregex-replace/all
		irregex-dfa irregex-dfa/search irregex-dfa/extract
		irregex-nfa irregex-flags irregex-lengths irregex-names
		irregex-num-submatches
		;MOSH: add
		irregex-fold/chunked
		)
	 (import (rnrs)
		 (only (rnrs r5rs (6)) quotient)
		 (rnrs mutable-strings)
		 (rnrs mutable-pairs))
;;; Original copyright statement:
;;; Copyright (c) 2005-2009 Alex Shinn
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;; irregex.scm -- IrRegular Expressions
;;
;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
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
;; Notes
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
;; History
;;
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
;; data structures

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

;MOSH: vector-copy! is not available
;(define (irregex-copy-matches m)
;  (and (vector? m)
;       (let ((r (make-vector (vector-length m))))
;         (vector-copy! m r)
;         r)))

(define (irregex-copy-matches m)
  (and (vector? m)
       (vector-map (lambda (e) e) m)))

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

(define (irregex-match-start-chunk m n)
  (vector-ref m (+ 3 (* n 4))))
(define (irregex-match-start-index m n)
  (vector-ref m (+ 4 (* n 4))))
(define (irregex-match-end-chunk m n)
  (vector-ref m (+ 5 (* n 4))))
(define (irregex-match-end-index m n)
  (vector-ref m (+ 6 (* n 4))))

(define (irregex-match-start-chunk-set! m n start)
  (vector-set! m (+ 3 (* n 4)) start))
(define (irregex-match-start-index-set! m n start)
  (vector-set! m (+ 4 (* n 4)) start))
(define (irregex-match-end-chunk-set! m n end)
  (vector-set! m (+ 5 (* n 4)) end))
(define (irregex-match-end-index-set! m n end)
  (vector-set! m (+ 6 (* n 4)) end))

(define (irregex-match-index m opt)
  (if (pair? opt)
      (cond ((number? (car opt)) (car opt))
            ((assq (car opt) (irregex-match-names m)) => cdr)
            (else (error "unknown match name" (car opt))))
      0))

(define (irregex-match-valid-index? m n)
  (and (< (+ 3 (* n 4)) (vector-length m))
       (vector-ref m (+ 4 (* n 4)))))

(define (irregex-match-substring m . opt)
  (let* ((cnk (irregex-match-chunker m))
         (n (irregex-match-index m opt)))
    (and (irregex-match-valid-index? m n)
         ((chunker-get-substring cnk)
          (irregex-match-start-chunk m n)
          (irregex-match-start-index m n)
          (irregex-match-end-chunk m n)
          (irregex-match-end-index m n)))))

(define (irregex-match-subchunk m . opt)
  (let* ((cnk (irregex-match-chunker m))
         (n (irregex-match-index m opt))
         (get-subchunk (chunker-get-subchunk cnk)))
    (if (not get-subchunk)
        (error "this chunk type does not support match subchunks")
        (and (irregex-match-valid-index? m n)
             (get-subchunk
              (irregex-match-start-chunk m n)
              (irregex-match-start-index m n)
              (irregex-match-end-chunk m n)
              (irregex-match-end-index m n))))))

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
;; string utilities

;;;; Unicode version (skip surrogates)
(define *all-chars*
  `(/ ,(integer->char 0) ,(integer->char #xD7FF)
      ,(integer->char #xE000) ,(integer->char #x10FFFF)))

;;;; ASCII version, offset to not assume 0-255
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
;; list utilities

;; like the one-arg IOTA case
(define (zero-to n)
  (if (<= n 0)
      '()
      (let lp ((i (- n 1)) (res '()))
        (if (zero? i) (cons 0 res) (lp (- i 1) (cons i res))))))

;; take the head of list FROM up to but not including TO, which must
;; be a tail of the list
(define (take-up-to from to)
  (let lp ((ls from) (res '()))
    (if (and (pair? ls) (not (eq? ls to)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse res))))

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
;; flags

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
;; parsing pcre strings (yuck)

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
                     (error "? can't follow empty sre" str res)
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
                   (error "duplicate repetition (e.g. **) in sre" str res))
                  ((sre-empty? x)
                   (error "can't repeat empty sre (e.g. ()*)" str res))
                  (else
                   (lp (+ i 1) (+ i 1) flags
                       (cons (list op x) (cdr res))
                       st)))))
              ((#\()
               (cond
                ((>= (+ i 1) end)
                 (error "unterminated parenthesis in regexp" str))
                ((not (eqv? #\? (string-ref str (+ i 1))))
                 (lp (+ i 1) (+ i 1) (flag-join flags ~save?) '() (save)))
                ((>= (+ i 2) end)
                 (error "unterminated parenthesis in regexp" str))
                (else
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
               (if (or (>= (+ i 1) end)
                       (not (or (char-numeric? (string-ref str (+ i 1)))
                                (eqv? #\, (string-ref str (+ i 1))))))
                   (lp (+ i 1) from flags res st)
                   (let* ((res (collect/single))
                          (x (car res))
                          (tail (cdr res))
                          (j (string-scan-char str #\} (+ i 1)))
                          (s2 (string-split-char (substring str (+ i 1) j) #\,))
                          (n (or (string->number (car s2)) 0))
                          (m (and (pair? (cdr s2)) (string->number (cadr s2)))))
                     (cond
                      ((null? (cdr s2))
                       (lp (+ j 1) (+ j 1) flags `((= ,n ,x) ,@tail) st))
                      (m
                       (lp (+ j 1) (+ j 1) flags `((** ,n ,m ,x) ,@tail) st))
                      (else
                       (lp (+ j 1) (+ j 1) flags `((>= ,n ,x) ,@tail) st)
                       )))))
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
    (define (go i chars ranges)
      (if (>= i end)
          (error "incomplete char set" str i end)
          (let ((c (string-ref str i)))
            (case c
              ((#\])
               (if (and (null? chars) (null? ranges))
                   (go (+ i 1) (cons #\] chars) ranges)
                   (let ((ci? (flag-set? flags ~case-insensitive?))
                         (hi-chars (if utf8? (filter high-char? chars) '()))
                         (chars (if utf8? (remove high-char? chars) chars)))
                     (list
                      ((lambda (res)
                         (if invert? (cons '~ res) (sre-alternate res)))
                       (append
                        hi-chars
                        (if (pair? chars)
                            (list
                             (list (list->string
                                    ((if ci?
                                         cset-case-insensitive
                                         (lambda (x) x))
                                     (reverse chars)))))
                            '())
                        (if (pair? ranges)
                            (let ((res (if ci?
                                           (cset-case-insensitive
                                            (reverse ranges))
                                           (reverse ranges))))
                              (list (cons '/ (alist->plist res))))
                            '())))
                      i))))
              ((#\-)
               (cond
                ((or (= i start)
                     (and (= i (+ start 1)) (eqv? #\^ (string-ref str start)))
                     (eqv? #\] (string-ref str (+ i 1))))
                 (go (+ i 1) (cons c chars) ranges))
                ((null? chars)
                 (error "bad char-set"))
                (else
                 (let* ((c1 (car chars))
                        (c2 (string-ref str (+ i 1))))
                   (apply
                    (lambda (c2 j)
                      (if (char<? c2 c1)
                          (error "inverted range in char-set" c1 c2)
                          (go j (cdr chars) (cons (cons c1 c2) ranges))))
                    (cond
                     ((and (eqv? #\\ c2) (assv c2 posix-escape-sequences))
                      => (lambda (x) (list (cdr x) (+ i 3))))
                     ((and (eqv? #\\ c2)
                           (eqv? (string-ref str (+ i 2)) #\x))
                      (string-parse-hex-escape str (+ i 3) end))
                     ((and utf8? (<= #x80 (char->integer c2) #xFF))
                      (let ((len (utf8-start-char->length c2)))
                        (list (utf8-string-ref str (+ i 1) len) (+ i 1 len))))
                     (else
                      (list c2 (+ i 2)))))))))
              ((#\[)
               (let* ((inv? (eqv? #\^ (string-ref str (+ i 1))))
                      (i2 (if inv? (+ i 2) (+ i 1))))
                 (case (string-ref str i2)
                   ((#\:)
                    (let ((j (string-scan-char str #\: (+ i2 1))))
                      (if (or (not j) (not (eqv? #\] (string-ref str (+ j 1)))))
                          (error "incomplete character class" str)
                          (let* ((cset (sre->cset
                                        (string->symbol
                                         (substring str (+ i2 1) j))))
                                 (cset (if inv? (cset-complement cset) cset)))
                            (go (+ j 2)
                                (append (filter char? cset) chars)
                                (append (filter pair? cset) ranges))))))
                   ((#\= #\.)
                    (error "collating sequences not supported" str))
                   (else
                    (go (+ i 1) (cons #\[ chars) ranges)))))
              ((#\\)
               (let ((c (string-ref str (+ i 1))))
                 (case c
                   ((#\d #\D #\s #\S #\w #\W)
                    (let ((cset (sre->cset (string->sre (string #\\ c)))))
                      (go (+ i 2)
                          (append (filter char? cset) chars)
                          (append (filter pair? cset) ranges))))
                   ((#\x)
                    (apply
                     (lambda (ch j)
                       (go j (cons ch chars) ranges))
                     (string-parse-hex-escape str (+ i 2) end)))
                   (else
                    (let ((c (cond ((assv c posix-escape-sequences) => cdr)
                                   (else c))))
                      (go (+ i 2)
                          (cons (string-ref str (+ i 1)) (cons c chars))
                          ranges))))))
              (else
               (if (and utf8? (<= #x80 (char->integer c) #xFF))
                   (let ((len (utf8-start-char->length c)))
                     (go (+ i len)
                         (cons (utf8-string-ref str i len) chars)
                         ranges))
                   (go (+ i 1) (cons c chars) ranges)))))))
    (if invert?
        (go (+ start 1)
            (if (flag-set? flags ~multi-line?) '(#\newline) '())
            '())
        (go start '() '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf8 utilities

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

(define (sre-adjust-utf8 sre flags) sre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation

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
                 => (lambda (nfa) (nfa->dfa nfa (* dfa-limit (length nfa)))))
                (else #f)))
         (dfa (cond ((and dfa/search (sre->nfa sre-dfa pat-flags))
                     => (lambda (nfa) (nfa->dfa nfa (* dfa-limit (length nfa)))))
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
;; sre analysis

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
;; sre manipulation

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
;; matching

(define irregex-basic-string-chunker
  (make-irregex-chunker (lambda (x) #f)
                        car
                        cadr
                        caddr
                        (lambda (src1 i src2 j)
                          (substring (car src1) i j))))

(define (irregex-search x str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
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
         (irregex-match-end-chunk matches 0)
         (irregex-match-end-index matches 0)
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
                 (irregex-match-end-chunk matches 0)
                 (irregex-match-end-index matches 0)
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
                    #f))))))))))

(define (irregex-match irx str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
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
       (= ((chunker-get-end cnk) (irregex-match-end-chunk matches 0))
          (irregex-match-end-index matches 0))
       (begin
         (irregex-match-start-chunk-set! matches 0 src)
         (irregex-match-start-index-set! matches
                                         0
                                         ((chunker-get-start cnk) src))
         ((irregex-dfa/extract irx)
          cnk src ((chunker-get-start cnk) src)
          (irregex-match-end-chunk matches 0)
          (irregex-match-end-index matches 0)
          matches)
         matches)))
     (else
      (let* ((matcher (irregex-nfa irx))
             (str ((chunker-get-str cnk) src))
             (i ((chunker-get-start cnk) src))
             (end ((chunker-get-end cnk) src))
             (m (matcher cnk src src str i end matches (lambda () #f))))
        (and m
             (not ((chunker-get-next cnk) (irregex-match-end-chunk m 0)))
             (= ((chunker-get-end cnk) (irregex-match-end-chunk m 0))
                (irregex-match-end-index m 0))
             m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFA matching

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
                                  (if (eqv? ch (car x))
                                      #t
                                      (and (pair? (car x))
                                           (char<=? (caar x) ch)
                                           (char<=? ch (cdar x)))))
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
                       (irregex-match-end-chunk matches index)
                       #t))))
           (else
            (let* ((ch (string-ref str i))
                   (cell (find (lambda (x)
                                 (if (eqv? ch (car x))
                                     #t
                                     (and (pair? (car x))
                                          (char<=? (caar x) ch)
                                          (char<=? ch (cdar x)))))
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
               ((and index (irregex-match-end-chunk matches index))
                #t)
               (else
                #f))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRE->NFA compilation
;;
;; An NFA state is a numbered node with a list of patter->number
;; transitions, where pattern is either a character, (lo . hi)
;; character range, or epsilon (indicating an empty transition).
;; There may be duplicate characters and overlapping ranges - since
;; it's an NFA we process it by considering all possible transitions.

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

;; Compile and return the list of NFA states.  The start state will be
;; at the head of the list, and all remaining states will be in
;; descending numeric order, with state 0 being the unique accepting
;; state.
(define (sre->nfa sre . o)
  ;; we loop over an implicit sequence list
  (let lp ((ls (list sre))
           (n 1)
           (flags (if (pair? o) (car o) ~none))
           (next (list (list 0))))
    (define (new-state-number state)
      (max n (+ 1 (caar state))))
    (define (extend-state next . trans)
      (and next
           (cons (cons (new-state-number next)
                       (map (lambda (x) (cons x (caar next))) trans))
                 next)))
    (if (null? ls)
        next
        (cond
         ((string? (car ls))
          ;; process literal strings a char at a time
          (lp (append (string->list (car ls)) (cdr ls)) n flags next))
         ((eq? 'epsilon (car ls))
          ;; chars and epsilons go directly into the transition table
          (extend-state (lp (cdr ls) n flags next) (car ls)))
         ((char? (car ls))
          (let ((alt (char-altcase (car ls))))
            (if (and (flag-set? flags ~case-insensitive?)
                     (not (eqv? (car ls) alt)))
                (extend-state (lp (cdr ls) n flags next) (car ls) alt)
                (extend-state (lp (cdr ls) n flags next) (car ls)))))
         ((symbol? (car ls))
          (let ((cell (assq (car ls) sre-named-definitions)))
            (and cell
                 (lp (cons (if (procedure? (cdr cell))
                               ((cdr cell))
                               (cdr cell))
                           (cdr ls)) n flags next))))
         ((pair? (car ls))
          (cond
           ((string? (caar ls))
            ;; enumerated character set
            (lp (cons (sre-alternate (string->list (caar ls))) (cdr ls))
                n
                flags
                next))
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
                 (and next (lp (cdar ls) (new-state-number next) flags next))))
              ((/ - & ~) 
               (let ((ranges (sre->cset (car ls)
                                        (flag-set? flags ~case-insensitive?))))
                 (case (length ranges)
                   ((1)
                    (extend-state (lp (cdr ls) n flags next) (car ranges)))
                   (else
                    (let ((next (lp (cdr ls) n flags next)))
                      (and
                       next
                       (lp (list (sre-alternate
                                  (map (lambda (x) (if (pair? x)
                                                  (list '/ (car x) (cdr x))
                                                  x))
                                       ranges)))
                           (new-state-number next)
                           (flag-clear flags ~case-insensitive?)
                           next)))))))
              ((or)
               (let ((next (lp (cdr ls) n flags next)))
                 (and
                  next
                  (if (null? (cdar ls))
                      ;; empty (or) always fails
                      `((,(new-state-number next)) ,@next)
                      ;; compile both branches and insert epsilon
                      ;; transitions to either
                      (let* ((b (lp (list (sre-alternate (cddar ls)))
                                    (new-state-number next)
                                    flags
                                    next))
                             (a (and b (lp (list (cadar ls))
                                           (new-state-number b)
                                           flags
                                           next))))
                        (and a
                             `((,(new-state-number a)
                                (epsilon . ,(caar a))
                                (epsilon . ,(caar b)))
                               ,@(take-up-to a next)
                               ,@b)))))))
              ((?)
               (let ((next (lp (cdr ls) n flags next)))
                 ;; insert an epsilon transition directly to next
                 (and
                  next
                  (let ((a (lp (cdar ls) (new-state-number next) flags next)))
                    (cond
                     (a
                      ;;`((,(caar a) (epsilon . ,(caar next)) ,@(cdar a))
                      ;;  ,@(cdr a))
                      (set-cdr! (car a) `((epsilon . ,(caar next)) ,@(cdar a)))
                      a)
                     (else
                      #f))))))
              ((+ *)
               (let ((next (lp (cdr ls) n flags next)))
                 (and
                  next
                  (let* ((new (lp '(epsilon)
                                  (new-state-number next)
                                  flags
                                  next))
                         (a (lp (cdar ls) (new-state-number new) flags new)))
                    (and
                     a
                     (begin
                       ;; for *, insert an epsilon transition as in ? above
                       (if (eq? '* (caar ls))
                           (set-cdr! (car a)
                                     `((epsilon . ,(caar new)) ,@(cdar a))))
                       ;; for both, insert a loop back to self
                       (set-cdr! (car new)
                                 `((epsilon . ,(caar a)) ,@(cdar new)))
                       a))))))
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
              (($ submatch => submatch-named)
               ;; ignore submatches altogether
               (lp (cons (sre-sequence (cdar ls)) (cdr ls)) n flags next))
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
          #f)))))

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
;; NFA->DFA compilation
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
    (let lp ((ls (list (nfa-closure nfa (list (caar nfa)))))
             (i 0)
             (res '()))
      (cond
       ((null? ls)
        (dfa-renumber (reverse res)))
       ((assoc (car ls) res)
        (lp (cdr ls) i res))
       (else
        (let* ((states (car ls))
               (trans (nfa-state-transitions nfa states))
               (accept? (and (memv 0 states) #t)))
          (and (or (not max-states) (< (+ i 1) max-states))
               (lp (append (map cdr trans) (cdr ls))
                   (+ i 1)
                   `((,states ,accept? ,@trans) ,@res)))))))))

;; When the conversion is complete we renumber the DFA sets-of-states
;; in order and convert the result to a vector for fast lookup.
(define (dfa-renumber dfa)
  (let ((states (map cons (map car dfa) (zero-to (length dfa)))))
    (define (renumber state)
      (cdr (assoc state states)))
    (list->vector
     (map
      (lambda (node)
        (cons (cadr node)
              (map (lambda (x) (cons (car x) (renumber (cdr x))))
                   (cddr node)))) 
      dfa))))

;; Extract all distinct characters or ranges and the potential states
;; they can transition to from a given set of states.  Any ranges that
;; would overlap with distinct characters are split accordingly.
(define (nfa-state-transitions nfa states)
  (let lp ((trans '())   ;; list of (char . state) or ((char . char) . state)
           (ls states)   ;; list of integers (remaining state numbers)
           (res '()))    ;; (char state ...) or ((char . char) state ...)
    (cond
     ((null? trans)
      (if (null? ls)
          (map (lambda (x) (cons (car x) (nfa-closure nfa (cdr x))))
               res)
          (let ((node (assv (car ls) nfa)))
            (lp (if node (cdr node) '()) (cdr ls) res))))
     ((eq? 'epsilon (caar trans))
      (lp (cdr trans) ls res))
     (else
      (lp (cdr trans) ls (nfa-join-transitions! res (car trans)))))))

(define (nfa-join-transitions! existing new)
  (define (join ls elt state)
    (if (not elt)
        ls
        (nfa-join-transitions! ls (cons elt state))))
  (cond
   ((char? (car new))
    (let ((ch (car new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          ;; done, just cons this on to the original list
          (cons (list ch (cdr new)) existing))
         ((eqv? ch (caar ls))
          ;; add a new state to an existing char
          (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
          existing)
         ((and (pair? (caar ls))
               (char<=? (caaar ls) ch)
               (char<=? ch (cdaar ls)))
          ;; split a range
          (apply
           (lambda (left right)
             (cons (cons ch (insert-sorted (cdr new) (cdar ls)))
                   (append (if left (list (cons left (cdar ls))) '())
                           (if right (list (cons right (cdar ls))) '())
                           res
                           (cdr ls))))
           (split-char-range (caar ls) (car new))))
         (else
          ;; keep looking
          (lp (cdr ls) (cons (car ls) res)))))))
   (else
    (let ((lo (caar new))
          (hi (cdar new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          (cons (list (car new) (cdr new)) existing))
         ((and (char? (caar ls)) (char<=? lo (caar ls)) (char<=? (caar ls) hi))
          ;; range enclosing a character
          (apply
           (lambda (left right)
             (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
             (join (join existing left (cdr new)) right (cdr new)))
           (split-char-range (car new) (caar ls))))
         ((and (pair? (caar ls))
               (or (and (char<=? (caaar ls) hi) (char<=? lo (cdaar ls)))
                   (and (char<=? hi (caaar ls)) (char<=? (cdaar ls) lo))))
          ;; overlapping ranges
          (apply
           (lambda (left1 left2 same right1 right2)
             (let ((old-states (cdar ls)))
               (set-car! (car ls) same)
               (set-cdr! (car ls) (insert-sorted (cdr new) old-states))
               (let* ((res (if right1
                               (cons (cons right1 old-states) existing)
                               existing))
                      (res (if right2 (cons (cons right2 old-states) res) res)))
                 (join (join res left1 (cdr new)) left2 (cdr new)))))
           (intersect-char-ranges (car new) (caar ls))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))))

(define (char-range c1 c2)
  (if (eqv? c1 c2) c1 (cons c1 c2)))

;; assumes ch is included in the range
(define (split-char-range range ch)
  (list
   (and (not (eqv? ch (car range)))
        (char-range (car range) (integer->char (- (char->integer ch) 1))))
   (and (not (eqv? ch (cdr range)))
        (char-range (integer->char (+ (char->integer ch) 1)) (cdr range)))))

;; returns (possibly #f) char ranges:
;;    a-only-1  a-only-2  a-and-b  b-only-1  b-only-2
(define (intersect-char-ranges a b)
  (if (char>? (car a) (car b))
      (reverse (intersect-char-ranges b a))
      (let ((a-lo (car a))
            (a-hi (cdr a))
            (b-lo (car b))
            (b-hi (cdr b)))
        (list
         (and (char<? a-lo b-lo)
              (char-range a-lo (integer->char (- (char->integer b-lo) 1))))
         (and (char>? a-hi b-hi)
              (char-range (integer->char (+ (char->integer b-hi) 1)) a-hi))
         (char-range b-lo (if (char<? b-hi a-hi) b-hi a-hi))
         #f
         (and (char>? b-hi a-hi)
              (char-range (integer->char (+ (char->integer a-hi) 1)) b-hi))))))

;; The `closure' of a list of NFA states - all states that can be
;; reached from any of them using any number of epsilon transitions.
(define (nfa-closure nfa states)
  (let lp ((ls states)
           (res '()))
    (cond
     ((null? ls)
      res)
     ((memv (car ls) res)
      (lp (cdr ls) res))
     (else
      (lp (append (map cdr
                       (filter (lambda (trans) (eq? 'epsilon (car trans)))
                               (cdr (assv (car ls) nfa))))
                  (cdr ls))
          (insert-sorted (car ls) res))))))

;; insert an integer uniquely into a sorted list
(define (insert-sorted n ls)
  (cond
   ((null? ls)
    (cons n '()))
   ((<= n (car ls))
    (if (= n (car ls))
        ls
        (cons n ls)))
   (else
    (cons (car ls) (insert-sorted n (cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            (let ((dfa (nfa->dfa (sre->nfa sre))))
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
               (match-once cnk start i end j matches)
               #t)))
          (($ submatch)
           (let ((match-one
                  (lp (sre-sequence (cdr sre)) (+ n 1) #t))
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
;; closure compilation - we use this for non-regular expressions
;; instead of an interpreted NFA matcher

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
                        (check cnk src2 src2 prev 0 len matches (lambda () #f)))
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
                     (if (irregex-match-end-chunk matches index)
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
                                 (irregex-match-end-chunk matches n))
                                (old-index
                                 (irregex-match-end-index matches n)))
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
                 (let ((old-source (irregex-match-start-chunk matches n))
                       (old-index (irregex-match-start-index matches n)))
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

(define (plist->alist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res)))))

(define (alist->plist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (cons (cdar ls) (cons (caar ls) res))))))

(define (sre->cset sre . o)
  (let lp ((sre sre) (ci? (and (pair? o) (car o))))
    (define (rec sre) (lp sre ci?))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (if ci?
              (cset-case-insensitive (string->list (car sre)))
              (string->list (car sre)))
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
             (let ((res (plist->alist (sre-flatten-ranges (cdr sre)))))
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
     ((char? sre) (rec (list (string sre))))
     ((string? sre) (rec (list sre)))
     (else
      (let ((cell (assq sre sre-named-definitions)))
        (if cell
            (rec (cdr cell))
            (error "not a valid sre char-set" sre)))))))

;;;; another debugging utility
;; (define (cset->sre cset)
;;   (let lp ((ls cset) (chars '()) (ranges '()))
;;     (cond
;;      ((null? ls)
;;       (sre-alternate
;;        (append
;;         (if (pair? chars) (list (list (list->string chars))) '())
;;         (if (pair? ranges) (list (cons '/ (alist->plist ranges))) '()))))
;;      ((char? (car ls)) (lp (cdr ls) (cons (car ls) chars) ranges))
;;      (else (lp (cdr ls) chars (cons (car ls) ranges))))))

(define (cset-contains? cset ch)
  (find (lambda (x)
          (or (eqv? x ch)
              (and (pair? x) (char<=? (car x) ch) (char<=? ch (cdr x)))))
        cset))

(define (cset-range x)
  (if (char? x) (cons x x) x))

(define (char-ranges-overlap? a b)
  (if (pair? a)
      (if (pair? b)
          (or (and (char<=? (car a) (cdr b)) (char<=? (car b) (cdr a)))
              (and (char<=? (cdr b) (car a)) (char<=? (cdr a) (car b))))
          (and (char<=? (car a) b) (char<=? b (cdr a))))
      (if (pair? b)
          (char-ranges-overlap? b a)
          (eqv? a b))))

(define (char-ranges-union a b)
  (cons (if (char<=? (car a) (car b)) (car a) (car b))
        (if (char>=? (cdr a) (cdr b)) (cdr a) (cdr b))))

(define (cset-union a b)
  (cond ((null? b) a)
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (cset-union
               (cset-union (append (take-up-to a ls) (cdr ls))
                           (list (char-ranges-union (cset-range (car ls))
                                                    (cset-range (car b)))))
               (cdr b))))
        (else (cset-union (cons (car b) a) (cdr b)))))

(define (cset-difference a b)
  (cond ((null? b) a)
        ((not (car b)) (cset-difference a (cdr b)))
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (apply
               (lambda (left1 left2 same right1 right2)
                 (let* ((a (append (take-up-to a ls) (cdr ls)))
                        (a (if left1 (cons left1 a) a))
                        (a (if left2 (cons left2 a) a))
                        (b (if right1 (cset-union b (list right1)) b))
                        (b (if right2 (cset-union b (list right2)) b)))
                   (cset-difference a b)))
               (intersect-char-ranges (cset-range (car ls))
                                      (cset-range (car b))))))
        (else (cset-difference a (cdr b)))))

(define (cset-intersection a b)
  (let intersect ((a a) (b b) (res '()))
    (cond ((null? b) res)
          ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
           => (lambda (ls)
                (apply
                 (lambda (left1 left2 same right1 right2)
                   (let* ((a (append (take-up-to a ls) (cdr ls)))
                          (a (if left1 (cons left1 a) a))
                          (a (if left2 (cons left2 a) a))
                          (b (if right1 (cset-union b (list right1)) b))
                          (b (if right2 (cset-union b (list right2)) b)))
                     (intersect a b (cset-union res (list same)))))
                 (intersect-char-ranges (cset-range (car ls))
                                        (cset-range (car b))))))
          (else (intersect a (cdr b) res)))))

(define (cset-complement a)
  (cset-difference (sre->cset *all-chars*) a))

(define (cset-case-insensitive a)
  (let lp ((ls a) (res '()))
    (cond ((null? ls) (reverse res))
          ((and (char? (car ls)) (char-alphabetic? (car ls)))
           (let ((c2 (char-altcase (car ls)))
                 (res (cons (car ls) res)))
             (lp (cdr ls) (if (cset-contains? res c2) res (cons c2 res)))))
          ((and (pair? (car ls))
                (char-alphabetic? (caar ls))
                (char-alphabetic? (cdar ls)))
           (lp (cdr ls)
               (cset-union (cset-union res (list (car ls)))
                           (list (cons (char-altcase (caar ls))
                                       (char-altcase (cdar ls)))))))
          (else (lp (cdr ls) (cset-union res (list (car ls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match and replace utilities (currently strings only)

(define (irregex-fold/fast irx kons knil str . o)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (finish (or (and (pair? o) (car o)) (lambda (i acc) acc)))
         (start (if (and (pair? o) (pair? (cdr o))) (cadr o) 0))
         (end (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                  (caddr o)
                  (string-length str))))
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
                (let* ((end (irregex-match-end-index m 0))
                       (acc (kons i m acc)))
                  (irregex-reset-matches! matches)
                  (lp end acc))))))))

(define (irregex-fold irx kons . args)
  (let ((kons2 (lambda (i m acc) (kons i (irregex-copy-matches m) acc))))
    (apply irregex-fold/fast irx kons2 args)))

(define (irregex-fold/chunked/fast irx kons knil cnk start . o)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (finish (or (and (pair? o) (car o)) (lambda (src i acc) acc)))
         (i (if (and (pair? o) (pair? (cdr o)))
                (cadr o)
                ((chunker-get-start cnk) start))))
    (irregex-match-chunker-set! matches cnk)
    (let lp ((start start) (i i) (acc knil))
      (if (not start)
          (finish start i acc)
          (let ((m (irregex-search/matches irx cnk start i matches)))
            (if (not m)
                (finish start i acc)
                (let* ((acc (kons start i m acc))
                       (end-src (irregex-match-end-chunk m 0))
                       (end-index (irregex-match-end-index m 0)))
                  (irregex-reset-matches! matches)
                  (lp end-src end-index acc))))))))

(define (irregex-fold/chunked irx kons . args)
  (let ((kons2 (lambda (s i m acc) (kons s i (irregex-copy-matches m) acc))))
    (apply irregex-fold/chunked/fast irx kons2 args)))

(define (irregex-replace irx str . o)
  (let ((m (irregex-search irx str)))
    (and
     m
     (string-cat-reverse
      (cons (substring str (irregex-match-end-index m 0) (string-length str))
            (append (irregex-apply-match m o)
                    (list (substring str 0 (irregex-match-start-index m 0)))
                    ))))))

(define (irregex-replace/all irx str . o)
  (irregex-fold/fast
   irx
   (lambda (i m acc)
     (let ((m-start (irregex-match-start-index m 0)))
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
                 (cons (substring (car (irregex-match-start-chunk m 0))
                                  0
                                  (irregex-match-start-index m 0))
                       res)))
            ((post)
             (let ((str (car (irregex-match-start-chunk m 0))))
               (lp (cdr ls)
                   (cons (substring str
                                    (irregex-match-end-index m 0)
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
  (apply irregex-fold/fast
         irx
         (lambda (i m a) (cons (irregex-match-substring m) a))
         (lambda (i a) (reverse a))
         o))

#;(define (irregex-split irx str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (irregex-fold/fast
     irx
     (lambda (i m a)
       (if (= i (irregex-match-start-index m 0))
           a
           (cons (substring str i (irregex-match-start-index m 0)) a)))
     (lambda (i a) ;<= MOSH: ?
       (reverse
        (if (= end (irregex-match-end-index m 0))
            a
            (cons (substring str (irregex-match-end-index m 0) end) a))))
     start
     end)))
)
