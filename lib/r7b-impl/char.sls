#!r6rs
(library (r7b-impl char)
         (export

;; from R7RS draft 4
char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
char-upper-case?  char-whitespace? digit-value string-ci<=? string-ci<?
string-ci=?  string-ci>=? string-ci>? string-downcase string-foldcase
string-upcase
   )
         (import (rnrs) (only (mosh) format))
(define (digit-value char)
  (and (char-numeric? char)
       (case char
         ((#\0) 0)
         ((#\1) 1)
         ((#\2) 2)
         ((#\3) 3)
         ((#\4) 4)
         ((#\5) 5)
         ((#\6) 6)
         ((#\7) 7)
         ((#\8) 8)
         ((#\9) 9)
         ((#\x0664) 4)
         ((#\x0AE6) 0)
         (else (assertion-violation 'digit-value (format #f "unsupported char ~a" char))))))
)
