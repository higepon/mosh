#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (yuni lib ssax private misc)
  (export
    inc dec
    ascii->char ucscode->char
    char-return char-tab char-newline
    call-with-input-string)
  (import
    (rnrs))
  
  (define (inc n) (+ n 1))  
  (define (dec n) (- n 1))
  
  (define ascii->char integer->char)
  (define ucscode->char ascii->char)
  (define char-return (ascii->char 13))
  (define char-tab (ascii->char 9))
  (define char-newline (ascii->char 10))
  
  (define (call-with-input-string str proc)
    (call-with-port (open-string-input-port str) proc))  
)
