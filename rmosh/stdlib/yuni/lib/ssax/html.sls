#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (yuni lib ssax html)
  (export
    SXML->HTML
    string->goodHTML
    enattr
    entag   
    
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules)
  (import
    (yuni lib ssax private to-html)
    (yuni lib ssax private to-html-ext))
)
