#!/usr/bin/env gosh
(use file.util)

(define laml-dir (string-append (current-directory) "/tools/schemedoc/"))

(define (main args)
  (load (string-append laml-dir "laml-init.scm"))
  (laml-style "xml-in-laml/schemedoc-2-for-monar/schemedoc")
  (manual
   (manual-front-matters
    (manual-title "Mosh User's Reference")
    (manual-author "Higepon")
    (manual-abstract (div (p "This is a reference manual of " (a 'href "http://code.google.com/p/mosh-scheme/" "Mosh") ", an R6RS Scheme implementation.")
                          (p "This manual is for version 0.0.6.")
                          ))
    'css-prestylesheet "normal"
    'css-stylesheet "monar"
    'laml-resource "false"
    'scheme-source-linking "false"
    'source-destination-delta "")

   (manual-from-scheme-files
    (manual-from-scheme-file 'src (string-append (sys-getenv "HOME") "/mosh/library.scm"))
    (manual-from-scheme-file 'src (string-append (sys-getenv "HOME") "/mosh/manual.scm"))
    ))
  (end-laml)
  0)
;  (sys-system "firefox /tmp/monar/manual.html"))

