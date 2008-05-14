; The elucidator laml setup file
; Change the capital names

; Load the styles file, hereby defining style function

(load (string-append laml-dir "laml.scm"))

(style "elucidator/elucidator")

; Set the directory in which this file resides.
; The directory ends in a '/'

(set-source-directory (startup-directory))

; Set the name of this file, without extension
(set-documentation-name "ELUCIDATOR-NAME")

; Set the level of processing. With commenting, process all source files.
; (process-only)

; (make-no-indexes)
(make-all-indexes)

(define make-large-source-files? #f) 
(define toc-columns-detail 3)  
(define present-hidden-ids? #t)
(define underline-program-links #f)
(define underline-documentation-links #f)
(define show-sectional-comment-name #f)

; The RELATIVE (back) path to the r4rs directory, which is located in the root of the LAML distribution.
(define rs4r-url-prefix "")

; Do you want a menu like selection of programs?
(define separate-program-menu? #f)

(define (laml-source-file f)
  (string-append laml-dir f))

(define elucidator-color-scheme 
  (make-color-scheme 
    "program" program-background-color-1
  ))


; Define the sourcefiles in this documentation bundle
(program-source
 (key "SOURCE-KEY")
 (file-location "SOURCE-PATH")
 (language "scheme")
 (group "program")
)

; Define the documentation body, here in terms of a documentation-from clause
(begin-documentation)

  (documentation-from "DOCUMENTATION-FILE-NAME")  ; the name of the file can be changed 

(end-documentation)
