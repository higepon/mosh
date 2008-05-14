(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/schemedoc-2/schemedoc")

(manual
  (manual-front-matters
    (manual-title "")                                    ; title of this manual
    (manual-author "")                                   ; the author name(s)
    (manual-affiliation "")                              ; Company, school etc.
    (manual-abstract                                     ; A short abstract of this manual
      (div "")
    )

;    (scheme-source-linking-manual                       ; Allows for linking to other manuals 
;      'key ""                                           ; when the attribute scheme-source-linking is "true".
;      'file-path "")                                    ; file-path: Relative path to manlsp file.

    'laml-resource "false"            
    'css-prestylesheet "compact"                         ; compact, normal
    'css-stylesheet "original"                           ; original, fancy
    'css-stylesheet-copying "true"                       
    'documentation-commenting-style "multi-semicolon"    ; multi-semicolon, documentation-mark
    'keep-syntactical-comment-file "false"
    'verbosity-level "0"                                 ; 0 or 1. 
    'source-destination-delta ""                         ; a relative file path ended in '/'. SchemeDoc does NOT make the directories.
;    'manual-destination-name "your-own-name"            ; the name of the HTML target file. 

    'scheme-source-linking "true"
    'scheme-report-version  "r5rs"                       ; r5rs, r4rs
;    'rnrs-url "URL-to-local-r5rs-dir"                   ; replace with your own url, for instance file://...
  )

  (manual-from-scheme-file 'src "")                      ; absolute or relative path to Scheme source file, 
                                                         ; including a possible file extension
)