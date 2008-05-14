#!/usr/bin/env gosh
(use file.util)

(define laml-dir (string-append (current-directory) "/tools/schemedoc/"))

(define (main args)
  (load (string-append laml-dir "laml-init.scm"))
;  (laml-style "xml-in-laml/schemedoc-index/schemedoc-index")
  (laml-style "xml-in-laml/schemedoc-index-isearch/schemedoc-index")
  (manual-index
   (manual-index-front-matters
    (manual-index-title "Monar Manual Index")
    'browser-type "one-step"     ; one-step or two-steps
    'left-frame-width "250px"
    'top-frame-height "200px"
    'initial-manual-frame "first-manual-contribution"
    'source-destination-delta ""
    'scheme-report "none"       ; none, include, merge
    )

   (manual-index-contributions
    (manual-index-contribution
     'informative-name ""
     'path "manual")
    )
   )

  (end-laml)
  (sys-system (string-append "firefox " (current-directory) "/doc/top.html"))
  0
  )

