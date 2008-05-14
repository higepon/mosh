(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/schemedoc-index/schemedoc-index")

(manual-index
  (manual-index-front-matters
    (manual-index-title "")
    'browser-type "two-steps"        ; one-step or two-steps
    'left-frame-width "250px"
    'top-frame-height "200px"
    'initial-manual-frame "info"
    'source-destination-delta "html/"
    'scheme-report "include"         ; none, include, merge
  )

  (manual-index-contributions

    (manual-index-contribution 
       'informative-name ""          ; a name of your choice
       'path ""                      ; relative path from the current dir to the relevant manlsp file 
                                     ; (with or without the manlsp extension)
    )

  )
)

(end-laml)