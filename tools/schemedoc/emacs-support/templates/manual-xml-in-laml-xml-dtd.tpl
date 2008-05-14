(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/schemedoc-2/schemedoc")

(manual
  (manual-front-matters
    (manual-title "")
    (manual-author "")
    (manual-affiliation "")
    (manual-abstract (div ""))

    'laml-resource "false"
    'css-prestylesheet "compact"
    'css-stylesheet "original"
    'css-stylesheet-copying "true"
    'make-dtd-manual-template "false" 

  )

  (manual-from-xml-dtd 'src "")

)