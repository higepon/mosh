(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/schemedoc-2/schemedoc")

(manual
  (manual-front-matters
     'documentation-commenting-style "DOC-COM-STYLE"
     'manual-destination-name "MAN-NAME"
  )

  (manual-from-scheme-file 'src "SCHEME-SOURCE-FILE-NAME")
)