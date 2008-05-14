(load (string-append laml-dir "laml.scm"))
(load (string-append laml-dir "tools/testsuite-documentation/testsuite-documentation.scm"))

(begin-laml)
  (document-testsuite "FULL-PATH-TO-TESTCASES")
(end-laml)
