(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/program-dissection/program-dissection")
(load (in-startup-directory "common.scm"))

(program-dissection
  'author-home-url "http://www.cs.auc.dk/~normark/"
  'course-home-url "http://www.cs.auc.dk/~normark/c-prog-03/html/programmering-i-c.html"
  'relative-source-destination-path "../../html/programs/"

  (title "Exercise 2.2 Page 115")

  (source-program 'src (basis-file "kn-c/exercises/"))

  (dissection-section 
    (source-program-extract 'from-line "" 'to-line "")
    (div  ))

  (dissection-section 
    (source-program-extract 'from-line "" 'to-line "")
    (div  ))

  (dissection-section 
    (source-program-extract 'from-line "" 'to-line "")
    (div  ))

  (dissection-section 
    (source-program-extract 'from-line "" 'to-line "")
    (div  ))


 
)
