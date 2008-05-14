(load (string-append laml-dir "laml.scm"))       
(laml-style "xml-in-laml/lecture-notes/lecture-notes")

(leno-front-matters
  (front-title "")           
  (front-subtitle "")   
  (front-author "")           
  (front-affiliation "")  
  (front-abstract "")  

  'slide-view "true"  
  'annotated-slide-view "true"
  'aggregated-view "true" 
  'primary-view "slide-view"

  'scheme-suffix "post-notes.scm"

  'course-home-url ""   
  'author-home-url ""   
  'note-download-url ""   
  'logo-url "http://www.auc.dk/"  
  'note-contents-description ""    
  'slide-header "normal"  ; minimal/normal/none 
  'trail-of-lecture "false"  
  'language "english"      

  'show-and-speak "false" 
  'default-showing-time "2"    
  'additional-showing-time "0" 
  'sound-source "wave-file" 
  'speak-file-prefix "sound/"
  'speak-url-prefix "../sound/"

  'exercise-model "none"   
  'quiz-support "false"   

  'word-index "true"  

  'news-flash-string  ""  
  'news-flash-level   "2"

  'verbosity-level "1"
  'clean-html-directory "false"

  'css-prestylesheet "normal-size" 
  'css-stylesheet "original"
)

(begin-notes)





(end-notes)


