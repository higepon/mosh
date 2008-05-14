(load (string-append laml-dir "laml.scm"))
(laml-style "simple-html4.01-transitional-validating")

(define meta-props (list 'http-equiv "Content-Type" 'content "text/html; charset=iso-8859-1"))
(define body-props (list 'bgcolor (rgb-color-list white) 'text (rgb-color-list black)
                         'link (rgb-color-list blue) 'vlink (rgb-color-list purple)))

(define validate-html? #t)
(define check-html-attributes? #t)
(define transliterate-character-data? #t)


(write-html '(raw prolog)
 (html 
  (head 
   (meta meta-props) (title "TITLE"))
  (body body-props
    "BODY")
 )
)


(end-laml)