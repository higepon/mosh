(load (string-append laml-dir "laml.scm"))
(laml-style "simple-xhtml1.0-strict-validating")
; (lib-load "xhtml1.0-convenience.scm")

(begin-laml)

(define current-xml-language 'xhtml10-strict)
(define laml-generation-meta (meta 'name "Generator" 'content "LAML"))
(define meta-props (list 'http-equiv "Content-Type" 'content "text/html; charset=iso-8859-1"))
(define html-props (list 'xmlns "http://www.w3.org/1999/xhtml"))

; Insert the LAML template "Processing Options" here
; if you need variations in the LAML processing

(write-html '(raw prolog)
 (html html-props
  (head 
   (meta meta-props) laml-generation-meta
   (title "TITLE"))
  (body 
    "BODY")
 )
)


(end-laml)