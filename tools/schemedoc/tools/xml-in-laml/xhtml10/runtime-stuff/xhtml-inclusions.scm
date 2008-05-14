; The XHTML URL extractor function
; Returns a list of URLs, among which there may be boolean values (#f).
(define (xhtml-url-extractor ast) 
 (traverse-and-collect-all-from-ast
   ast

  (lambda (ast-node)
    (member (ast-element-name ast-node) (list "a" "img" "link" "area" "frame" "iframe")))

  (lambda (node-with-linking)
    (cond ((member (ast-element-name node-with-linking) (list "a" "link" "area"))
             (ast-attribute node-with-linking 'href #f))
          ((member (ast-element-name node-with-linking) (list "img" "input" "script" "frame" "iframe"))
             (ast-attribute node-with-linking 'src #f))
          (else #f)))))

; The XHTML base URL extract function.
; Extracts the href attribute from base element in the head, of #f.
; Finds base fast, because it is guided by XML navigation information.
; Returns #f if no base href is there.
(define (xhtml-base-url-extractor ast)
 (let ((base-ast (find-first-ast ast "base")))
   (if base-ast
       (let ((candidate-url (ast-attribute base-ast 'href #f)))
         (if candidate-url 
             candidate-url
             #f))
       #f)))  

; Setting the URL extractor functions of the XHTML languages.
(set-xml-link-checking-functions 'xhtml10-strict xhtml-url-extractor xhtml-base-url-extractor)
(set-xml-link-checking-functions 'xhtml10-transitional xhtml-url-extractor xhtml-base-url-extractor)
(set-xml-link-checking-functions 'xhtml10-frameset xhtml-url-extractor xhtml-base-url-extractor) 