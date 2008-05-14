; LAML processing options:
(set! xml-validate-contents? #t)  
(set! xml-check-attributes? #t)
(set! xml-check-language-overlap? #t)
(set! xml-error-truncation-length 130)
(set-xml-transliterate-character-data-in current-xml-language #t) 
(set-xml-char-transformation-table-in current-xml-language html-char-transformation-table) 
(set-xml-accept-only-string-valued-attributes-in current-xml-language #t)
(set-xml-accept-extended-contents-in  current-xml-language #f)
(define xml-check-error display-xml-warning)  ; display-xml-warning or laml-error
(set! xml-link-checking 'relative-urls) ; none, relative-urls, absolute-urls, all
