; The second generation of SchemeDoc, based on the XML-in-LAML conventions. 
; Processes an XML-in-LAML manual file and generates an XHTML manual with CSS styling.
; This software is composed by a front-end part and a backend part.
; The front-end part is responsible for the XML-in-LAML level.
; The back-end part is responsible for XHTML presentation of the manual. 
; Of historical reasons, the front-end and the back-end are relatively isolated from each other.

; .schemedoc-dependencies "../../../lib/xml-in-laml/man/xml-in-laml"


; ---------------------------------------------------------------------------------------------------
; Constants

; The directory which holds the SchemeDoc software.
; (The directory that contains this source file)
(define schemedoc-software-directory (string-append laml-dir "styles/xml-in-laml/schemedoc-2/"))

; The directory which holds a possible XML-in-LAML source script.
(define manual-source-directory (startup-directory))

(define manual-scheme-documentation-commenting-style #f) ; assigned later

; Do we want the SchemeDoc manual style to display information while it processes a manual.
; 0 means minimal displayed information during processing. 1 implies that somewhat more info is being displayed. 
(define schemedoc-verbosity-level 0)

; The URL of SchemeDoc as such:
(define schemedoc-url "http://www.cs.aau.dk/~normark/schemedoc/index.html")


; ---------------------------------------------------------------------------------------------------
; Loading Section.

; XHTML mirror:
; if conservative-xhtml-loading do not unnecessarily reload xhtml10-transitional.
(if (eq? laml-load-variation 'conservative-xhtml-loading)
    (begin
      (if (not (memq 'xhtml10-transitional (languages-in-use)))
          (begin
            (lib-load "xml-in-laml/xml-in-laml.scm")
            (lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")))

    )   
    (begin
      (lib-load "xml-in-laml/xml-in-laml.scm")
      (lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
    )
)

; XHTML convenience:
(lib-load "xhtml1.0-convenience.scm")
(load (string-append laml-dir "styles/xml-in-laml/schemedoc-2/mirror/schemedoc2-mirror.scm"))

; Some basic LAML libraries:
(lib-load "color.scm")
(lib-load "time.scm")
(lib-load "file-read.scm")
(lib-load "testcase-access.scm")

; The SchemeDoc extraction tool:
(load (string-append laml-dir "tools/schemedoc-extractor/schemedoc-extractor.scm"))
(set! delete-comment-file? #t)

(load (string-append laml-dir "tools/dtd-parser/element-attribute-access.scm"))


; ===============================================================================================================
; SchemeDoc Frontend stuff. 
; ===============================================================================================================

; Action procedure:

(define (manual! manual-ast)
 (manual-briding! manual-ast) 
)

; ---------------------------------------------------------------------------------------------------
; Global variables - both assigned later (if needed) to the list of parsed XML elements and the list of
; parsed XML attributes.

(define element-list '())  
(define attribute-list '())

; Assigned to front matters attribute
(define manual-source-destination-delta-attr #f)
(define mirror-info-path #f)

; The name of a function from element-name/function-name (titles) to URLs.
; Intended to map title names to URLs that provided additional information about the name.
; A symbol, or #f.
(define schemedoc-element-cross-reference-url-fn #f)

(define schemedoc-element-cross-reference-anchor-text "Additional resources")

(define xml-protected-descriptions? #t)

; ---------------------------------------------------------------------------------------------------

(define (type-of-manual manual-ast)
  (let ((manual-page-list (find-asts manual-ast "manual-page"))
        (manual-section-list (find-asts manual-ast "manual-section")))
    (cond ((or (find-first-ast manual-ast "manual-from-scheme-files")
               (find-first-ast manual-ast "manual-from-scheme-file"))     'manual-from-scheme-file)
          ((and (find-first-ast manual-ast "manual-from-xml-dtd")
                (null? manual-page-list) (null? manual-section-list))     'manual-from-xml-dtd)
          ((find-first-ast manual-ast "merged-manual-from-xml-dtd")       'merged-manual-from-xml-dtd)
          (else                                                           'manual-pages-sections-only))))

; Reset the state of SchemeDoc
(define (reset-schemedoc!)
  (reset-schemedoc-extractor!)
  (set! delete-comment-file? #t)
  (set! manual-scheme-documentation-commenting-style #f)
  (set! element-list '())  
  (set! attribute-list '())
  (set! manual-source-destination-delta-attr #f)

   (let ((manual-html-char-transformation-table (list->vector (make-list 256 #t))))

; eliminates July 13, 2006:
;     (set-html-char-transformation-entry! manual-html-char-transformation-table (char->integer #\<) "&lt;")    ; show '<' as is.   Allows for raw HTML
;     (set-html-char-transformation-entry! manual-html-char-transformation-table (char->integer #\>) "&gt;")    ; show '>' as is.   Allows for raw HTML
 
     (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\") "&quot;")
     (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\') "&#39;")
     (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\&) "&amp;")
 
     ; Assign the xhtml10-transitional table to the newly made manual-html-char-transformation-table:
     (set-xml-char-transformation-table-in 'xhtml10-transitional manual-html-char-transformation-table))

  ; In order to preserve white space in textual pre elements from manual entries and manual section entries:
  (set! xml-always-render-white-space? #t)

  (set! max-section-number #f) ; assigned by manual-page
  (set! next-section-number 0)

  ; The absolute destination path to the directory with HTML files etc. 
  ; Re-initialized by make-manual (for historical reasons).
  (set! the-manual-destination-path (startup-directory))

  ; The kind of sorting applied on attributes.
  ; Either as-in-dtd (default) or by-attribute-name
  (set! attribute-sorting-kind 'as-in-dtd) 

  ; A boolean variable that controls the creation of links to HTML, elucidative programming style source programs   
  (set! link-to-ep-source-program? #f)

  ; Both assigned from front matters attribtues in manual-briding! 
  (set! manual-front-matters-scheme-report-version-attr #f)
  (set! manual-front-matters-rnrs-url-attr #f)

  ; Corresponds to the Elucidator manual source list. Only used if the variable link-to-ep-source-program? is true.
  (set! scheme-source-linking-manual-list '())

  (set! manual-meta-information '())

  (set! css-stylesheet-schema 'laml)

  ; Copy a css stylesheet from the stylesheet directory of the manual software source directory
  ; to the location determined by the variable css-stylesheet-schema. 
  ; If #f, no copying is done.
  (set! copy-css-stylesheet? #t)

  ; The name of the css stylesheet, as present in the source stylesheets directory. String without file file extension.
  ; If copy-css-stylesheet?, this stylesheet is copied to the target stylesheet location.
  (set! the-manual-stylesheet "original")

  (set! laml-source-file-name-without-extension (source-filename-without-extension))

  ; The explanation of attributes, for which no attribute-description is available
  (set! standard-attribute-explanation "-")

  (set! the-manual-title "")
  (set! the-manual-author "")
  (set! the-manual-affiliation "")
  (set! the-manual-abstract "")

  (set! home-url #f)
  (set! manual-master-index #f)

  (set! manual-name (if (and laml-source-file-name-without-extension (not (equal? laml-source-file-name-without-extension "")))
                        laml-source-file-name-without-extension
                        "manual"))


  (set! manual-page-list '()) ; the list in which we collect manual page descriptions

  (set! manual-background white)

  (set! language-preference 'english) ; redefinition

;  (set! laml-manual-stuff #t)   ; Do not re-assign. Always initialized by action procedure of front-matters.


  ; The name of the pre css stylesheet which is put in front of the the-manual-stylesheet.
  (set! the-manual-prestylesheet "compact")

  (set! explicit-section-id "")
  (set! end-remark "")
  (set! next-section-number 0)
 
  (set! parsed-dtd-elements #f)
)

(define (manual-briding! manual-ast)
  (reset-schemedoc!)
  (let* ((man-title (find-first-ast manual-ast "manual-title" ast-subtrees))
         (manual-aut (find-first-ast manual-ast "manual-author" ast-subtrees))
         (manual-affil (find-first-ast manual-ast "manual-affiliation" ast-subtrees))
         (manual-abstr (find-first-ast manual-ast "manual-abstract" ast-subtrees))

         (laml-resource (as-boolean (unique-ast-attribute manual-ast 'laml-resource "false")))

         (manual-type (type-of-manual manual-ast))
         (attribute-explanation-default (unique-ast-attribute manual-ast 'default-attribute-explanation "-"))
         (documentation-commenting-style-attr
            (as-symbol (unique-ast-attribute manual-ast 'documentation-commenting-style "multi-semicolon")))
         (keep-syntactical-comment-file-attr (as-boolean (unique-ast-attribute manual-ast 'keep-syntactical-comment-file "false")))
         (css-stylesheet (unique-ast-attribute manual-ast 'css-stylesheet #f))
         (css-prestylesheet (unique-ast-attribute manual-ast 'css-prestylesheet #f))
         (css-copying? 
           (as-boolean 
            (unique-ast-attribute manual-ast 'css-stylesheet-copying
             (if laml-resource #f #t))))
         (manual-destination-name-attr (unique-ast-attribute manual-ast 'manual-destination-name #f))
         (attribute-sorting-attr (as-symbol (unique-ast-attribute manual-ast 'attribute-sorting "as-in-dtd")))

         (scheme-source-linking-attr (as-boolean (unique-ast-attribute manual-ast 'scheme-source-linking "false")))
         (scheme-report-version-attr (as-symbol (unique-ast-attribute manual-ast 'scheme-report-version "r5rs")))
         (rnrs-url-attr (if (or (eq? 'r4rs scheme-report-version-attr) (eq? 'r5rs scheme-report-version-attr))
			    (unique-ast-attribute manual-ast 'rnrs-url 
					   (string-append "http://www.cs.aau.dk/~normark/scheme/distribution/laml/"
							  (as-string scheme-report-version-attr)"/"))
			    #f))
         (scheme-source-linking-manual (find-asts manual-ast "scheme-source-linking-manual"))
         (source-destination-delta-attr (unique-ast-attribute manual-ast "source-destination-delta" ""))

         (verbosity-level-attr (as-number (unique-ast-attribute manual-ast "verbosity-level" "0")))

         (element-cross-reference-url-fn (unique-ast-attribute manual-ast "element-cross-reference-url-fn" #f))
         (element-cross-reference-anchor-text (unique-ast-attribute manual-ast "element-cross-reference-anchor-text" "Additional resources"))
         (xml-protected-descriptions-attr (as-boolean (unique-ast-attribute manual-ast "xml-protected-descriptions" #t)))
        )

    (display-message "LAML SchemeDoc")

    (set! schemedoc-verbosity-level verbosity-level-attr)
    (set! schemedoc-element-cross-reference-url-fn (if element-cross-reference-url-fn (as-symbol element-cross-reference-url-fn) #f))
    (set! schemedoc-element-cross-reference-anchor-text element-cross-reference-anchor-text)
    (set! manual-source-destination-delta-attr source-destination-delta-attr)

    (set! the-manual-title (if man-title man-title ""))
    (set! the-manual-abstract (if manual-abstr manual-abstr ""))
    (set! the-manual-author (if manual-aut manual-aut ""))
    (set! the-manual-affiliation (if manual-affil manual-affil ""))


;     (cond ((and manual-aut manual-affil)
;              (set! the-manual-author (append manual-aut manual-affil)))
;           (manual-aut
;              (set! the-manual-author manual-aut))
;           (manual-affil
;              (set! the-manual-author manual-affil))
;           (else (set! the-manual-author '())))

    ; Assignment of schemedoc extracting variable: defined in tools/schemedoc/schemedoc.scm
    (set! scheme-documentation-commenting-style documentation-commenting-style-attr)
    (set! manual-scheme-documentation-commenting-style documentation-commenting-style-attr) ; also set a manual variable

    ; some of the assignments below may be redone in manual-briding-scheme-source!
    ; based on values extracted from the documetation abstract in the Scheme source file.
    (set! laml-manual-stuff laml-resource)
    (set! standard-attribute-explanation attribute-explanation-default)
    (if css-stylesheet (set! the-manual-stylesheet css-stylesheet))
    (if css-prestylesheet (set! the-manual-prestylesheet css-prestylesheet))
    (set! copy-css-stylesheet? css-copying?)
    (set! css-stylesheet-schema (if laml-manual-stuff 'laml 'local))
    (if manual-destination-name-attr (set! manual-name manual-destination-name-attr)) ; instead of laml source file name
    (set! attribute-sorting-kind attribute-sorting-attr)
    (set! link-to-ep-source-program? scheme-source-linking-attr)
    (set! manual-front-matters-scheme-report-version-attr scheme-report-version-attr)
    (set! manual-front-matters-rnrs-url-attr rnrs-url-attr)    
    (set! xml-protected-descriptions? xml-protected-descriptions-attr)

    (set! scheme-source-linking-manual-list
       (map
         (lambda (linking-manual-entry) 
            (scheme-source-linking-manual-transform linking-manual-entry source-destination-delta-attr))
          scheme-source-linking-manual))

    ; Handle all manual-type specific data:
    (cond ((eq? manual-type 'manual-pages-sections-only)
             (manual-briding-standard! manual-ast))
          ((eq? manual-type 'manual-from-scheme-file)
             (manual-briding-scheme-source! manual-ast))
          ((eq? manual-type 'manual-from-xml-dtd)
             (manual-briding-xml-dtd! manual-ast))
          ((eq? manual-type 'merged-manual-from-xml-dtd)
             (manual-briding-merged-xml-dtd! manual-ast))
          (else (laml-error "manual-briding!: Unknown kind of manual type" manual-type)))

  )

  (end-laml)
  (display-message "")
)

; Transformation of EP-related manual source AST.
(define (scheme-source-linking-manual-transform manual-source-ast this-manual-source-destination-delta)
 (let* ((key (ast-attribute manual-source-ast 'key "no-key")) 
        (file-path (ast-attribute manual-source-ast 'file-path))
        (url (ast-attribute manual-source-ast 'url)) ; NOT used
        (referred-manual-source-destination-delta (get-manual-source-destination-delta file-path manual-source-directory))
        (url-derived 
         (cond ((absolute-file-path? file-path) 
                    (laml-error "scheme-source-linking-manual-transform: Absolute path not allowed" file-path))
               (else (begin
                       (string-append 
                        "../"  ; out of -ep directory  OK 
                        (inverse-return-path this-manual-source-destination-delta manual-source-directory) ; OK 
                        (file-name-initial-path file-path)  ; OK  
                        referred-manual-source-destination-delta ; OK
                        (file-name-proper file-path) "." "html"))  ; OK 
         )))
       )
    (list (list 'key key) (list 'file-location (abs-path file-path)) (list 'url-location url-derived)
          (list 'friendly-name (ast-text manual-source-ast))
         )))

; Return the source-destination-delta attribute of the manual given by manual-file-path.
; If manual-file-path is relative, it is relative to ep-source-directory (which is the absolute path of the EP source directory).
; manual-file-path is the value of the file-path attribute.
; source-directory is the absolute directory of this manual.
(define (get-manual-source-destination-delta manual-file-path source-directory)
  (let* ((abs-manual-path
           (if (absolute-file-path? manual-file-path)
               manual-file-path
               (string-append source-directory manual-file-path)))
         (abs-manual-path-1 (string-append (file-name-initial-path abs-manual-path) (file-name-proper abs-manual-path) "." "manlsp")) 
         (manlsp-structure (if (file-exists? abs-manual-path-1) (file-read abs-manual-path-1) '()))
         (meta-alist (if (null? manlsp-structure) '() (car manlsp-structure)))
         (sdd (defaulted-get 'source-destination-delta meta-alist 'not-provided))
        )
    (if (eq? sdd 'not-provided)
        ""
        sdd)))



; make an absolute path out of path
(define abs-path   
 (lambda (path)
   (if (absolute-file-path? path) path (in-startup-directory path))))

(define (perhaps-abs-url url)
 (if (absolute-url? url)
     url
     (string-append "../" url) ; arrange that the resulting url is relative to the HTML directory of manual 
))

    

; Manual sections and manual pages only.
(define (manual-briding-standard! manual-ast)

  ; Adjust the HTML transformation table. There will be no RAW HTML to take care of.
  (let ((manual-html-char-transformation-table (list->vector (make-list 256 #t))))
    (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\') "&#39;")
    (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\&) "&amp;")

    ; Assign the xhtml10-transitional table to the newly made manual-html-char-transformation-table:
    (set-xml-char-transformation-table-in 'xhtml10-transitional manual-html-char-transformation-table))

  (let ((manual-destination-path (get-manual-destination-path manual-ast))
        (doc-list
           (traverse-and-collect-all-from-ast 
              manual-ast
              (lambda (ast) (or (equal? "manual-page" (ast-element-name ast))
                                (equal? "manual-section" (ast-element-name ast))))
              transform-page-or-section)))
    (set! the-manual-destination-path manual-destination-path)
    (set! manual-meta-information 
          (make-manual-meta-information
             manual-ast
             (ast-text-deep (find-first-ast manual-ast "manual-title"))
             (ast-text-deep (find-first-ast manual-ast "manual-author"))
             (ast-text-deep (find-first-ast manual-ast "manual-affiliation"))
             (ast-text-deep (find-first-ast manual-ast "manual-abstract"))))
    (make-manual doc-list 'manual-pages-sections-only  manual-destination-path "")  

   )              
)

; Return the absolute destination path, taken from the front matters attribute source-destination-delta.
(define (get-manual-destination-path manual-ast)
 (let ((rel-path (unique-ast-attribute manual-ast "source-destination-delta" #f)))
  (cond (rel-path (string-append (startup-directory) rel-path))
        (else (startup-directory)))))

(define (transform-page-or-section page-or-section-ast)
 (cond ((equal? "manual-page" (ast-element-name page-or-section-ast)) 
            (transform-manual-page page-or-section-ast))
       ((equal? "manual-section" (ast-element-name page-or-section-ast)) 
            (transform-manual-section  page-or-section-ast))
       (else (laml-error "transform-page-or-section: Expects AST of type manual-page or manual-section. Got:" 
                         (ast-element-name page-or-section-ast)))))

(define (transform-manual-page page-ast)
  (let* ((name-attr (ast-attribute page-ast 'name))
         (page-subclause-list (filter ast? (ast-subtrees page-ast)))
         (external-reference-clauses (filter (ast-of-type? 'element-name "external-reference") page-subclause-list))
         (internal-references-clauses (filter (ast-of-type? 'element-name "internal-references") page-subclause-list))
         (other-clauses 
           (filter 
             (lambda (ast) (and (not (equal? (ast-element-name ast) "external-reference")) 
                           (not (equal? (ast-element-name ast) "internal-references"))))
             page-subclause-list))

         (transformed-clauses
           (append 
              (map clause-transform other-clauses) 
              (list 
                (cons 'cross-references 
                      (append (map clause-transform external-reference-clauses)
                              (map clause-transform internal-references-clauses))))))
        )
    (cons (list 'kind "manual-page") (cons (list 'title name-attr) transformed-clauses))
  )
)


(define (transform-manual-section section-ast)
  (let ((section-id (ast-attribute section-ast 'id #f))
        (section-subclause-list (filter ast? (ast-subtrees section-ast))))
    (if section-id
        (cons (list 'section-id section-id)
              (cons (list 'kind "manual-section")
                    (map clause-transform section-subclause-list)))
        (cons (list 'kind "manual-section")
          (map clause-transform section-subclause-list)))))

(define (clause-transform page-clause)
 (let ((transformer (find-transformer (as-symbol (ast-element-name page-clause)))))
    (transformer page-clause)))

(define (find-transformer name)
  (cond 
        ((eq? 'section-title name) section-title-transformer)
        ((eq? 'section-body name) section-body-transformer)
        ((eq? 'title name) title-transformer)
        ((eq? 'form name) form-transformer)
        ((eq? 'description name) description-transformer)
        ((eq? 'example-form name) example-form-transformer)
        ((eq? 'precondition name) precondition-transformer)
        ((eq? 'postcondition name) postcondition-transformer)
        ((eq? 'parameters name) parameters-transformer)
        ((eq? 'examples name) examples-transformer)
        ((eq? 'external-reference name) external-reference-transformer)
        ((eq? 'internal-references name) internal-references-transformer)
        ((eq? 'comment name) comment-transformer)
        ((eq? 'misc name) misc-transformer)
        ((eq? 'returns name) returns-transformer)
        ((eq? 'attributes name) attributes-transformer)
        ((eq? 'attribute-descriptions name) attribute-descriptions-transformer)
        (else (laml-error "find-transformer: Cannot locate transformation function for element of name" name))))

(define (section-title-transformer ast)
  (list 'section-title (aggregated-contents ast)))  ; abstraher igen

(define (section-body-transformer ast)
  (list 'section-body (aggregated-contents ast)))

(define (title-transformer ast)
  (list 'title (aggregated-contents ast)))

(define (form-transformer ast)
  (list 'form (aggregated-contents ast)))

(define (description-transformer ast)
  (list 'description (aggregated-contents ast)))

(define (example-form-transformer ast)
  (list 'xml-in-laml-example-form (aggregated-contents ast)))

(define (precondition-transformer ast)
  (list 'pre-condition (aggregated-contents ast)))

(define (postcondition-transformer ast)
  (list 'post-condition (aggregated-contents ast)))

(define (parameters-transformer parameters-ast)
  (cons 'parameters
        (traverse-and-collect-all-from-ast 
             parameters-ast
             (ast-of-type? 'element-name "parameter")
             (lambda (par-ast) (list 'parameter (ast-attribute par-ast 'name) (aggregated-contents par-ast))))))

(define (examples-transformer examples-ast)
  (cons 'examples
        (traverse-and-collect-all-from-ast 
             examples-ast
             (ast-of-type? 'element-name "example")
             (lambda (ex-ast) (list 'example (aggregated-contents ex-ast))))))

(define (example-transformer ast)  ; not used
 (list 'example (aggregated-contents ast)))

(define (external-reference-transformer external-reference-ast)
  (list 'reference
        (ast-attribute external-reference-ast 'category)
        (aggregated-contents external-reference-ast)
        (ast-attribute external-reference-ast 'href)))

(define (internal-references-transformer internal-references-ast)
  (append 
     (list 'internal-references (ast-attribute internal-references-ast 'category))
     (traverse-and-collect-all-from-ast 
        internal-references-ast 
        (ast-of-type? 'element-name "name-ref")
        (lambda (name-ref-ast) (aggregated-contents name-ref-ast)))))

(define (comment-transformer ast)
 (list 'comment (aggregated-contents ast)))

(define (misc-transformer ast)
 (list 'misc (aggregated-contents ast)))

(define (returns-transformer ast)
 (list 'returns (aggregated-contents ast)))

(define (attributes-transformer attributes-ast)
  (cons 'attributes
        (traverse-and-collect-all-from-ast 
             attributes-ast
             (ast-of-type? 'element-name "attribute")
             (lambda (attr-ast) (list 'attribute (ast-attribute attr-ast 'name) 
                                      (ast-attribute attr-ast 'type) (ast-attribute attr-ast 'default-status)
                                      (aggregated-contents attr-ast))))))

(define (attribute-descriptions-transformer attribute-descriptions-ast)
  (cons 'attribute-descriptions
        (traverse-and-collect-all-from-ast 
             attribute-descriptions-ast
             (ast-of-type? 'element-name "attribute-description")
             (lambda (attr-ast) (list 'attribute-description (ast-attribute attr-ast 'name) 
                                      (aggregated-contents attr-ast))))))

; Manual from scheme source only.
(define (manual-briding-scheme-source! manual-ast)

 ; Re-assign the char transformation table: Preserve native HTML markup in documentation from Scheme source:
 (let ((manual-html-char-transformation-table (list->vector (make-list 256 #t))))
    (set-html-char-transformation-entry! manual-html-char-transformation-table (char->integer #\<) #t)   ; do not transform
    (set-html-char-transformation-entry! manual-html-char-transformation-table (char->integer #\>) #t)   ; do not transform

    (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\") #t)  ; do not transform   ; earlier: "&quot;"
    (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\') "&#39;")
    (set-html-char-transformation-entry! manual-html-char-transformation-table  (char->integer #\&) "&amp;")

    ; Assign the xhtml10-transitional table to the newly made manual-html-char-transformation-table:
    (set-xml-char-transformation-table-in 'xhtml10-transitional manual-html-char-transformation-table))

 (let* ((is-empty? (lambda (x) (or (and (string? x) (empty-string? x)) (and (boolean? x) (not x)) (and (list? x) (null? x)))))

        (laml-resource (unique-ast-attribute manual-ast 'laml-resource #f))
        (keep-syntactical-comment-file-attr (unique-ast-attribute manual-ast 'keep-syntactical-comment-file "false"))
        (css-stylesheet (unique-ast-attribute manual-ast 'css-stylesheet #f))
        (css-prestylesheet (unique-ast-attribute manual-ast 'css-prestylesheet #f))
        (css-copying? (unique-ast-attribute manual-ast 'css-stylesheet-copying #f))
        (rel-path (unique-ast-attribute manual-ast "source-destination-delta" #f))
        (source-linking? (unique-ast-attribute manual-ast "scheme-source-linking" #f))  
        (example-repository (unique-ast-attribute manual-ast "example-repository" #f))  
       )

  (set! delete-comment-file? (not (as-boolean keep-syntactical-comment-file-attr)))

  (let* ((manual-from-scheme-file-clauses (find-first-ast manual-ast "manual-from-scheme-files"))
         (manual-from-scheme-file-clause (find-first-ast manual-ast "manual-from-scheme-file"))
         (temp-source-file-path (string-append (laml-temp-file-path) "schemedoc-source-file.scm"))
        
         (final-manual-source-path 
           (if manual-from-scheme-file-clauses   ; multiple source files
	       temp-source-file-path
	       (let ((manual-source-path (ast-attribute manual-from-scheme-file-clause 'src)))
		 (if (absolute-file-path? manual-source-path)
		     manual-source-path
		     (string-append (startup-directory) manual-source-path)))))

         (info-manual-source-path
            (let ((manual-source-path (ast-attribute manual-from-scheme-file-clause 'src)))
	      (if (absolute-file-path? manual-source-path)
		  manual-source-path
		  (string-append (startup-directory) manual-source-path))))
	       
       )
 
   (if manual-from-scheme-file-clauses   ; multiple scheme file sources - create a single temp source in temp dir
       (begin
         (if (file-exists? temp-source-file-path) (delete-file temp-source-file-path))
         (write-text-file
	  (accumulate-right string-append ""
			    (map (lambda  (src-file)
				   (let ((abs-source-path (if (absolute-file-path? src-file)
							      src-file
							      (string-append (startup-directory) src-file))))
				     (read-text-file abs-source-path)))
				 (find-asts manual-from-scheme-file-clauses "manual-from-scheme-file" (lambda (ast) (ast-attribute ast 'src)))))
	  temp-source-file-path)))

   (let* (
          (doc-list 
             (extract-documentation-from-scheme-file
                final-manual-source-path))

          (doc-list-1 
             (if (or link-to-ep-source-program?
                    (and (not source-linking?)   ; no scheme-source-linking attribute in LAML file
                         (as-boolean extracted-scheme-source-linking) ; asked for scheme-source-linking in scheme source file
                    ))
                (let* ((ep-dir-name (string-append manual-name "-" "ep"))
                       (version-number 1)
                       (ep-html-file-name (string-append manual-name "-" (as-string version-number)))
                      )
                   (extend-doc-list-with-source-references doc-list ep-dir-name ep-html-file-name))
                doc-list))

          (doc-list-2 
            (if (and schemedoc-element-cross-reference-url-fn (bound? schemedoc-element-cross-reference-url-fn))
                (extend-doc-list-with-additional-cross-reference doc-list-1 (eval-cur-env schemedoc-element-cross-reference-url-fn))
                doc-list-1))

          (doc-list-3
            (if example-repository
                (let* ((absolute-path-to-testsuite (string-append manual-source-directory example-repository))
                       (examples-from-testsuite (if (file-exists? absolute-path-to-testsuite) (file-read-all absolute-path-to-testsuite) #f)))
                  (if examples-from-testsuite
                      (extend-doc-list-with-examples examples-from-testsuite doc-list-2)
                      doc-list-2))
                doc-list-2))

          (manual-destination-path 
             (normalize-file-path 
                (if rel-path
                    (get-manual-destination-path manual-ast)
                    (string-append (startup-directory) extracted-source-destination-delta))))

        )

     (set! the-manual-destination-path manual-destination-path)

     ; Use extracted manual information, assigned by extract-documentation-from-scheme-file.
     ; Only use this information if it is not explicitly supplied in the manual-front-matters element.
;     (if (is-empty? the-manual-abstract) (set! the-manual-abstract (xml-parse-string (string-append "<div>" extracted-manual-abstract "</div>") 'xhtml10-transitional)))   ; experimental
     (if (is-empty? the-manual-abstract) (set! the-manual-abstract  extracted-manual-abstract))
     (if (is-empty? the-manual-title) (set! the-manual-title extracted-manual-title))
     (if (is-empty? the-manual-author) (set! the-manual-author extracted-manual-author))
     (if (is-empty? the-manual-affiliation) (set! the-manual-affiliation extracted-manual-affiliation))

     (set! manual-meta-information 
          (make-manual-meta-information 
            manual-ast
            (if (is-empty? the-manual-title) extracted-manual-title the-manual-title)
            (if (is-empty? the-manual-author) extracted-manual-author the-manual-author)
            (if (is-empty? the-manual-affiliation) extracted-manual-affiliation the-manual-affiliation)
            (if (is-empty? the-manual-abstract) extracted-manual-abstract the-manual-abstract)
            (normalize-file-path info-manual-source-path)
          )
     )

     (if (not laml-resource) (set! laml-manual-stuff (as-boolean extracted-laml-resource)))
     (if (not css-prestylesheet) (set! the-manual-prestylesheet extracted-css-prestylesheet))
     (if (not css-stylesheet) (set! the-manual-stylesheet extracted-css-stylesheet))
     (if (not css-copying?) (set! copy-css-stylesheet? (as-boolean extracted-css-stylesheet-copying)))

     (make-manual (reverse doc-list-3) 'manual-from-scheme-file manual-destination-path info-manual-source-path)

     (if (or link-to-ep-source-program?
             (and (not source-linking?) ; no scheme-source-linking attribute in LAML file
                  (as-boolean extracted-scheme-source-linking) ; asked for scheme-source-linking in scheme source file
                  ))
         (make-ep-source-program manual-name final-manual-source-path manual-destination-path))  
   )
   (if (and manual-from-scheme-file-clauses (file-exists? temp-source-file-path)) ; multiple scheme file sources -  temp source in temp dir to delete
       (delete-file temp-source-file-path))

  )
 )
)

; Read and return the parsed DTD which is located at full-dtd-path.
; As a side-effect, assign the two variables element-list and attribute-list, as required by tools/dtd-parser/element-attribute-access.scm
(define (parsed-dtd-file-read full-dtd-path)
  (let* ((all-parsed-info (file-read full-dtd-path))
         (el-list (filter (lambda (el) (eq? (car el) 'element)) all-parsed-info))
         (attr-list (filter (lambda (el) (eq? (car el) 'attribute)) all-parsed-info)))
    (set! element-list el-list)
    (set! attribute-list attr-list)
    all-parsed-info))

; Manual from XML DTD only.
(define (manual-briding-xml-dtd! manual-ast)
  (let* ((manual-from-xml-dtd-clause (find-first-ast manual-ast "manual-from-xml-dtd"))
         (dtd-source-path (ast-attribute manual-from-xml-dtd-clause 'src))
         (dtd-source-path-1 (string-append (file-name-initial-path dtd-source-path) (file-name-proper dtd-source-path)))
         (dtd-source-path-2 (if (absolute-file-path? dtd-source-path-1) dtd-source-path-1 (string-append (startup-directory) dtd-source-path-1)))
         (mirror-names-defined-attr (as-boolean (unique-ast-attribute manual-ast "mirror-names-defined" "true")))
         (mirror-name-prefix-attr (unique-ast-attribute manual-ast "mirror-name-prefix" ""))
         (language-name (file-name-proper dtd-source-path))
         (dtd-list (parsed-dtd-file-read (string-append dtd-source-path-2 "." "lsp")))
         (doc-list
           (let ((entries (map 
                               (manual-extend 'description (string-append "A Scheme mirror function of an XML element, as defined in the " language-name " XML DTD."))
                               (manual-from-parsed-dtd dtd-list mirror-name-prefix-attr)))
                )
            (if mirror-names-defined-attr
                entries
                (map 
                 (manual-extend 'misc "This mirror function is solely available via the language map.")
                 entries))
               ))
         (doc-list-1 
            (if (and schemedoc-element-cross-reference-url-fn (bound? schemedoc-element-cross-reference-url-fn))
                (extend-doc-list-with-additional-cross-reference doc-list (eval-cur-env schemedoc-element-cross-reference-url-fn))
                doc-list))
         (manual-destination-path (get-manual-destination-path manual-ast))
         (make-manual-template? (as-boolean (unique-ast-attribute manual-ast "make-dtd-manual-template" #f)))
         (template-file-name (ensure-non-existing-file-in-dir  "manual-template.laml" manual-destination-path))

       )

    (set! mirror-info-path (string-append dtd-source-path-2 "." "info"))  ; abs path

    (set! manual-meta-information 
          (make-manual-meta-information
             manual-ast
             (find-first-ast manual-ast "manual-title")
             (find-first-ast manual-ast "manual-author")
             (find-first-ast manual-ast "manual-affiliation")
             (find-first-ast manual-ast "manual-abstract")))

    (set! the-manual-destination-path manual-destination-path)
     
    ; Generate manual template on new fresh laml file:
    (if make-manual-template?
     (write-text-file
       (manual-template-from-parsed-dtd dtd-list)
       (string-append manual-destination-path template-file-name)))
    
    (make-manual doc-list-1 'manual-from-xml-dtd manual-destination-path ""))
)

; Manual controlled from manual pages and manual sections, merged with manual contributions from an XML dtd
(define (manual-briding-merged-xml-dtd! manual-ast)
  (let* ((manual-from-xml-dtd-clause (find-first-ast manual-ast "merged-manual-from-xml-dtd"))
         (dtd-source-path (ast-attribute manual-from-xml-dtd-clause 'src))
         (dtd-source-path-1 (string-append (file-name-initial-path dtd-source-path) (file-name-proper dtd-source-path)))
         (dtd-source-path-2 (if (absolute-file-path? dtd-source-path-1) dtd-source-path-1 (string-append (startup-directory) dtd-source-path-1)))
         (language-name (file-name-proper dtd-source-path))
         (page-section-doc-list
           (traverse-and-collect-all-from-ast 
              manual-ast
              (lambda (ast) (or (equal? "manual-page" (ast-element-name ast))
                                (equal? "manual-section" (ast-element-name ast))))
              transform-page-or-section)) 
         (dtd-list (parsed-dtd-file-read (string-append dtd-source-path-2 "." "lsp")))
         (dtd-doc-list (manual-from-parsed-dtd dtd-list ""))
         (manual-destination-path (get-manual-destination-path manual-ast))
         (reversed-page-section-list page-section-doc-list)    ; passing it through (earlier we reversed).
         (merged-manual-pages (merge-manual-pages reversed-page-section-list dtd-doc-list))
         (merged-manual-pages-1 
            (if (and schemedoc-element-cross-reference-url-fn (bound? schemedoc-element-cross-reference-url-fn))
                (extend-doc-list-with-additional-cross-reference merged-manual-pages (eval-cur-env schemedoc-element-cross-reference-url-fn))
                merged-manual-pages))

         (make-manual-template? (as-boolean (unique-ast-attribute manual-ast "make-dtd-manual-template" #f)))
         (template-file-name (ensure-non-existing-file-in-dir  "manual-template.laml" manual-destination-path))
        )

    (set! mirror-info-path (string-append dtd-source-path-2 "." "info"))  ; abs path

    (set! the-manual-destination-path manual-destination-path)

    (set! manual-meta-information 
          (make-manual-meta-information
             manual-ast
             (ast-text-deep (find-first-ast manual-ast "manual-title"))
             (ast-text-deep (find-first-ast manual-ast "manual-author"))
             (ast-text-deep (find-first-ast manual-ast "manual-affiliation"))
             (ast-text-deep (find-first-ast manual-ast "manual-abstract"))))

    (merged-xml-dtd-completeness-control! reversed-page-section-list dtd-doc-list)

    ; Generate manual template on new fresh laml file:
    (if make-manual-template?
     (write-text-file
       (manual-template-from-parsed-dtd dtd-list)
       (string-append manual-destination-path template-file-name)))

    (make-manual merged-manual-pages-1 'merged-manual-from-xml-dtd manual-destination-path ""))
)

; Aggregate xml-crossing to a single string or a single ast.
(define (aggregate-xml-crossing xml-crossing)
  (cond ((ast? xml-crossing) xml-crossing)
        ((string? xml-crossing) xml-crossing) 
        ((and (list? xml-crossing) (= 1 (length xml-crossing)) (ast? (car xml-crossing))) (car xml-crossing))
        ((and (list? xml-crossing) (= 1 (length xml-crossing)) (string? (car xml-crossing))) (car xml-crossing))
        ((list? xml-crossing) (span xml-crossing))
        (else "aggregate-xml-crossing: Unknown kind of input:" xml-crossing)))

(define (aggregated-contents ast)
  (aggregate-xml-crossing (ast-subtrees ast)))

; ---------------------------------------------------------------------------------------------------
; Manual template generation - for DTD manual merging.


; Return a manual template from the xml dtd-list-structure, in which
; attribute descriptions etc. can be inserted. Returns a text string.
(define (manual-template-from-parsed-dtd dtd-list-structure)
 (let* ((attributes (filter (lambda (x) (eq? 'attribute (car x))) dtd-list-structure))
        (elements   (filter (lambda (x) (eq? 'element (car x))) dtd-list-structure))
        (element-names (map cadr elements))
        (sorted-attributes (sort-list attributes (generate-leq element-names cadr string=?)))
       )
  (string-append
   "(load (string-append laml-dir \"laml.scm\"))" CR
   "(laml-style \"xml-in-laml/schemedoc-2/schemedoc\")" CR
   "(manual"  CR
   "  (manual-front-matters"  CR
   "    (manual-title \"\")"  CR
   "    (manual-author \"\")"  CR
   "    (manual-affiliation \"\")"  CR
   "    (manual-abstract \"\")"  CR
   "    'css-prestylesheet \"normal\""  CR
   "    'css-stylesheet \"original\""  CR
   "  )" CR CR
   
    (list-to-string
     (map manual-template-page-from-attribute sorted-attributes)
     (string-append CR)) CR

   "  (merged-manual-from-xml-dtd 'src \"\")" CR CR

   ")"  CR)))

(define (manual-template-page-from-attribute dtd-attribute-structure)
  (let ((element-name (attribute-name-dtd dtd-attribute-structure))
        (attr-list (attribute-list-dtd dtd-attribute-structure)))
    (string-append
   "  (manual-page 'name " (string-it (as-string element-name))  CR  
   "    (description \"\")"  CR  
   "    (attribute-descriptions "  CR   
             
    (list-to-string
     (map attribute-description-template (map first attr-list))
     (string-append CR)) CR

   "    )" CR
   "  )" CR)))

(define (attribute-description-template attr-name)
  (string-append
    "      (attribute-description 'name " (string-it (as-string attr-name)) " \"\")"))
       
; ---------------------------------------------------------------------------------------------------

(define (adapt-informative-path path)
  (if laml-manual-stuff
      (if (looking-at-substring? path 0 laml-dir)
          (remove-trailing-slash  ; slash introduced by normalize-relative-file-path - this is messy and strange programming - bøjet søm!
             (normalize-relative-file-path (substring path (string-length laml-dir) (string-length path))))
          path)
      path))

; Ad hoc, used only by adapt-informative-path
(define (remove-trailing-slash str)
  (let ((lgt (string-length str)))
    (if (eqv? #\/ (string-ref str (- lgt 1)))
        (substring str 0 (- lgt 1))
        str)))



; ---------------------------------------------------------------------------------------------------------------
; EP Interfacing


(define (in-laml-temp suffix)
  (string-append laml-dir "temp/" suffix))

(define fixed-image-directory #f) ; assigned later

; Activate the Scheme elucidator procedure elucidate-program-source on the Scheme source program of this manual.
; Before that, setup the Scheme elucidator by low level means - circumventing the normal XML-in-LAML setup.
; manual-name is the proper name of the manual (typically source-file-name-without-extension).
; original-source-file-path is the full path to the original source file (with original lexical comments).
; manual-destination-path is the full file path to the directory in which the HTML manual is generated.
(define (make-ep-source-program manual-name original-source-file-path manual-destination-path)
 (let ((local-manual-source-list #f))
  (load (string-append laml-dir "styles/xml-in-laml/elucidator-2/elucidator.scm"))  ; Perhaps move
  (display-message "Rendering the Scheme Source Program with the Scheme Elucidator 2...")

  (let* ((source-file-path-syntactical-comments (in-laml-temp "temp-source-file.scm"))
         (source-key manual-name) 
         (local-ep-dir-name (string-append manual-name "-ep"))
         (ep-dir-path (string-append manual-destination-path local-ep-dir-name "/"))
         (version-number 1)
         (html-destination-file-path (string-append ep-dir-path  manual-name  "-" (as-string version-number) "." "html"))         
        )

   ; Create EP directory:
   (ensure-directory-existence! manual-destination-path local-ep-dir-name)

   ; (Re)make syntactical comments:
   (set! scheme-documentation-commenting-style manual-scheme-documentation-commenting-style)
   (lexical-to-syntactical-comments! original-source-file-path source-file-path-syntactical-comments)

   (let ((source-list (read-source source-file-path-syntactical-comments source-key version-number)))

     ; Establish necessary EP context
     (if laml-manual-stuff
         (begin   ; access images from central location
           (set! source-directory (string-append manual-destination-path local-ep-dir-name "/"))
           (set! elucidator-source-destination-delta "")
           (set! image-file-access 'fixed)
           (set! fixed-image-directory (string-append (laml-dir-prefix (html-directory)) "images/elucidator/"))
           (set! ep-stylesheet-approach 'central)
     ))

     (set! program-source-list
        (list (list (list 'key source-key) (list 'file-location source-file-path-syntactical-comments)
              (list 'language "Scheme")
              (list 'group "core") (list 'version version-number) 
              (list 'process #t)
              (list 'friendly-name "Program")
             )))
     (set! store-defined-names? #f)
     (set! manual-frame-from-program #f)
     (set! schemedoc-back-linking? #t)     

     (set! program-source-key-version-map (list (cons source-key version-number)))
     (set! make-large-source-files? #f)
     (set! cross-reference-index-support 'none)
     (set! the-scheme-report-version 'r5rs)
     (set! scheme-syntax-procedure-list (read-scheme-knowledge 5))
     (set! scheme-syntax-procedure-names (map first scheme-syntax-procedure-list))
     
     (set! the-scheme-report-version manual-front-matters-scheme-report-version-attr)

     (set! rnrs-url-prefix
           (if (and laml-manual-stuff (member (laml-version-kind) (list "development" "full")) (memq manual-front-matters-scheme-report-version-attr (list 'r4rs 'r5rs)))
               (string-append
                 (laml-home-url-prefix
                    1			; due to ep-dir
                    (string-append manual-source-directory manual-source-destination-delta-attr))
                 (as-string manual-front-matters-scheme-report-version-attr) "/")
               manual-front-matters-rnrs-url-attr))

     (set! scheme-syntax-procedure-list (if (or (eq? 'r4rs manual-front-matters-scheme-report-version-attr) (eq? 'r5rs manual-front-matters-scheme-report-version-attr))
					    (read-scheme-knowledge manual-front-matters-scheme-report-version-attr)
                                            '()))
     (set! scheme-syntax-procedure-names (map first scheme-syntax-procedure-list))

     (set! elucidator-verbose-mode #f)

     (set! local-manual-source-list scheme-source-linking-manual-list)

     (set! manual-source-list
       (filter ; only those contributions which are present in the current LAML distribution
               ; Rationale: The slim version does not include the LAML manuals.
         (lambda (mse)
           (let ((file-loc (get-value 'file-location mse)))
             (file-exists?
              (string-append (file-name-initial-path file-loc) (file-name-proper file-loc) ".manlsp"))))          
       local-manual-source-list))

     (set! manual-name-file-map (pre-process-manual-lsp-files manual-source-list))



     ; Now elucidate source program
     (elucidate-program-source 
      source-file-path-syntactical-comments
      html-destination-file-path 
      source-list
      (make-defining-name-occurences program-source-list)        ; defined-names
      '()                               ; documented-names
      'small
      source-key 
      version-number                                 ; source-version
      "core")

     ; Copy CSS stylesheet:
     (if (not laml-manual-stuff)
         (ensure-directory-existence! (string-append manual-destination-path local-ep-dir-name "/") "stylesheets"))

     ; Copy program stylesheet:
     (if (not laml-manual-stuff)
       (let ((program-ep-software-css-filepath 
              (string-append elucidator-software-directory "stylesheets/" "program" ".css"))
             (program-target-css-filepath 
              (string-append manual-destination-path local-ep-dir-name "/" "stylesheets/" "program.css")) 
             )
         (write-text-file
          (string-append (read-text-file-if-exists program-ep-software-css-filepath))
          program-target-css-filepath)))

     ; Make images directory:
     (if (not laml-manual-stuff) (ensure-directory-existence! (string-append manual-destination-path local-ep-dir-name "/") "images"))

     ; Copy Elucidator image files:
     (if (not laml-manual-stuff)
         (copy-files 
           elucidator-image-files
           (string-append elucidator-software-directory "images/")
           (string-append manual-destination-path local-ep-dir-name "/" "images/")))

     (display-message "Elucidator: DONE")
   )
   (delete-file source-file-path-syntactical-comments)
 )))


(define (extend-doc-list-with-source-references doc-list ep-dir-name ep-html-file-name)
  (cond ((null? doc-list) '())
        ((manual-page? (car doc-list))
           (cons (extend-doc-entry-with-source-reference (car doc-list) ep-dir-name ep-html-file-name) 
                 (extend-doc-list-with-source-references (cdr doc-list) ep-dir-name ep-html-file-name)))
        (else 
           (cons (car doc-list) 
                 (extend-doc-list-with-source-references (cdr doc-list) ep-dir-name ep-html-file-name)))))

(define (extend-doc-entry-with-source-reference doc-entry ep-dir-name ep-html-file-name)
  (let* ((cross-references-clause (get-defaulted-prop 'cross-references doc-entry #f))
         (title-clause (get-prop 'title doc-entry))
         (new-reference `(reference
                         "Scheme source file"
                         ,title-clause
                         ,(string-append ep-dir-name "/" ep-html-file-name "." "html" "#" title-clause))
                       ))
    (if cross-references-clause  ; is there at least one cross references clause in doc-entry?
        (append 
            (filter (negate is-cross-reference-clause?) doc-entry)
            (list `(cross-references ,@(cons new-reference (flatten (map cdr (filter is-cross-reference-clause? doc-entry))))))
        )
        (cons `(cross-references ,new-reference)
              doc-entry))))

(define (extend-doc-list-with-additional-cross-reference doc-list element-name-to-url-fn)
  (cond ((null? doc-list) '())
        ((manual-page? (car doc-list))
           (cons (extend-doc-entry-with-additional-cross-reference (car doc-list) element-name-to-url-fn) 
                 (extend-doc-list-with-additional-cross-reference (cdr doc-list) element-name-to-url-fn)))
        (else 
           (cons (car doc-list) 
                 (extend-doc-list-with-additional-cross-reference (cdr doc-list) element-name-to-url-fn)))))

(define (extend-doc-entry-with-additional-cross-reference doc-entry element-name-to-url-fn)
  (let* ((cross-references-clause (get-defaulted-prop 'cross-references doc-entry #f))
         (title-clause (get-prop 'title doc-entry))
         (url (element-name-to-url-fn title-clause))
         (new-reference `(reference
                           ,schemedoc-element-cross-reference-anchor-text
                           ,title-clause
                           ,url  ; call the element-to-url function on the name of the element/the name of the function/...
                       ))
        )
    (if url
        (if cross-references-clause ; is there at least one cross references clause in doc-entry?
	    (append 
	     (filter (negate is-cross-reference-clause?) doc-entry)
	     (list `(cross-references ,@(cons new-reference (flatten (map cdr (filter is-cross-reference-clause? doc-entry))))))
	     )
	    (cons `(cross-references ,new-reference)
		  doc-entry))
        doc-entry)))

(define (is-cross-reference-clause? doc-entry-clause)
  (eq? (car doc-entry-clause) 'cross-references))

; ---------------------------------------------------------------------------------------------------------------
; Testcase interface stuff: From version 32: Loaded from lib/testcase-access.scm.
; Shared with other Scheme tools that need access to testcases.


; Extend doc-list with examples from examples-from-testsuite.
; The parameter examples-from-testsuite is a list of testcases, as we represent it in the testcases file in Scheme unit testing.
(define (extend-doc-list-with-examples examples-from-testsuite doc-list)
  (cond ((null? doc-list) '())
        ((manual-page? (car doc-list))
           (cons (extend-doc-entry-with-examples examples-from-testsuite (car doc-list)) 
                 (extend-doc-list-with-examples examples-from-testsuite (cdr doc-list))))
        (else 
           (cons (car doc-list) 
                 (extend-doc-list-with-examples examples-from-testsuite (cdr doc-list))))))

; Extend doc-entry with examples from testcase-list.
; As a somewhat serious complication, we will have to take out existing an examples (plural) clause if it exists, and add the testcase examples.
(define (extend-doc-entry-with-examples testcase-list doc-entry)
  (let* ((examples-clause (defaulted-get 'examples doc-entry #f))    ; plural: examples.  The list of example (singular) clauses. 

         (name (get-prop 'title doc-entry))
         (relevant-testcases (filter (lambda (testcase) (is-testcase-about-doc-entry testcase name)) testcase-list))
         (example-list (map make-example-from-testcase relevant-testcases)))

    (if examples-clause
        (append                                              ; add testcase examples to existing examples
          (filter (negate is-examples-clause?) doc-entry)
          (list `(examples ,@examples-clause ,@example-list))
        )                                                    ; else form a brand new examples (plural) clause
        (append doc-entry (list `(examples ,@example-list))))))

(define (is-examples-clause? doc-entry-clause)
  (eq? (car doc-entry-clause) 'examples))

(define (is-testcase-about-doc-entry testcase doc-entry-name)
 (let ((example-marker (test-case-example-info-of testcase)))
  (or (and (list? example-marker)
           (= 1 (length example-marker))                                ; wildcard: test is relevant for all names that occur in the testcase
           (eq? (first example-marker) 'use-as-example)
           (is-testcase-about testcase doc-entry-name)                  ; doc-entry-name appears in testcase
      )
      (and (list? example-marker)
           (> (length example-marker) 1)                                ; testcase enumerates names for which the testcase acts as example
           (eq? (first example-marker) 'use-as-example)
           (member doc-entry-name (cdr example-marker))       ; is doc-entry-name among these?
      )
  ))) 


(define (is-testcase-about testcase name)
  (let* ((expr (real-test-case-expression-of testcase))
         (idx (substring-index expr 0 (as-string name)))
         (separator-chars (list #\space #\newline #\( #\)))
         (name-lgt (string-length name))
        )
   (turn-into-boolean
    (and
      (number? idx)                                                                      ; name is part of testcase
      (or (= idx 0) (memv (string-ref expr (- idx 1)) separator-chars))                  ; to the left of name there is a no-name char
      (or (= idx 0) (memv (string-ref expr (+ idx name-lgt)) separator-chars))))))       ; and the same to the right

(define (make-example-from-testcase testcase)
 (cond ((eq? (test-case-type-of testcase) 'error-functional-testcase)
              (list 'example
		    (string-append (real-test-case-expression-of testcase) ": " "ERROR")))

       ((eq? (test-case-type-of testcase) 'functional-testcase)
              (list 'example
		    (string-append (real-test-case-expression-of testcase) " => " (real-test-case-result-of testcase))))))
    

; end testcase stuff
; ---------------------------------------------------------------------------------------------------------------


; Return an association list of meta information for this manual.
; In case a given attribute is not provided, the attribute is mapped to the symbol not-provided.
; The optional parameter scheme-source-file can be a full path to the Scheme source file from which documentation is extracted.
(define (make-manual-meta-information manual-ast manual-title manual-author manual-affiliation manual-abstract . optional-parameter-list)
 (let ((scheme-source-file (optional-parameter 1 optional-parameter-list 'not-provided)))

   (append

    (list
     (cons 'title manual-title)
     (cons 'author manual-author)
     (cons 'affiliation manual-affiliation)
     (cons 'abstract manual-abstract)
     (cons 'scheme-source-file scheme-source-file)  
     )

    (map (lambda (attr-name) (cons attr-name (unique-ast-attribute manual-ast (as-string attr-name) 'not-provided)))
	 '(laml-resource 
	   documentation-commenting-style
	   default-attribute-explanation
	   source-destination-delta
	   css-prestylesheet
	   css-stylesheet
	   css-stylesheet-copying
	   make-dtd-manual-template
	   keep-syntactical-comment-file
	   manual-destination-name
	   mirror-names-defined
	   mirror-name-prefix
	   attribute-sorting
	   scheme-source-linking)))))


; A version of ast-text-deep from the common XML-in-LAML library that also accepts a string.
(define (ast-text-deep-1 x)
  (cond ((string? x) x)
        ((and (list? x) (> (length x) 0) (string-list? x)) (accumulate-right string-append "" x))  ; hack...
        (else (ast-text-deep x))))

; x is known to be non-empty at the out activation
(define (string-list? x)
  (cond ((null? x) #t)
        (else (and (string? (car x)) (string-list? (cdr x))))))


; ===============================================================================================================
; SchemeDoc Backend stuff. Manual presentation with XHTML.
; Earlier known as manual-kernel.scm
; ===============================================================================================================

; Make a more sparse html char transformation table for xhtml in manuals.
; This table does not transliterate '<' and '>'




; Eliminated Dec 30, 2005:
; (define manual-html-char-transformation-table
;     (list->vector (make-list 256 #t)))
; 
; (set-html-char-transformation-entry! html-char-transformation-table (char->integer #\<) "&lt;")
; (set-html-char-transformation-entry! html-char-transformation-table (char->integer #\>) "&gt;")
; 
; (set-html-char-transformation-entry! html-char-transformation-table (char->integer #\") "&quot;")
; (set-html-char-transformation-entry! html-char-transformation-table (char->integer #\') "&#39;")
; (set-html-char-transformation-entry! html-char-transformation-table (char->integer #\&) "&amp;")
; 
; (set-xml-char-transformation-table-in 'xhtml10-transitional manual-html-char-transformation-table) 




; In order to preserve white space in textual pre elements from manual entries and manual section entries:
(set! xml-always-render-white-space? #t)

; ---------------------------------------------------------------------------------------------------
; Global variables

(define max-section-number #f) ; assigned by manual-page

; The absolute destination path to the directory with HTML files etc. 
; Re-initialized by make-manual (for historical reasons).
(define the-manual-destination-path (startup-directory))

; The kind of sorting applied on attributes.
; Either as-in-dtd (default) or by-attribute-name
(define attribute-sorting-kind 'as-in-dtd) 

; A boolean variable that controls the creation of links to HTML, elucidative programming style source programs   
(define link-to-ep-source-program? #f)

; Both assigned from front matters attribtues in manual-briding! 
(define manual-front-matters-scheme-report-version-attr #f)
(define manual-front-matters-rnrs-url-attr #f)

; Corresponds to the Elucidator manual source list. Only used if the variable link-to-ep-source-program? is true.
(define scheme-source-linking-manual-list '())


; ---------------------------------------------------------------------------------------------------
; A variation of as-quoted-string from the library general.scm which treats subexpressions of the
; form (quote ...) better.
(define (as-quoted-string-1 x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) (string-it x))
        ((boolean? x) 
            (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
        ((char? x) (char->string x))
        ((and (list? x) (not (null? x)) (symbol? (car x)) (eq? (car x) 'quote) (not (null? (cdr x))))
            (string-append "'" 
              (as-quoted-string-1 (cadr x))
            ))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-quoted-string-1 x) (make-list (- (length x) 1) " "))
               ")"))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-quoted-string-1 y) " ")) (proper-part x))
               )
               " . " (as-quoted-string-1 (first-improper-part x))
               ")"))
        (else "??")))

; ---------------------------------------------------------------------------------------------------

; Meta information about the current manual. Written as the first element in the internal -.manlsp file
(define manual-meta-information '())

; CSS stylesheet management.
; CSS stylesheets can be defined in the stylesheets directory of the manual source directory
; or in the stylesheets directory of the manual software directory. Exactly as in LENO.
; An aggregation of the pre css stylesheet and the css stylesheet from these two directories
; is copied to the destination directory. The destination directory dependes on the the variable
; css-stylesheet-schema.

; Where to find the manual css stylesheet. A symbol.
; laml: Use manual-stylesheet.css in the css-stylesheets/manual directory of the laml root.
; local: Use the local css file of the same proper name as the manual source file.
(define css-stylesheet-schema 'laml)

; Copy a css stylesheet from the stylesheet directory of the manual software source directory
; to the location determined by the variable css-stylesheet-schema. 
; If #f, no copying is done.
(define copy-css-stylesheet? #t)

; The name of the css stylesheet, as present in the source stylesheets directory. String without file file extension.
; If copy-css-stylesheet?, this stylesheet is copied to the target stylesheet location.
(define the-manual-stylesheet "original")

; The name of the pre css stylesheet which is put in front of the the-manual-stylesheet.
(define the-manual-prestylesheet "compact")

; Return the relative path from the startup directory to the css manual stylesheet.
; Destination dir is normalized (precondition).
; The parameter style-sheet-schema is supposed to be the constant css-stylesheet-schema.
(define (css-stylesheet-path stylesheet-schema destination-dir)
  (cond ((eq? stylesheet-schema 'laml)
           (string-append (laml-dir-prefix destination-dir) "css-stylesheets/manual/" "manual-stylesheet.css"))
        ((eq? stylesheet-schema 'local)
         (string-append manual-name "." "css"))
        (else (laml-error "css-stylesheet-path: Unknown style-sheet schema" stylesheet-schema))))

(define (read-text-file-if-exists file-path)
  (if (file-exists? file-path)
      (read-text-file file-path)
      ""))

(define (copy-css-stylesheet! absolute-destination-dir)
  (let ((manual-source-css-pre-filepath (string-append manual-source-directory "stylesheets/" the-manual-prestylesheet ".css"))
        (manual-software-css-pre-filepath  (string-append schemedoc-software-directory "stylesheets/" the-manual-prestylesheet ".css"))
        (manual-source-css-filepath (string-append manual-source-directory "stylesheets/" the-manual-stylesheet ".css"))
        (manual-software-css-filepath  (string-append schemedoc-software-directory "stylesheets/" the-manual-stylesheet ".css"))
        (manual-target-css-filepath 
             (cond ((eq? css-stylesheet-schema 'laml)
                    (string-append laml-dir "css-stylesheets/manual/" "manual-stylesheet.css"))
                   ((eq? css-stylesheet-schema 'local)
                    (string-append absolute-destination-dir manual-name "." "css"))
                   (else (laml-error "css-stylesheet-path: Unknown style-sheet schema" css-stylesheet-schema))))
       )
    (write-text-file
     (string-append
      (read-text-file-if-exists manual-software-css-pre-filepath) CR CR (read-text-file-if-exists manual-source-css-pre-filepath) CR CR
      (read-text-file-if-exists manual-software-css-filepath) CR CR (read-text-file-if-exists manual-source-css-filepath)
      )
     manual-target-css-filepath
     )

    (if (and (not (file-exists? manual-source-css-pre-filepath))
             (not (file-exists? manual-software-css-pre-filepath)))
        (display-warning "Cannot locate any CSS prestylesheet named" the-manual-prestylesheet))

    (if (and (not (file-exists? manual-source-css-filepath))
             (not (file-exists? manual-software-css-filepath)))
        (display-warning "Cannot locate any CSS stylesheet named" the-manual-stylesheet))
    ))

; ---------------------------------------------------------------------------------------------------
; MAINLY PREAMBLE FUNCTION DEFINITIONS

(define laml-source-file-name-without-extension (source-filename-without-extension))

; The explanation of attributes, for which no attribute-description is available
(define standard-attribute-explanation "-")

(define the-manual-title "")
(define the-manual-author "")
(define the-manual-affiliation "")
(define the-manual-abstract "")

(define home-url #f)
(define manual-master-index #f)

(define manual-name (if (not (equal? laml-source-file-name-without-extension ""))
                        laml-source-file-name-without-extension
                        "manual"))


(define manual-page-list '()) ; the list in which we collect manual page descriptions

(define manual-background white)

(define language-preference 'english) ; redefinition

(define laml-manual-stuff #t)   ; true if making a manual for laml-stuff. Affects the header of the generated html file.



;;; Programmatic support of manual generation. 
;;; The functions in the following are selected functions from the style
;;; manual.scm, which are useful when manuals are made from other programs.


;; Given an entry (or page). Is it a manual page (as opposed to a manual section)
(define (manual-page? elements)
  (let ((res (assq 'kind elements)))
    (if res
        (equal? (cadr res) "manual-page")
        #f)))

;; Given an entry (or page). Is it a manual section (as opposed to a manual page)
(define (manual-section? elements)
  (let ((res (assq 'kind elements)))
    (if res
        (equal? (cadr res) "manual-section")
        #f)))


(define manual-first-sentence-in-string first-sentence-in-string)

; return a reference to another a-named place in the current document
(define (internal-ref anchor)
  (a-tag (this-reference anchor) anchor))

(define (this-reference ref)
  (string-append "#" ref))



; the mutual ordering among manual elements (tags):
(define manual-element-order 
  '(section-id section-title section-body title library kind form xml-in-laml-example-form description content-model attributes 
    pre-condition parameters xml-in-laml-attributes returns post-condition examples cross-references misc comment))

; the list of possible and supported top level elements of a manual page. Manual section entries are not included in this list.
(define supported-manual-page-elements 
  '(title library kind form description pre-condition post-condition parameters examples cross-references misc comment returns
          xml-in-laml-example-form attributes content-model level xml-in-laml-attributes))  ; add attribute-descriptions - and bring plain-manual-attribute-descriptions up to date!

; the list of possible and supported top level elements of a manual section. Manual page entries are not included in this list.
(define supported-manual-section-elements
  '(kind section-id section-title section-body level))

; ---------------------------------------------------------------------------------------------------
; DISPATCHING STUFF:

(define element-tag car)
(define element-contents cdr) 


(define (find-processor style kind)
  ; return function given the symbols style and kind
 (cond ((eq? style 'plain-manual-style)
        (cond ((eq? kind 'title) plain-manual-title)
              ((eq? kind 'form) plain-manual-form)
              ((eq? kind 'pre-condition) plain-manual-pre-condition)
              ((eq? kind 'post-condition) plain-manual-post-condition)
              ((eq? kind 'description) plain-manual-description)
              ((eq? kind 'parameters) plain-manual-parameters)
              ((eq? kind 'parameter) plain-manual-parameter)  ; not used
              ((eq? kind 'examples) plain-manual-examples)
              ((eq? kind 'example) plain-manual-example)  ; not used
              ((eq? kind 'cross-references) plain-manual-cross-references)
;              ((eq? kind 'reference) plain-manual-reference)
;              ((eq? kind 'internal-references) plain-manual-internal-references)
              ((eq? kind 'comment) plain-manual-comment)
              ((eq? kind 'misc) plain-manual-misc)
              ((eq? kind 'returns) plain-manual-returns)
              ((eq? kind 'kind) empty-element)

              ((eq? kind 'section-title) plain-manual-section-title)
              ((eq? kind 'section-body) plain-manual-section-body)
              ((eq? kind 'section-id) plain-manual-section-id)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) plain-manual-xml-in-laml-example-form)   ; information from XML DTDs
              ((eq? kind 'content-model) plain-manual-content-model)
              ((eq? kind 'attributes) plain-manual-attributes)
              ((eq? kind 'attribute) plain-manual-attribute)

              ((eq? kind 'attribute-descriptions) plain-manual-attribute-descriptions)   ; manually authored information - does it work?
              ((eq? kind 'attribute-description) plain-manual-attribute-description)   

              ((eq? kind 'xml-in-laml-attributes) plain-manual-xml-in-laml-attributes)   ; Extracted from Scheme program - documentation of XML-in-LAML ad hoc abstractions
              ; ((eq? kind 'xml-in-laml-attribute)  plain-manual-xml-in-laml-attribute)   

              ((eq? kind 'level) empty-element)

              (else plain-generic-manual-processor)))  ; Before Nov 3, 2005: (laml-error  (as-string kind) ": " "Cannot find tag in plain-manual-style")

       ((eq? style 'plain-manual-style-level-2)                        ; only the title field is special
        (cond ((eq? kind 'title) plain-manual-title-level-2)
              ((eq? kind 'section-title) plain-manual-section-title-level-2)
              (else (find-processor 'plain-manual-style kind))))


       ((eq? style 'header-manual-style)
        (cond ((eq? kind 'title) header-manual-title)
              ((eq? kind 'form) header-manual-form)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
              ((eq? kind 'description) header-manual-description)
              ((eq? kind 'parameters) empty-element)
              ((eq? kind 'parameter) empty-element)
              ((eq? kind 'example) empty-element)
              ((eq? kind 'examples) empty-element)
              ((eq? kind 'cross-references) empty-element)
              ((eq? kind 'reference) empty-element)
              ((eq? kind 'internal-references) empty-element)
              ((eq? kind 'comment) empty-element)
              ((eq? kind 'misc) empty-element)
              ((eq? kind 'returns) empty-element)
              ((eq? kind 'kind) empty-element)

              ((eq? kind 'section-title) empty-element)
              ((eq? kind 'section-body) empty-element)
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) header-manual-library)

              ((eq? kind 'xml-in-laml-example-form) header-manual-form)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              ((eq? kind 'level) empty-element)

              (else empty-element)))  ; Before Nov 2, 2005: (error (string-append (as-string kind) ": " "Cannot find tag in header-manual-style"))

       ((eq? style 'header-manual-style-xml)              ; Headers for manuals that stem from XML DTDs
        (cond ((eq? kind 'title) header-manual-title)
              ((eq? kind 'form) empty-element)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
              ((eq? kind 'description) header-manual-description)
              ((eq? kind 'parameters) empty-element)
              ((eq? kind 'parameter) empty-element)
              ((eq? kind 'example) empty-element)
              ((eq? kind 'examples) empty-element)
              ((eq? kind 'cross-references) empty-element)
              ((eq? kind 'reference) empty-element)
              ((eq? kind 'internal-references) empty-element)
              ((eq? kind 'comment) empty-element)
              ((eq? kind 'misc) empty-element)
              ((eq? kind 'returns) empty-element)
              ((eq? kind 'kind) empty-element)

              ((eq? kind 'section-title) empty-element)
              ((eq? kind 'section-body) empty-element)
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) header-manual-library)

              ((eq? kind 'xml-in-laml-example-form) empty-element)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              ((eq? kind 'level) empty-element)

              (else empty-element))) ; Before Nov 2, 2005: (error (string-append (as-string kind) ": " "Cannot find tag in header-manual-style-xml"))

       ((eq? style 'toc-manual-style)
        (cond ((eq? kind 'title) empty-element)
              ((eq? kind 'form) empty-element)
              ((eq? kind 'pre-condition) empty-element)
              ((eq? kind 'post-condition) empty-element)
              ((eq? kind 'description) empty-element)
              ((eq? kind 'parameters) empty-element)
              ((eq? kind 'parameter) empty-element)
              ((eq? kind 'example) empty-element)
              ((eq? kind 'examples) empty-element)
              ((eq? kind 'cross-references) empty-element)
              ((eq? kind 'reference) empty-element)
              ((eq? kind 'internal-references) empty-element)
              ((eq? kind 'comment) empty-element)
              ((eq? kind 'misc) empty-element)
              ((eq? kind 'returns) empty-element)
              ((eq? kind 'kind) empty-element)
              ((eq? kind 'section-title) toc-manual-section-title)
              ((eq? kind 'section-body) empty-element)
              ((eq? kind 'section-id) empty-element)
              ((eq? kind 'library) empty-element)

              ((eq? kind 'xml-in-laml-example-form) empty-element)
              ((eq? kind 'content-model) empty-element)
              ((eq? kind 'attributes) empty-element)
              ((eq? kind 'attribute) empty-element)

              ((eq? kind 'level) empty-element)

              ((eq? kind 'attribute-descriptions) empty-element)
              ((eq? kind 'attribute-description) empty-element)

              ((eq? kind 'xml-in-laml-attributes) empty-element)

              (else empty-element)))  ; Before Nov 2, 2005: (error (string-append (as-string kind) ": " "Cannot find tag in toc-manual-style"))

       (else (error "Unknown style"))))

; In an AST based XHTML representation it comes in handy to represent the empty element as the explicit white space
(define (empty-element . pars)
  explicit-space)

(define (empty-manual-page-element? x)
  (forced-white-space? x))


(define (present-element style kind contents)
  (let ((processing-procedure (find-processor style kind)))
    (apply processing-procedure kind contents)))


; ---------------------------------------------------------------------------------------------------
; GENERATION OF PREDICATE BASED ON manual-element-order

; generate-leq is in the general library 
(define (manual-element-ordering)  (generate-leq manual-element-order element-tag))

; ---------------------------------------------------------------------------------------------------
; Primary/secondary elements
; It is possible to decide whether an element is considered as primary or secondary.
; Per default, all element are primary. The function primary-element? may be redefined
; to change the default. The classification can be determined on basis of the fields of
; the manual entry. The primary-element-indication and secondary-element-indication 
; can return a string which is printed after the title to the indicate primary and secondary
; classification. The plain-secondary-manual-style manual style is used to present
; elements classified as secondary elements.

; Is manual-entry considered as a primary-element.
; If not, it is a secondary element.
; Secondary elements can for instance be older elements, which are presented in
; another way as primary elements
; Can be redefined in laml files that produce concrete manuals.
(define (primary-element? manual-entry) #t)

; Primary/secondary element indications to be shown after the title.
; Can be redefined in laml files that produce concrete manuals.
(define (primary-element-indication) "")
(define (secondary-element-indication) "")


; ---------------------------------------------------------------------------------------------------
; CENTRAL PRESENTATION FUNCTIONS:

; A variable in which we collect given section-id in manual-section clauses
(define explicit-section-id "")

; Returns a list of HTML markup.
; The parameter elements (a list) represents of a single manual page or a single manual-section.
(define (format-manual-page elements primary-style-level-1 primary-style-level-2 . optional-parameter-list)
 (set! explicit-section-id "")
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style-level-1))
       (the-level (get-defaulted-prop 'level elements 1))
      )
  (cond ((and (manual-section? elements) (= the-level 1))
           (map
            (lambda (e)
              (present-element
               (if (primary-element? elements) primary-style-level-1 secondary-style)
               (element-tag e)
               (element-contents e)) 
              )
            (sort-list elements (manual-element-ordering)))
        )
        ((and (manual-section? elements) (= the-level 2))
           (map
            (lambda (e)
              (present-element
               (if (primary-element? elements) primary-style-level-2 secondary-style)  
               (element-tag e)
               (element-contents e)) 
              )
            (sort-list elements (manual-element-ordering)))

        )
        ((and (manual-page? elements) (= the-level 1)) 
          (filter (compose not empty-manual-page-element?) 
            (map
             (lambda (e)
               (present-element
                (if (primary-element? elements) primary-style-level-1 secondary-style)
                (element-tag e)
                (element-contents e)) 
               )
             (sort-list elements (manual-element-ordering)))
            ))
        ((and (manual-page? elements) (= the-level 2)) 
          (filter (compose not empty-manual-page-element?) 
            (map
             (lambda (e)
               (present-element
                (if (primary-element? elements) primary-style-level-2 secondary-style)
                (element-tag e)
                (element-contents e)) 
               )
             (sort-list elements (manual-element-ordering)))
            ))
        (else  (laml-error "format-manual-page: unknown manual entry" elements primary-style-level-1 primary-style-level-2)))))


(define (end-manual-entry)
  (tr 'class (manual-plain-css-class "end-of-entry") (td 'class (manual-plain-css-class "end-of-entry") 'colspan "4" (hr 'class (manual-plain-css-class "end-of-entry")))))


(define (line-breaker str)
  (con str (br)))

; title info, for processing feedback.
(define (info-title manual-page)
 (let ((ttl1 (assq 'title manual-page))
       (ttl2 (assq 'section-title manual-page)))
  (cond (ttl1 (cadr ttl1))
        (ttl2 (cadr ttl2))
        (else "????"))))

; manual is a list, such as manual-page-list
; thus manual is the whole manual struture.
(define (format-manual manual primary-style-level-1 primary-style-level-2  . optional-parameter-list)
 (let ((secondary-style (optional-parameter 1 optional-parameter-list primary-style-level-1))
       (presented-pages (filter page-to-be-presented? manual)))
   (if (null? presented-pages)
       ""
       (html:table 'class "manual-table"
           (tr  (td 'width "3000px" 'colspan "4" ))  ; Empty row enforcing a very broad table.
	   (map
	    (lambda (manual-page)
	      (if (>= schemedoc-verbosity-level 1) (display-message (info-title manual-page)))
	      (append
	       (format-manual-page
		manual-page primary-style-level-1 primary-style-level-2 secondary-style)
	       (list (end-manual-entry)))
	      )
	    presented-pages)))))

(define (page-to-be-presented? manual-page)
  (let ((the-level (get-defaulted-prop 'level manual-page 1)))
    (<= the-level 2))) 

; OLD - not used.
(define (present-author-info au)
 (let ((au1 (if (not (null? au)) (cons (copyright-owner (car au)) (cdr au)) au)))
   (h3 
     (map (lambda (au-el) (span 'class "manual-author-element" au-el)) au1))))

(define (present-author-and-affiliation author-string affiliation-string)
   (h3 (span 'class "manual-author-element" author-string)
       (span 'class "manual-affiliation-element" affiliation-string)))


(define (present-abstract abstr)
 (div 'class "manual-abstract" abstr))



;; This function makes the manual (a XHTML file) from manual-page-list.
;; The order or the manual entries in manual-page-list is preserved by this function. 
;; In this sense, this function is the backend part of SchemeDoc.
;; The XHTML file is written to a particular destination-directory.
;; .parameter manual-page-list The list of manual pages and manual sections of this manual. Defaults to the value of the variable manual-page-list.
;; .parameter manual-kind The kind of manual, manual-pages-sections-only, manual-from-scheme-file, manual-from-xml-dtd, or merged-manual-from-xml-dtd. Defaults to manual-from-scheme-file. A symbol.
;; .parameter destination-dir The absolute destination directory of the HTML file. Defaults to the value of destination-path.
;; .parameter absolute-source-file-path The absolute name of the Scheme source file from which documentation has been extracted.\
;;            Bound to the empty string in case this information does not make sense (in case of DTD documentation, or manually authored documentation).
(define (make-manual manual-page-list manual-kind destination-dir absolute-source-file-path)
 (let* ((manual-page-list-2 (merge-attributes-and-descriptions manual-page-list)) ; join attributes and attribute-descriptions
                                                                                   ; by adding attribute description as an extra element
                                                                                   ; to an attribute. Attribute-descriptions stay
                                                                                   ; as non-presented informations in manual-page-list-2

        (manual-page-list-3 (map clean-manual-page manual-page-list-2))   ; remove unkwown fields from manual pages
        (table-of-contents? (> (number-of-sections? manual-page-list-3) 0))
        (yr (car (time-decode (current-time))))
       )

  (set! the-manual-destination-path destination-dir)
  (set! max-section-number (length (filter manual-section? manual-page-list-3)))

  (if (not laml-manual-stuff)   ; copy images to a local man-images dir
      (begin
        (ensure-directory-existence! destination-dir "man-images")
        (copy-files 
         (list "small-next-blind.gif" "small-next.gif" "small-prev-blind.gif" "small-prev.gif" "small-up-blind.gif" "small-up.gif")
         (string-append schemedoc-software-directory "images/")
         (string-append destination-dir "man-images/"))))

  (write-html '(raw prolog)
   (html
     (head (html:title the-manual-title)
           (link 'href 
                 (css-stylesheet-path css-stylesheet-schema destination-dir)
                 'rel "stylesheet" 'type "text/css")
           (if laml-manual-stuff (link 'rel "SHORTCUT ICON" 'href (string-append (laml-home-url-prefix 0 the-manual-destination-path) "images/16-16-icon.ico")) '())
     )
     (body 'class "manual-body"
        (a-name "MANUAL-TOP")
        (if laml-manual-stuff 
            (left-middle-right-banner (span 'class "manual-top-banner-when-generated" (when-generated)) (span "Copyright " copyright  (as-string yr) ", Kurt Nørmark")
                                      (laml-home-button 0 "laml-home.gif" destination-dir))

            (left-middle-right-banner (span 'class "manual-top-banner-when-generated" (when-generated)) "" 
                                      (span 'class "manual-top-banner-is-schemedoc" "A" (a 'css:color "inherit" 'href schemedoc-url "SchemeDoc") "Manual") )
        )
            
        (h1 'class "manual-title" the-manual-title) (p) 
;        (present-author-info the-manual-author) (p)
        (present-author-and-affiliation the-manual-author the-manual-affiliation) (p)
        (if (and #f home-url (not (equal? home-url ""))) (con (a-tag home-url "Home") (space 3)) "")   ; no home page ref any more
        
        (if manual-master-index 
            (con (a-tag manual-master-index "Master index") (br))
            "")

        (if (not (empty-string? absolute-source-file-path))
            (div 'class "manual-source-file-description" (if laml-manual-stuff "LAML Source file:" "Source file:") (adapt-informative-path absolute-source-file-path))
            "")
        (p)

        (a-name "MANUAL-ABSTRACT")
        (present-abstract the-manual-abstract) (p)

        (if table-of-contents?
          (con
            (a-name "MANUAL-TOC")
            (div 'class "manual-table-of-contents-header" "Table of Contents:")
            (indent-pixels 12 
              (div 
               (if (null? manual-page-list-3)
		   ""
		   (manual-table-of-contents manual-page-list-3 'toc-manual-style))
               (if (or (eq? manual-kind 'manual-from-xml-dtd) (eq? manual-kind 'merged-manual-from-xml-dtd))
		   (a 'href "#MIRROR-INFO" (font 'size "2" "A. Mirror Information Appendix"))
		   '()))
            )
            (p))
          "")
;; [monar] not necessary
;;          (if (not (null? manual-page-list-3))
;;            (con
;;             (a-name "MANUAL-INDEX")
;;             (div 'class "manual-alphabetic-index-header" "Alphabetic index:")
;;             (indent-pixels 12 
;;                (if (null? manual-page-list-3)
;;                    ""
;;                    (manual-index manual-page-list-3 
;;                                   (if (or (eq? manual-kind 'manual-from-xml-dtd) (eq? manual-kind 'merged-manual-from-xml-dtd)) 
;;                                       'header-manual-style-xml
;;                                       'header-manual-style))))
;;             (p)
;;            )
;;            "")



        (format-manual manual-page-list-3 'plain-manual-style 'plain-manual-style-level-2 'plain-secondary-manual-style) 

        (if (and (or (eq? manual-kind 'manual-from-xml-dtd) (eq? manual-kind 'merged-manual-from-xml-dtd)) (file-exists? mirror-info-path))
            (div 'css:font-size "80%"
              (p) (hr) (p)
              (a-name "MIRROR-INFO")
              (div 'class "manual-table-of-contents-header" "Mirror Information Appendix.")   ; separate class would be better
              (present-mirror-info mirror-info-path)
              (p) (hr) (p)
            )
            '())

;        (p) (hr) 
;        (font-1 2 red (when-generated)) (br)
;        (font-1 2 red (span "Generated by LAML" (a 'href schemedoc-url (font-color red "SchemeDoc")) "using" "LAML" laml-version)) (br)
         (if (not (null? manual-page-list-3))
           (con
            (a-name "MANUAL-INDEX")
            (div 'class "manual-alphabetic-index-header" "Alphabetic index:")
            (indent-pixels 12 
               (if (null? manual-page-list-3)
                   ""
                   (manual-index manual-page-list-3 
                                  (if (or (eq? manual-kind 'manual-from-xml-dtd) (eq? manual-kind 'merged-manual-from-xml-dtd)) 
                                      'header-manual-style-xml
                                      'header-manual-style))))
            (p)
           )
           "")

        (div 'class "manual-end-remark"
          (when-generated) (br)
          "Generated by LAML" (a 'css:color "inherit" 'href schemedoc-url "SchemeDoc") "using" "LAML" laml-version (br)
          end-remark)

        (vertical-space 0)  ; to allow for scrolling at the end of a manual page
      )

     )
    (string-append destination-dir (as-string manual-name) ".html" ))

   ; Write the uncleaned manual page to a -.manlsp file. We keep non-standard, specialized fields in the saved manual.
   (write-lisp-form-manual manual-page-list manual-source-directory)

   (if copy-css-stylesheet?
       (copy-css-stylesheet! destination-dir))

 )
)


; For each attribute, find out if there is a separate attribute description. If there is, 
; join it with the attribute record.
(define (merge-attributes-and-descriptions manual-page-list)
  (let ((res (map merge-attributes-and-descriptions-entry manual-page-list)))
     res))


(define (merge-attributes-and-descriptions-entry entry)
 (cond ((manual-page? entry) (merge-attributes-and-descriptions-page entry)) 
       (else entry)))

(define (merge-attributes-and-descriptions-page page)
 (let* ((attributes (defaulted-get 'attributes page '()))
        (attribute-descriptions (defaulted-get 'attribute-descriptions page '()))
        
        ; local selectors:
        (id-of-attribute second)
        (id-of-attribute-descr second)
        (explanation-of-description third)
 
        (new-attributes 
          (map 
           (lambda (a) 
             (let* ((id (id-of-attribute a))
                    (corresponding-description 
                     (find-in-list (lambda (desc) (equal? (id-of-attribute-descr desc) id)) attribute-descriptions))
                    )
               (if corresponding-description
                   (append a (list (explanation-of-description corresponding-description)))
                   (append a (list standard-attribute-explanation)))))
          attributes))
      )
   (cond ((null? attributes) page)
         (else (append 
                (filter (lambda (x) (not (eq? (car x) 'attributes))) page) ; all fields but attributes
                (list (cons 'attributes new-attributes)))))))


;; filter a manual page such that only known elements are processed.
(define (clean-manual-page page)
 (cond ((manual-page? page)
           (filter 
            (lambda (element) (memq (car element) supported-manual-page-elements)) 
            page))
       ((manual-section? page)
           (filter 
            (lambda (element) (memq (car element) supported-manual-section-elements)) 
            page))
       (else (laml-error "clean-manual-page: Unknown kind of page:" page))))


    

;; A possible end remark added as the last line of the documentation in small font. Default is the empty string.
(define end-remark "") 

; return a horizontal slize of a manual contribution
; As an optional parameter this function accept an explanation of the attribute
(define (manual-slice left-contribution right-contribution . optional-parameter-list)
 (tr (td "-")))

; ---------------------------------------------------------------------------------------------------
;;; Index functions. 
;;; The functions supporting the generation of a manual index.

;; The widths of the colums of the manual index.  A list of three integers
(define manual-index-width-list (list 140 300 400))

(define (manual-index manual style)
  (html:table 'border "3" 'class "manual-alphabetic-index"
    (map
      (lambda (manual-page)
         (tr (format-manual-page (only-one-form manual-page) style style)))
      (sort-list 
         (filter manual-page? (filter page-to-be-presented? manual))  ; disregard manual sections for the index
          manual-index-order-leq?))))

; Ensure that only one of form and xml-in-laml-example-form are on manual-page.
; Keep form
(define (only-one-form manual-page)
 (let ((form (get-defaulted-prop 'form manual-page #f)) 
       (xml-in-laml-form (get-defaulted-prop 'xml-in-laml-example-form manual-page #f))
      )
  (cond ((and form xml-in-laml-form)
          (filter (lambda (x) (not (eq? (car x) 'xml-in-laml-example-form))) manual-page))
        (else manual-page))))

;; The predicate ordering the manual entries for index sorting
(define (manual-index-order-leq? x y)
  (string<=? (cadr (assq 'title x)) (cadr (assq 'title y))))

;; Writes a manual structure (list of entries) to a file.
;; The filename is manual-name.manlsp, where manual-name is the value of a global SchemeDoc variable.
;; If the optional parameter dir is given write the manlsp file in this directory.
;; Else write it in the manual source directory.
;; If provided, dir ends in a slash. 
;; The manlsp file is useful for programmatic access to the entire body of manual information.
(define (write-lisp-form-manual manual-page-list . optional-parameter-list)
 (let ((dir (optional-parameter 1 optional-parameter-list manual-source-directory)))
  (let* ((lisp-form-manual-file-name (string-append dir (as-string manual-name) ".manlsp"))
         (extended-manual-page-list 
           (append
             (list manual-meta-information)
             (manlsp-textual-purification-list manual-page-list)  ; Dec 1, 2005: Experimental textual purification of certain manlsp fields 
           )
         )
        )
    (if (file-exists? lisp-form-manual-file-name)
        (delete-file lisp-form-manual-file-name))

    (with-output-to-file lisp-form-manual-file-name
                         (lambda () (write  extended-manual-page-list))))))


; ------------------------------------------------------------------------------------------
; Functions that transform AST contents in certain SchemeDoc manual fields
; to pure text intead of ASTs and various mixed forms.

(define (manlsp-textual-purification-list manual-page-list)
  (map manlsp-textual-purification-entry manual-page-list))

(define (manlsp-textual-purification-entry manual-entry) 
  (map manlsp-textual-purification-field manual-entry))

; Maybe still incomplete
(define (manlsp-textual-purification-field manual-field)
  (let ((field-name (car manual-field))
        (field-content (cdr manual-field)))
    (cond ((eq? field-name 'attribute-descriptions)  ; plural
             (cons field-name (map manlsp-textual-purification-field field-content)))

          ((eq? field-name 'attribute-description)   ; singular
             (let ((attr-name (first field-content))
                   (attr-descr (second field-content)))
                (list field-name attr-name (xml-render-as-simple-text attr-descr))))

          ((eq? field-name 'parameters)  ; plural
             (cons field-name (map manlsp-textual-purification-field field-content)))

          ((eq? field-name 'parameter)   ; singular
             (let ((par-name (first field-content))
                   (par-descr (second field-content)))
                (list field-name par-name (xml-render-as-simple-text par-descr))))

          ((memq field-name '(description content-model section-body misc)) (cons field-name (list (xml-render-as-simple-text field-content))))

          (else manual-field))))

; ------------------------------------------------------------------------------------------



; ---------------------------------------------------------------------------------------------------
; MANUAL PLAIN STYLE:

(define (plain-manual-title kind title-string)
  (manual-slice-1  
     (manual-plain-css-class "title")
     (span (a 'name title-string) title-string)))

(define (plain-manual-title-level-2 kind title-string)
  (manual-slice-1  
     (manual-plain-css-class "title-level-2")
     (span (a 'name title-string) title-string)))


(define (plain-manual-form  kind form-list)
  (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Form"
    (manual-plain-css-class "form") (as-string form-list)))

(define (plain-manual-pre-condition kind precon)
 (manual-slice-2  (manual-plain-css-class "entry-kind") "Precondition" (manual-plain-css-class "condition") precon))

(define (plain-manual-post-condition kind postcon)
 (manual-slice-2  (manual-plain-css-class "entry-kind") "Postcondition" (manual-plain-css-class "condition") postcon))


(define (plain-manual-description kind descr)
 (manual-slice-2
   (manual-plain-css-class "entry-kind") "Description"
   (manual-plain-css-class "description")
   (if xml-protected-descriptions?
       (let ((description (strip-initial-spaces (xml-render-as-xml descr)))) 
	     (if (empty-string? description) "" (capitalize-string-nd description)))
       descr)))

(define (plain-manual-parameters kind . par-list)
 (if (not (null? par-list))
     (let ((first-par (car par-list))
           (rest-pars (cdr par-list)))
       (cons
         (manual-slice-3-rowspan (length par-list) 
           (manual-plain-css-class "entry-kind") "Parameters"
           (manual-plain-css-class "parameter-name") (manual-parameter-name first-par)
           (manual-plain-css-class "parameter-description") (manual-parameter-description first-par))
         (map 
           (lambda (par)
             (manual-slice-2 
              (manual-plain-css-class "parameter-name") (manual-parameter-name par)
              (manual-plain-css-class "parameter-description") (manual-parameter-description par)))
           rest-pars)))
     '()))

(define manual-parameter-name (make-selector-function 2 "manual-parameter-name"))
(define manual-parameter-description (make-selector-function 3 "manual-parameter-description"))

; Is intended to handle manually authored attribute description, which are not merged with XML DTD info.
; Probably not used ???
(define (plain-manual-attribute-descriptions kind . par-list)
 (map
  (lambda (par)
   (present-element 
        'plain-manual-style
        (element-tag par)
        (element-contents par)))
   par-list))

; Is intended to handle manually authored attribute description, which are not merged with XML DTD info.
; Probably not used ???
(define (plain-manual-attribute-description kind attr-id attr-explanation)
  (list attr-id attr-explanation))


; Handles attributes of ad hoc XML-in-LAML abstractions. 
(define (plain-manual-xml-in-laml-attributes kind . attr-list)
 (if (not (null? attr-list))
     (let ((first-attr (car attr-list))
           (rest-attrs (cdr attr-list))
           (presence-indication (lambda (status)    
                                   (if (equal? status "required") (font-color red (b " *")) "")))
          )
       (cons
         (manual-slice-3-rowspan (length attr-list) 
           (manual-plain-css-class "entry-kind") "Attributes"
           (manual-plain-css-class "attribute-name") (con (manual-xml-in-laml-attribute-name first-attr) (presence-indication (manual-xml-in-laml-attribute-status first-attr)))
           (manual-plain-css-class "attribute-description") (manual-xml-in-laml-attribute-description first-attr)
           (con "Required: " (font-color red (b "*")) )
         )
         (map 
           (lambda (attr)
             (manual-slice-2 
              (manual-plain-css-class "attribute-name") (con (manual-xml-in-laml-attribute-name attr) (presence-indication (manual-xml-in-laml-attribute-status attr)))
              (manual-plain-css-class "attribute-description") (manual-xml-in-laml-attribute-description attr)))
           rest-attrs)))
     '()))

(define manual-xml-in-laml-attribute-name (make-selector-function 2 "manual-xml-in-laml-attribute-name"))
(define manual-xml-in-laml-attribute-status (make-selector-function 3 "manual-xml-in-laml-attribute-status"))
(define manual-xml-in-laml-attribute-description (make-selector-function 4 "manual-xml-in-laml-attribute-description"))

(define (plain-manual-example kind ex)
 (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Example"
    (manual-plain-css-class "example") ex))

(define (plain-manual-examples kind . ex-list)
 (if (not (null? ex-list))
     (let ((first-ex (car ex-list))
           (rest-exs (cdr ex-list)))
       (cons
         (manual-slice-2-rowspan (length ex-list) 
           (manual-plain-css-class "entry-kind") "Examples"
           (manual-plain-css-class "example") (example-text-of first-ex))
         (map 
           (lambda (ex)
             (manual-slice-1
;              (manual-plain-css-class "blank") ""
              (manual-plain-css-class "example") (example-text-of ex)))
           rest-exs)))
     '()))

(define example-text-of (make-selector-function 2 "example-text-of"))


(define (plain-manual-cross-references kind . ref-list)
  (if (not (null? ref-list))
      (let ((first-ref (car ref-list))
            (rest-refs (cdr ref-list)))
        (cons
          (manual-slice-3-rowspan (length ref-list) 
            (manual-plain-css-class "entry-kind") "See also"
            (manual-plain-css-class "reference-role") (manual-reference-role first-ref)
            (manual-plain-css-class "reference-anchor") 
            (if (eq? (manual-reference-kind first-ref) 'reference)
                (a 'class (manual-plain-css-class "external-reference") 
                   'href (manual-reference-url first-ref) (manual-reference-anchor first-ref))
                (separate-list 
                   (map (lambda (r) (a 'class (manual-plain-css-class "internal-reference")
                                       'href (string-append "#" r) r)) (manual-internal-references first-ref)) "")))
                   
          (map 
            (lambda (ref)
              (manual-slice-2 
                (manual-plain-css-class "reference-role") (manual-reference-role ref)
                (manual-plain-css-class "reference-anchor") 
                (if (eq? (manual-reference-kind ref) 'reference)
                    (a 'class (manual-plain-css-class "external-reference")
                       'href (manual-reference-url ref) (manual-reference-anchor ref))
                    (separate-list 
                      (map (lambda (r) (a 'class (manual-plain-css-class "internal-reference")
                                          'href (string-append "#" r) r)) (manual-internal-references ref)) ""))))  
            rest-refs))
       )
      '()))

(define manual-reference-kind (make-selector-function 1 "manual-reference-kind"))
(define manual-reference-role (make-selector-function 2 "manual-reference-role"))

(define manual-reference-anchor (make-selector-function 3 "manual-reference-anchor"))
(define manual-reference-url (make-selector-function 4 "manual-reference-url"))

(define manual-internal-references cddr)


(define (plain-manual-comment kind comment)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Internal remark"
    (manual-plain-css-class "comment") comment))

(define (plain-manual-misc kind m)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Note"
    (manual-plain-css-class "misc") m))

(define (plain-manual-returns kind r)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "Returns"
    (manual-plain-css-class "returns") r))


(define (plain-manual-section-title kind title-string)
  (let ((section-id (next-section-id)) ; increments next-section-number!
        (number next-section-number)
       )
    (tr  'class "manual-section" 
      (td 'class "manual-section" 'colspan "4"  
         (if (empty-string? explicit-section-id) '() (a 'name explicit-section-id))
         (a 'name section-id)

         (manual-section-navigation-banner next-section-number) (br)

         (span 'class "manual-section-number" (as-string number))
         (span 'class "manual-section-title" title-string)))))

(define (plain-manual-section-title-level-2 kind title-string)
    (tr  'class "manual-section" 
      (td 'class "manual-section-level-2" 'colspan "4"  
         (span 'class "manual-section-title-level-2" title-string))))



(define (plain-manual-section-body kind body)
  (tr (td 'colspan "4" 'class "manual-section-description" body)))

; This abstraction returns the empty contribution, and it has a side effect
; on the variable explicit-section-id
(define (plain-manual-section-id kind id)
  (set! explicit-section-id (as-string id))
  explicit-space)


(define (plain-manual-xml-in-laml-example-form kind f)
 (manual-slice-2 
    (manual-plain-css-class "entry-kind") "Example form"
    (manual-plain-css-class "form")
      (cond ((string? f) f)
            (else (as-quoted-string-1 f)))))

(define (plain-manual-content-model kind cm)
 (manual-slice-2
    (manual-plain-css-class "entry-kind") "XML content model"
    (manual-plain-css-class "xml-content-model") cm))

(define (plain-manual-attributes kind . attr-list)
  (if (not (null? attr-list))
     (let* ((sorted-attr-list
               (sort-list attr-list (lambda (x y) (string<=? (second x) (second y)))))
            (attr-list-1 (if (eq? attribute-sorting-kind 'by-attribute-name) sorted-attr-list attr-list))
            (first-attr (car attr-list-1))
            (rest-attrs (cdr attr-list-1)))
       (cons
         (single-attribute-presentation 
            (manual-attribute-name first-attr) (manual-attribute-type first-attr)
            (manual-attribute-presence first-attr) (manual-attribute-description first-attr) (length attr-list-1) )
                                        
         (map 
           (lambda (attr)
             (single-attribute-presentation (manual-attribute-name attr) (manual-attribute-type attr)
                                            (manual-attribute-presence attr) (manual-attribute-description attr) #f))
           rest-attrs)))
     '())
)

(define manual-attribute-name (make-selector-function 2 "manual-attribute-name"))
(define manual-attribute-type (make-selector-function 3 "manual-attribute-type"))
(define manual-attribute-presence (make-selector-function 4 "manual-attribute-presence"))
(define manual-attribute-description (make-selector-function 5 "manual-attribute-description"))

(define (single-attribute-presentation attr-name attr-type attr-presence-default attribute-explanation first-row)  
 (let* (
        (default-marking (lambda (x) (font-color red x)))
        (attribute-type-present 
          (lambda (at)
           (cond ((string? at) 
                    (con
                      at
                      (if (member attr-presence-default (list "#REQUIRED" "#IMPLIED"))
                          ""
                          (con (horizontal-space 5) "(" (font-color red attr-presence-default) ")"))))
                 ((list? at) (con "(" (separate-list (mark-default at attr-presence-default default-marking) " | ") ")"))
                 (else "?????"))))
        (required-attribute? (lambda (ap) (equal? "#REQUIRED" (upcase-string attr-presence-default))))
        (presence-indication 
         (if (required-attribute? attr-presence-default) (font-color red (b " *")) ""))
       )
  (if first-row 
      (manual-slice-4-rowspan first-row
              (manual-plain-css-class "entry-kind") "XML attributes"
              (manual-plain-css-class "attribute-name") (con attr-name presence-indication)
              (manual-plain-css-class "attribute-type") (attribute-type-present attr-type)
              (manual-plain-css-class "attribute-description") attribute-explanation
              (con "Required: " (font-color red (b "*")) (br)
                 "Default values: " (font-color red "red") )
           )
      (manual-slice-3
              (manual-plain-css-class "attribute-name") (con attr-name presence-indication)
              (manual-plain-css-class "attribute-type") (attribute-type-present attr-type)
              (manual-plain-css-class "attribute-description") attribute-explanation
           )
  )))


; Make val in lst with the mark m. Compare elements with equal?
(define (mark-default lst val m)
 (if (not (or (equal? val "#REQUIRED") (equal? val "#IMPLIED")))
     (map (lambda (el) (if (equal? val el) (m el) el)) lst)
     lst))


; Present other (custom tags) in a generic way.
(define (plain-generic-manual-processor kind info)
 (manual-slice-2 
    (manual-plain-css-class "entry-kind") (schemedoc-custom-tag-presentation kind)
    (manual-plain-css-class "custom") info))

; A function which presents a tag of type kind (a symbol).
; This function is supposed to return a string.
; The default return value is (as-string kind).
; SchemeDoc users can redefine this function to make better presentation of custom tag names.
(define (schemedoc-custom-tag-presentation kind)
  (as-string kind))


; ---------------------------------------------------------------------------------------------------
; HEADER PLAIN STYLE:


(define (header-manual-title kind title-string)
  (td 'class (manual-header-css-class "title") (a-tag (string-append "#" title-string) title-string)))

(define (header-manual-form kind form-list)
  (td 'class (manual-header-css-class "form") (as-string form-list)))

(define (header-manual-description kind descr)
  (td 'class (manual-header-css-class "description") 
       (if xml-protected-descriptions?
           (strip-initial-spaces (manual-first-sentence-in-string (xml-render-as-xml descr)))
           descr))) 

(define (header-manual-library kind library)
 (b library))

; ---------------------------------------------------------------------------------------------------
; TABLE OF CONTENTS STYLE:

(define (toc-manual-section-title kind title-string)
  title-string
 ; (a-tag (string-append "#" title-string) title-string)
 )


;; Return the saved name.manlsp manual info. Skip manual meta information. Augment each entry with the library name and path information.
;; In that way we know for each manual entry where it comes from.
;; The second parameter is optional. If provided it gives the directory from which to read the manlsp file.
;; The directory information is relative to the-library directory.
;; If it is not provided, read from the current directory.
(define (read-and-augment-lsp-file name . dir)
  (let ((dir-1 (if (not (null? dir)) (first dir) "")))
   (let ((res (cdr (file-read (string-append the-library dir-1 name ".manlsp")))))   ; cdr: Skip manual meta information.
    (map 
      (lambda (entry) (cons (list 'path dir-1) (cons (list 'library name) entry)))
      res))))



; ;; The column widths of the master index. A list of 4 integers.
; (define master-manual-index-width-list (list 140 70 300 400))
; 
; ;; Return the master index given an augmented manual.
; ;; The augmentation is information about the library of each entry.
; (define (manual-masterindex augmented-manual style)
;   (table-1 1 master-manual-index-width-list (list white white white white)   
;     (map
;       masterindex-present-special
;       (sort-list 
;          (filter manual-page? augmented-manual)  ; disregard manual sections for the index
;           manual-index-order-leq?))))
; 
; (define (masterindex-present-special entry)
;   ; We use this special presentation because we need a non-local reference from the first column.
;   ; Thus we do not use the nice style-based presentation, already prepared for the library entry
;   ; this information cannot be made from the title alone.
;   (let ((title (cadr (assq 'title entry)))
;         (library (cadr (assq 'library entry)))
;         (path (cadr (assq 'path entry)))
;         (form (form-of-entry entry))
;         (description (cadr (assq 'description entry))))
;     (list (master-index-ref title library path) (b library) (as-string form) (manual-first-sentence-in-string description))))
; 
; ; Return the form or xml-in-laml-example form of entry
; (define (form-of-entry entry)
;  (let ((form (get-defaulted-prop 'form entry #f))   ; TOO FEW PARAMETERS!
;        (xml-in-laml-ex-form (get-defaulted-prop 'xml-in-laml-example-form entry #f))
;       )
;   (cond (form form)
;         (xml-in-laml-ex-form xml-in-laml-ex-form)
;         (else "??"))))  
; 
; (define (master-index-ref title library path)
;   (a-tag (string-append path library ".html" "#" title) title))
; 
; 
; ;; NOT USED.
; ;; Write a master index to the file manual-name.html.
; ;; The second parameter is optional. If provided it gives the directory of the resulting file. If not, write in the current directory.
; ;; A master index is a normal index, but in addition it contains a libray column
; ;; Normally the parameters all-entries are made by mapping read-and-augment-lsp-file over the saved lsp files with
; ;; manual information. Next these lists are append accumulated.
; (define (make-master-index all-entries . dir)
;   (let ((dir-1 (if (not (null? dir)) (first dir) ""))
;         (yr (car (time-decode (current-time))))
;        )
;    (write-html '(raw prolog)
;     (html 
;       (head (html:title the-manual-title))
;       (body
;         (left-middle-right-banner (when-generated) (span "Copyright " copyright (as-string yr) "," _ "Kurt Nørmark") (laml-home-button 0 "laml-home.gif"))
;         (h 1 the-manual-title) (p) 
;         (present-author-info the-manual-author) (p)
;         (if (and #f home-url) (con (a-tag home-url "Home") (p)) "")
;         (indent-pixels 12 
;            (con (em "Alphabetic index:") 
;            (manual-masterindex all-entries 'header-manual-style))) (p)
;         (font-1 2 red (when-generated)) (br)
;         (font-1 2 red end-remark)
;        ))
;      (string-append dir-1 (as-string manual-name) ".html"))))


(define (manual-table-of-contents manual style)
 (let ((sections (filter manual-section? (filter page-to-be-presented? manual)))
       (toc-width (accumulate-right + 0 manual-index-width-list)))
  (n-column-list 3
    (map2
      (lambda (manual-section section-number) 
        (let* ((formatted-section (format-manual-page manual-section style style))
               (filtered-section-text (filter (compose not white-space-related?) formatted-section))
               (section-text (car filtered-section-text))
              )
          (a-tag
            (string-append "#" (section-id section-number))
            (font-size 2 (con (as-string section-number) _ ". " section-text))))
      )
      sections
      (number-interval 1 (length sections)))
    toc-width )))

(define (number-of-sections? manual)
  (length (filter manual-section? manual)))

; ---------------------------------------------------------------------------------------------------
; SECTION ID ADMINISTRATION

(define next-section-number 0)

; Return the next section id.
(define (next-section-id)
  (set! next-section-number (+ next-section-number 1))
  (make-section-id next-section-number))

; The numerical section id of section number
(define (make-section-id number)
 (string-append "SECTION" (as-string number)))
  

; Return section id of section number (an integer)
(define (section-id number)
  (string-append "SECTION" (as-string number)))

; ---------------------------------------------------------------------------------------------------
;;; XML-in-LAML support. 
;;; Extraction of manual stuff from a parsed dtd and merging of manual pages.

; DTD selector functions

(define tag-dtd (make-selector-function 1 "tag-dtd"))
(define element-name-dtd (make-selector-function 2 "element-name-dtd"))
(define element-content-model-dtd (make-selector-function 5 "element-content-model-dtd"))
(define attribute-name-dtd (make-selector-function 2 "attribute-name-dtd"))
(define attribute-list-dtd (make-selector-function 3 "attribute-list-dtd"))

; Global variables, assigned by manual-from-parsed-dtd

; The list of parsed elements, in the same order as parsed-dtd-attributes
(define parsed-dtd-elements #f)

; The list of parsed attributes, in the same order as parsed-dtd-elements
(define parsed-dtd-attributes #f)

;; Return a manual list, as extracted from a parsed dtd.
;; .parameter dtd-list-structure the Lisp structure, as generated by the LAML DTD parser.
;; .parameter mirror-name-prefix the prefix of all mirror functions of the XML language. A string. Use the empty string if no explicit name-prefix is used.
(define (manual-from-parsed-dtd dtd-list-structure mirror-name-prefix)
 (let* ((scheme-names (map symbol-of-scheme-knowledge (read-scheme-knowledge 5)))
        (elements   (filter (lambda (x) (eq? 'element (tag-dtd x))) dtd-list-structure))
        (attributes (filter (lambda (x) (eq? 'attribute (tag-dtd x))) dtd-list-structure))

        (filtered-elements (filter (negate (scheme-name-element scheme-names)) elements)) 
        (filtered-attributes (filter (negate (scheme-name-attribute scheme-names)) attributes))

        (sorted-filtered-attributes              ; attributes sorted in the same order as elements
         (sort-list
          filtered-attributes
          (generate-leq (map (compose as-symbol cadr) filtered-elements) (compose as-symbol cadr)))))

  ; Assign the global variables parsed-dtd-elements and parsed-dtd-attributes
  (set! parsed-dtd-elements filtered-elements)
  (set! parsed-dtd-attributes sorted-filtered-attributes)

  ; do the real work..
  (map2 (lambda (el attr) (manual-entry-from-parsed-dtd el attr mirror-name-prefix)) 
        filtered-elements sorted-filtered-attributes)))


(define (manual-entry-from-parsed-dtd el attr mirror-name-prefix)
 (let* ((title-contr (string-append mirror-name-prefix (element-name-dtd el)))
        (content-contr (element-content-model-dtd el))
        (attr-list-contr (attribute-list-dtd attr))
        (attribute-maker (lambda (alc) (cons 'attribute (eliminate-fixed-component alc))))
        (cross-ref-elements (all-elements-using (element-name-dtd el) parsed-dtd-elements))
        (cross-ref-length (length cross-ref-elements))
       )
   (filter (compose not null?)
    (list
     (list 'kind "manual-page")
     (list 'title title-contr)
     (list 'content-model (present-content-model content-contr))
     (cons 'attributes (map attribute-maker attr-list-contr))
     (if (> cross-ref-length 0) 
      (list 'cross-references 
            (cond ((> cross-ref-length 1)
                   (cons 'internal-references 
                         (cons "enclosing elements" cross-ref-elements)))
                  ((= cross-ref-length 1)
                   (cons 'internal-references 
                         (cons "enclosing element" cross-ref-elements)))
                  (else (cons 'internal-references '()))))
      '())
   ))
 )
)

; Return a predicate that checks if an element corresponds to a Scheme name.
(define (scheme-name-element scheme-name-list)
  (lambda (element)
   (turn-into-boolean (memq (as-symbol (element-name-of element)) scheme-name-list))))

; Return a predicate that checks if an attribute corresponds to a Scheme name.
(define (scheme-name-attribute scheme-name-list)
  (lambda (element)
   (turn-into-boolean (memq (as-symbol (attribute-name-of element)) scheme-name-list))))



; Transform elements such as 
; (name ("preserve") "preserve" "#FIXED" "-") to
; (name ("preserve") "preserve" "-").
; Keep element such as 
; (name y #IMPLIED -)
; unchanged.
(define (eliminate-fixed-component attribute-structure)
  (if (and (= (length attribute-structure) 4) (equal? (as-string (fourth attribute-structure)) "#FIXED"))
      (front-sublist attribute-structure 3)
      attribute-structure))

; Unparsing of the parsed content model, as referred by content-contr.
; (define (present-content-model content-contr)
;  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
;         (lenient-string<=? (lambda (x y) (string<=? (as-string x) (as-string y))))
;         (sorter (lambda (lst) (sort-list lst lenient-string<=?)))
;        )
;   (cond ((string? content-contr) content-contr)
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'sequence-with-optionals))
;        (string-append 
;         "(" 
;         (list-to-string (map (lambda (x) (if (symbol? x) (string-append (mr (as-string x)) "?") (mr x))) (cdr content-contr)) ",")
;         ")" ))
; 
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'one-or-more))
;        (string-append 
;         "(" 
;         (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
;         ")+"))
; 
;       ((and (list? content-contr) (not (null? content-contr)) (eq? (car content-contr) 'zero-or-more))
;        (string-append 
;         "(" 
;         (string-append (list-to-string  (map mr (sorter (cdr content-contr))) " | "))
;         ")*"))
; 
;       ((and (symbol? content-contr) (eq? 'pcdata-checker content-contr)) "(#PCDATA)")
; 
;       ((symbol? content-contr) (as-string content-contr))
; 
;       (else (laml-error "present-content-model: unknown kind of content model:" content-contr)))))

; ---------------------------------------------------------------------------------------------------
; Content model selectors. 
; Duplicate code from tools/xml-in-laml/xml-in-laml.scm

(define (empty-element-rhs? parsed-content-model)
  (and (symbol? parsed-content-model) (eq? parsed-content-model 'empty)))

(define (any-element-rhs? parsed-content-model)
  (and (symbol? parsed-content-model) (eq? parsed-content-model 'any)))

(define (mixed-content-rhs? parsed-content-model) ; (mixed-content pcdata)
  (and (list? parsed-content-model) (> (length parsed-content-model) 1)
       (eq? (car parsed-content-model) 'mixed-content)))

(define (mixed-content-pcdata-only-rhs? parsed-content-model) ; THE example: (mixed-content pcdata)
  (and (mixed-content-rhs? parsed-content-model) (= (length parsed-content-model) 2)
       (eq? (cadr parsed-content-model) 'pcdata)))

(define (element-content-rhs? parsed-content-model) ; (mixed-content pcdata)          ; an example: (element-content (seq one (name one "head") (name one "body")))
  (and (list? parsed-content-model) (> (length parsed-content-model) 1)
       (eq? (car parsed-content-model) 'element-content)))

; ---------------------------------------------------------------------------------------------------

; Unparsing of the parsed content model, as referred by content-contr.
; As of March 2003.
(define (present-content-model content-contr)
  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
         (lenient-string<=? (lambda (x y) (string<=? (as-string x) (as-string y))))
         (sorter (lambda (lst) (sort-list lst lenient-string<=?)))
        )
   (cond ((empty-element-rhs? content-contr) "EMPTY")
         ((any-element-rhs? content-contr) "ANY")
         ((mixed-content-pcdata-only-rhs? content-contr) "(#PCDATA)")
         ((mixed-content-rhs? content-contr) 
          (let ((choice-list (cddr (cadr content-contr))))  ; cddr: take #PCDATA for granted.
            (con
             "(#PCDATA |"
             (separate-list (map mr (sorter choice-list)) " | ")
             ")*")))
         ((element-content-rhs? content-contr)
           (let ((content-substance (cadr content-contr)))
             (present-element-content content-substance)))
         (else (laml-error "present-content-model: Unknown form of parsed content model" content-contr)))))

; Recursive unparsing of element-content-forms
(define (present-element-content content-substance)
  (let* ((mr (lambda (name) (a-tag (string-append "#" name) name)))
        )
   (let ((kind (car content-substance))
         (mult (cadr content-substance))
         (body-list (cddr content-substance))
         (mult-unparse (lambda (mult) 
                         (cond ((eq? mult 'one) "")
                               ((eq? mult 'optional) "?")
                               ((eq? mult 'zero-or-more) "*")
                               ((eq? mult 'one-or-more) "+")
                               (else (laml-error "mult-unparse: Unknown kind of multiplicity symbol" mult)))))
         )
     (cond ((eq? kind 'name)
            (let ((name (car body-list)))
              (con (mr (as-string name)) (mult-unparse mult))))
           ((eq? kind 'seq)
            (con "(" (separate-list (map present-element-content body-list) ",") ")" (mult-unparse mult)))
           ((eq? kind 'choice)
            (con "(" (separate-list (map present-element-content body-list) " | ") ")" (mult-unparse mult)))
           ((eq? kind 'empty)        ; does not appear in parsed forms
            "")))))

; ---------------------------------------------------------------------------------------------------


;; Merge the manual page lists man-list-1 and man-list-2.
;; Appart from title, the fields in the two manual lists should be disjoint.
;; The first one controls the ordering of the resulting list.
;; Entries of man-list-2 which do not have a counterpart in man-list-1 are not included in the result.
(define (merge-manual-pages man-list-1 man-list-2)
  (map (lambda (mp1) (merge-manual-page mp1 man-list-2)) man-list-1))

(define (merge-manual-page mp1 man-list-2)
  (let ((other-mp 
         (find-in-list
          (lambda (mp2) 
            (let ((mp2-title (assq 'title mp2))
                  (mp1-title (assq 'title mp1)))
              (if (and mp1-title mp2-title)
                  (equal? (cadr mp2-title) (cadr mp1-title))
                  #f)))
          man-list-2))
        (remove-title-from (lambda (mp-lst) (filter (lambda (e) (not (eq? 'title (car e)))) mp-lst)))
       )
    (if other-mp 
        (append mp1 (remove-title-from other-mp)) 
        mp1)))


; Return a function which extends a manual-entry with field (symbol) and value.
(define (manual-extend field value)
  (lambda (manual-entry)
    (cons (list field value) manual-entry)))

; Return a function which extends a manual-entry with  fields (symbol) and the values in value-lst.
(define (manual-extend-list field value-lst)
  (lambda (manual-entry)
    (cons (cons field valuue-list) manual-entry)))



; ---------------------------------------------------------------------------------------------------
        

; Get property key from the property list.
; In this context a property list is a structure like ((k1 v1) (k2 v2) ... ),
; thus in reality an association list with (k v) instead of (k . v) entries.
(define (get-prop key prop-list)
 (let ((res-list (get key prop-list)))
   (if (and res-list (pair? res-list))
       (car res-list)
       (laml-error "get-prop: not possible to extract" key))))

; A defaulted variant of get-prop
(define (get-defaulted-prop key prop-list default)
 (let ((res-list (defaulted-get key prop-list default)))
   (if (and res-list (pair? res-list))
       (car res-list)
       default)))


; ---------------------------------------------------------------------------------------------------


(define html:title (xhtml10-transitional 'title))
(define html:table (xhtml10-transitional 'table))

(define con-space con)
(define con-par con)

(define (separate-list lst separator)
 (let ((lgt-lst (length lst)))
  (merge-lists-simple lst (make-list (- lgt-lst 1) separator))))



; ---------------------------------------------------------------------------------------------------

; Return a horizontal slize of a manual contribution with one cell.
(define (manual-slice-1 class contribution)
  (tr (td 'colspan "4" 'class class contribution)))

; Return a horizontal slize of a manual contribution with two cells.
; As an optional parameter this function accept an explanation of the left contribution.
(define (manual-slice-2 left-class left-contribution right-class right-contribution . optional-parameter-list)
 (let ((explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 
        left-contribution 
        (if explanation
            (list (br) (span 'class "manual-kind-explanation" explanation))
            '()))
    (td 'colspan "3" 'class right-class right-contribution))))




(define (manual-slice-2-rowspan rowspan left-class left-contribution right-class right-contribution . optional-parameter-list) 
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 'rowspan (as-string rowspan)
        left-contribution 
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class right-class 'colspan "2" right-contribution))))

; Return a horizontal slize of a manual contribution with three cells.
; As an optional parameter this function accept an explanation of the attribute
(define (manual-slice-3 left-class left-contribution middle-class middle-contribution right-class right-contribution . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 
        left-contribution 
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class middle-class middle-contribution)
    (td  'class right-class right-contribution))))


(define (manual-slice-3-rowspan rowspan left-class left-contribution middle-class middle-contribution right-class right-contribution . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class left-class 'rowspan (as-string rowspan)
        left-contribution 
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class middle-class middle-contribution)
    (td  'class right-class 'colspan "2" right-contribution))))


(define (manual-slice-4-rowspan rowspan class-1 contribution-1 class-2 contribution-2 class-3 contribution-3 class-4 contribution-4  . optional-parameter-list)
 (let ((kind-explanation (optional-parameter 1 optional-parameter-list #f)))
  (tr 'class "manual-row"
    (td 'class class-1 'rowspan (as-string rowspan)
        contribution-1
        (if kind-explanation
            (list (br) (span 'class "manual-kind-explanation" kind-explanation))
            '()))
    (td  'class class-2 contribution-2)
    (td  'class class-3 contribution-3)
    (td  'class class-4 contribution-4))))

(define (manual-plain-css-class class-name-suffix)
  (string-append "manual-plain" "-" class-name-suffix))

(define (manual-header-css-class class-name-suffix)
  (string-append "manual-header" "-" class-name-suffix))

; ---------------------------------------------------------------------------------------------------------------

; CSS stylesheet copying for LAML scripts that allows easy global change of stylesheet in the LAML distribution.
; NOT USED directly in the SchemeDoc software, but used for convenient replacement of CSS stylesheets in the LAML
; directory  css-stylesheets/manual/change-manual-stylesheet/
(define (do-copy-css-stylesheet! manual-software-css-pre-filepath manual-software-css-filepath manual-target-css-filepath)
    (write-text-file
     (string-append
      (read-text-file-if-exists manual-software-css-pre-filepath) CR CR
      (read-text-file-if-exists manual-software-css-filepath)
      )
     manual-target-css-filepath
     ))

; ---------------------------------------------------------------------------------------------------------------

; Check that all elements from dtd-page-list is contained in authored-page-section-list.
; Also check that all described elements in authored-page-section-list are in the DTD.
(define (merged-xml-dtd-completeness-control! authored-page-section-list dtd-page-list)
  (let* ((authored-page-list (filter manual-page? authored-page-section-list))
         (authored-element-names (map (lambda (page-str) (car (get 'title page-str))) authored-page-list))
         (dtd-element-names (map (lambda (page-str) (car (get 'title page-str))) dtd-page-list))
         (non-authored-elements (list-difference dtd-element-names authored-element-names string=?))
         (non-dtd-elements      (list-difference authored-element-names dtd-element-names string=?))
        )
    (if (not (null? non-authored-elements))
        (display-warning "The following XML DTD elements are not described:" (list-to-string non-authored-elements ", ")))

    (if (not (null? non-dtd-elements))
        (display-warning "The following documented elements are not in the XML DTD:" (list-to-string non-dtd-elements ", ")))
  )
)

; ---------------------------------------------------------------------------------------------------
; Manual section navigation

(define (manual-section-navigation-banner section-number)
 (let ((title-of-prev "Previous manual section")
       (title-of-next "Next manual section")
       (title-of-up "Manual top")
       (up #t) 
       (prev (> section-number 1))
       (next (< section-number max-section-number))
      )
  (span
    (if up (a 'href "#MANUAL-TOP" (manual-image "small-up.gif" title-of-up)) (manual-image "small-up-blind.gif" "")) 
    
    (if prev 
        (a 'href  (string-append "#" (make-section-id (- section-number 1))) (manual-image "small-prev.gif" title-of-prev))
        (manual-image "small-prev-blind.gif" ""))
 
    (if next
        (a 'href  (string-append "#" (make-section-id (+ section-number 1))) (manual-image "small-next.gif" title-of-next))
        (manual-image "small-next-blind.gif" "")))))

(define (manual-image file-name help-text) (img 'src (string-append (man-image-prefix) file-name) 'title help-text 'alt "" 'border "0"))

; The relative path from the manual directory to the image directory 
; Inside the LAML software packages, manual images are in the images directory of the LAML root directory.
; Manuals outside the LAML directory will hold a man-images subdirectory with a copy of the images.
(define (man-image-prefix)
  (if laml-manual-stuff
      (string-append (laml-dir-prefix the-manual-destination-path) "images/")  ; earlier:  (laml-home-url-prefix 0)
      "man-images/"))


; ---------------------------------------------------------------------------------------------------
; Mirror info presentation

(define (present-mirror-info mirror-info-path)
 (let ((yes-no (lambda (x)
                  (cond ((and (boolean? x) x) "Yes")
                        ((and (boolean? x) (not x)) "No")
                        ((and (string? x) (equal? x "#t")) "Yes")
                        ((and (string? x) (equal? x "#f")) "No")
                        (else (as-string x))))))
  (let ((mirror-info-a-list (if (file-exists? mirror-info-path) (file-read mirror-info-path) '())))
    (indent-pixels 10
      (table 'border "0"
	     (tr (td "Mirror name:") (td (defaulted-get 'mirror-name mirror-info-a-list "?"))) 
	     (tr (td "Automatic loading of common XML-in-LAML library?") (td (yes-no (defaulted-get 'auto-lib-loading mirror-info-a-list "?")))) 
	     (tr (td "List of action elements") (td (as-string (defaulted-get 'action-elements mirror-info-a-list "?")))) 
	     (tr (td "Generation of named mirror functions:") (td (yes-no (defaulted-get 'define-named-mirror-functions? mirror-info-a-list "?"))))
      	     (tr (td "Mirror name prefix:") (td (if (empty-string? (defaulted-get 'mirror-name-prefix mirror-info-a-list "?"))
                                                    (em "None")
                                                    (as-string (defaulted-get 'mirror-name-prefix mirror-info-a-list "?")))))
	     (tr (td "Transliteration of CDATA?") (td (yes-no (defaulted-get 'default-xml-transliterate-character-data? mirror-info-a-list "?"))))
	     (tr (td "List of elements for which transliteration does not apply:") (td (as-string (defaulted-get 'default-xml-non-transliteration-elements mirror-info-a-list "?"))))
	     (tr (td "List of elements for which all white spacing is preserved:") (td (as-string (defaulted-get 'default-xml-preformatted-text-elements mirror-info-a-list "?")))) 
	     (tr (td "Name of HTML character transformation table:") (td (as-string (defaulted-get 'default-xml-char-transformation-table mirror-info-a-list "?"))))            
	     (tr (td "Are default DTD attributes passed explicitly?") (td (yes-no (defaulted-get 'default-pass-default-dtd-attributes? mirror-info-a-list "?"))))
	     (tr (td "Are attributes only allowed to be text strings?") (td (yes-no (defaulted-get 'default-xml-accept-only-string-valued-attributes? mirror-info-a-list "?"))))
	     (tr (td "Is extended textual contents allowed?") (td (yes-no (defaulted-get 'default-xml-accept-extended-contents? mirror-info-a-list "?"))))
	     (tr (td "Is white space represented by this mirror?") (td (yes-no (defaulted-get 'default-xml-represent-white-space mirror-info-a-list "?"))))
	     (tr (td "How are duplicated XML attributes handled:") (td (as-string (defaulted-get 'default-xml-duplicated-attribute-handling mirror-info-a-list "?"))))
                         
	     )))))


(end-laml-loading)
