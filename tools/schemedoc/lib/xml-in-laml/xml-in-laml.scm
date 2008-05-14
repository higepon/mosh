;=>man/xml-in-laml.sdoc

;;;; The XML-in-LAML common parts, which are shared between all XML-in-LAML languages.
;;;; The library is used together with
;;;; <a href="../mirrors/man/xhtml10-transitional-mirror.html">XHTML 1.0 transitional</a>,
;;;; <a href="../mirrors/man/xhtml10-strict-mirror.html">XHTML 1.0 strict</a>,
;;;; <a href="../mirrors/man/xhtml10-frameset-mirror.html">XHTML 1.0 frameset</a>,
;;;; <a href="../mirrors/man/xhtml11-mirror.html">XHTML1.1</a>, 
;;;; <a href="../mirrors/man/svg11-mirror.html">SVG 1.1</a>, 
;;;; <a href="../../../styles/xml-in-laml/lecture-notes/man/lecture-notes.html">LENO</a>, 
;;;; and other similar mirrors.<p>
;;;;
;;;; This library loads <a href="../../man/final-state-automaton.html">the the LAML finite state automation library</a>
;;;; for the sake of full document validation at document generation time.<p>
;;;;
;;;; <a href="../../../tools/xml-in-laml/man/xml-in-laml.html">The XML-in-LAML Mirror Generation tool</a>
;;;; is able to create a set of Scheme mirror functions for a given XML DTD. The generated mirror functions together with
;;;; the shared XML-in-LAML common library (this file) make up the effective mirror of an XML language.<p>
;;;;
;;;; This library is also designed to co-exists with non-validating HTML mirrors, such as
;;;; <a href="../../html4.0-loose/man/surface.html"> Html4.0</a>
;;;; (mainly the sake of LENO).
;;;; However, it <b>cannot</b> be used together with other validating AST-based HTML mirrors, such as 
;;;; <a href="../../html4.01-transitional-validating/man/surface.html"> Html4.01 transitional validating</a>.
;;;; You should use a 100% XML-IN-LAML solution instead. By that we mean a solution, where also the
;;;; HTML stuff is based on XML-in-LAML. In practical terms, it means that you should use one of the XHTML mirrors mentioned above.<p>
;;;; .title Reference Manual of the XML-in-LAML library

(lib-load "final-state-automaton.scm")

;;; XML front matters and end matters stuff.
;;; This section contains the XML declaration and other XML front matter stuff. 
;;; In addition it holds the end-laml function.
;;; .section-id front-matters

;; The standard-prolog function as redefined for XML. It returns the xml-declaration,
;; the document type declaration, and a copyright comment.
;; The standard prolog is the document part before the document root element.
;; A default version of the standard-prolog is found in laml.scm. 
;; The present function relies on another function, xml-document-type-declaration-in, which returns
;; an appropriate document type declaration
;; .form (standard-prolog [language])
;; .internal-references "applied function" "xml-declaration"
;; .internal-references "applied function" "xml-document-type-declaration-in"
;; .reference "applied function" "copyright-clause" "../../../man/laml.html#copyright-clause"
(define (standard-prolog . optional-parameter-list)
 (let* ((language (optional-parameter 1 optional-parameter-list #f))
        (doc-type-decl (xml-document-type-declaration-in language))
       )
  (string-append
   (xml-declaration) (as-string #\newline)
   doc-type-decl
   (if (not (empty-string? doc-type-decl)) (as-string #\newline) "")
   (copyright-clause)
   (if (not (empty-string? (copyright-clause))) (as-string #\newline) ""))))

;; The version of XML used together with LAML. 
;; A string.
(define laml-xml-version "1.0")

;; The character encoding used together with XML in LAML.  Goes into the xml declaration.
;; A string.
(define laml-character-encoding "iso-8859-1")   ; Alternative: "utf-8"

;; Return the xml declaration
(define (xml-declaration)
  (string-append "<?xml version=" (string-it laml-xml-version) 
                 " "
                 "encoding" "=" (string-it laml-character-encoding) 
                 "?>"))

;; A redefinition of end-laml from the fundamental LAML library.
;; This redefined function calls the function check-id-and-idref-attributes!, which reports on
;; possible violations of ID and IDREF(S) attribute constraints.
;; It also checks relative and absolute links, if desired.
;; Calls the original end-laml function as the last action. 
;; .reference "original end-laml" "laml.scm" "../../../man/laml.html#end-laml"
;; .internal-references "ID and IDREF check function" "check-id-and-idref-attributes!"
;; .internal-references "Control of link checking" "xml-link-checking"
(define (end-laml)
  ; Check for XML constraints of ID and IDREF(S) attributes:

  (check-id-and-idref-attributes!)

  ; Do the final part of link checking - first relative links:
  (if (memq xml-link-checking (list 'all 'relative-urls))
      (if (> (length relative-url-list-for-later-checking) 0)
          (begin
            (display-message "Checking" (length relative-url-list-for-later-checking) "relative links...")
            (check-relative-url-list! relative-url-list-for-later-checking)
            (if (= 0 relative-url-problem-count) (display-message "All relative links are OK"))
            (set! relative-url-list-for-later-checking '())
            (set! relative-url-problem-count 0)
            )
          (display-message "No relative links to check"))
  )

  ; Next absolute links:
  (if (and (memq xml-link-checking (list 'all 'absolute-urls)) (> (length absolute-url-list-for-later-checking) 0))
      (begin
         (display-message "Checking" (length absolute-url-list-for-later-checking) "absolute links...")
         (check-absolute-url-list! absolute-url-list-for-later-checking)
         (if (= 0 absolute-url-problem-count) (display-message "All absolute links are OK"))
         (set! absolute-url-list-for-later-checking '())
         (set! absolute-url-problem-count 0)
      )
  )
 
  (original-end-laml))
  

;;; XML-in-LAML processing parameters.
;;; The variables and functions in this part control a number of general properties of XML-in-LAML processing. Some of these
;;; are generic for all XML-in-LAML languages; Others are specific to a single language.
;;; A number of the properties are related to the checking, validation, and error reporting of the XML mirror functions.
;;; .section-id processing-par

;; A variable that determines the internal representation used for XML-in-LAML.
;; Possible values: laml and sxml.
;; The value laml implies usage of the original LAML ASTs.
;; The value sxml implies usage of the SXML list representation of XML. 
;; Use of the value sxml is still somewhat experimental.
;; If the value of this variable is changed, the xml-in-laml library must be reloaded.
(define laml-internal-representation 'laml) 


;; A boolean variable which controls the checking of element attributes.
;; If true, check the element attributes against the attributes as defined in the DTD file.
;; This variable is generic and common for all XML-in-LAML languages.
;; The default value is true.
(define xml-check-attributes? #t)

;; A boolean variable that controls the XML validation.
;; If true, do validate the generated XML document against the DTD.
;; This variable is generic and common for all XML-in-LAML languages.
;; The default value is true.
(define xml-validate-contents? #t)

;; Defines the amount of link checing that is to be carried out by LAML.
;; Possible values are none, relative-urls, absolute-urls, and all (symbols),
;; none: No link checking is done at alle.
;; relative-urls: Only relative urls are checked.
;; absolute-urls: Only absolute urls are checked. This includes URLs that are formed relative to a given base-url.
;; all: All URLs, both absolute and relative, are checked. 
;; The default value is relative-urls.
;; The use of the value all requires that url-target-exists? target is implemented. 
;; In the current version of LAML, url-target-exists? is only implemented when LAML is used with MzScheme
;; (via the lib/url-read.scm).
;; .internal-references "link checking functions" "collect-links-for-later-checking-in-ast!" "check-relative-url-list!" "check-absolute-url-list!"
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define xml-link-checking 'relative-urls) 

;; A boolean variable that controls the check of XML language overlap.
;; If true, check that no elements are used via ambiguous simple names.
;; Quite naturally, this variable is generic and common for XML-in-LAML loaded at the same time. 
(define xml-check-language-overlap? #f)


;; Return if CDATA  (Character data) is transformed through an HTML/XML character transformation table. 
;; The value is boolean, and normally true.
;; .internal-references "mutator" "set-xml-transliterate-character-data-in"
;; .reference "transformation table" "laml.scm" "../../../man/laml.html#html-char-transformation-table"
(define (xml-transliterate-character-data-in? language)
  (assert-known-xml-language language "xml-transliterate-character-data-in?")
  (eval-cur-env (aggregated-variable (as-string language) "xml-transliterate-character-data?")))

;; Ask for transliteration of all CDATA characters via an HTML/XML character transformation table. 
;; new-value must be boolean.
;; .internal-references "selector" "xml-transliterate-character-data-in?"
;; .internal-references "table getter" "xml-char-transformation-table-in"
;; .internal-references "table setter" "set-xml-char-transformation-table-in"
;; .internal-references "exceptions" "xml-non-transliteration-elements-in"
;; .reference "transformation table" "laml.scm" "../../../man/laml.html#html-char-transformation-table"
;; .parameter language The name of the XML-in-LAML language - see the first few lines of relevant mirror library manuals - a symbol.
;; .parameter new-value A boolean value.
;; .comment Consider macro implementation.
(define (set-xml-transliterate-character-data-in language new-value)
  (assert-known-xml-language language "set-xml-transliterate-character-data-in")
  (eval-cur-env (list 'set! (aggregated-variable (as-string language) "xml-transliterate-character-data?") new-value)))

;; Return the HTML/XML character transformation table used for language.
;; .returns A character transformation table.
;; .reference "transformation table" "laml.scm" "../../../man/laml.html#html-char-transformation-table"
;; .internal-references "table setter" "set-xml-char-transformation-table-in"
;; .internal-references "exceptions" "xml-non-transliteration-elements-in"
(define (xml-char-transformation-table-in language)
  (assert-known-xml-language language "xml-char-transformation-table-in")
  (eval-cur-env (aggregated-variable (as-string language) "xml-char-transformation-table")))

;; Set the HTML/XML character transformation table used for language.
;; The value must be a character transformation table. 
;; .internal-references "table getter" "xml-char-transformation-table-in"
;; .internal-references "exceptions" "xml-non-transliteration-elements-in"
;; .reference "transformation table" "laml.scm" "../../../man/laml.html#html-char-transformation-table"
;; .comment  Consider macro implementation.
;; .parameter language The name of the XML-in-LAML language - see the first few lines of relevant mirror library manuals - a symbol.
;; .parameter new-value A character transformation table.
(define (set-xml-char-transformation-table-in language new-value)
  (assert-known-xml-language language "set-xml-char-transformation-table-in")
  (eval-cur-env (list 'set! (aggregated-variable (as-string language) "xml-char-transformation-table") new-value)))



;; Return the list of element names, for which we do not carry out the character transliteration in language.
;; In XHTML, the list typically include the elements style and script.
;; The value is a list of strings.
;; The value of this function is locked and bound at mirror generation time.
;; .internal-references "transliteration" "xml-transliterate-character-data-in?"
(define (xml-non-transliteration-elements-in language)
  (assert-known-xml-language language "xml-non-transliteration-elements-in")
  (eval-cur-env (aggregated-variable (as-string language) "xml-non-transliteration-elements")))

;; Return the list of element names, for which we consistenly render all white space content characters as given in the input.
;; In XHTML, the list typically include the pre element.
;; The value is a list of strings.
;; The value of this function is locked and bound at mirror generation time.
(define (xml-preformatted-text-elements-in language)
  (assert-known-xml-language language "xml-preformatted-text-elements-in")
  (eval-cur-env (aggregated-variable (as-string language) "xml-preformatted-text-elements")))



;; How many characters of the validation error messages to be presented.
;; If you want longer error messages, just increase the value of this variable.
;; The default value is currently 130.
(define xml-error-truncation-length 130)


;; Controls whether the default attribute values, as specified in the DTD, are passed explicitly as attribute values
;; in every instance of the element. 
;; The value is boolean, and normally false. 
;; A true value in most cases give problems relative to the XML attribute validation.
;; The value of this function is locked and bound at mirror generation time.
;; .internal-references "validation control" "xml-check-attributes?"
(define (xml-pass-default-dtd-attributes-in? language)
  (assert-known-xml-language language "xml-pass-default-dtd-attributes-in?")
  (eval-cur-env (aggregated-variable (as-string language) "xml-pass-default-dtd-attributes?")))


;; A boolean variable that controls how rigid LAML handles attribute values in language (first parameter).
;; If this function returns #t, only strings are allowed as attribute values. 
;; If it returns #f, LAML will attempt to convert the attribute value to a string using the function as-string.
;; (Certain lists, which internally represents ASTs etc) are not converted, however). 
;; If no language information is present (i.e., if the language parameter is #f) always return #t. 
;; .internal-references "setter" "set-xml-accept-only-string-valued-attributes-in"
;; .parameter language The name of an XML language (a symbol)
;; .misc The default value returned by this function is defined as a parameter of the mirror generation script.
(define (xml-accept-only-string-valued-attributes-in? language)
  (if language
      (begin
        (assert-known-xml-language language "xml-accept-only-string-valued-attributes-in?")
        (eval-cur-env (aggregated-variable (as-string language) "xml-accept-only-string-valued-attributes?"))
      )
      #t))

;; Controls the handling of LAML attribute values in language.
;; If new-value is true, LAML only accept string valued XML attribute.
;; If new-value is false, LAML string converts whatever follows an attribute symbol.
;; The value must be a boolean value. True is recommended.
;; .internal-references "getter" "xml-accept-only-string-valued-attributes-in?"
;; .comment Consider macro implementation.
;; .parameter language The name of the XML-in-LAML language - see the first few lines of relevant mirror library manuals - a symbol.
;; .parameter new-value A boolean value.
;; .misc Overwrites the default value, as defined by mirror generation script.
(define (set-xml-accept-only-string-valued-attributes-in language new-value)
  (assert-known-xml-language language "set-xml-accept-only-string-valued-attributes-in")
  (eval-cur-env (list 'set! (aggregated-variable (as-string language) "xml-accept-only-string-valued-attributes?") new-value)))


;; A boolean variable that controls how rigid LAML handles element contents items.
;; If the value returned is #t, numbers and characters are accepted as element content items in addition to
;; strings, ASTs, and character references.
;; If no language information is present (i.e., if the language parameter is #f) always return #f. 
;; .internal-references "setter" "set-xml-accept-extended-contents-in"
;; .parameter language The name of an XML language (a symbol)
;; .misc The default value returned by this function is defined as a parameter of the mirror generation script.
(define (xml-accept-extended-contents-in? language)
  (if language
      (begin
        (assert-known-xml-language language "xml-accept-extended-contents-in?")
        (eval-cur-env (aggregated-variable (as-string language) "xml-accept-extended-contents?"))
      )
      #f))

;; Controls the handling of LAML element contents items in language.
;; If new-value is true, LAML accepts characters and numbers of contents items in addition to strings, ASTs and character references.
;; If new-value is false, LAML only accept strings, ASTs, and character references as element contents items.
;; The value must be a boolean value. 
;; .internal-references "getter" "xml-accept-extended-contents-in?"
;; .parameter language The name of the XML-in-LAML language - see the first few lines of relevant mirror library manuals - a symbol.
;; .parameter new-value A boolean value.
;; .misc Overwrites the default value, as defined by mirror generation script.
(define (set-xml-accept-extended-contents-in language new-value)
  (assert-known-xml-language language "set-xml-accept-extended-contents-in")
  (eval-cur-env (list 'set! (aggregated-variable (as-string language) "xml-accept-extended-contents?") new-value)))



;; Return the document type declaration of language.
;; The empty string signals that no document type declaration is available.
;; The value of this function is locked and bound at mirror generation time.
(define (xml-document-type-declaration-in language)
  (if language
      (begin
        (assert-known-xml-language language "xml-document-type-declaration-in")
        (eval-cur-env (aggregated-variable (as-string language) "xml-document-type-declaration")))
      ""))


;; This value controls the representation of white space in the internal AST representation of a document.
;; If true, white space markers are inserted. If false, a clean AST without any white space marking is produced.
;; In some languages, white space handling is essential. This is the case in XHTML.
;; In XML languages that are not related to presentation at all, white space handling is not relevant.
;; The value of this function is locked and bound at mirror generation time.
(define (xml-represent-white-space-in? language)
  (if language
      (begin
        (assert-known-xml-language language "xml-represent-white-space-in?")
        (eval-cur-env (aggregated-variable (as-string language) "xml-represent-white-space?"))
      )
      #t))

;; This value controls the handling of attributes that occur more than once in an XML attribute list.
;; If the function returns keep-all, the attribute list is not affected at all; All attributes are passed to the application.
;; If the function returns keep-first, only the first attribute name/value pair is passed for further processing, 
;; and only the first is passed on to the validation procedure.
;; If the function returns keep-last, only the last attribute name/value pair is passed for further processing,
;; and only the last is passed on to the validation procedure.
;; The value of this function is locked and bound at mirror generation time.
;; .returns either keep-all, keep-first, keep-last (a symbol)
(define (xml-duplicated-attribute-handling language)
  (if language
      (begin
        (assert-known-xml-language language "xml-duplicated-attribute-handling")
        (eval-cur-env (aggregated-variable (as-string language) "xml-duplicated-attribute-handling"))
      )
      'keep-all))


; Form a variable of language-string variable-string and return the symbol.
(define (aggregated-variable language-string variable-string)
  (as-symbol (string-append language-string "-" variable-string)))

; Assert that language is legal relative to the XML-in-LAML languages in use.
; The optional second parameter is intended to hold the operation causing the problems.
; If not, stop the program (fatal error)
(define (assert-known-xml-language language . optional-parameter-list)
 (let ((context (optional-parameter 1 optional-parameter-list #f)))
  (if (not (symbol? language))
      (laml-error (if context (string-append (as-string context) ":") "") "The XML-in-LAML language must be given as a symbol:" language))
  (if (not (memq language (languages-in-use)))
      (laml-error (if context (string-append (as-string context) ":" (as-string #\newline)) "")
                  "Fatal error: The language" language 
                  "is not among the currently loaded XML-in-LAML languages:"
                  (list-to-string (map as-string (languages-in-use)) ", ")))))


; A map from XML language names (symbols) to lists of url-extractor and base-url-extractor functions.
(define xml-link-checking-map '())

;; Register url-extractor-function and base-url-extractor-function in the XML link checking map of xml-language.
;; The XML link checking map maps an XML language name to a list of two functions: url-extractor-function base-url-extractor-function.
;; The url-extractor-function can be applied on an XML AST in order to deliver a list of URLs that appear in that language.
;; The base-url-extractor-function can also be applied on an XML AST. It delivers a possible base url (for use when resolving relative URLs) of the AST, of #f in no such base URL occurs.
;; .parameter xml-language The name of the XML language (a symbol).
;; .parameter url-extractor-function A URL extraction function (takes an AST parameter), or #f if no such function is provided.
;; .parameter base-url-extractor-function A base URL extraction function (takes an AST parameter), or #f if no such function is provided.
(define (set-xml-link-checking-functions xml-language url-extractor-function base-url-extractor-function)
  (set! xml-link-checking-map 
        (cons (cons xml-language (list url-extractor-function base-url-extractor-function))
              xml-link-checking-map)))

;; Return the registered url-extractor function of xml-language.
;; The url-extractor function can be applied on an XML-ast and it delivers a list of URLs that occur in the AST.
;; .internal-references "applied by" "collect-links-for-later-checking-in-ast!"
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (url-extractor-of-xml-language xml-language)
  (let ((res (assq xml-language xml-link-checking-map)))
    (if res
        (first (cdr res))
        #f)))

;; Return the registered base-url-extractor function of xml-language.
;; The base-url-extractor function can be applied on an XML-ast and it delivers a possible base url (for use when resolving relative URLs) in the AST.
;; .internal-references "applied by" "collect-links-for-later-checking-in-ast!"
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (base-url-extractor-of-xml-language xml-language)
  (let ((res (assq xml-language xml-link-checking-map)))
    (if res
        (second (cdr res))
        #f)))

;;; Other Constants.
;;; .section-id other-const

;; The explicit white space value, as used internally in ast. The default value is boolean #t.
(define explicit-space #t)

;; The explicit white space suppress value. The default value is boolean #f.
(define explicit-space-suppress #f)

;; The symbolic name of explicit white space suppress.
(define _ explicit-space-suppress)

;; An integer that expresses the prefered maximum column width for pretty printed ouput.
(define preferred-maximum-width 90)

;; An integer which gives the level of indentation for pretty printed output.
(define indentation-delta 3)

;; Controls the rendering of white space in the textual contents.
;; If #t, white space characters are rendered exactly as they are present in the textual content.
;; If #f, white space is eliminated to the minimal amount when rendering.
;; In normal use, always use #f. In situations where HTML formatting (especially PRE) is present as textual elements, it may be useful to use #t.
;; In this context, notice the concept of preformatted text elements, as returned by the function xml-preformatted-text-elements-in.
;; .internal-references "relevant function" "xml-preformatted-text-elements-in"
(define xml-always-render-white-space? #f)


; ---------------------------------------------------------------------------------------------------
;;; AST constructors and basic selector selectors.
;;; In this section we document the functions that work on abstract syntax trees (ASTs).
;;; More specifically, we describe the AST constructor function and the most basic functions that
;;; select a constituent of an AST.
;;; Abstract syntax trees make up the internal representation of XML documents in LAML.
;;; All XML mirror functions return abstract syntax trees, and as such they can be thought as
;;; convenient, high level AST constructors. Notice that the implementation of the functions in this
;;; section depend heavily on the value of the variable laml-internal-representation.
;;; .section-id ast-cons-sel

; ----------------------------------------------------------------------------------------
; The original LAML internal representation.

(define (laml-make-ast element-name contents attributes kind language . optional-parameter-list)
 (let ((internal-attributes (optional-parameter 1 optional-parameter-list '()))
       (subtrees (cond ((ast? contents) (list contents))
                       ((cdata? contents) (list contents))
                       ((forced-white-space? contents) (list contents))
                       ((delayed-procedural-contents-element? contents) (list contents))
                       ((char-ref? contents) (list contents))
                       ((xml-comment? contents) (list contents))
                       ((cdata-section? contents) (list contents))
                       ((processing-instruction? contents) (list contents))
                       ((list? contents) contents)
                       (else (laml-error "make-ast: Contents must be a single content item or a list of these: " 
                                         (as-string contents))))))
  (list 'ast (as-string element-name) subtrees attributes (as-symbol kind) (as-symbol language) internal-attributes)))

(define laml-ast-element-name (make-selector-function 2 "ast-element-name"))

(define laml-ast-subtrees (make-selector-function 3 "ast-subtrees"))

(define laml-ast-attributes (make-selector-function 4 "ast-attributes"))

(define laml-ast-kind (make-selector-function 5 "ast-kind"))

(define laml-ast-language (make-selector-function 6 "ast-language"))

(define laml-ast-internal-attributes (make-selector-function 7 "ast-internal-attributes"))


; ----------------------------------------------------------------------------------------
; The SXML internal representation.

(define (sxml-make-ast element-name contents attributes-proplist kind language . optional-parameter-list)
 (let ((internal-attributes (optional-parameter 1 optional-parameter-list '()))
       (subtrees (cond ((ast? contents) (list contents))
                       ((cdata? contents) (list contents))
                       ((forced-white-space? contents) (list contents))
                       ((delayed-procedural-contents-element? contents) (list contents))
                       ((char-ref? contents) (list contents))
                       ((xml-comment? contents) (list contents))
                       ((cdata-section? contents) (list contents))
                       ((processing-instruction? contents) (list contents))
                       ((list? contents) contents)
                       (else (laml-error "make-ast: Contents must be ast, cdata, forced white space, or a list of these: " 
                                         (as-string contents))))))
  (cons (as-symbol element-name)
   (cons (sxml-attributes attributes-proplist)
    (cons (sxml-aux-list kind language)
      subtrees)))))


(define (sxml-attributes attributes-proplist)
 (let ((attributes-alist (propertylist-to-alist attributes-proplist)))
  (cons '@ 
    (map
     (lambda (aname-aval-pair) (list (as-symbol (car aname-aval-pair)) (as-string (cdr aname-aval-pair))))
     attributes-alist))))

(define (sxml-aux-list kind language)
  (cons '@@
   (cons (list '*NAMESPACES*)
    (cons (list 'element-kind kind)
     (cons (list 'language language) '())))))

(define sxml-ast-element-name (compose as-string car))

(define (sxml-ast-subtrees ast)
 (cond ((null? (cdr ast))
           '())
       ((not (sxml-attribute-or-aux-related? (cadr ast)))   ; at least two elements
          (cdr ast))
       ((null? (cddr ast))                                  
           '())
      ((not (sxml-attribute-or-aux-related? (caddr ast)))   ; at least three elements
          (cddr ast))
       (else (cdddr ast))))

(define (sxml-attribute-or-aux-related? x)
  (or (sxml-attribute-related? x) (sxml-aux-related? x)))

(define (sxml-attribute-related? x)
  (and (pair? x) (eq? (car x) '@)))

(define (sxml-aux-related? x)
  (and (pair? x) (eq? (car x) '@@)))


(define (sxml-ast-attributes ast)
  ; if there are attributes in x, it will have to be the second element.
  (if (null? (cdr ast))
      '()
      (if (sxml-attribute-related? (cadr ast))
          (let ((attribute-pair-list (cdr (cadr ast))))
            (flatten attribute-pair-list))
          '())))

; Return the kind, single or double of ast.
; This function first attempt to extract the information from the aux list of the AST node.
; Second, the function tries to look up the information in the content model of XML element.
; If all this fails, it returns double.
(define (sxml-ast-kind ast)
  (let* ((aux-constituent (sxml-aux-constituent-of-ast ast))) ; a structure of the form (X (*NAMESPACES*) ...) where X is two at characters
    (if aux-constituent
        (let ((candidate (defaulted-get 'element-kind (cdr aux-constituent) #f)))
          (if candidate
              (car candidate)
              (let* ((aux-language (defaulted-get 'language (cdr aux-constituent) #f))
                     (language (if aux-language aux-language (ast-language ast)))
                    )
                (if language
                    (let ((content-model (content-model-of (ast-element-name ast) language)))
                      (if (eq? content-model 'empty) 'single 'double))
                    'double))))
        (let ((language (ast-language ast)))
          (if language
              (let ((content-model (content-model-of (ast-element-name ast) language)))
                (if (eq? content-model 'empty) 'single 'double))
              'double)))))

; Return the XML language used by AST.
; If no registration exists in the SXML aux list, return the earliest registered xml language of the
; current LAML document (corresponding the language whose mirror is loaded first).
(define (sxml-ast-language ast)
  (let* ((default-language (last (languages-in-use)))
         (aux-constituent (sxml-aux-constituent-of-ast ast))  ; a structure of the form (X (*NAMESPACES*) ...) where X is two at characters
        )
    (if aux-constituent
        (let ((candidate (defaulted-get 'language (cdr aux-constituent) #f)))
          (if candidate
              (car candidate)
              default-language
          ))
        default-language)))

; Return the internal attributes of AST.
(define (sxml-ast-internal-attributes ast)
  (laml-error "sxml-ast-internal-attributes: Not yet supported"))

; Return the aux constituent of ast (either second or third element), 
; or #f if it does not exist.
(define (sxml-aux-constituent-of-ast ast)
  (cond ((null? (cdr ast))
           #f)
        ((sxml-aux-related? (cadr ast))   ; at least two elements
           (cadr ast))
        ((null? (cddr ast))
           #f)
        ((sxml-aux-related? (caddr ast))  ; at least three elements
          (caddr ast))
        (else #f)))


; ----------------------------------------------------------------------------------------


; A boolean variable that controls whether or not we check the internal soundness of ASTs.
; If the basic mirror is used directly, it makes good sense to set this variable to #t.
; Else we recommend a #f value.
(define check-ast-constituents? #f)

;; Make an AST given element-name, contents, a property list of attributes, a kind (single/double),
;; and an XML language name. 
;; The parameter contents may be a list of subtrees. It may also be an element which satisfies
;; either ast?, cdata?, forced-white-space?, delayed-procedural-contents-element?, char-ref?,
;; xml-comment?, cdata-section?, or processing-instruction? In these cases, we embed the single element into a list.
;; attributes is the list of attributes on property list format (such as '(a1 "v1" a2 "v2")). 
;; attribute names are always symbols, and attribute values are normally strings.
;; .parameter element-name A symbol or a string.
;; .parameter contents A list of contents in term of CDATA (strings), white space markers, or other ASTs.
;; .parameter attributes The XML attributes represented as a property list.
;; .parameter kind The symbol double or single. The kind single represents empty XML elements.
;; .parameter language A symbol or string that represent the XML-in-LAML language name.
;; .parameter internal-attributes A collection of internal attributes represented as a property list. Internal attributes can be used as arbitrary annotations of an XML document. Internal attributes do not affect the document rendering. Use internal attributes for AST annotations that cannot easily be made via use of XML attributes. 
;; .form (make-ast element-name contents attributes kind language [internal-attributes])
(define make-ast
  (cond ((eq? laml-internal-representation 'laml) laml-make-ast)
        ((eq? laml-internal-representation 'sxml) sxml-make-ast)
        (else (laml-error "make-ast: Unknown value of laml-internal-representation:" laml-internal-representation))))



; AST selectors.

;; Return the root element name of ast. 
;; The type of the returned value is a string.
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-element-name ast)
(define ast-element-name 
  (cond ((eq? laml-internal-representation 'laml) laml-ast-element-name)
        ((eq? laml-internal-representation 'sxml) sxml-ast-element-name)
        (else (laml-error "ast-element-name: Unknown value of laml-internal-representation:" laml-internal-representation))))

;; Return the list of substrees of ast.  This is also known as the contents of the ast. It is a list of CDATA (strings), ASTs, and white space markers.
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-subtrees ast)
;; .internal-references "similar function" "ast-subtree"
(define ast-subtrees 
  (cond ((eq? laml-internal-representation 'laml) laml-ast-subtrees )
        ((eq? laml-internal-representation 'sxml) sxml-ast-subtrees )
        (else (laml-error "ast-subtrees : Unknown value of laml-internal-representation:" laml-internal-representation))))


;; Return the list of attributes of the ast.
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-attributes ast)
;; .misc The functions get-prop and defaulted-get-prop are useful for accessing the individual attribute values in the returned property list.
;; .reference "Relevant function" "get-prop" "../../man/general.html#get-prop"
;; .reference "Relevant function" "defaulted-get-prop" "../../man/general.html#defaulted-get-prop"
;; .returns The list of attributes, as a property list.
;; .internal-references "high-level selector" "ast-attribute"
;; .internal-references "high-order selector" "attribute-getter"
(define ast-attributes
  (cond ((eq? laml-internal-representation 'laml) laml-ast-attributes)
        ((eq? laml-internal-representation 'sxml) sxml-ast-attributes)
        (else (laml-error "ast-attributes : Unknown value of laml-internal-representation:" laml-internal-representation))))


;; Return the kind of the ast. 
;; The type of the returned value is a symbol, either single or double.
;; ASTs of type single are also known as empty elements.
;; Tells whether the ast is to be rendered as a single or double tag.
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-kind ast)
(define ast-kind 
  (cond ((eq? laml-internal-representation 'laml) laml-ast-kind)
        ((eq? laml-internal-representation 'sxml) sxml-ast-kind)
        (else (laml-error "ast-kind : Unknown value of laml-internal-representation:" laml-internal-representation))))


;; Return the language of the ast.
;; The type of the returned value is a symbol.
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-language ast)
(define ast-language 
  (cond ((eq? laml-internal-representation 'laml) laml-ast-language)
        ((eq? laml-internal-representation 'sxml) sxml-ast-language)
        (else (laml-error "ast-language  : Unknown value of laml-internal-representation:" laml-internal-representation))))

;; Return the list of internal attributes of the ast. These are non XML attributes which can annotate the ast. 
;; .pre-condition ast is an abstract syntax tree that satisfies the ast? predicate
;; .form (ast-internal-attributes ast)
;; .returns The list of internal attributes, as a property list.
;; .internal-references "internal attribute mutators" "ast-mutators"
;; .internal-references "convenient accessors" "ast-internal-attribute" "has-internal-ast-attribute?"
(define ast-internal-attributes
  (cond ((eq? laml-internal-representation 'laml) laml-ast-internal-attributes)
        ((eq? laml-internal-representation 'sxml) sxml-ast-internal-attributes)
        (else (laml-error "ast-internal-attributes : Unknown value of laml-internal-representation:" laml-internal-representation))))



;;; High-level AST selector functions.
;;; In this section we describe a set of more elaborate functions that access the constituents of an AST.
;;; In many contexts, the functions in this section are more convenient than the basic selector functions
;;; from the previous section.
;;; .section-id highlevel-ast-sel

;; Return a specific subtree of ast, namely the n'th subtree with element-name el-name.
;; If no such subtree exists, return #f.
;; The first subtree of a given name counts as number 1 (not 0).
;; .pre-condition n > 0
;; .returns A subtree of AST (if located) or #f
;; .parameter ast The ast in which to locate a subtree
;; .parameter el-name The ast element name of a subtree of ast (string or symbol)
;; .parameter n The constituent number regarding el-name subtrees (an integer). Defaults to 1.
;; .form (ast-subtree ast el-name [n])
;; .internal-references "basic selector" "ast-subtrees"
;; .internal-references "generalized accessors" "traverse-and-collect-all-from-ast" "traverse-and-collect-first-from-ast"
(define (ast-subtree ast el-name . optional-parameter-list)
 (let ((n (optional-parameter 1 optional-parameter-list 1))
       (subtrees (ast-subtrees ast)))
  (sub-ast-1 subtrees (as-string el-name) n)))

(define (sub-ast-1 subtree-list el-name n)
 (cond ((null? subtree-list) #f)
       ((and (and (ast? (car subtree-list)) (equal? el-name (ast-element-name (car subtree-list)))) (= n 1))
         (car subtree-list))
       ((and (and (ast? (car subtree-list)) (equal? el-name (ast-element-name (car subtree-list)))) (> n 1))
         (sub-ast-1 (cdr subtree-list) el-name (- n 1)))
       (else (sub-ast-1 (cdr subtree-list) el-name n))))

        

;; Return the value of the attribute name in ast, or #f if no such attribute exists. Only the attributes of ast are considered.
;; This function is a convenient shortcut of (defauted-get-prop name (ast-attributes ast) default-value).
;; The optional parameter serves as the attribute value in case no name attribute is found in
;; the attribute list of ast. 
;; .form (ast-attribute ast name [default-attribute-value])
;; .parameter ast An AST.
;; .parameter name The name of an attribute (a symbol).
;; .parameter default-attribute-value The default value, used if no attribute of name is found. A string.
;; .internal-references "basic selector" "ast-attributes"
;; .internal-references "related function" "unique-ast-attribute"
(define (ast-attribute ast name . optional-parameter-list)
 (let ((default-attribute-value (optional-parameter 1 optional-parameter-list #f)))
   (defaulted-get-prop name (ast-attributes ast) default-attribute-value)))



;; A higher-order function which returns an attribute getter function on ASTs.
;; The returned function takes an AST as parameter.
;; .parameter attribute-name the attribute name - a symbol.
;; .parameter default-value the default value returned if there is no attribute named attribute-name in the attribute-list.
;; .form (attribute-getter attribute-name [default-value])
;; .internal-references "lower-order function" "ast-attribute"
(define (attribute-getter attribute-name . optional-parameter-list)
 (let ((default-value (optional-parameter 1 optional-parameter-list #f)))
   (if default-value
    (lambda (ast)
      (let ((attribute-plist (ast-attributes ast)))
        (defaulted-get-prop attribute-name attribute-plist default-value)))
    (lambda (ast)
      (let ((attribute-plist (ast-attributes ast)))
        (get-prop attribute-name attribute-plist))))))



; Return a string of the aggregated CDATA contents of ast.
; White space markers are taken into account, but ast subtrees of ast are ignored in this aggregation.
; All other AST constituents are ignored.
; .internal-references "alias" "ast-text"
(define (aggregated-ast-cdata-contents ast)
  (aggregated-ast-cdata-contents-1 (ast-subtrees ast) ""))

(define (aggregated-ast-cdata-contents-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list)) 
           (aggregated-ast-cdata-contents-1 (cdr contents-list) (string-append res (car contents-list))))
        ((forced-white-space? (car contents-list)) (aggregated-ast-cdata-contents-1 (cdr contents-list) (string-append res " ")))
        (else (aggregated-ast-cdata-contents-1 (cdr contents-list) res))))

; Return a string of the aggregated CDATA contents of ast.
; White space markers are taken into account, and text from ast subtrees does also contribute recursively. 
; All other AST constituents are ignored.
; .internal-references "alias" "ast-text-deep"
(define (aggregated-ast-cdata-contents-deep ast)
  (aggregated-ast-cdata-contents-deep-1 (ast-subtrees ast) ""))

(define (aggregated-ast-cdata-contents-deep-1 contents-list res)
  (cond ((null? contents-list) res)
        ((cdata? (car contents-list)) 
           (aggregated-ast-cdata-contents-deep-1 (cdr contents-list) (string-append res (car contents-list))))
        ((forced-white-space? (car contents-list)) (aggregated-ast-cdata-contents-deep-1 (cdr contents-list) (string-append res " ")))
        ((ast? (car contents-list)) 
           (aggregated-ast-cdata-contents-deep-1 (cdr contents-list) (string-append res (aggregated-ast-cdata-contents-deep (car contents-list)))))
        (else (aggregated-ast-cdata-contents-deep-1 (cdr contents-list) res))))

;; Return a string of the aggregated CDATA contents of ast.
;; White space markers are taken into account, but ast subtrees of ast do not contribute in this aggregation.
;; .form (ast-text ast)
;; .returns The immediate aggregated textual contents of ast. Returns the empty string, in case there is no contents.
(define ast-text aggregated-ast-cdata-contents)

;; Return a string of the aggregated CDATA contents of ast.
;; Ast subtrees of ast contribute recursively in this aggregation.
;; White space markers are also taken into account.
;; .form (ast-text-deep ast)
;; .returns The full aggregated textual contents of ast. Returns the empty string, in case there is no contents.
(define ast-text-deep aggregated-ast-cdata-contents-deep)

;; Return the value of the internal attribute name in ast, or #f if the internal attribute does not exist.
;; This function is a convenient shortcut of (defauted-get-prop name (ast-internal-attributes ast) default-value).
;; The optional parameter serves as the internal attribute value in case no name attribute is found in
;; the attribute list of ast. 
;; .form (ast-internal-attribute ast name [default-attribute-value])
;; .parameter ast An AST.
;; .parameter name The name of an internal attribute (a symbol).
;; .parameter default-attribute-value The default value, used if no attribute of name is found. Of arbitrary type. 
;; .internal-references "basic selector" "ast-internal-attributes"
;; .internal-references "internal attribute mutators" "ast-mutators"
(define (ast-internal-attribute ast name . optional-parameter-list)
 (let ((default-attribute-value (optional-parameter 1 optional-parameter-list #f)))
   (defaulted-get-prop name (ast-internal-attributes ast) default-attribute-value)))

;; Does the internal ast attribute called name exist?
;; .returns A boolean answer (#t or #f).
;; .internal-references "basic selector" "ast-internal-attributes"
;; .internal-references "internal attribute mutators" "ast-mutators"
(define (has-internal-ast-attribute? ast name)
  (let ((internal-attr-list (ast-internal-attributes ast)))
    (turn-into-boolean (find-in-property-list name internal-attr-list))))

;; Return those of the internal attributes of ast whose attribute name are prefixed with prefix.
;; The attribute names are truncated, by removing the prefix from them. 
;; (This may lead to empty attribut names in case some attribute names are equal to prefix).
;; This function is convenient if you need to extract a subset of systematically prefixed internal attributes and pass the non-prefixed attributes on to another function. 
;; .parameter ast An AST.
;; .parameter prefix A string.
;; .returns A property list of attributes. The prefix has been romoved from the attribute names.
(define (selected-internal-attributes ast prefix)
  (let ((all-attributes (ast-internal-attributes ast)))
    (selected-attributes-in all-attributes prefix)))

; Helping function of selected-internal-attributes. 
; As a precondition, assume that attribute-prop-list is in fact a property list (has an even number of elements).
(define (selected-attributes-in attribute-prop-list prefix)
  (cond ((null? attribute-prop-list) '())
        (else
           (let ((key (as-string (first attribute-prop-list)))
                 (val (second attribute-prop-list)))
             (if (looking-at-substring? key 0 prefix)
                 (let ((new-key (as-symbol (substring key (string-length prefix) (string-length key) ))))
                    (cons new-key (cons val (selected-attributes-in (cddr attribute-prop-list) prefix))))
                 (selected-attributes-in (cddr attribute-prop-list) prefix))))))

;;; AST Mutators.
;;; ASTs can only be mutated with respect to the internal attributes. In this section you will find a set of such mutators.
;;; In addition we provide a functional mutator of AST attributes.
;;; .section-id ast-mutators

;; Shallow copy ast and replace selected attributes of ast with new attributes. 
;; If the attributes in new-attributes do not exist in ast, the new attributes are just added.
;; Only one AST node is copied by this function.
;; .parameter ast An AST
;; .parameter new-attributes A property list of attributes which should replace existing attributes of ast.
;; .returns A copy of AST in which the attributes in new-attributes replaces attributes in the original AST.
(define (copy-ast-mutate-attributes ast . new-attributes)
 (let ((new-attributes-1 (alist-to-propertylist (map (lambda (pair) 
                                                       (cons (car pair) (as-string (cdr pair))))
                                                     (propertylist-to-alist new-attributes)))))
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append new-attributes-1 (but-props (ast-attributes ast) (every-second-element new-attributes)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast))))

;; Replace the internal attributes of ast with the attributes in prop-list (a property list).
;; Using this procedure, all existing internal attributes are replaced by a new set of attributes.
;; A procedure, not a function.
(define (set-internal-ast-attributes! ast prop-list)
  (set-car! (list-tail ast 6) prop-list))

;; Replace the attributes of ast with the attributes in prop-list (a property list).
;; Using this procedure, all existing attributes are replaced by a new set of attributes.
;; A procedure, not a function.
(define (set-ast-attributes! ast prop-list)
  (set-car! (list-tail ast 3) prop-list))

;; Mutate the value of the internal ast attribute name to value.
;; If name does not exist as an internal attribute of ast, this function inserts the attribute in the internal attribute list (as the first key value pair).
(define (set-internal-ast-attribute! ast name value)
  (let* ((internal-attr-list (ast-internal-attributes ast))
         (p-list-section (find-in-property-list name internal-attr-list))
        )
    (if p-list-section
        (set-car! (cdr p-list-section) value)
        (set-internal-ast-attributes! ast (cons name (cons value internal-attr-list))))))

;; Remove the internal attribute name and its value from the internal attribute list of ast.
;; If the attribute called name exists more than once in ast, remove them all.
(define (remove-internal-ast-attribute! ast name)
 (let* ((internal-attr-list (ast-internal-attributes ast)))
  (set-internal-ast-attributes! ast 
    (remove-prop! name internal-attr-list))))

;; Remove the attribute name and its value from the attribute list of ast.
;; If the attribute called name exists more than once in ast, remove them all.
(define (remove-ast-attribute! ast name)
 (let* ((attr-list (ast-attributes ast)))
  (set-ast-attributes! ast 
    (remove-prop! name attr-list))))

;; Remove all the attributes in name-list and their values from the attribute list of ast.
;; If an attribute in name-list exists more than once in ast, remove them all.
(define (remove-ast-attributes! ast name-list)
 (let* ((attr-list (ast-attributes ast)))
  (set-ast-attributes! ast 
    (remove-props! name-list attr-list))))


;;; XML-in-LAML Predicates.
;;; The functions in this section are predicates that work on ASTs and their constituents.
;;; .section-id ast-pred

(define (laml-ast? x)
  (and (pair? x) (eq? (car x) 'ast)
       (list? x) (= 7 (length x))))

(define (sxml-ast? x)
  (and (pair? x) (symbol? (car x))))

(define (sxml-ast-strong? x)
  (and (list? x) (>= (length x) 3) 
       (symbol? (first x))
       (list? (second x)) (eq? (car (second x)) '@)
       (list? (third x)) (eq? (car (third x)) '@@)))


;; Is the parameter x an AST.
;; .form (ast? x)
(define ast? 
  (cond ((eq? laml-internal-representation 'laml) laml-ast?)
        ((eq? laml-internal-representation 'sxml) sxml-ast?)
        (else (laml-error "ast?: Unknown value of laml-internal-representation:" laml-internal-representation))))

;; Is the parameter x an AST in a stronger interpretation than ast?
;; Using the native LAML representation, ast-strong? and ast? are identical.
;; Using the SXML representation, (ast-strong? T) requires that the tree T has attribute and aux information.
;; This corresponds to the 3. normal form of SXML.
;; The strong predicate is used during the interpretation of parameters to the mirror functions.
;; Rationale: If the weak ast predicate is used for interpretation of the mirror parameters, ambiguities will
;; occur between lists of contents/attributes and AST structures.
;; .form (ast-strong? x)
(define ast-strong? 
  (cond ((eq? laml-internal-representation 'laml) laml-ast?)
        ((eq? laml-internal-representation 'sxml) sxml-ast-strong?)
        (else (laml-error "ast-strong?: Unknown value of laml-internal-representation:" laml-internal-representation))))

;; Is the parameter x CDATA
;; .form (cdata? x)
(define cdata? string?)

;; Is the parameter x a contents element item.
;; In other words, is x either an ast, a character reference, or a string.
;; If the optional language parameter is passed, and if this language accepts extended contents, also numbers and characters are considered as contents data.
;; .form (contents-data? x [language])
(define (contents-data? x . optional-parameter-list)
 (let ((language (optional-parameter 1  optional-parameter-list #f)))
   (if language
       (if (xml-accept-extended-contents-in? language)
           (or (cdata? x) (ast? x) (char-ref? x) (cdata-section? x) (extended-contents-data? x))
           (or (cdata? x) (ast? x) (char-ref? x) (cdata-section? x)))
       (or (cdata? x) (ast? x) (char-ref? x) (cdata-section? x)))))

;; Is the parameter x a contents element item, with ASTs recognized in the strong form (using ast-strong?)
;; .form (contents-data-strong? x [language])
;; .internal-references "strong ASTs" "ast-strong?"
(define (contents-data-strong? x . optional-parameter-list)
 (let ((language (optional-parameter 1  optional-parameter-list #f)))
   (if language
       (if (xml-accept-extended-contents-in? language)
           (or (cdata? x) (ast-strong? x) (char-ref? x) (cdata-section? x) (extended-contents-data? x))
           (or (cdata? x) (ast-strong? x) (char-ref? x) (cdata-section? x)))
       (or (cdata? x) (ast-strong? x) (char-ref? x) (cdata-section? x)))))

; Is x considered as extended contents data. 
(define (extended-contents-data? x)
 (or (number? x) (char? x)))

;; Is x a delayed procedural content element?
(define (delayed-procedural-contents-element? x)
  (procedure? x))

;; Is x a character reference.
(define (char-ref? x)
 (and (list? x) (>= (length x) 2) (eq? (car x) 'char-ref) (or (number? (cadr x)) (symbol? (cadr x)))))

;; Is the parameter x a white space mark.
(define (forced-white-space? x)
 (eq? x explicit-space))

;; Is the parameter x a white suppress space mark.
(define (white-space-suppress? x)
 (eq? x explicit-space-suppress))

;; Is the parameter x white space related?
(define (white-space-related? x)
 (or (eq? x explicit-space) (eq? x explicit-space-suppress)))

;; Is x an ast nodes without subtrees?
(define (terminal-ast-node? x)
 (and (ast? x)
      (null? (ast-subtrees x))))

;; Is x an XML comment?
(define (xml-comment? x)
 (and (list? x) (>= (length x) 1) (eq? (car x) 'xml-comment)))

;; Is x an XML CDATA section?
(define (cdata-section? x)
 (and (list? x) (>= (length x) 1) (eq? (car x) 'cdata-section)))

;; Is x an XML processing instruction?
(define (processing-instruction? x)
 (and (list? x) (>= (length x) 1) (eq? (car x) 'processing-instruction)))


;; A higher-order function that returns an AST predicate, which checks if the type of the AST is name.
;; The type is either element-name, kind, or language (a symbol).
;; The resulting predicate may be applied on both ASTs and non-ASTs. If applied on a non-AST, the predicate always returns false.
;; If type is the symbol element-name, the generated predicate checks if a given AST has that element name at top level.
;; If type is the symbol kind, the generated predicate checks if a given AST is either double or single.
;; If type is the symbol language, the generated predicate checks if a given AST belongs to the language.
;; .parameter type One of the symbols element-name, kind, or language.
;; .parameter name The element-name, the kind-name (double or single), or the language-name, depending on the value of the parameter type. A string.
;; .returns A specialized AST predicate.
(define (ast-of-type? type name)
  (cond ((eq? type 'element-name)
           (lambda (ast) 
             (and (ast? ast) (equal? (as-string (ast-element-name ast)) (as-string name)))))
        ((eq? type 'kind)
           (lambda (ast) 
             (and (ast? ast) (equal? (as-string (ast-kind ast)) (as-string name)))))
        ((eq? type 'language)
           (lambda (ast) 
             (and (ast? ast) (equal? (as-string (ast-language ast)) (as-string name)))))
        (else 
            (laml-error "ast-of-type?: Unknown first parameter (type): " type))))



;;; Character Entities.
;;; The function char-ref returns a character-reference. In XML, character references are used to
;;; denote characters which are not directly accessible from normal input devices. Notice that
;;; the predicated char-ref? is located in the AST predicate section.
;;; .section-id char-ent

;; Return a character reference value for x. The parameter may be a positive, decimal integer or 
;; a symbolic name (string or symbol), such as amp, gt, or lt. Character references are represented internally
;; as an appropriate list structure, similar to an AST list structure. In xml-render, these structures are
;; converted to  XML's character notation. With this we avoid a conflict with character transformation using
;; the HTML transformation tables.
(define (char-ref x)
 (list 'char-ref
    (cond ((number? x) x)
          ((string? x) (as-symbol x))
          ((symbol? x) x)
          (else (laml-error "char-ref: Invalid parameter:" x)))))


;; Return the number or symbol of char-ref-structure.
(define (char-ref-value char-ref-structure)
  (cadr char-ref-structure))

(define (xml-render-char-ref char-ref)
 (if (not (char-ref? char-ref)) (laml-error "xml-render-char-ref: XML rendering non char-ref:" char-ref))
 (letrec ((char-ref-render 
           (lambda (x)
             (cond ((number? x) (string-append "&#" (as-string x) ";")) ; we do not enforce a three-digit representation
                   ((symbol? x) (string-append "&" (as-string x) ";"))
                   (else (laml-error "xml-render-char-ref: the parameter must be numeric, a symbol, or a string" x)))))
          (three-digit-string 
            (lambda (n)
             (cond ((and (>= n 0) (< n 10)) (string-append "00" (as-string n)))
                   ((and (>= n 10) (< n 100)) (string-append "0" (as-string n)))
                   ((< n 1000) (as-string n))
                   (else (error "three-digit-string: parameter must be between 0 and 999")))))
         )
   (char-ref-render (char-ref-value char-ref))))

; ---------------------------------------------------------------------------------------------------------------
;;; XML Comments.
;;; The xml-comment form documented below allows for native XML comments in an XML-in-LAML document.
;;; Use the xml-comment form if it is important for you that LAML outputs native XML comments.
;;; In other cases, you may use Scheme comments in your LAML document.
;;; XML comments are not rendered if they occur in empty elements.
;;; .section-id xml-comments

;; Construct an XML comment and return it.
;; This function accepts a list of comment text elements, which together form the aggregated comment text.
;; In this context, a comment text element is a string,
;; or a value which can be converted to a string via use of the function as-string.
;; .pre-condition The substring "--" does not occur as part of a comment text element.
(define (xml-comment . comment-text-list)
  (list 'xml-comment (map as-string comment-text-list)))

;; Return the comment text of an XML comment.
;; .returns a list of strings.
(define (xml-comment-contents xml-comment)
   (cadr xml-comment))

;; Render the xml-comment with space in between the individual comment text elements.
;; .returns The rendered string.
(define (xml-render-xml-comment xml-comment)
  (string-append "<!--"  (list-to-string (xml-comment-contents xml-comment) " ") "-->")) 

; ---------------------------------------------------------------------------------------------------------------
;;; XML CDATA Sections.
;;; CDATA sections are typically used in script and style elements, to protect the characters '<', '>', and others.
;;; Most browsers do not render CDATA sections, if they appear within the textual contents.
;;; .section-id cdata-sections

;; Construct an XML CDATA section and return it.
;; This function accepts a list of comment text elements, which together form the aggregated CDATA section text.
;; In this context, a CDATA text element is a string,
;; or a value which can be converted to a string via use of the function as-string.
;; .pre-condition The substring "]]>" does not occur as part of a CDATA text element.
(define (cdata-section . cdata-text-list)
  (list 'cdata-section (map as-string cdata-text-list)))

;; Return the cdata text of an XML comment.
;; .returns a list of strings.
(define (cdata-section-contents cdata-section)
  (cadr cdata-section))

;; Render the cdata section with space in between the individual cdata text elements.
;; .returns The rendered string.
(define (xml-render-cdata-section cdata-section)
  (string-append "<![CDATA[" (list-to-string (cdata-section-contents cdata-section) " ") "]]>"))

; ---------------------------------------------------------------------------------------------------------------
;;; XML Processing Instructions.
;;; A processing instruction is an XML technicality for telling something to an application, which 
;;; processes the document. Processing instructions are only rendered if they occur in non-empty elements.
;;; .section-id processing-instructions

;; Construct an XML processing instruction and return it.
;; This function accepts a pitarget and a list of text elements.
;; In this context, text element is a string,
;; or a value which can be converted to a string via use of the function as-string.
;; .pre-condition The substring "?>" does not occur as part of a CDATA text element. The pi-target is not "XML" (using upper or lower cases).
;; .parameter pi-target A name used to identify the application, towards which the instruction is targeted. 
;; .parameter text-list Additional text (a list) 
(define (processing-instruction pi-target . text-list)
  (list 'processing-instruction (as-string pi-target) (map as-string text-list)))

;; Return the processing instruction pi-target of the processing instruction pi.
;; .returns a text string.
(define (processing-instruction-target pi)
  (cadr pi))

;; Return the processing instruction text part of the processing instruction pi.
;; .returns a list of strings.
(define (processing-instruction-contents pi)
  (caddr pi))

;; Render the processing instruction pi.
;; .returns The rendered string.
(define (xml-render-processing-instruction pi)
  (string-append "<?" (processing-instruction-target pi) " " (list-to-string (processing-instruction-contents pi) " ") "?>"))

; ---------------------------------------------------------------------------------------------------
;;; XML Language Bookkeeping.
;;; XML-in-LAML keeps track of the XML-in-LAML languages in use, and it will warn you if an ambiguously named mirror function is used.
;;; The boolean variable xml-check-language-overlap? can be used to control the reporting of language overlaps (use of ambiguous mirror functions.)<p>
;;; Each XML language has a name when used in LAML. The names of all loaded mirrors is returned by the parameter-less function languages-in-use.
;;; If N is the name of an XML-in-LAML language, (N 'el-name) returns the mirror function named el-name in N.<p>
;;; A language map is an association list that maps mirror function names to the mirror functions. 
;;; Via use of a language map, it is always possible to acces a mirror function independt of name clashes with 
;;; other XML language mirrors.
;;; .section-id language-bookkeeping

;; The list of names (symbols) which causes name clashes relative to 
;; the current set of languages in use.
;; Assigned by register-xml-in-laml-language.
;; Thus, this variable is always up-to-date in between registrations of languages.
;; Be careful not to redefine this variable by double loading this file.
(define xml-in-laml-name-clashes '())

;; Register that language (the first parameter) is an XML language in use in the current LAML session.
;; This function is called by the mirror function libraries.
;; The first parameter, language, is a symbol.
;; The second parameter, language-map, is the language map of the language.
;; As a precondition it is assumed that we do not register the same language more than once.
(define (register-xml-in-laml-language language language-map)
 (set! xml-in-laml-name-clashes (precompute-name-clashes language-map xml-in-laml-name-clashes)) 
 (if (not (memq language (languages-in-use)))
     (set! xml-in-laml-languages-in-use (cons (cons language language-map) xml-in-laml-languages-in-use)))
)

; Return the new name clashes given that new-language-map is about to be registered.
; The existing name clashes is passed as the parameter existing-name-clashes.
(define (precompute-name-clashes new-language-map existing-name-clashes)
  (let* ((all-existing-names (flatten (map element-names-of-language (languages-in-use))))
         (new-names (map car new-language-map))
         (intersection (list-intersection-by-predicate new-names all-existing-names eq?))
        )
    (remove-duplicates-by-predicate (append existing-name-clashes intersection) eq?)))


;; Return the language map of language.
;; If language is not registered via register-xml-in-laml-language, return #f.
;; The parameter language is a symbol.
(define (language-map-of language)
  (defaulted-get language xml-in-laml-languages-in-use #f))

;; is language in use - is it registered as an xml-in-laml language?
(define (language-in-use? language)
  (let ((lg-map (defaulted-get language xml-in-laml-languages-in-use #f)))
    (if lg-map #t #f)))

;; Return the list of language in used, as registered by register-xml-in-laml-language.
(define (languages-in-use) 
  (map car xml-in-laml-languages-in-use))

;; Return the element names of language, as defined by its language map.
(define (element-names-of-language language)
  (let ((lg-map (defaulted-get language xml-in-laml-languages-in-use '())))
    (map car lg-map))) 

;; Is name involved in a name clash among the registered xml-in-laml languages?
(define (causes-xml-in-laml-name-clash? name)
  (memq name xml-in-laml-name-clashes))

;; Return an activator function for language.
;; .parameter language The name of an XML language (a symbol).
;; .returns A function which given an XML element name (a string or a symbol) in language returns the appropriate mirror function.
(define (activator-via-language-map language)
 (lambda (element-name)
  (let ((lg-map (language-map-of language)))
    (get-mirror-function lg-map element-name))))

; A global variable which holds bindings of a set of variables. 
(define the-name-binding-stack '())

; Push an entry with namebindings for name-list on the name binding list.
(define (push-name-bindings name-list)
  (let ((name-fu-map (map (lambda (n) (cons n (eval-cur-env n))) name-list)))
    (set! the-name-binding-stack (cons name-fu-map the-name-binding-stack))))

; Pop the name binding list, and reestablish the bindings in the top entry.
; As a precondition it is assumed that the name binding stack is not empty.
(define (pop-and-restore-name-bindings)
  (let ((name-fu-map (car the-name-binding-stack)))
    (set! the-name-binding-stack (cdr the-name-binding-stack))
    (for-each
      (lambda (name-fu-pair)
         (if (procedure? (cdr name-fu-pair))
             (eval-cur-env `(set! ,(car name-fu-pair) ,(cdr name-fu-pair)))
             (eval-cur-env `(set! ,(car name-fu-pair) (quote ,(cdr name-fu-pair)))))
             
      )
      name-fu-map)))

; Assign the variables in xml-element-variable-list to the corresponding mirror functions in xml-language. 
; xml-element-variable-list is a list of symbols. 
; xml-language is the name of an xml language in use (a symbol).
(define (establish-xml-in-laml-name-bindings xml-element-variable-list xml-language)
 (let ((lang-map (language-map-of xml-language)))
   (for-each 
     (lambda (xml-element-name)
        (eval-cur-env `(set! ,xml-element-name ,(get-mirror-function lang-map xml-element-name)))
     )
     xml-element-variable-list)))


;; A macro which globally establishes the name bindings of xml-language and evaluate forms in this state.
;; Does also handle separate checking of ID and IDREF attributes in forms.
;; Works in a manner similar to a fluid-let.
;; As a matter of optimization, only assign the names in the overlap between the XML languages in use.
;; Defined as a syntactical abstraction.
;; .form (with-xml-languge language-name . forms)
;; .misc If the overlap between the XML languages in use includes central Scheme functions, such as map or filter, you should use with-xml-language! instead (and put the central Scheme functions in the minus-elements).
;; .parameter language-name The name of a language map (a symbol).
;; .parameter forms A number of Scheme forms presumably involving mirror function names from the XML language language-name.
;; .returns The value of the last expression in forms
;; .internal-references "similar function" "with-xml-language!"
(define-syntax with-xml-language 
 (syntax-rules ()
   ((with-xml-language xml-language-name form ...)
    (let ((name-clashes xml-in-laml-name-clashes))   
      (push-name-bindings (append (list 'xml-id-attribute-list 'xml-idref-attribute-list) name-clashes))
      (establish-xml-in-laml-name-bindings name-clashes xml-language-name)
      (set! xml-id-attribute-list '()) (set! xml-idref-attribute-list '())
      (let ((result (begin form ...)))
        (check-id-and-idref-attributes!)
        (pop-and-restore-name-bindings)
        result)))))

;; A macro which globally establishes the name bindings of xml-language and evaluate forms in this state.
;; Does also handle separate checking of ID and IDREF attributes in forms.
;; Works in a manner similar to a fluid-let.
;; All elements of xml-language are redefined, except the explictly given minus elements.
;; .form (with-xml-languge! language-name minus-elements . forms)
;; .parameter language-name The name of a language map (a symbol).
;; .parameter minus-elements The list of element names which are not allowed to be reassigned. 
;; .parameter forms A number of Scheme forms presumably involving mirror function names from the XML language language-name.
;; .returns The value of the last expression in forms
;; .internal-references "similar function" "with-xml-language"
(define-syntax with-xml-language!
 (syntax-rules ()
   ((with-xml-language! xml-language-name minus-elements form ...)
    (let ((name-clashes (list-difference (element-names-of-language xml-language-name) minus-elements)))  
      (push-name-bindings (append (list 'xml-id-attribute-list 'xml-idref-attribute-list) name-clashes))
      (establish-xml-in-laml-name-bindings name-clashes xml-language-name)
      (set! xml-id-attribute-list '()) (set! xml-idref-attribute-list '())
      (let ((result (begin form ...)))
        (check-id-and-idref-attributes!)
        (pop-and-restore-name-bindings)
        result)))))


; ---------------------------------------------------------------------------------------------------
;;; The Language Map.
;;; The language map is a mapping that allows us to access the element mirror functions independent of
;;; any name clash. 
;;; In this version of LAML the language map is an association that maps element names (symbols) to function objects.
;;; In the longer run, it should be a more efficient search structure.
;;; .section-id language-map

; An auxilary variable used by the XML-in-LAML mirror libraries.
(define temp-language-map '())

; An auxilary variable used by the XML-in-LAML mirror libraries.
(define temp-mirror-function #f)


;; Return the mirror function based on element-name from language map.
;; .returns The mirror function or #f if no mirror function exists in the map.
;; .parameter element-name The name an XML element. Either a symbol or a string.
(define (get-mirror-function language-map element-name)
 (let ((element-name-symbol (as-symbol element-name)))
   (defaulted-get element-name-symbol language-map #f)))

;; Update the language map by the association of element-name and mirror function.
;; Returns the updated map.
;; The parameter element-name is either a symbol or a string.
;; Causes an error if there already is an entry for element-name
(define (put-mirror-function language-map element-name mirror-function)
 (let ((element-name-symbol (as-symbol element-name)))
   (if (get-mirror-function language-map element-name-symbol)
       (laml-error "put-mirror-function: The name" element-name "is defined twice.")
       (cons (cons element-name-symbol mirror-function) language-map))))



; ---------------------------------------------------------------------------------------------------

;;; XML Navigation Information.
;;; XML navigation information provides for "smart searching" of XML ASTs. The smartness
;;; is based on the following knowledge: (1) The possible direct and indirect sub-elements
;;; of a given element. (2) The possible attributes of an element and its direct and indirect 
;;; sub-elements. This information is pre-computed in the XML-in-LAML mirror generation tool.
;;; The functions in this section provides access to this pre-computed information.
;;; Notice that the functions in this section return static information, as derived from the DTD.
;;; .section-id xml-navigation

;; Register navigator-structure and an XML-in-LAML navigator for language. 
;; This function is called "automatically" when the mirror functions are loaded.
(define (register-xml-in-laml-navigator language navigator-structure)
 (set! xml-in-laml-navigator-structures (cons (cons language navigator-structure) xml-in-laml-navigator-structures))
)

;; Return the XML navigator structure of language.
;; If language is not registered via register-xml-in-laml-language, return #f.
;; The parameter language is a symbol.
(define (xml-navigator-of language)
  (defaulted-get language xml-in-laml-navigator-structures #f))

; Is x an xml-navigator?
(define (xml-navigator? x)
 (and (list? x) (= (length x) 2) (eq? (car x) 'xml-navigator)))

; Return the navigator vector of xml-navigator.
; As a precondition, it is assumed that the parameter satisfy the predicate xml-navigator?
(define (xml-navigator-vector xml-navigator)
 (cadr xml-navigator))

; Navigator triple selectors:
(define navigator-triple-element-name (make-selector-function 1 "navigator-triple-element-name"))
(define navigator-triple-possible-element-vector (make-selector-function 2 "navigator-triple-possible-element-vector"))
(define navigator-triple-possible-attribute-vector (make-selector-function 3 "navigator-triple-possible-attribute-vector"))


;; Return a list of element names that can appear as direct or indirect constituents of
;; an AST rooted by element-name. Reflexively, element-name will always be returned as part of 
;; the result.
;; .pre-condition language is a registered XML language in the enclosing LAML session, and element-name is an existing name of an element in language.
;; .returns The list of possible elements names (a list of symbols). The list is sorted alphabetically.
(define (possible-elements-rooted-by-element element-name language)
 (let* ((nav-vector (xml-navigator-vector (xml-navigator-of language)))
        (relevant-tripple 
          (binary-search-in-vector nav-vector (as-symbol element-name) navigator-triple-element-name eq? symbol-leq?))
       )
  (if relevant-tripple 
      (let ((element-vector (navigator-triple-possible-element-vector relevant-tripple)))
        (vector->list element-vector))
      (laml-error "possible-elements-rooted-by-element: Cannot locate" element-name "in" language))))


;; Return a list of attribute names that can appear as direct or indirect constituents of an
;; AST rooted by element-name. Reflexively, the attributes of element-names are always parts of the
;; result.
;; .pre-condition language is a registered XML language in the enclosing LAML session, and element-name is an existing name of an element in language.
;; .returns The list of possible attribute names (a list of symbols). The list is sorted alphabetically.
(define (possible-attributes-rooted-by-element element-name language)
 (let* ((nav-vector (xml-navigator-vector (xml-navigator-of language)))
        (relevant-tripple 
          (binary-search-in-vector nav-vector (as-symbol element-name) navigator-triple-element-name eq? symbol-leq?))
       )
  (if relevant-tripple 
      (let ((attribute-vector (navigator-triple-possible-attribute-vector relevant-tripple)))
        (vector->list attribute-vector))
      (laml-error "possible-attributes-rooted-by-element: Cannot locate" element-name "in" language))))

;; Can ast have a direct or indirect constituent (sub-ast) with element name el-name.
;; The result is always true if (ast-element-name ast) equals to el-name.
;; .parameter ast An AST
;; .parameter el-name is the name of an element in the language of ast (a string or a symbol).
(define (can-have-element-constituent? ast el-name)
 (can-have-element-constituent-help (ast-element-name ast) el-name (xml-navigator-of (ast-language ast))))

; Can el-name be a constituent of an AST rooted by in-element-name, using xml-navigator.
; The function doing the real work of can-have-element-constituent?
(define (can-have-element-constituent-help in-element-name el-name xml-navigator)
 (let* ((nav-vector (xml-navigator-vector xml-navigator))
        (relevant-tripple 
          (binary-search-in-vector nav-vector (as-symbol in-element-name) navigator-triple-element-name eq? symbol-leq?))
       )
  (if relevant-tripple 
      (let ((element-vector (navigator-triple-possible-element-vector relevant-tripple)))
        (turn-into-boolean (binary-search-in-vector element-vector (as-symbol el-name) id-1 eq? symbol-leq?)))
      #f)))

; Is symbol sym1 considered less than or equal to symbol sym2
(define (symbol-leq? sym1 sym2)
  (string<=? (symbol->string sym1) (symbol->string sym2)))


;;; Mirror Generation Functions.
;;; The function generate-xml-mirror-function is the (higher-order) function that generates the mirror of XML elements in Scheme.
;;; In addition, this section contains the function that implement the LAML parameter passing rules of the mirror functions,
;;; namely, xml-sort-tag-parameters.
;;; .section-id mirror-gen


;; Separates parameters according to the mirror rules of LAML.
;; In other words, this is the function which implements the central LAML mirror function rules.
;; Returns a cons cell of contents and attributes.
;; Attributes are returned in property list format.
;; Contents are returned as a list contents elements (strings, ASTs, booleans).
;; .internal-references "similar function" "xml-sort-superficially-tag-parameters"
;; .form (xml-sort-tag-parameters parameters tag-name [language])
(define (xml-sort-tag-parameters parameters tag-name . optional-parameter-list)
 (let* ((language (optional-parameter 1 optional-parameter-list #f)))
  (xml-sort-tag-parameters-1 parameters tag-name language #f)))

;; Like xml-sort-tag-parameters, but collect the content contribution in a relatively raw surface form.
;; Handle attributes in the same way as xml-sort-tag-parameters. As a contrast to xml-sort-tag-parameters, 
;; white space related values are passed unchanged by this function.
;; Use this kind of 'sorting' to obtain XML-in-LAML parameter passing in abstractions, which transfer data 
;; to XML-in-LAML mirror functions (which in turn use xml-sort-tag-parameters).
;; .internal-references "similar function" "xml-sort-tag-parameters"
;; .form (xml-sort-superficially-tag-parameters parameters tag-name [language])
(define (xml-sort-superficially-tag-parameters parameters tag-name . optional-parameter-list)
 (let ((language (optional-parameter 1  optional-parameter-list #f)))
  (xml-sort-tag-parameters-1 parameters tag-name language #t)))

(define (xml-sort-tag-parameters-1 parameters tag-name language superficial?)
 (let* ((white-space? (xml-represent-white-space-in? language)))
  (letrec ((strip-initial-explicit-spaces 
            (lambda (cl) (if (and (pair? cl) (eq? (car cl) explicit-space)) (strip-initial-explicit-spaces (cdr cl)) cl)))

           (list-not-laml-special? (lambda (x) (and (list? x) 
                                                    (not (or (ast-strong? x) (char-ref? x) (xml-comment? x) (cdata-section? x)
                                                             (processing-instruction? x))))))


           (maybe-string (lambda (x) (if (extended-contents-data? x) (as-string x) x)))

           (as-string-attr-val (lambda (x)
                                 (cond ((char-ref? x) (xml-render-char-ref x))
                                       (else (as-string x)))))

           (xml-flatten-parameters 
            (lambda (parameters)
              (xml-flatten-parameters-1 parameters '())))

           (xml-flatten-parameters-1 
            (lambda (parameters res)
              (cond ((null? parameters) res)
                    ((list-not-laml-special? (car parameters))
                       (xml-flatten-parameters-1 (cdr parameters) (append (xml-flatten-parameters-1 (car parameters) '()) res)))
                    (else (xml-flatten-parameters-1 (cdr parameters) (cons (car parameters) res))))))

           (xml-split-parameters ; Do a basic split of parameters in contents and attributes. 
                                        ; Parameters is reversed in the starting point.
                                        ; Do not perform any error checking.
                                        ; Return a cons pair of contents and attributes. 
            (lambda (parameters)
              (xml-split-parameters-1 parameters '() '())))

           (xml-split-parameters-1 
            (lambda (parameters content-list attribute-list)  ; parameter is reversed
              (cond ((null? parameters) (cons content-list attribute-list))
                    ((and (not (null? (cdr parameters))) ; thus parameters has at least two elements
                          (symbol? (cadr parameters))) ; input is reversed
                     (let ((attr-name (cadr parameters))
                           (attr-val (car parameters)))
                       (xml-split-parameters-1 (cddr parameters) content-list (cons attr-name (cons attr-val attribute-list)))))
                    ((symbol? (car parameters)) 
                      (let ((attr-name (car parameters)))
                       (xml-split-parameters-1 (cdr parameters) content-list (cons attr-name attribute-list))))
                    (else (xml-split-parameters-1 (cdr parameters) (cons (car parameters) content-list) attribute-list)))))

           (xml-process-contents 
            (lambda (contents-list)
              (xml-process-contents-1 contents-list '())))

           (xml-process-contents-1
            (lambda (content-parameters res)
              (cond ((null? content-parameters) res)

                    ; CONTENTS DATA - CDATA, CHAR-REF, XML-comment, PI, DELAYED-PROCEDURAL-CONTENT or AST - 
                    ; and then a space suppress value - no space after
                    ((and (x-contents-data-strong? (car content-parameters) language) (not (null? (cdr content-parameters)))
                          (white-space-related? (cadr content-parameters)) (white-space-suppress? (cadr content-parameters)))              
                     (xml-process-contents-1
                      (cddr content-parameters) 
                      (if white-space? 
                          (if superficial? 
                             (cons explicit-space-suppress
                               (cons (maybe-string (car content-parameters)) res)) (cons (maybe-string (car content-parameters)) res))
                          (cons (maybe-string (car content-parameters)) res))))
                      

                     ; CONTENTS DATA - CDATA, CHAR-REF, XML-comment, PI, DELAYED-PROCEDURAL-CONTENT, or AST - 
                     ; and then a space value -  space after (not typical)
                    ((and (x-contents-data-strong? (car content-parameters) language) (not (null? (cdr content-parameters)))
                          (white-space-related? (cadr content-parameters)) (forced-white-space? (cadr content-parameters))) ; space after
                     (xml-process-contents-1
                      (cddr content-parameters)
                      (if white-space? 
                          (cons explicit-space
                           (cons (maybe-string (car content-parameters)) res))
                          (cons (maybe-string (car content-parameters)) res))))

                    ; CONTENTS DATA - CDATA, CHAR-REF, XML-comment, PI, DELAYED-PROCEDURAL-CONTENT or AST - 
                    ((x-contents-data-strong? (car content-parameters) language) ; space after
                     (xml-process-contents-1  
                      (cdr content-parameters)
                      (if white-space? 
                          (cons explicit-space
                                (cons (maybe-string (car content-parameters)) res))
                          (cons (maybe-string (car content-parameters)) res))))

                    ; White space in other situations - just ignore it
                    ((white-space-related? (car content-parameters))  
                       (xml-process-contents-1 (cdr content-parameters) res))

                    (else 
                     (let* ((extended-contents (xml-accept-extended-contents-in? language))
                            (hint (if (not extended-contents) 
                                      (string-append (as-string #\newline) 
                                      "You may consider use of extended contents: (set-xml-accept-extended-contents-in "
                                       "'" (as-string language) " #t" ")")
                                      ""))
                            ) 
                       (xml-sort-error (string-append "Fatal error in an XML-in-LAML  " tag-name "  element." 
                                                       hint)
                                       (xml-render-error-message parameters)))
                     ))))

           (xml-process-attributes 
            (lambda (attributes)
              (xml-process-attributes-1 attributes '())))

           (xml-process-attributes-1 
            (lambda (attribute-prop-list res)
              (cond ((null? attribute-prop-list) res)
                    ((and (symbol? (car attribute-prop-list)) (not (null? (cdr attribute-prop-list))))
                      (let* ((attr-name (car attribute-prop-list))
                             (attr-val (cadr attribute-prop-list))
                             (problematic-attr-value-type? 
                               (or (ast-strong? attr-val) (char-ref? attr-val) (xml-comment? attr-val) (processing-instruction? attr-val)
                                   (delayed-procedural-contents-element? attr-val)))
                            )
                        (if (and (xml-accept-only-string-valued-attributes-in? language) (not (string? attr-val)))
                 
                            (cond (problematic-attr-value-type?
                                   (xml-check-error "An unpsupported attribute value type is passed as the value of the attribute"
                                                   (as-string #\newline) " "  (as-string attr-name) "  in an instance of the "
                                                   tag-name " element. " (as-string #\newline)
                                                    " "  "The attribute is ignored."))
                                  (else
                                   (let* ((extended-attributes? (not (xml-accept-only-string-valued-attributes-in? language)))
                                          (hint (if (not extended-attributes?) 
                                                    (string-append (as-string #\newline) 
                                                                   "   You may consider use of relaxed attributes:" (as-string #\newline) 
                                                                   "   " "(set-xml-accept-only-string-valued-attributes-in "
                                                                   "'" (as-string language) " #f" ")")
                                                    ""))
                                          )
                                     (xml-check-error "A non-string value " (as-string attr-val)
                                                      " is passed as the value of the attribute" (as-string #\newline)
                                                      " " (as-string attr-name) "  in an instance of the " tag-name " element."
                                                      (as-string #\newline) 
                                                      " " "The attribute value is converted to a string." hint))))

                            (cond (problematic-attr-value-type?
                                   (xml-check-error "An unpsupported attribute value type is passed as the value of the attribute"
                                                    (as-string #\newline) " "  (as-string attr-name) "  in an instance of the "
                                                    tag-name " element. " (as-string #\newline)
                                                    " "  "The attribute is ignored."))
                                  (else
                                   #f))
                            )
                        (xml-process-attributes-1 
                         (cddr attribute-prop-list)
                         (if (not problematic-attr-value-type?) (cons (as-string-attr-val attr-val) (cons attr-name res)) res))
                        ))
                    ((and (symbol? (car attribute-prop-list)) (null? (cdr attribute-prop-list)) )
                       (xml-sort-error 
                        (string-append "Fatal error in an XML-in-LAML element: " "Attributes of the  " tag-name
                                       "  element must be of the form  'symbol \"value\" "  (as-string #\newline) "   "
                                       "Only the symbol  " (as-string (car attribute-prop-list))
                                       "  appears in last encountered attribute." ) (xml-render-error-message parameters)))
                    (else (laml-error (string-append "Fatal error in XML-in-LAML element: Malformed attribute list in")
                                           (xml-render-error-message parameters) ))

              ))
           ))

    (let* ((flat-parameters (xml-flatten-parameters parameters))
           (splited-flat-parameters (xml-split-parameters flat-parameters))
           (raw-contents (car splited-flat-parameters))
           (raw-attributes (cdr splited-flat-parameters))
           (contents (xml-process-contents raw-contents))
           (attributes (xml-process-attributes raw-attributes))
           )
      (cons
       (reverse (strip-initial-explicit-spaces contents))
       (xml-modify-attribute-list (reverse attributes) (xml-duplicated-attribute-handling language)))))))

; A extended variant of contents-data-strong?, which also takes XML comments and XML processing instructions into account.
; Used in xml-sort-tag-parameters.
(define (x-contents-data-strong? x . optional-parameter-list)
 (let ((language (optional-parameter 1  optional-parameter-list #f)))
   (if language
       (or (contents-data-strong? x language)
           (xml-comment? x)
           (processing-instruction? x)
           (delayed-procedural-contents-element? x)
       )
       (or (contents-data-strong? x)
           (xml-comment? x)
           (processing-instruction? x)
           (delayed-procedural-contents-element? x)
       )))) 

(define (xml-modify-attribute-list attribute-prop-list kind)
 (cond ((eq? kind 'keep-all) attribute-prop-list)
       ((eq? kind 'keep-first) (remove-duplicate-properties-keep-first attribute-prop-list))
       ((eq? kind 'keep-last) (remove-duplicate-properties-keep-last attribute-prop-list))
       (else (laml-error "xml-modify-attribute-list: Unknown kind of attribute modification" kind))))

; Remove duplicate properties non-destructively. Keep first duplicate. Comparison done by eq?
(define (remove-duplicate-properties-keep-first proplist)
 (letrec ((remove-duplicate-properties-keep-first-help
             (lambda (proplist seen-keys)
               (cond ((null? proplist) '())
                     ((memq (car proplist) seen-keys) (remove-duplicate-properties-keep-first-help (cddr proplist) seen-keys))
                     (else (cons (car proplist) (cons (cadr proplist) 
                              (remove-duplicate-properties-keep-first-help (cddr proplist) (cons (car proplist) seen-keys)))))))))
  (remove-duplicate-properties-keep-first-help proplist '())))

; Remove duplicate properties non-destructively. Keep last duplicated. Comparison done by eq?
(define (remove-duplicate-properties-keep-last proplist)
 (letrec ((memq-prop-list 
            (lambda (prop proplist)
              (if (null? proplist)
                  #f
                  (or (eq? (car proplist) prop) (memq-prop-list prop (cdr proplist))))))
                  
          (remove-duplicate-properties-keep-last-help
             (lambda (proplist)
               (cond ((null? proplist) '())
                     ((memq-prop-list (car proplist) (cddr proplist)) 
                         (remove-duplicate-properties-keep-last-help (cddr proplist)))
                     (else (cons (car proplist) (cons (cadr proplist) 
                              (remove-duplicate-properties-keep-last-help (cddr proplist))))))))
         )
  (remove-duplicate-properties-keep-last-help proplist)))


  




(define (xml-sort-error message parameters)
 (let* ((max-str-lgt xml-error-truncation-length)  
        (parameters-2 (if (> (string-length parameters) max-str-lgt) 
                        (string-append (substring parameters 0 (- max-str-lgt 1)) "...")
                        parameters)))
  (laml-error
      "***" message (as-string #\newline)
      "  The list of parameters: " parameters-2 (as-string #\newline))))

;; Prepare laml-list to be used as input to a mirror function.
;; This function is useful when the result of transform-ast-list is passed as input to a XML mirror function.
;; Elimiate forced-white-space markers, and introduce explict-space-suppres when necessary.
;; .internal-references "in relation to" "transform-ast-list"
(define (laml-source-prepare laml-lst)
  (cond ((null? laml-lst) laml-lst)
        ((and (white-space-related? (car laml-lst)) (forced-white-space? (car laml-lst)))  ; always remove forced-white-space marker
           (laml-source-prepare (cdr laml-lst)))
        ((and (not (null? (cdr laml-lst))) (not (white-space-related? (car laml-lst))) (not (white-space-related? (cadr laml-lst))))  ; insert white space suppress 
           (cons (car laml-lst) (cons explicit-space-suppress (laml-source-prepare (cons (cadr laml-lst) (cddr laml-lst))))))
        (else (cons (car laml-lst) (laml-source-prepare (cdr laml-lst))))))

; -----------------------------------------------------------------------------------------------------------------------

;; Return an XML mirror surface function, in which textual content parameters and attribute
;; value pairs can be given in a very liberal fashion. 
;; In the generated mirror function, the internal attribute run-action-procedure controls if an action procedure is executed. 
;; (The value false of internal:run-action-procedure prevents an associated action procedure to be executed. The default value of the internal attribute is true).
;; .parameter validation-procedure the procedure that validates all aspects of the application of the element mirror function.
;; .parameter tag-name a string which represents the name of the tag (used for error message purposes).
;; .parameter default-dtd-attributes an alist of attribute key value pairs, as specified in the DTD. 
;; .parameter single-double-kind either the symbol single, double or possible-single.
;; .parameter language the language in use (a symbol).
;; .parameter overlap-check? controls whether to check for name clashes in between the registered XML-in-LAML languages.
;; .parameter action-procedure serves both as a boolean and a procedure. If #f, no action procedure is supplied. If #t, an action procedure exists. If a procedure, it is the action procedure.
(define (generate-xml-mirror-function validation-procedure tag-name default-dtd-attributes single-double-kind language overlap-check? action-procedure)
 (cond ((eq? single-double-kind 'double)
        (lambda parameters
          (let* ((contents-attributes (xml-sort-tag-parameters parameters tag-name language))
                 (contents (car contents-attributes))
                 (attributes (if (xml-pass-default-dtd-attributes-in? language)
                                 (append (cdr contents-attributes) default-dtd-attributes)
                                 (cdr contents-attributes)))
                 (split-attributes (split-xml-and-internal-attributes attributes))
                 (xml-attributes (car split-attributes))
                 (internal-attributes (cdr split-attributes))
                 (run-action-procedure? (as-boolean (defaulted-get-prop 'run-action-procedure internal-attributes #t)))  
                )
            (if (not (has-procedural-content-items? contents)) (validation-procedure tag-name xml-attributes contents overlap-check?))  ; postpone validation in case of procedural contents!
            (if action-procedure
                (let ((real-action-procedure (cond ((boolean? action-procedure) 
                                                     (action-procedure-of-language tag-name language))
                                                    ((procedure? action-procedure)
                                                      action-procedure)
                                                    (else (laml-error "generate-xml-mirror-function: Invalid action procedure"))))
                      (the-ast (make-ast tag-name contents xml-attributes 'double language internal-attributes)))
                  (if run-action-procedure? (real-action-procedure the-ast))
                  the-ast)
                 (make-ast tag-name contents xml-attributes 'double language internal-attributes)
            ))))
           

       ((eq? single-double-kind 'single)   
          (lambda parameters
            (let* ((contents-attributes (xml-sort-tag-parameters parameters tag-name language))
                   (contents (car contents-attributes))
                   (attributes (if (xml-pass-default-dtd-attributes-in? language)
                                   (append (cdr contents-attributes) default-dtd-attributes)
                                   (cdr contents-attributes)))
                   (split-attributes (split-xml-and-internal-attributes attributes))
                   (xml-attributes (car split-attributes))
                   (internal-attributes (cdr split-attributes))
                   (run-action-procedure? (as-boolean (defaulted-get-prop 'run-action-procedure internal-attributes #t)))  
                  )
              (if (not (has-procedural-content-items? contents)) (validation-procedure tag-name xml-attributes contents overlap-check?))  ; postpone validation in case of procedural contents!
              (if action-procedure
                  (let ((real-action-procedure (cond ((boolean? action-procedure) 
                                                     (action-procedure-of-language tag-name language))
                                                    ((procedure? action-procedure)
                                                      action-procedure)
                                                    (else (laml-error "generate-xml-mirror-function: Invalid action procedure"))))
                        (the-ast (make-ast tag-name '() xml-attributes 'single language internal-attributes)))
                    (if run-action-procedure? (real-action-procedure the-ast))
                    the-ast)
                  (make-ast tag-name '() xml-attributes 'single language internal-attributes)  ; disregarding contents, including XML comments and PIs
              )
            )
          )
       )

       (else (error (string-append "generate-xml-mirror-function: unknown single-double-kind: " (as-string single-double-kind))))
 )
)

; Return a cons pair of ordinary XML attributes and the internal attributes.
; Strips the 'internal:' part of the name of internal attributes. 
(define (split-xml-and-internal-attributes prop-list)
  (split-xml-and-internal-attributes-1 prop-list '() '())) 

(define (split-xml-and-internal-attributes-1 prop-list prop-list-xml prop-list-internal)
  (cond ((null? prop-list) (cons (reverse prop-list-xml) (reverse prop-list-internal)))
        ((internal-attribute-name? (car prop-list))
               (split-xml-and-internal-attributes-1 (cddr prop-list) prop-list-xml (cons (cadr prop-list) (cons (drop-internal-prefix (car prop-list)) prop-list-internal))))
        (else  (split-xml-and-internal-attributes-1 (cddr prop-list) (cons (cadr prop-list) (cons (car prop-list) prop-list-xml)) prop-list-internal))))
  
; Is attr-name (a symbol) an internal attribute name.
; True if it has the prefix 'internal:'
(define (internal-attribute-name? attr-name)
  (let ((attr-name-string (as-string attr-name)))
    (and (>= (string-length attr-name-string) 9)
         (equal? (substring attr-name-string 0 9) "internal:"))))

; Precondition: internal-attr-name is a name (symbol) with prefix 'internal:'
; Return the attribute name (a symbol) without the 'internal:' prefix.
(define (drop-internal-prefix internal-attr-name)
 (let* ((internal-attr-name-string (as-string internal-attr-name)) 
        (str-lgt (string-length internal-attr-name-string)))
  (as-symbol (substring internal-attr-name-string 9 str-lgt))))
  



; ---------------------------------------------------------------------------------------------------------------

;;; Contents Validation. 
;;; The definitions in this section carry out the content validation. 
;;; Throughout the procedures in this section it is assumed that delayed procedural content items have been expaneded
;;; before content validation is attempted. 
;;; .section-id validation

; XML document validation.
; The non-trivial validation cases are based on the automata generated by tools/xml-in-laml/xml-in-laml.scm.


; The symbol which is assumed to end all input to the dfa. 
; The value of this variable must comply with the similar constant in tools/xml-in-laml/xml-in-laml.scm.
(define terminator-symbol 'terminator$$)

; The symbol that represents textual contents (pcdata) during validation.
(define textual-content-symbol 'textual-contents$$)

; The symbol that represents "non textual contents",  such as XML comments.
(define non-textual-content-symbol 'non-textual-contents$$)


;; Apply the deterministic finte state automaton dfa on contents. 
;; If contents is not accepted call xml-check-error for an appropriate reporting of the validation error.
;; .parameter contents A list of content items, such as strings, ASTs and white space markers.
;; .parameter dfa A deterministic final state automation which controls the acceptance.
;; .parameter tag-name The name of the element - used for error message purposes. 
(define (validate-contents-by-dfa! contents dfa tag-name)
 (if (not (list? contents)) (laml-error "Contents passed to validate-contents-by-dfa! is assumed to be a list"))
 (let* ((augmented-contents (xml-prepare-contents-for-validation contents))
        (accepted (automaton-accepts? dfa augmented-contents)))
  (if (not accepted)
      (cond ((not last-automaton-input-symbol)
               (xml-check-error "Empty and insufficient input to" (a-or-an tag-name) (as-string-spacy tag-name) "element." (as-string #\newline)
                        "  " (truncate-string (xml-render-error-message contents))))
            ((eq? last-automaton-input-symbol terminator-symbol)
               (xml-check-error "Abrupt termination of" (a-or-an tag-name) (as-string-spacy tag-name) "element:" (as-string #\newline)
                        "  " (truncate-string (xml-render-error-message contents))))
            ((eq? last-automaton-input-symbol textual-content-symbol)
               (let ((the-textual-contents (list-ref (filter (negate white-space-related?) contents) (max 0 (- automaton-input-number 1)))))
                (xml-check-error "Textual contents" (xml-render-error-message the-textual-contents) "is illegal in" (a-or-an tag-name) 
                                  (as-string-spacy tag-name) "element:" (as-string #\newline)
                        "  " (truncate-string (xml-render-error-message contents)))))
            (else 
               (if (not (extraordinary-allow-element? (as-symbol last-automaton-input-symbol) (as-symbol tag-name)))
                   (xml-check-error "Encountered a misplaced" (as-string-spacy last-automaton-input-symbol) "element within"
                                     (a-or-an tag-name) (as-string-spacy tag-name) "element:" (as-string #\newline)
                                     "  " (truncate-string (xml-render-error-message contents)))))))))


;; Validate that contents is pure PCDATA.
;; .parameter contents A list of content items, such as strings, ASTs and white space markers.
;; .parameter tag-name The name of the element - used for error message purposes. 
(define (validate-as-pcdata! contents tag-name)
  (if (not (list? contents)) (laml-error "Contents passed to validate-as-pcdata! is assumed to be a list"))
  (let ((res (do-validate-pcdata-contents contents)))
    (cond ((or (symbol? res) (string? res))
             (xml-check-error "Encountered a misplaced" (as-string-spacy res) "element in" (a-or-an tag-name)
                              (as-string-spacy tag-name) "element, where only textual contents is allowed." (as-string #\newline)
                "  "  (truncate-string (xml-render-error-message contents))))
          ((and (boolean? res) (not res))
             (xml-check-error "Unindentified problem in" (a-or-an tag-name) (as-string-spacy tag-name) "element." (as-string #\newline)
                "  "  (truncate-string (xml-render-error-message contents)))))))

(define (do-validate-pcdata-contents contents)
  (call-with-current-continuation 
    (lambda (exit)
     (do-validate-pcdata-contents-1 contents exit))))

(define (do-validate-pcdata-contents-1 contents return)
  (cond ((null? contents) #t) 
        (else (let ((content-item (car contents)))
                 (cond ((ast? content-item)  (return (ast-element-name content-item)))
                       ((cdata? content-item) #t)
                       ((char-ref? content-item) #t)
                       ((cdata-section? content-item) #t)
                       ((white-space-related? content-item) #t)
                       (else (return #f)))
                 (do-validate-pcdata-contents-1 (cdr contents) return)))))

;; Validate that contents corresponds to mixed contents. Mixed contents includes PCDATA a number of other choices in a 'zero-or-more' structure.
;; It means that it is PCDATA or one of a number of possible ASTs.
;; .parameter contents A list of content items, such as strings, ASTs or white space markers.
;; .parameter tag-name The name of the element in which we check the constituents.
;; .parameter symbol-choice-list A list of symbols for the possible choices - a list of symbols.
(define (validate-mixed-contents-by-simple-means! contents symbol-choice-list tag-name)
  (if (not (list? contents)) (laml-error "Contents passed to validate-mixed-contents-by-simple-means! is assumed to be a list"))
  (let ((res (do-validate-mixed-contents contents symbol-choice-list tag-name)))
    (cond ((or (symbol? res) (string? res))
             (xml-check-error "Encountered a misplaced" (as-string-spacy res) "in" (a-or-an tag-name) (as-string-spacy tag-name) "element." (as-string #\newline) 
                "  "  (truncate-string (xml-render-error-message contents))))
          ((and (boolean? res) (not res))
             (xml-check-error "Unidentified problem in" (a-or-an tag-name) (as-string-spacy tag-name) "element." (as-string #\newline)
                "  "  (truncate-string (xml-render-error-message contents)))))))
   

(define (do-validate-mixed-contents contents symbol-choice-list element-name)
  (call-with-current-continuation 
    (lambda (exit)
     (do-validate-mixed-contents-1 contents symbol-choice-list exit element-name))))

(define (do-validate-mixed-contents-1 contents symbol-choice-list return element-name)
  (cond ((null? contents) #t) 
        (else (let ((content-item (car contents)))
                 (cond ((ast? content-item) (if (or (memq (as-symbol (ast-element-name content-item)) symbol-choice-list)
                                                    (extraordinary-allow-element? (as-symbol (ast-element-name content-item)) (as-symbol element-name)))
                                                #t
                                                (return (ast-element-name content-item))))
                       ((cdata? content-item) #t)
                       ((char-ref? content-item) #t)
                       ((xml-comment? content-item) #t)
                       ((processing-instruction? content-item) #t)
                       ((cdata-section? content-item) #t)
                       ((white-space-related? content-item) #t)
                       (else (return #f)))
                 (do-validate-mixed-contents-1 (cdr contents) symbol-choice-list return element-name)))))

;; Is element-name extraordinarily allowed within context-element-name (both symbols).
;; By redefining this function you may extraordinarily allow element-name to appear within context-element-name.
;; By default, this function always returns the value #f (false).
(define (extraordinary-allow-element? element-name context-element-name)
  #f)


(define (xml-prepare-contents-for-validation contents)
  (append
    (map (lambda (content-item)
           (cond ((ast? content-item) (as-symbol (ast-element-name content-item)))
                 ((cdata? content-item) textual-content-symbol)
                 ((char-ref? content-item) textual-content-symbol)
                 ((cdata-section? content-item) textual-content-symbol)
                 (else (laml-error "xml-prepare-contents-for-validation: Unknown element content item:" content-item))))
         (filter (negate (disjunction processing-instruction? (disjunction white-space-related? xml-comment?))) contents))
    (list terminator-symbol)))

; Empty contents checking of empty elements. Allow XML comments and processing instructions.
(define (xml-check-for-empty-contents! contents tag-name)
 (let ((filtered-contents   ; filtering away PIs and XML comments. Notice that these are (p.t.) not rendered anyway in empty elements.
         (filter (negate (disjunction white-space-related? (disjunction processing-instruction?  xml-comment?))) contents)))
  (if (not (null? filtered-contents))
      (xml-check-error 
        (string-append "The empty element" (as-string-spacy tag-name) "is not supposed to have any content:" (as-string #\newline)
                       "  " (xml-render-error-message filtered-contents) (as-string #\newline) "  " "The element content is ignored.")))))

; Return "a" or "an" depending on following-word.
; following-word is string-converted by this function.
(define (a-or-an following-word)
 (let ((following-word-1 (as-string following-word)))
  (cond ((blank-string? following-word-1) "a")
        ((> (string-length following-word-1) 0)
         (let ((first-char (string-ref following-word-1 0)))
           (if (memv first-char (list #\a #\e #\i #\o #\u #\y ))
               "an"
               "a")))
        (else "a"))))

(define (as-string-spacy x)
 (string-append " " (as-string x) " "))


(define (indented-terminal-lines line-list)
 (let ((sep (string-append (as-string #\newline) "  ")))
  (string-append sep (list-to-string line-list sep))))


; ; A predicate which always gives positive validation.
; ; Used if the xml-in-laml tool constant use-manually-programmed-validation-predicates is #f.
; (define (faked-generice-content-checker x)
;   #t)


; and as a function - as opposed to a macro.
(define (and-fn x y) (and x y))
(define (or-fn x y) (or x y))



; --------------------------------------------------------------------------------------------------------------------------------
;;; Attribute Checking. 
;;; The definitions in this section is used by the validation procedures, which are specific
;;; for each supported XML language.
;;; <em> Current status of attribute checking</em>: 
;;; From LAML version 27: The constrains regarding ID, IDREF, and IDREFS are now handled. See the function check-id-and-idref-attributes!.
;;; NotationTypes are not dealt with - mostly because I have never encountered them, so the motivation to
;;; program check for this special enumeration type is not high. The #FIXED default keyword is present in the parsed DTD
;;; information, but we do not carry out any check against it (which is: If the attribute is given it must have the fixed default value).
;;; The attribute value normalization called for in section 3.3.3 of the XML specificaiton is not done either. Attribute duplication
;;; is checked for, as well as presence of angle characters in attribute values.
;;; .section-id attribute-check

; A list of attribute values for ID attributes.
; Such attribute values are required to be unique within the document. 
; This is checked by the procedure check-id-and-idref-attributes! .
(define xml-id-attribute-list '())

; A list of attribute values for IDREF attributes.
; Such attribute values are required to refer to one of the ID attributes, collected in xml-id-attribute-list.
; This is checked by the procedure check-id-and-idref-attributes! .
(define xml-idref-attribute-list '())

; Attribute tripple selectors
(define att-name (make-selector-function 1))
(define att-type (make-selector-function 2))
(define att-status (make-selector-function 3))

;; Check the attributes (first par) in the calling form against the attribute definitions taken from the DTD file (second par).
;; The first parameter, attribute, is the attributes of the calling form, on property list form:
;; (a1 v1 ... an vn). ai is a symbol and vi is a string.
;; The second parameter dtd-attribute-definition is the attributes as defined
;; in the dtd for the element in question. The third parameter number-of-req-attributes is the number
;; of required attributes in dtd-attr-definitions. It happens to be the case that all the required attributes
;; are located in the front of dtd-attribute-definition.
;; The fourth parameter, tag-name, is the name of the enclosing tag. dtd-attribute-definition is a list of 
;; triples (attr-name attr-type require-status). attr-name is a string, attr-type is
;; a string or a list of strings (possibilities), and require-status is a string
;; such as "#IMPLIED" or "#REQUIRED"
(define (xml-check-attributes! attributes dtd-attribute-definition number-of-req-attributes tag-name)
 (if (even? (length attributes))  ; if not, we report the error elsewhere
  (let ((required-attribute-names 
           (map (compose as-symbol att-name) (front-sublist dtd-attribute-definition number-of-req-attributes)))
        (dtd-attribute-names (map (compose as-symbol car) dtd-attribute-definition))
        (attribute-names (if (null? attributes) '() (every-second-element attributes)))
        (attribute-values (if (null? attributes) '() (every-second-element (cdr attributes))))
       )
    (xml-check-required-attributes! attribute-names required-attribute-names tag-name)
    (xml-check-for-attribute-existence! attribute-names dtd-attribute-names tag-name)
    (xml-check-for-attribute-types! attribute-names attribute-values dtd-attribute-definition tag-name)
    (xml-check-for-attribute-duplicates! attribute-names tag-name)
)))

;; Check that the ID and IDREF attribute values are used according to the XML 1.0 specification.
;; This function is called by the end-laml function, as redefined in this library (lib/xml-in-laml/xml-in-laml.scm).
;; You may also chose to call this function yourself, as part of the processing of your XML document.
;; This procedure resets the variables bookkeeping of ID and IDREF attribute checking such that if the procedure is called again
;; it will not report the same errors again.
(define (check-id-and-idref-attributes!)
  ; See section 3.3.1 of the XML 1.0 Specificatioin.

  (let ((id-duplicates (duplicates-by-predicate xml-id-attribute-list equal?)))

    ; Check that all ID attribute values are unique:
    (if (not (null? id-duplicates))
        (xml-check-error "The following ID attribute values are duplicated:" (list-to-string (map string-it id-duplicates) ",")))

    ; Check that all IDREF (and IDREFS) attribute values refer to an ID attribute in the current document.
    (for-each
      (lambda (idref-attr-val)
        (if (not (member idref-attr-val xml-id-attribute-list))
            (xml-check-error "The IDREF attribute value" (string-it idref-attr-val) "does not refer to an ID attribute."))
      )
      (reverse xml-idref-attribute-list))

    ; Make sure that we do not make double reporting:
    (set! xml-id-attribute-list '())
    (set! xml-idref-attribute-list '())
   ))
      

; Check that all elements in required-attribute-names are in fact present in attribute-names.
; The last parameter, tag-name is the name of the enclosing tag - used for error message purposes.
(define (xml-check-required-attributes! attribute-names required-attribute-names tag-name)
  (if (not (null? required-attribute-names))
      (begin
         (xml-check-one-required-attribute! attribute-names (car required-attribute-names) tag-name)
         (xml-check-required-attributes! attribute-names (cdr required-attribute-names) tag-name))))

(define (xml-check-one-required-attribute! attribute-names required-attribute tag-name)
  (if (not (memq required-attribute attribute-names))
      (xml-check-error 
        "The required attribute" (as-string-spacy required-attribute) "is not present in the" (as-string-spacy tag-name) "element.")))


;;; Link Checking.
;;; Web documents are able to link to other web documents. As an example, in XHTML the href attribute of the a element represents such a link.
;;; The procedures in this section are used to check that links actually address existing resources.

; A number of global variables related to link checking are located in laml.scm
; Reason: Avoids resetting (re-initialization) of this variables in case xml-in-laml.scm (this source file) is reloaded.

;; Collect the urls in xml-ast, which are about to be written to the file absolute-target-html-file (full path).
;; The urls are collected in the global variables relative-url-list-for-later-checking and
;; absolute-url-list-for-later-checking. The links are intended to be checked at a later point in time.
;; Each XML language L comes with an URL extraction function, returned by (url-extractor-of-xml-language L), which returns a list
;; of URLs when applied on an AST. Similarly, theres is a function (url-extractor-of-xml-language L) for handling of base URLs.
;; .internal-references "URL extraction" "url-extractor-of-xml-language" "base-url-extractor-of-xml-language"
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (collect-links-for-later-checking-in-ast! xml-ast absolute-target-html-file)
  (letrec ((url-not-deal-with?
            (lambda (url-string)
              (or (looking-at-substring? url-string 0 "mailto:")
                  (looking-at-substring? url-string 0 "file://")  ; LAML should deal with it!
                  (looking-at-substring? url-string 0 "ftp://")   ; LAML should deal with it!
                  (looking-at-substring? url-string 0 "prospero://")
                  (looking-at-substring? url-string 0 "wais://")
                  (looking-at-substring? url-string 0 "telnet://")
                  (looking-at-substring? url-string 0 "gopher://")
                  (looking-at-substring? url-string 0 "news:")))))
    (let* ((xml-language (ast-language xml-ast))
           (url-extractor-fn (url-extractor-of-xml-language xml-language))
           (base-url-extractor-fn (base-url-extractor-of-xml-language xml-language))
           (base-url (if base-url-extractor-fn (base-url-extractor-fn xml-ast) #f)) 
           (absolute-target-html-file-path (file-name-initial-path absolute-target-html-file))
           (url-list-1 (if url-extractor-fn (url-extractor-fn xml-ast) '()))
           (url-list-2 (if base-url
                           (map 
                            (lambda (url)
                              (if (relative-url? url) 
                                  (url-relative-to-base-url base-url url)  ; now ABSOLUTE url, to be further checked as such...
                                  url))
                            url-list-1)
                           url-list-1))
           )
      (for-each
       (lambda (url)
         (cond ((and (boolean? url) (not url))   ; URL extractor returned #f
                 'do-nothing)
               ((url-not-deal-with? url)         ; Some URLs, as recognized by the local function url-not-dealt-with?, are not supported (yet).
                'do-nothing)
               ((and (absolute-url? url) (memq xml-link-checking (list 'all 'absolute-urls)))
                (set! absolute-url-list-for-later-checking (cons url absolute-url-list-for-later-checking)))
               ((and (relative-url? url) (memq xml-link-checking (list 'all 'relative-urls)))
                (set! relative-url-list-for-later-checking 
                      (cons (list url absolute-target-html-file-path) relative-url-list-for-later-checking)))
               (else 'do-nothing)))
       url-list-2))))

; Resolve relative-url relative to absolute-base-url, as (for instance) provided in a an XHTML base element.
(define (url-relative-to-base-url absolute-base-url relative-url)
  (cond ((anchor-part-alone? relative-url) (string-append absolute-base-url relative-url))
        (else (string-append (file-name-initial-path absolute-base-url) relative-url))))

; Is url just an anchor part alone, such as "#foo"
(define (anchor-part-alone? url)
  (and (string? url) (> (string-length url) 0) (eqv? (string-ref url 0) #\#)))

;; Checks the existense of the resources addressed by the urls in relative-urls
;; Some of the elements may be boolean #f, which signals that they should not be checked.
;; In a typical checking sessions, the same file or directory may be checked more than once.
;; As an alternative to the current implementation of this function, we could eliminate duplicate
;; entries in relative-url-list before the existence check is done.
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (check-relative-url-list! relative-urls)
  (for-each
    (lambda (rel-url-entry)
     (let* ((rel-url (first rel-url-entry))
            (rel-url-without-anchor (eliminate-anchor-part-of-url rel-url))

            (rel-url-initial-path (file-name-initial-path rel-url-without-anchor))
            (rel-url-file-name-proper (file-name-proper rel-url-without-anchor))
            (rel-url-extension (file-name-extension rel-url-without-anchor))

            (initial-absolute-file-path (second rel-url-entry))
            (normalized-absolute-file-path
             (string-append
              (normalize-file-path (string-append initial-absolute-file-path rel-url-initial-path))
              rel-url-file-name-proper
              (if (empty-string? rel-url-extension) "" (string-append "." rel-url-extension)))) 
           )
      (if (and rel-url (not (empty-string? rel-url-without-anchor))   ; do not check relative urs of the form "#some-anchor" - they are relative to the current XML file.
               (and (not (file-exists? normalized-absolute-file-path))          ; check both file and directory existence - some urls address a directory!
                    (not (directory-exists? normalized-absolute-file-path)))
          )
          (begin 
             (xml-check-error "LINKING PROBLEM: URL  " rel-url "  RELATIVE TO  " initial-absolute-file-path)
             (set! relative-url-problem-count (+ relative-url-problem-count 1)) 
          )
      )
     ))
    relative-urls))

; Eliminate the anchor part of URL - the part starting with '#'
(define (eliminate-anchor-part-of-url url)
  (let ((hash-pos (find-in-string-from-end url #\#)))
    (if hash-pos
        (substring url 0 hash-pos)
        url)))

;; Checks the existense of the resources addressed by the urls in absoute-urls.
;; A given URL in the list is only checked once. 
;; Also increment the counter absolute-url-problem-count in case of problems with absolute-urls.
;; Requires the function url-target-exists? which is one of the LAML compatibility functions.
;; Currently, we do not a have a good implementation of url-target-exists? in the LAML distribution.
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (check-absolute-url-list! absolute-urls)
 (let ((unique-absolute-urls (remove-duplicates absolute-urls))) 
  (for-each
    (lambda (abs-url)
      (if (not (url-target-exists? abs-url))
          (begin
             (xml-check-error "LINKING PROBLEM TO  " abs-url)
             (set! absolute-url-problem-count (+ absolute-url-problem-count 1)))))
          
    unique-absolute-urls)))


;;; XML validation procedures.
;;; The functions in this section provide access to the XML-in-LAML validation procedures.
;;; The validation functions are part of the mirrors of a given XML language in Scheme.
;;; In normal and native use of XML-in-LAML, the validation procedures are called by the
;;; mirror functions. If the internal AST structure is created by other means (for instance
;;; authored directly, or made via a parser) it is relevant to get access to the validation procedures.
;;; This is the rationale behind the functions in this section. Thus, the procedures in this section
;;; use the functions from the sections Contents Validation and Attribute Checking.
;;; .section-id validation-functions

;; A validation procedure map of an XML language is a sorted, associative vector that maps
;; element names to XML validation procedures. The validation procedures are produced
;; by the XML-in-LAML mirror generation tool. This function returns the sorted associative vector
;; of language. If no validation procedure map exists for language, this function returns false.
;; .parameter language The name of the language (string or symbol)
(define (validation-procedure-map-of language)
  (defaulted-get (as-symbol language) xml-in-laml-validator-structures #f))

; Selector functions of an entry in a validation procedure map
(define validation-element-name-of-validator-entry (make-selector-function 1 "validation-element-name-of-validator-entry"))
(define validation-procedure-of-validator-entry (make-selector-function 2 "validation-procedure-of-validator-entry"))

;; Return the XML validation procedure of the element named element-name in language.
;; A validation procedure is created by the XML-in-LAML mirror generation tool on
;; basis of an XML DTD of language.
;; A validation procedure takes four parameters: element-name (string), attributes (property list), 
;; element content item list, and a boolean (XML language overlap check or not).
;; If language does not make sense, or if the element is unknown in the validation procedure map, return #f
;; .parameter element-name The name of the element (string or symbol)
;; .parameter language The name of the language (string or symbol)
(define (validation-procedure-of element-name language)
  (let* ((validator-map (validation-procedure-map-of language)))
    (if validator-map
        (let ((validator-proc 
                (binary-search-in-vector 
                  validator-map (as-string element-name) validation-element-name-of-validator-entry string=? string<=?)))
          (if validator-proc
              (validation-procedure-of-validator-entry validator-proc)
              #f)
        )
        #f)))

;; Register validator-structure for language. 
;; This function is called "automatically" when the mirror functions are loaded.
(define (register-xml-in-laml-validators language validator-structure)
 (set! xml-in-laml-validator-structures (cons (cons language validator-structure) xml-in-laml-validator-structures))
)

;; Validate ast.
;; XML validation is integrated in the mirrors of the XML elements in Scheme. 
;; Thus, this procedure is only useful if the AST is created by other means (manually, or via a parser, for instance).
;; Validation problems are reported by the procedure xml-check-error.
;; This is a procedure that produces errors or warnings (depending on xml-check-error).
;; The ast is valid if no error messages or warnings are produced by this function. 
;; .internal-references "error function" "xml-check-error"
;; .form (validate-ast! ast [given-language language-overlap-check?])
;; .parameter ast The internal representation of the document to be checked
;; .parameter given-language The XML language to which ast belongs. Defaults to (ast-language ast). A symbol.
;; .parameter language-overlap-check? Check for mutual naming overlap among the loaded XML languages. A boolean. Defaults to #t.
(define (validate-ast! ast . optional-parameter-list)
 (let ((given-language (optional-parameter 1 optional-parameter-list (ast-language ast)))
       (overlap-check? (optional-parameter 2 optional-parameter-list #t))
      )
  (let* ((el-name (ast-element-name ast))
         (attr-prop-list (ast-attributes ast))
         (element-content-items (ast-subtrees ast))
         (val-proc! (validation-procedure-of el-name given-language))
        )
    (if val-proc! 
        (val-proc! el-name attr-prop-list element-content-items overlap-check?)
        (xml-check-error "Using unknown XML element name: " el-name)) 
 
    (for-each 
      (lambda (ast) (validate-ast! ast given-language overlap-check?))  ; maybe problem with the passing of given-language
      (filter ast? element-content-items)))))

;;; XML warning procedures.
;;; .section-id warning-proc

;; A variant of display-warning which prefixes the warning text with a 'XML Warning' prefix.
(define (display-xml-warning . messages)
 (display (string-append "XML Warning: " (laml-aggregate-messages messages))) (newline))


;; The procedure that reports XML validation errors.
;; The default is display-xml-warning, which issues non-fatal warning messages.
;; As an alternative you can use laml-error which provides for fatal error messaging.
;; Both of these accept an arbitrary number of parameters, which are string converted prior to string appending them.
;; .form (xml-check-error . messages)
;; .internal-references "default value" "display-xml-warning"
;; .reference "alternative value" "laml-error" "../../man/general.html#laml-error"
(define xml-check-error display-xml-warning)


; Check that all names in attribute-names found in the list of names dtd-attribute-names
; tag-name is the name of the enclosing tag
; CSS attributes should not be checked for existence. 
(define (xml-check-for-attribute-existence! attribute-names dtd-attribute-names tag-name)
   (if (not (null? attribute-names))
       (begin
         (xml-check-one-attribute-existence! (car attribute-names) dtd-attribute-names tag-name)
         (xml-check-for-attribute-existence! (cdr attribute-names) dtd-attribute-names tag-name))))

(define (xml-check-one-attribute-existence! name dtd-attribute-names tag-name)
  (if (and (not (xml-css-key? name)) (not (memq name dtd-attribute-names)))
      (xml-check-error "The XML attribute" (as-string-spacy name) "is not valid in the"  (as-string-spacy tag-name) "element.")))

; Check that the attributes (as splitted into attribute-names and attribute-values - lists of equal lengths)
; have correct types according the dtd-attribute-names.
(define (xml-check-for-attribute-types! attribute-names attribute-values dtd-attributes tag-name)
  (if (not (null? attribute-names))
      (begin
         (xml-check-one-attribute-type! (car attribute-names) (car attribute-values) dtd-attributes tag-name)
         (xml-check-for-attribute-types! (cdr attribute-names) (cdr attribute-values) dtd-attributes tag-name))))

(define (xml-check-one-attribute-type! name value dtd-attributes tag-name)
  (let ((attribute-descriptor 
          (find-in-list (lambda (tripple) (eq? (as-symbol (att-name tripple)) name)) dtd-attributes)))
    (if attribute-descriptor
        (xml-check-attribute-value! name value (att-type attribute-descriptor) tag-name))))

; Is value a 'member of' attribute-type. value is a string.
; '<' characters are allowed, because they are transformed to &lt; by means of the character transformation table.
(define (xml-check-attribute-value! name value attribute-type tag-name)

  (cond ((list? attribute-type) 
            (if (not (member value attribute-type))  
                (xml-check-error (string-append "The value  " (string-it value) "  of the XML attribute  "
                                            (as-string name) "  is not valid in the "  tag-name " element."))))

        ((equal? "CDATA" attribute-type)
            #t)

        ((member attribute-type (list "ID" ))
            (let ((legal-name? (is-legal-xml-name? value)))
              (set! xml-id-attribute-list (cons value xml-id-attribute-list)) ; side effect: register ID attribute value
              (if (not legal-name?)
                  (xml-check-error "The ID attribute value" (string-it value) "is illegal according to the XML 1.0 Name production."))))

        ((member attribute-type (list "IDREF"))
            (let ((legal-name? (is-legal-xml-name? value)))
              (set! xml-idref-attribute-list (cons value xml-idref-attribute-list)) ; side effect: register IDREF attribute value
              (if (not legal-name?)
                  (xml-check-error "The IDREF attribute value" (string-it value) "is illegal according to the XML 1.0-spec Name production."))))

        ((member attribute-type (list "IDREFS"))
            (let ((name-list (extract-name-list-from-names-attribute value)))
              (for-each
                (lambda (value)
                  (let ((legal-name? (is-legal-xml-name? value)))
                    (set! xml-idref-attribute-list (cons value xml-idref-attribute-list))
                    (if (not legal-name?)
                        (xml-check-error "The IDREFS attribute value" (string-it value) "is illegal according to the XML 1.0-spec Name production.")))
                )
                name-list)
            ))

        ((member attribute-type (list "ENTITY" "ENTITIES" "NMTOKEN" "NMTOKENS"))
            #t)

        (else (xml-check-error (string-append "DTD error!!! The type  " (string-it attribute-type) "  of the XML attribute  "
                                            (as-string name) "  is not valid in the "  tag-name " element.")))
  ))


; The parameter names is a white space separated list of names (a string).
; Return the list of individual names (a list of strings).
(define (extract-name-list-from-names-attribute names)
  (split-string-by-predicate names (lambda (ch) (memv ch white-space-char-list))))


; Is x a legal XML name, as defined by Name production of the XML 1.0 spec.
; A conservative predicates that only allows english ASCII characters.
; Assume as a precondition that x is a string. 
(define (is-legal-xml-name? x)
  (if (empty-string? x)
      #f
      (let ((first-char (string-ref x 0))
            (suffix-str (substring x 1 (string-length x)))
            (digits (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 ))
            (letters (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                           #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
           )
        (and 
          (or (memv first-char letters) (eqv? first-char #\_) (eqv? first-char #\:))
          (string-of-char-list? suffix-str (append letters digits (list #\. #\- #\_ #\:)))))))

(define (xml-check-for-attribute-duplicates! attribute-names tag-name)
 (let ((duplicated-attribute-names (duplicates-by-predicate attribute-names eq?)))
   (cond ((and (not (null? duplicated-attribute-names)) (null? (cdr duplicated-attribute-names))) ; only one duplicated attribute
           (xml-check-error 
            "The attribute" (as-string-spacy (car duplicated-attribute-names))
                           "is not allowed to appear more than once in" (a-or-an tag-name) (as-string-spacy tag-name) "element."))
         ((not (null? duplicated-attribute-names))  ; two or more duplicated attribut names
           (xml-check-error 
            "The attributes" " " (list-to-string (map as-string duplicated-attribute-names) ", ") " "
                           "are not allowed to appear more than once in" (a-or-an tag-name) (as-string-spacy tag-name) "element."))
         (else #t) ; do nothing
   )))




; ---------------------------------------------------------------------------------------------------------------
; Check of language overlap

(define (check-language-overlap! name)
  (if (causes-xml-in-laml-name-clash? name)
      (xml-check-error 
       "The mirror function named" (as-string-spacy name) "is ambiguous. Please use it via an appropriate language map.")))


; ---------------------------------------------------------------------------------------------------------------
; Html character transformation table mutations.
; Mutations that cause literal presentation of the characters #\< , #\>, #\", #\' and #\&

(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\<) "&lt;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\>) "&gt;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\") "&quot;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\') "&#39;")
(set-html-char-transformation-entry! html-char-transformation-table (char->integer #\&) "&amp;")

; ---------------------------------------------------------------------------------------------------------------


;;; Rendering functions and procedures.
;;; The functions and procedures in this section transform the internal document representation
;;; (abstract syntax trees) to a textual form (text strings) which can be delivered in various ways (as strings or in streams).
;;; Both pretty printed and non pretty printed renderings are supported.
;;; In the start of the section we document a couple of variable that controls aspects of the 
;;; .section-id rendering

;; A boolean variable that controls how end tags are rendered when pretty printing is applied.
;; If the value of the variable is true (#t) there will be no newline character and no indentation in front of an end tag.
;; Use of the value true implies that a number of end tags will follow each other on the same line, and immediately after the element contents.
;; This is not a nice pretty printing, but it is necessary to obtain a correct rendering in most browsers.
;; If the value of the variable is false (#f) the end tag will be nicely aligned with the start tag.
;; We recommend the value false (#f) because most browsers leaves a trace of white space before end tags.
;; The default value is false (#f).
(define compact-end-tag-rendering? #t)

;; A boolean variable that controls how elements without contents are rendered.
;; If the value of the variable is true (#t) always use the empty element tag for elements that happen to have no contents.
;; If the value of the variable is false (#f), only use the empty element tag for elements that are statically declared to be empty.
;; For reasons of interoperability with old browsers we recommend the value false (#f).
;; The default value is false (#f).
(define use-empty-tags-for-elements-without-contents #f)

; ---------------------------------------------------------------------------------------------------
; Fast AST rendering.

;; Render the XML clause (an AST) to output-port.
;; output-port is assumed to be open. output-port will not be closed by this procedure.
;; .form (render-to-output xml-clause output-port [prolog epilog])
;; .parameter xml-clause an AST
;; .parameter output-port an open output port
;; .parameter prolog The symbol prolog, in which case (standard-prolog) is inserted, or a prolog string to be inserted
;; .parameter epilog The symbol epilog in which case (standard-epilog) is inserted, or a epilog string to be inserted
;; .reference "standard-prolog" "laml.scm" "../../../man/laml.html#standard-prolog"
;; .reference "standard-epilog" "laml.scm" "../../../man/laml.html#standard-epilog"
(define (render-to-output-port xml-clause output-port . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (language (if (ast? xml-clause) (ast-language xml-clause) #f))  ; if - just to be conservative.
        (prolog-text (cond ((and (symbol? prolog) (eq? prolog 'prolog)) (standard-prolog language))
                           ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((and (symbol? epilog) (eq? epilog 'epilog)) (standard-epilog))
                           ((string? epilog) epilog)
                           (else "")))
        (put-fn (put-in-sink-stream-generator output-port))
       )
  (put-fn prolog-text)
  (render-fast xml-clause put-fn xml-always-render-white-space?)
  (put-fn epilog-text)))

;; Pretty print the XML clause (an AST) to output-port.
;; output-port is assumed to be open. output-port will not be closed by this procedure.
;; The constants preferred-maximum-width and indentation-delta affect the pretty printing.
;; .form (pretty-render-to-output-port xml-clause output-port [prolog epilog])
;; .parameter xml-clause an AST
;; .parameter output-port an open output port
;; .parameter prolog The symbol prolog, in which case (standard-prolog) is inserted, or a prolog string to be inserted
;; .parameter epilog The symbol epilog in which case (standard-epilog) is inserted, or a epilog string to be inserted
;; .reference "standard-prolog" "laml.scm" "../../../man/laml.html#standard-prolog"
;; .reference "standard-epilog" "laml.scm" "../../../man/laml.html#standard-epilog"
(define (pretty-render-to-output-port xml-clause output-port . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (language (if (ast? xml-clause) (ast-language xml-clause) #f))  ; if - just to be conservative.
        (prolog-text (cond ((and (symbol? prolog) (eq? prolog 'prolog)) (standard-prolog language))
                           ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((and (symbol? epilog) (eq? epilog 'epilog)) (standard-epilog))
                           ((string? epilog) epilog)
                           (else "")))
        (put-fn (put-in-sink-stream-generator output-port))
       )
  (put-fn prolog-text)
  (pp-render-fast xml-clause put-fn xml-always-render-white-space? 0 #f)
  (put-fn epilog-text)))

;; Render the start-tag of xml-clause (a LAML AST) to output-port.
;; With this function, only the start tag and the attributes of the top-level element is rendered to the port.
;; This function is primarily useful for stepwise imperative processing of an XML document.
;; .parameter xml-clause an AST
;; .parameter output-port an open output port
(define (render-start-tag-to-output-port xml-clause output-port)
 (let ((put-fn (put-in-sink-stream-generator output-port)))
  (render-fast xml-clause put-fn xml-always-render-white-space? 'start-tag)))


;; Render the end-tag of xml-clause (a LAML AST) and return the rendered string.
;; With this function, only the end tag of the top-level element is rendered to the port.
;; This function is primarily useful for stepwise imperative processing of an XML document.
;; .parameter xml-clause an AST
;; .parameter output-port an open output port
(define (render-end-tag-to-output-port xml-clause output-port)
 (let ((put-fn (put-in-sink-stream-generator output-port)))
  (render-fast xml-clause put-fn xml-always-render-white-space? 'end-tag)))

;; Render the xml-clause (a LAML AST) and return the rendered string. In this context, rendering means
;; linearization of the AST to its textual form.
;; .form (xml-render xml-clause [prolog epilog])
;; .parameter xml-clause an AST
;; .parameter prolog The symbol prolog, in which case (standard-prolog) is inserted, or a prolog string to be inserted
;; .parameter epilog The symbol epilog in which case (standard-epilog) is inserted, or a epilog string to be inserted
(define (xml-render xml-clause . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (prolog-text (cond ((and (symbol? prolog) (eq? prolog 'prolog)) (standard-prolog))
                           ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((and (symbol? epilog) (eq? epilog 'epilog)) (standard-epilog))
                           ((string? epilog) epilog)
                           (else "")))
       )
   (reset-sink-string)
   (render-fast xml-clause put-in-sink-text-string xml-always-render-white-space?)
   (string-append
     prolog-text
     (sink-string)  
     epilog-text)))


;; Pretty print xml-clause (a LAML AST) and return the rendered string. In this context, rendering means
;; linearization of the AST to its textual, pretty printed form.
;; The constants preferred-maximum-width and indentation-delta affect the pretty printing.
;; .form (pretty-xml-render xml-clause [prolog epilog])
;; .parameter xml-clause an AST
;; .parameter prolog The symbol prolog, in which case (standard-prolog) is inserted, or a prolog string to be inserted
;; .parameter epilog The symbol epilog in which case (standard-epilog) is inserted, or a epilog string to be inserted
(define (pretty-xml-render xml-clause . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (prolog-text (cond ((and (symbol? prolog) (eq? prolog 'prolog)) (standard-prolog))
                           ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((and (symbol? epilog) (eq? epilog 'epilog)) (standard-epilog))
                           ((string? epilog) epilog)
                           (else "")))
       )
   (reset-sink-string)
   (pp-render-fast xml-clause put-in-sink-text-string xml-always-render-white-space? 0 #f)
   (string-append
     prolog-text
     (sink-string)  
     epilog-text)))





;; Render the start-tag of xml-clause (a LAML AST) and return the rendered string.
;; With this function, only the start tag and the attributes of the top-level element is rendered and returned.
;; This function is primarily useful for stepwise imperative processing of an XML document.
(define (start-tag-of xml-clause)
  (reset-sink-string)
  (render-fast xml-clause put-in-sink-text-string xml-always-render-white-space? 'start-tag)
  (sink-string))

;; Render the end-tag of xml-clause (a LAML AST) and return the rendered string.
;; With this function, only the end tag of the top-level element is rendered and returned.
;; This function is primarily useful for stepwise imperative processing of an XML document.
(define (end-tag-of xml-clause)
  (reset-sink-string)
  (render-fast xml-clause put-in-sink-text-string xml-always-render-white-space? 'end-tag)
  (sink-string))


; ---------------------------------------------------------------------------------------------------
; Contents rendering, for instance for error message purposes

; A variable which controls the style of xml-in-laml related error messages. Either the symbol laml or xml
(define xml-in-laml-error-message-style 'laml) 

; Render contents for error message purposes, xml style.
(define (xml-render-error-message contents)
 (cond ((eq? xml-in-laml-error-message-style 'laml) (xml-render-as-laml contents))
       ((eq? xml-in-laml-error-message-style 'xml) (xml-render-as-xml contents))
       (else (laml-error "xml-render-error-message" "Problem to render contents of error message"))))

; A slight generalization of xml-render - for error message purposes only.
(define (xml-render-as-xml contents)
  (cond ((ast? contents) (xml-render contents))
        ((cdata? contents) contents)
        ((char-ref? contents) (xml-render-char-ref contents))
        ((cdata-section? contents) (xml-render-cdata-section contents))
        ((forced-white-space? contents) "")
        ((delayed-procedural-contents-element? contents) "#<delayed-procedural-contents-element>")
        ((list? contents) (list-to-string (map xml-render-as-xml contents) " "))
        (else "??")))

(define (truncate-string str)
 (if (> (string-length str) xml-error-truncation-length)
     (string-append (substring str 0 xml-error-truncation-length) "...")
     str))



;; An rendering function which delivers a source-like Scheme expression as result (as a text string).
;; Can be used for error message purposes, and other purposes as well.
;; This function is slow due to use of heavy recursive string concatenation.  
;; See the faster alternative referred below.
;; If you write the resulting string to a file, with write-text-file, it is often useful to pretty-print this file with scheme-pp.
;; .form (xml-render-as-laml contents)
;; .parameter contents An AST, textual contents (a string) (and others), or a list of such content items.
;; .returns A text string.
;; .misc This function actually accepts an optional parameter, which we do not document.
;; .internal-references "Faster alternative" "render-as-laml-string"
;; .internal-references "Output port alternative" "render-as-laml-to-output-port"
;; .reference "Writing text string to file" "write-text-file" "../../../lib/man/file-read.html#write-text-file"
;; .reference "Useful post processing function" "scheme-pp" "../../../man/laml.html#scheme-pp"
(define (xml-render-as-laml contents . optional-parameter-list)
 (let ((contents-after (optional-parameter 1 optional-parameter-list 'none)))
  (string-append
   (cond ((ast? contents) (xml-render-ast-as-laml contents))
         ((char-ref? contents) (xml-render-char-ref-as-laml contents))
         ((cdata-section? contents) (xml-render-cdata-section-as-laml contents))
         ((white-space-related? contents) "")              ; ignore - already handled
         ((delayed-procedural-contents-element? contents) "#<delayed-procedural-contents-element>")
         ((list? contents) (list-to-string (map xml-render-as-laml contents) " "))
         (else (as-source-string contents)))
   (if (not (white-space-related? contents))
    (if (not (eq? contents-after 'none))
        (cond ((eq? contents-after explicit-space) "")
              (else " _"))
        "")
    ""))))
 

(define (xml-render-ast-as-laml ast)
 (let ((attributes (ast-attributes ast))
       (the-subtrees (ast-subtrees ast))
      )
  (string-append 
   "(" (ast-element-name ast) (if (not (null? attributes)) " " "")
       (xml-render-attribute-list-as-laml attributes) (if (not (null? the-subtrees)) " " "")
       (let ((subtrees the-subtrees))
         (string-merge (map-contextual xml-render-as-laml subtrees) (make-list (- (length subtrees) 1) " ")))
   ")")))

; A variant of map that passes both the current and the following element (if available) to the function.
; Consequently, f should be prepared (by an optional parameter) to accept two elements.
(define (map-contextual f lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (f (car lst))))
        (else (cons (f (car lst) (cadr lst)) (map-contextual f (cdr lst))))))


(define (xml-render-attribute-list-as-laml attr-property-list)
 (let ((lgt (length attr-property-list)))
   (string-merge (map as-source-string attr-property-list) (make-list (- lgt 1) " "))))

(define (xml-render-char-ref-as-laml char-ref)
 (string-append 
  "(" "char-ref" " "
      (as-source-string (char-ref-value char-ref))
  ")"))

(define (xml-render-cdata-section-as-laml cdata-section)
 (string-append 
  "(" "cdata-section" " "
      (list-to-string (map as-source-string (cdata-section-contents cdata-section)) " ")
  ")"))

(define (as-source-string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (string-append "'" (symbol->string x)))
        ((string? x) (string-it x))
        ((boolean? x) 
            (if x "#t" "#f"))  
        ((char? x) (string-append (as-string (as-char 35)) (as-string (as-char 92)) (char->string x)))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-source-string x) (make-list (- (length x) 1) " "))
               ")"))
        ((vector? x)
          (let ((lst (vector->list x)))
            (string-append "#(" 
               (string-merge (map as-source-string lst) (make-list (- (length lst) 1) " "))
               ")")))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-source-string y) " ")) (proper-part x))
               )
               " . " (as-source-string (first-improper-part x))
               ")"))
        ((procedure? x)
            "<PROCEDURE>")
        (else "??")))


; ---------------------------------------------------------------------------------------------------------------
; Begin June 3, 2007

;; Render the XML clause (an AST) as a (non-pretty printed) LAML source text to output-port.
;; output-port is assumed to be open. output-port will not be closed by this procedure.
;; As of June 2007, this function is still experimental. Some aspects of the AST may not yet be handled correctly. 
;; .form (render-as-laml-to-output xml-clause output-port)
;; .parameter xml-clause an AST
;; .parameter output-port an open output port
;; .parameter prolog a prolog string to be inserted (a prefix)
;; .parameter epilog an epilog string to be inserted (a suffix)
;; .internal-references "A variant that returns a string" "render-as-laml-string"
(define (render-as-laml-to-output-port xml-clause output-port . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (language (if (ast? xml-clause) (ast-language xml-clause) #f))  ; if - just to be conservative.
        (prolog-text (cond ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((string? epilog) epilog)
                           (else "")))
        (put-fn (put-in-sink-stream-generator output-port))
       )
  (put-fn prolog-text)
  (render-fast-as-laml xml-clause put-fn)
  (put-fn epilog-text)))

;; Render the xml-clause (a LAML AST) as a (non-pretty printed) LAML source text to output-port and return the rendered string.
;; As of June 2007, this function is still experimental. Some aspects of the AST may not yet be handled correctly. 
;; .form (render-as-laml-new xml-clause [prolog epilog])
;; .parameter xml-clause an AST
;; .parameter prolog  A prolog string to be inserted
;; .parameter epilog  An epilog string to be inserted
;; .internal-references "A much slower version" "xml-render-as-laml"
;; .internal-references "A variant that renders to port" "render-as-laml-to-output-port"
(define (render-as-laml-string xml-clause . optional-parameter-list)
 (let* ((prolog (optional-parameter 1 optional-parameter-list #f))
        (epilog (optional-parameter 2 optional-parameter-list #f))
        (prolog-text (cond ((string? prolog) prolog)
                           (else "")))
        (epilog-text (cond ((string? epilog) epilog)
                           (else "")))
       )
   (reset-sink-string)
   (render-fast-as-laml xml-clause put-in-sink-text-string)
   (string-append
     prolog-text
     (sink-string)  
     epilog-text)))


; Render ast via the put function. 
; render-what is fixed and constant: all.
(define (render-fast-as-laml ast put)
 (let* ((render-what 'all)
        (tag-name (ast-element-name ast)) 
        (contents-list (ast-subtrees ast))
        (attribute-properlist (ast-attributes ast))  ; split in normal and css here
        (attribute-alist (propertylist-to-alist attribute-properlist))
        (kind (ast-kind ast))
        (extraordinary-preserve-white-space #f) ; fixed and constant
        (language (ast-language ast))
       )

  (cond ((eq? kind 'single)  ; empty element
          (put #\() 
          (put tag-name) (put #\space) 

          ; Linearize HTML attributes - duplicate code inlined for double elements!
          (for-each 
            (lambda (attr-pair) 
             (let ((key (car attr-pair))
                   (val (cdr attr-pair)))
               (put #\space)
               (put #\') (put (symbol->string key)) (put #\space)
               (put quote-char)
               (do ((lgt (string-length val))
                    (i 0 (+ i 1))
                   )
                   ((= i lgt) 'done)
                 (let ((ch (string-ref val i)))
                   (put ch))
                 )
               (put quote-char) (put #\space))
            )
          attribute-alist)

        (put ")"))

        ((eq? kind 'double)  ; non-empty element
          (if (or (eq? render-what 'all) (eq? render-what 'start-tag))
            (begin
              (put #\() 
              (put tag-name) (put #\space)

                                        ; Linearize HTML attributes:
              (for-each 
               (lambda (attr-pair) 
                 (let ((key (car attr-pair))
                       (val (cdr attr-pair)))
                   (put #\space)
                   (put #\') (put (symbol->string key)) (put #\space)
                   (put quote-char)
                   (do ((lgt (string-length val))
                        (i 0 (+ i 1))
                        )
                       ((= i lgt) 'done)
                     (let ((ch (string-ref val i)))
                       (put ch)))
                   (put quote-char) (put #\space))
                 )
               attribute-alist)

            ))

          (if (or (eq? render-what 'all) (eq? render-what 'contents))
              (linearize-contents-list-fast-as-laml contents-list put tag-name ))

          (put ")")
       )

       (else (error "render-fast-as-laml: Either a single or double kind of ast expected.")))))

(define (linearize-contents-list-fast-as-laml contents-list put tag-name)
  (for-each 
    (lambda (contents) (linearize-contents-fast-as-laml contents put tag-name))
    contents-list))


(define (linearize-contents-fast-as-laml contents put tag-name)
  (cond ((char-ref? contents) (put (xml-render-char-ref-as-laml contents)))
        ((cdata-section? contents) (put (xml-render-cdata-section-as-laml contents)))
        ((cdata? contents) 
          (put (as-source-string contents)) (put #\space)
        )
        ((forced-white-space? contents) (put " "))    ; not correct rendering. I cannot easily insert "_" due to possitive white space repr. in the AST
        ((white-space-related? contents) (put ""))
        ((ast? contents) (render-fast-as-laml contents put))
        (else 'do-nothing)
   )
)

; End June 3, 2007
; ---------------------------------------------------------------------------------------------------------------



; A very simple pure textual rendering of content (an AST, a list of ASTs, char-ref, etc). Strips all markup.
; No formatting is provided at all.
; .returns a text string without markup and without any formatting.
; Old and primitive version.
; (define (xml-render-as-simple-text contents)
;  (cond ((ast? contents) (xml-render-as-simple-text (ast-subtrees contents)))
;        ((char-ref? contents) (as-string (char-ref-value contents)))
;        ((cdata-section? contents) (xml-render-cdata-section cdata-section))
;        ((and (white-space-related? contents) (eq? contents explicit-space)) " ")
;        ((white-space-related? contents) "")
;        ((delayed-procedural-contents-element? contents) "")  ; ignore
;        ((list? contents) (list-to-string (map xml-render-as-simple-text  contents) ""))
;        (else (as-string contents))))


; ----------------------------------------------------------------------------------------
; Rendering of ASTs (lists of ASTs, etc) as pure and simple text.
; Modelled after the function render-fast.


;; Pure textual rendering of contents. Strips all markup.
;; Suppress all superfluous white spacing.
;; No formatting is provided at all.
;; .returns a text string without markup and without any formatting.
;; .parameter contents An AST, textual contents (a string, CDATA), char-ref, etc, and a list of such content items.
(define (xml-render-as-simple-text contents)
   (reset-sink-string)
   (xml-render-as-simple-text-1 contents)
   (sink-string))

(define (xml-render-as-simple-text-1 contents)
   (cond ((ast? contents) (render-fast-simple-text contents put-in-sink-text-string #f))  ; xml-always-render-white-space?
         ((list? contents) (for-each (lambda (c) (xml-render-as-simple-text-1 c)) contents))
	 ((char-ref? contents) (put-in-sink-text-string (as-string (char-ref-value contents))))
	 ((cdata-section? contents) (put-in-sink-text-string (xml-render-cdata-section cdata-section)))
	 ((and (white-space-related? contents) (eq? contents explicit-space)) (put-in-sink-text-string #\space)) 
	 ((white-space-related? contents) 'do-nothing)
	 ((delayed-procedural-contents-element? contents) 'do-nothing)
         ((cdata? contents) (linearize-contents-fast-simple-text contents put-in-sink-text-string #f '() 'no-char-trans-table-needed #f))  ; in order to eliminate extra white space in pure text
         (else (put-in-sink-text-string (as-string contents)))))

; Adapted from render-fast
(define (render-fast-simple-text ast put always-render-white-space?)
 (let* ((render-what 'all)
        (tag-name (ast-element-name ast)) 
        (contents-list (ast-subtrees ast))
        (attribute-properlist (ast-attributes ast))  ; split in normal and css here
        (attribute-alist (propertylist-to-alist attribute-properlist))
        (reorganized-attribute-alist (html-css-split attribute-alist '() '()))
        (html-attribute-alist (car reorganized-attribute-alist))
        (css-attribute-alist  (cdr reorganized-attribute-alist))
        (kind (ast-kind ast))

        (language (ast-language ast))
        (xml-transliterate-character-data? #f)
        (xml-non-transliteration-elements (xml-non-transliteration-elements-in language))
        (xml-preformatted-text-elements (xml-preformatted-text-elements-in language))  ; (list "pre")
        (xml-char-transformation-table (xml-char-transformation-table-in language))

       )

  (cond ((eq? kind 'single)  ; empty element
          'do-nothing
        )

        ((eq? kind 'double)  ; non-empty element
          
          (if (or (eq? render-what 'all) (eq? render-what 'contents))
              (linearize-contents-list-fast-simple-text contents-list put xml-transliterate-character-data? xml-non-transliteration-elements
                                            xml-char-transformation-table 
                                            (or always-render-white-space? (member tag-name xml-preformatted-text-elements))))
        )

        (else (error "render-fast-simple-text: Either a single or double kind of ast expected.")))))


(define (linearize-contents-list-fast-simple-text contents-list put xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table always-render-white-space?)
  (for-each 
    (lambda (contents) 
      (linearize-contents-fast-simple-text contents put 
                                                xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table always-render-white-space?))
    contents-list))


(define (linearize-contents-fast-simple-text contents put xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table 
                                 always-render-white-space?)
  (cond ((char-ref? contents) (put (as-string (char-ref-value contents))))  ; ??
        ((xml-comment? contents) 'do-nothing)
        ((processing-instruction? contents) 'do-nothing)
        ((cdata-section? contents) (put (xml-render-cdata-section contents)))
        ((cdata? contents) 
          (let ((white-space-printed? #f)) ; true if last printed char is white space
            (do ((lgt (string-length contents))
		 (i 0 (+ i 1))
		 )
		((= i lgt) 'done)
	      (let ((ch (string-ref contents i)))
		(if always-render-white-space?
		    (put ch)
		    (begin
		      (if (not (and white-space-printed? (memv ch white-space-char-list)))
			  (if (memv ch white-space-char-list) (put #\space) (put ch)))
		      (if (memv ch white-space-char-list) (set! white-space-printed? #t) (set! white-space-printed? #f))
                      )
		    )
		)
	      )
           )
        )
        ((forced-white-space? contents) (put #\space))
        ((ast? contents) (render-fast-simple-text contents put always-render-white-space?))
        ((delayed-procedural-contents-element? contents) 'do-nothing)
        (else 'do-nothing)
   )
)

; End of simple text rendering.
; ----------------------------------------------------------------------------------------





; --------------------------------------------------------------------
; String sink in terms of segments (a list) of strings.

(define sink-string-segment-size 20000)
(define sink-string-segment-limit (- sink-string-segment-size 1))
(define output-sink-segments '())
(define output-sink-string (make-string sink-string-segment-size))
(define next-sink-ptr 0)

; Put x (string or char) into output sink.
(define (put-in-sink-text-string x)
  (cond ((char? x) 
          (begin
           (string-set! output-sink-string next-sink-ptr x)
           (set! next-sink-ptr (+ 1 next-sink-ptr))
           (if (= next-sink-ptr sink-string-segment-size) (prepare-next-sink-segment))))

        ((string? x)
          (do ((lgt (string-length x))
               (i 0 (+ i 1))
               (j next-sink-ptr (if (< j sink-string-segment-limit) (+ j 1) 0))
              )
              ((= i lgt) (set! next-sink-ptr j))
               (string-set! output-sink-string j (string-ref x i))
               (if (= j sink-string-segment-limit) (prepare-next-sink-segment))
              ))

        (else "put-in-sink-string: Can only output chars or strings")))

(define (prepare-next-sink-segment)
 (set! output-sink-segments (cons (string-copy output-sink-string) output-sink-segments))
 (set! next-sink-ptr 0)
)

; Return the resulting sink string
(define (sink-string)
 (string-append 
   (list-to-string (reverse output-sink-segments) "")
   (substring output-sink-string 0 next-sink-ptr)))

(define (reset-sink-string)
 (set! output-sink-segments '())
 (set! next-sink-ptr 0))

; --------------------------------------------------------------------
; Stream sink.

; Generate an output port put procedure
(define (put-in-sink-stream-generator port)
 (lambda (x)
  (cond ((char? x) 
          (write-char x port))
        ((string? x)
          (do ((lgt (string-length x))
               (i 0 (+ i 1))
              )
              ((= i lgt) 'done)
               (write-char (string-ref x i) port)))
        (else "put-in-sink-stream: Can only output chars or strings"))))


; --------------------------------------------------------------------
; Sink independet raw rendering. 
; The function render-fast, as shown below, has a 'pretty printing counterpart' called pp-render-fast.

(define quote-char #\")

; Render ast via the put function. 
; The optional parameter, render-what is either all, start-tag, contents, end-tag.
; It only applies for double elements (elements with contents).
; The default valus is all. If render-what is start-tag, only the start-tag is rendered.
; If render-what is contents, only the contents in between the start and end tag is rendered.
; If render-what is end-tag, only the end tag is rendered.
(define (render-fast ast put always-render-white-space? . optional-parameter-list)
 (let* ((render-what (optional-parameter 1 optional-parameter-list 'all))
        (tag-name (ast-element-name ast)) 
        (contents-list (ast-subtrees ast))
        (attribute-properlist (ast-attributes ast))  ; split in normal and css here
        (attribute-alist (propertylist-to-alist attribute-properlist))
        (reorganized-attribute-alist (html-css-split attribute-alist '() '()))
        (html-attribute-alist (car reorganized-attribute-alist))
        (css-attribute-alist  (cdr reorganized-attribute-alist))
        (kind (ast-kind ast))

        (extraordinary-preserve-white-space (as-boolean (ast-internal-attribute ast 'white-space-preserve #f)))
        (alternate-xml-char-transformation-table (ast-internal-attribute ast 'xml-char-transformation-table #f))

        (language (ast-language ast))
        (xml-transliterate-character-data? (xml-transliterate-character-data-in? language))
        (xml-non-transliteration-elements (xml-non-transliteration-elements-in language))
        (xml-preformatted-text-elements (xml-preformatted-text-elements-in language))  ; like:  (list "pre")
        (xml-char-transformation-table (if alternate-xml-char-transformation-table
                                           (eval-cur-env (as-symbol alternate-xml-char-transformation-table))
                                           (xml-char-transformation-table-in language)))

       )

  (cond ((eq? kind 'single)  ; empty element
          (put #\<) 
          (put tag-name) 

          ; Linearize HTML attributes - duplicate code inlined for double elements!
          (for-each 
            (lambda (attr-pair) 
             (let ((key (car attr-pair))
                   (val (cdr attr-pair)))
               (put #\space)
               (put (symbol->string key))
               (put #\=) (put quote-char)
               (do ((lgt (string-length val))
                    (i 0 (+ i 1))
                   )
                   ((= i lgt) 'done)
                 (let ((ch (string-ref val i)))
                   (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table))))
                 )
               (put quote-char))
            )
          html-attribute-alist)

          ; Linearize CSS attributes - duplicate code inlined for double elements!
          (if (not (null? css-attribute-alist))
              (begin
                (put #\space) 
                (put "style=") (put quote-char)
                (for-each
                 (lambda (attr-pair) 
                   (let* ((key (symbol->string (car attr-pair)))
                          (non-cssed-key (substring key 4 (string-length key)))
                          (val (cdr attr-pair)))
                     (put non-cssed-key)
                     (put #\:)
                     (do ((lgt (string-length val))
                          (i 0 (+ i 1))
                         )
                         ((= i lgt) 'done)
                       (let ((ch (string-ref val i)))
                         (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                     (put #\;))) 
                css-attribute-alist)
                (put quote-char)))

          (put " />"))

        ((eq? kind 'double)  ; non-empty element
          (if (or (eq? render-what 'all) (eq? render-what 'start-tag))
            (begin
              (put #\<) 
              (put tag-name) 

                                        ; Linearize HTML attributes:
              (for-each 
               (lambda (attr-pair) 
                 (let ((key (car attr-pair))
                       (val (cdr attr-pair)))
                   (put #\space)
                   (put (symbol->string key))
                   (put #\=) (put quote-char)
                   (do ((lgt (string-length val))
                        (i 0 (+ i 1))
                        )
                       ((= i lgt) 'done)
                     (let ((ch (string-ref val i)))
                       (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                   (put quote-char))
                 )
               html-attribute-alist)

                                        ; Linearize CSS attributes:
              (if (not (null? css-attribute-alist))
                  (begin
                    (put #\space) 
                    (put "style=") (put quote-char)
                    (for-each
                     (lambda (attr-pair) 
                       (let* ((key (symbol->string (car attr-pair)))
                              (non-cssed-key (substring key 4 (string-length key)))
                              (val (cdr attr-pair)))
                         (put non-cssed-key)
                         (put #\:)
                         (do ((lgt (string-length val))
                              (i 0 (+ i 1))
                              )
                             ((= i lgt) 'done)
                           (let ((ch (string-ref val i)))
                             (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                         (put #\;))) 
                     css-attribute-alist)
                    (put quote-char)))

              (if (and use-empty-tags-for-elements-without-contents (null? contents-list)) (put " />") (put #\>)) ))

          (if (or (eq? render-what 'all) (eq? render-what 'contents))
              (linearize-contents-list-fast contents-list put tag-name xml-transliterate-character-data? xml-non-transliteration-elements
                                            xml-char-transformation-table 
                                            (or always-render-white-space? (member tag-name xml-preformatted-text-elements) extraordinary-preserve-white-space)))

          (if (not (and use-empty-tags-for-elements-without-contents (null? contents-list)))
              (if (or (eq? render-what 'all) (eq? render-what 'end-tag))
		  (begin (put "</")  (put tag-name) (put #\>)))))

       (else (error "render-fast: Either a single or double kind of ast expected.")))))

(define (linearize-contents-list-fast contents-list put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table always-render-white-space?)
  (for-each 
    (lambda (contents) (linearize-contents-fast contents put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table always-render-white-space?))
    contents-list))


(define (linearize-contents-fast contents put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table 
                                 always-render-white-space?)
  (cond ((char-ref? contents) (put (xml-render-char-ref contents)))
        ((xml-comment? contents) (put (xml-render-xml-comment contents)))
        ((processing-instruction? contents) (put (xml-render-processing-instruction contents)))
        ((cdata-section? contents) (put (xml-render-cdata-section contents)))
        ((cdata? contents) 
          (let ((white-space-printed? #f)) ; true if last printed char is white space
           (if (and xml-transliterate-character-data? (not (member tag-name xml-non-transliteration-elements)))

               (do ((lgt (string-length contents))
                    (i 0 (+ i 1))
                    )
                   ((= i lgt) 'done)
                 (let* ((ch (html-char-transform (string-ref contents i) xml-char-transformation-table)) ; a string!
                        (ch-white-space? (and (not (empty-string? ch)) (string-of-char-list? ch white-space-char-list)))
                       )
                  (if always-render-white-space?
                      (put ch)
                      (begin
                        (if (not (and white-space-printed? ch-white-space?))
                            (if ch-white-space? (put #\space) (put ch)))
                        (if ch-white-space? (set! white-space-printed? #t) (set! white-space-printed? #f))
                      )
                  )
                 )
               )

               (do ((lgt (string-length contents))
                    (i 0 (+ i 1))
                    )
                   ((= i lgt) 'done)
                 (let ((ch (string-ref contents i)))
                  (if always-render-white-space?
                      (put ch)
                      (begin
                       (if (not (and white-space-printed? (memv ch white-space-char-list)))
                           (put ch))
                       (if (memv ch white-space-char-list) (set! white-space-printed? #t) (set! white-space-printed? #f))
                      )
                  )
                 )
               )
            )
           )
        )
        ((forced-white-space? contents) (put #\space))
        ((ast? contents) (render-fast contents put always-render-white-space?))
        ((delayed-procedural-contents-element? contents)
           (display-warning "Attempting to render delayed procedural content element - ignored"))
        (else 'do-nothing)
   )
)


; Return whether key is a css-key, for instance a key like css:k or CSS:k
; A true value is the key proper, such as k (as a string).
(define (xml-css-key? key)
  (let* ((key-str (symbol->string key))
         (lgt (string-length key-str)))
   (if
    (and (>= lgt 4) 
         (or (eqv? (string-ref key-str 0) #\c) (eqv? (string-ref key-str 0) #\C))
         (or (eqv? (string-ref key-str 1) #\s) (eqv? (string-ref key-str 1) #\S))
         (or (eqv? (string-ref key-str 2) #\s) (eqv? (string-ref key-str 2) #\S))
         (eqv? (string-ref key-str 3) #\:))
    (substring key-str 4 lgt)
    #f)))

; Split attribute-alist in two parts: the HTML attributes and the CSS attributes.
; Return (cons html-alist css-alist) where html-alist is the association lists of HTML attributes
; and css-alist is the association list of CSS attributes.
(define (html-css-split attribute-alist html-alist css-alist)
  (cond ((null? attribute-alist) (cons (reverse html-alist) (reverse css-alist)))
        ((xml-css-key? (caar attribute-alist)) 
            (html-css-split (cdr attribute-alist) html-alist (cons (car attribute-alist) css-alist)))
        (else 
            (html-css-split (cdr attribute-alist) (cons (car attribute-alist) html-alist) css-alist))))


; ----------------------------------------------------------------------------------------------------------------------------------------------------------
; Pretty printed, fast AST rendering.
; The procedures pp-render-fast, pp-linearize-contents-list-fast, and pp-linearize-contents-fast
; are very similar to render-fast, linearize-contents-list-fast, and linearize-contents-fast. 
; This is an important notice for maintenance purposes. 
; In case of single line rendering of an XML fragment, pp-render-fast calls render-fast.


(define (pp-render-fast ast put always-render-white-space? start-col single-lining?)
 (if (single-liner-form? ast start-col preferred-maximum-width)
     (render-fast ast put #f)
     (let* ((tag-name (ast-element-name ast)) 
            (contents-list (ast-subtrees ast))
            (attribute-properlist (ast-attributes ast)) ; split in normal and css here
            (attribute-alist (propertylist-to-alist attribute-properlist))
            (reorganized-attribute-alist (html-css-split attribute-alist '() '()))
            (html-attribute-alist (car reorganized-attribute-alist))
            (css-attribute-alist  (cdr reorganized-attribute-alist))
            (kind (ast-kind ast))

            (language (ast-language ast))
            (xml-transliterate-character-data? (xml-transliterate-character-data-in? language))
            (xml-non-transliteration-elements (xml-non-transliteration-elements-in language))
            (xml-preformatted-text-elements (xml-preformatted-text-elements-in language)) ; (list "pre")
            (xml-char-transformation-table (xml-char-transformation-table-in language))

            (attr-width 0)  ; approximative printed attribute width
            (attr-cnt 0)
            (attr-lgt (length attribute-alist))

           )

       (cond ((eq? kind 'single)        ; empty element
              (put #\<) 
              (put tag-name) 

                                        ; Linearize HTML attributes - duplicate code inlined for double elements!
              (for-each 
               (lambda (attr-pair) 
                 (let ((key (car attr-pair))
                       (val (cdr attr-pair)))
                   (put #\space)
                   (put (symbol->string key))
                   (put #\=) (put quote-char)
                   (do ((lgt (string-length val))
                        (i 0 (+ i 1))
                        )
                       ((= i lgt) 'done)
                     (let ((ch (string-ref val i)))
                       (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                   (put quote-char)
                   (set! attr-width (+ attr-width (string-length (symbol->string key)) (string-length val) 3))
                   (set! attr-cnt (+ 1 attr-cnt)) 
                   (if (and (> (+ attr-width start-col) preferred-maximum-width) (< attr-cnt attr-lgt))
                       (begin (put #\newline) (put-indentation put (+ (string-length tag-name) start-col 1)) (set! attr-width 0))) ; PP!!
                  )
                 )
               html-attribute-alist)

                                        ; Linearize CSS attributes - duplicate code inlined for double elements!
              (if (not (null? css-attribute-alist))
                  (begin
                    (put #\space) 
                    (put "style=") (put quote-char)
                    (for-each
                     (lambda (attr-pair) 
                       (let* ((key (symbol->string (car attr-pair)))
                              (non-cssed-key (substring key 4 (string-length key)))
                              (val (cdr attr-pair)))
                         (put non-cssed-key)
                         (put #\:)
                         (do ((lgt (string-length val))
                              (i 0 (+ i 1))
                              )
                             ((= i lgt) 'done)
                           (let ((ch (string-ref val i)))
                             (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                         (put #\;))) 
                     css-attribute-alist)
                    (put quote-char)))

              (put " />"))

             ((eq? kind 'double)        ; non-empty element
              (put #\<) 
              (put tag-name) 

                                        ; Linearize HTML attributes:
              (for-each 
               (lambda (attr-pair) 
                 (let ((key (car attr-pair))
                       (val (cdr attr-pair)))
                   (put #\space)
                   (put (symbol->string key))
                   (put #\=) (put quote-char)
                   (do ((lgt (string-length val))
                        (i 0 (+ i 1))
                        )
                       ((= i lgt) 'done)
                     (let ((ch (string-ref val i)))
                       (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                   (put quote-char)
                   (set! attr-width (+ attr-width (string-length (symbol->string key)) (string-length val) 3)) 
                   (set! attr-cnt (+ 1 attr-cnt)) 
                   (if (and (> (+ attr-width start-col) preferred-maximum-width) (< attr-cnt attr-lgt))
                       (begin (put #\newline) (put-indentation put (+ (string-length tag-name) start-col 1)) (set! attr-width 0)))
                  )
                 )
               html-attribute-alist)

                                        ; Linearize CSS attributes:
              (if (not (null? css-attribute-alist))
                  (begin
                    (put #\space) 
                    (put "style=") (put quote-char)
                    (for-each
                     (lambda (attr-pair) 
                       (let* ((key (symbol->string (car attr-pair)))
                              (non-cssed-key (substring key 4 (string-length key)))
                              (val (cdr attr-pair)))
                         (put non-cssed-key)
                         (put #\:)
                         (do ((lgt (string-length val))
                              (i 0 (+ i 1))
                              )
                             ((= i lgt) 'done)
                           (let ((ch (string-ref val i)))
                             (if (eqv? ch #\&) (put ch) (put (html-char-transform ch xml-char-transformation-table)))))
                         (put #\;))) 
                     css-attribute-alist)
                    (put quote-char)))

              (if (and use-empty-tags-for-elements-without-contents (null? contents-list))
                  (put " />")
                  (begin
		    (put #\>)
		    (put #\newline) (put-indentation put (+ start-col indentation-delta)) ; PP!!

		    (pp-linearize-contents-list-fast 
		     contents-list put tag-name 
		     xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table
		     (or always-render-white-space? (member tag-name xml-preformatted-text-elements))
		     (+ start-col indentation-delta) single-lining?)

		    (if (not compact-end-tag-rendering?) ; Jan 17, 2006. Experimentally removed to avoid white space problems.
			(begin (put #\newline) (put-indentation put start-col)))

					; !!!!!
		    (put "</")  (put tag-name) (put #\>))))

             (else (error "pp-render-fast: Either a single or double kind of ast expected."))))))

(define (pp-linearize-contents-list-fast contents-list put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table 
                                        always-render-white-space? start-col single-lining?)
  (for-each 
    (lambda (contents) (pp-linearize-contents-fast contents put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table
                                                   always-render-white-space? start-col single-lining?))
    contents-list))


(define (pp-linearize-contents-fast contents put tag-name xml-transliterate-character-data? xml-non-transliteration-elements xml-char-transformation-table 
                                    always-render-white-space? start-col single-lining?)
  (cond ((char-ref? contents) (put (xml-render-char-ref contents)))
        ((xml-comment? contents) (put (xml-render-xml-comment contents)))
        ((processing-instruction? contents) (put (xml-render-processing-instruction contents)))
        ((cdata-section? contents) (put (xml-render-cdata-section contents)))
        ((cdata? contents) 
          (let ((white-space-printed? #f)) ; true if last printed char is white space
           (if (and xml-transliterate-character-data? (not (member tag-name xml-non-transliteration-elements)))

               (do ((lgt (string-length contents))   ; With HTML transformations
                    (i 0 (+ i 1))
                    (j 0 (+ j 1))
                    )
                   ((= i lgt) 'done)
                 (let* ((ch (html-char-transform (string-ref contents i) xml-char-transformation-table)) ; a string!
                        (ch-white-space? (and (not (empty-string? ch)) (string-of-char-list? ch white-space-char-list)))
                       )
                  (if always-render-white-space?     
                      (put ch)
                      (begin
                        (if (not (and white-space-printed? ch-white-space?))
                            (if ch-white-space? 
                               (if (> (+ j start-col) preferred-maximum-width)
                                   (begin (put #\newline) (put-indentation put start-col) (set! j 0)) ; PP!!
                                   (put #\space)
                               )
                               (put ch))
                        )
                        (if ch-white-space? (set! white-space-printed? #t) (set! white-space-printed? #f))
                      )
                  )
                 )  
               )

               (do ((lgt (string-length contents))   ; Without HTML transformations - must be tested carefully.
                    (i 0 (+ i 1))
                    (j 0 (+ j 1))
                   )
                   ((= i lgt) 'done)
                 (let* ((ch (string-ref contents i))
                        (ch-white-space? (memv ch white-space-char-list))
                       )
                  (if always-render-white-space?
                      (put ch)
                      (begin
                       (if (not (and white-space-printed? ch-white-space?))
                           (if ch-white-space?
                               (if (> (+ j start-col) preferred-maximum-width)
                                   (begin (put #\newline) (put-indentation put start-col) (set! j 0)) ; PP!!
                                   (put #\space)
                               )                               
                               (put ch))
                       )
                       (if (memv ch white-space-char-list) (set! white-space-printed? #t) (set! white-space-printed? #f))
                      )
                  )
                 )
               )
            )
           )
        )
        ((forced-white-space? contents)
           (put #\newline) (put-indentation put start-col)  ; PP!!
        )
        ((ast? contents)
           (pp-render-fast contents put always-render-white-space? start-col single-lining?)
        )
        ((delayed-procedural-contents-element? contents)
           (display-warning "Attempting to render delayed procedural content element - ignored"))
        (else 'do-nothing)
   )
)

(define (put-indentation put n)
 (do ((i 1 (+ i 1)))
    ((> i n) 'done) 
  (put #\space)))

(define (single-liner-form? ast start-col max-width)
 (let ((width (measure-xml-in-laml-form ast)))
   (<= (+ width start-col) max-width)))


; Approximative measurement of an AST, string, character reference or white space marker.
(define (measure-xml-in-laml-form x)  
 (cond ((string? x) (string-length x))  ; not accurate due to use of the HTML transformation table.
       ((forced-white-space? x) 1)
       ((char-ref? x) 
        (let ((value (char-ref-value x)))
          (cond ((symbol? value) (+ 2 (string-length (symbol->string value))))
                ((number? value) (+ 3 (string-length (number->string value))))
                (else (laml-error "measure-xml-in-laml-form: Error in character reference" x))))
       )
       ((xml-comment? x) 
        (let* ((comment-contents (xml-comment-contents x))  ; a list of strings
               (contents-sum (sum-list (map string-length comment-contents)))
               (comment-count (length comment-contents)))
          (+  contents-sum comment-count 6)
        )
       )
       ((processing-instruction? x) 
        (let* ((target (processing-instruction-target x))
               (contents (processing-instruction-contents x))  ; a list of strings
               (contents-sum (sum-list (map string-length contents)))
               (count (length contents)))
          (+  (string-length target) contents-sum count 4)
        )
       )
       ((cdata-section? x)
        (let* ((cdata-contents (cdata-section-contents x))  ; a list of strings         
               (contents-sum (sum-list (map string-length cdata-contents)))
               (count (length cdata-contents)))
          (+ contents-sum count 10))
       )

       ((ast? x)
         (let ((tag-name (ast-element-name x))
               (attributes (propertylist-to-alist (ast-attributes x)))
               (content-list (ast-subtrees x))
              )
           (+ (* 2 (string-length tag-name))                       ; tag-names, start and end
              5                                                    ; angle brackets in start and end tag, '/' in end tag.
              (measure-attribute-list attributes)                  ; attributes
              (sum-list
                (map measure-xml-in-laml-form content-list))       ; contents
           )
         )
       )
       ((delayed-procedural-contents-element? x) 0)   ; very bad estimate indeed. We actually assume that we never measure ASTs with delayed procedural content elements...
       (else (laml-error "measure-xml-in-laml-form: Unknown constituent" x))
  )
)

; Measure attribute-list, which is represented on a-list form.
; Approximative measure - does not take css style attributes into special account
(define (measure-attribute-list attribute-alist)
  (sum-list (map measure-attribute attribute-alist)))

; Measure a single attribute, non CSS. Approximative.
(define (measure-attribute key-val)
 (let ((key (car key-val))
       (val (cdr key-val)))
  (+ (string-length (symbol->string key))
     (string-length val)
     4  ; string quotes, '=', and space after.
  )))


; -------------------------------------------------------------------------------------------------------------------------------------------------
; Attribute linearization functions.  Linearizes to a string.
; These functions are NOT used in render-fast (which uses an imperative, 
; more efficient, html-char-table transformed approach).
; The functions are used in other corners of LAML, such as in relation to AST tree transformation, error messaging,
; and pretty printed rendering.

; Return a pair of strings: The head is the string represeting XML attributes.
; The tail is the string representing the CSS attributes (to go into an HTML style attribute).
(define (xml-linearize-attributes attr-list)
 (let ((lgt (length attr-list)))
  ; catch a typical error here: non-symbol key, probably old-fashioned calling convention from html-v1.
  (if (and (>= lgt 1) (not (symbol? (car attr-list))))
      (error (string-append "xml-linearize-attributes: Non-symbol key encountered: " (as-string (car attr-list)) " in attribute list " (as-string attr-list) ". Maybe conversion problem from html-v1.")))
  (xml-linearize-attributes-1 (reverse attr-list) "" "" lgt attr-list)))

; Internal helping operation to xml-linearize-attributes. The parameter whole-attr-list is for error messaging purposes
(define (xml-linearize-attributes-1 attr-list html-attr-string css-attr-string lgt whole-attr-list)
  (cond ((= lgt  0 ) (cons (strip-trailing-characters (list #\space) html-attr-string) css-attr-string))
        ((>= lgt 2) (let* ((val (car attr-list))   ; val and key are reversed at this time
                           (key (cadr attr-list))
                           (css-key (xml-css-key? key))
                         )
                     (cond (css-key                  ; CSS attribute
                              (xml-linearize-attributes-1 
                                (cddr attr-list)
                                html-attr-string 
                                (string-append (xml-linearize-attribute-pair-css val css-key) ";" css-attr-string)
                                (- lgt 2)
                                whole-attr-list))
                           (else                            ; HTML attribute
                             (xml-linearize-attributes-1 
                               (cddr attr-list)
                               (string-append (xml-linearize-attribute-pair-html val key) " " html-attr-string)
                               css-attr-string
                               (- lgt 2)
                               whole-attr-list)))))
        ((< lgt 2) (error (string-append "Xml-Linearize-attributes-1: Called with an odd length attribute list. Not a Lisp property list: " (as-string whole-attr-list))))))


(define (xml-linearize-attribute-pair-html val key)
  (string-append (as-string key) " = " (string-it (as-string val))))

(define (xml-linearize-attribute-pair-css val key)
  (string-append key ": " (as-string val)))

; ---------------------------------------------------------------------------------------------------------------


;;; Element related functions.
;;; This section contains (mostly) higher-order functions that are related to 
;;; the mirrors of the XML elements.  
;;; .section-id element-mod

;; Bind some attributes content elements of element (the first parameter) and return a new, 'modified element function'.
;; With this, some attributes and some content elements are pre-bound to certain values in the modified element.
;; The parameter attributes-and-contents is of the same form as the parameters to a LAML surface mirror functions.
;; In fact, attributes-and-contents is appended to the actual parameters, which are passed to the modified element function.
;; .example (define a-main (modify-element a 'target "main"))
;; .returns an attribute-modified mirror function
;; .internal-references "similar function" "xml-modify-element-prepend"
(define (xml-modify-element element . attributes-and-contents)
  (lambda parameters (apply element (append parameters attributes-and-contents))))

;; A function similar to xml-modify-element, but instead of appending attributes-and-contents to the
;; actual parameters of the modified function, it prepends attributes-and-contents.
;; .internal-references "similar function" "xml-modify-element"
(define (xml-modify-element-prepend element . attributes-and-contents)
  (lambda parameters (apply element (append attributes-and-contents parameters))))


;;; XML in LAML abstraction functions.
;;; The functions in this category are higher-order functions which generate functions that obey XML-in-LAML parameter passing rules.
;;; In other words, the function that are generated use the same parameter conventions as the mirror functions of the HTML and XML elements.
;;; The most useful function is xml-in-laml-abstraction.  xml-in-laml-positional-abstraction is a generalized version of  xml-in-laml-abstraction.
;;; The function xml-in-laml-parametrization is only useful if you want to interface between XML-in-LAML parameter passing rules
;;; and a function with simple, conventional, positional parameter passing.
;;; .section-id abstraction-fu


;; Generate a function with XML-in-LAML parameter passing rules, which passes its contents to the first parameter of f, and its attributes to the second parameter of f.
;; The function f can call and XML-in-LAML mirror function, or another XML-in-LAML abstraction.
;; .form (xml-in-laml-abstraction f [parameter-validator! f-name language])
;; .internal-references "internally applied function" "xml-sort-superficially-tag-parameters"
;; .internal-references "similar function" "xml-in-laml-parametrization"
;; .internal-references "generalized function" "xml-in-laml-positional-abstraction"
;; .internal-references "useful par. validator" "required-implied-attributes"
;; .parameter f A transformer function of two parameters - contents and the attribute property list. The generated function returns the value of f applied on contents and attributes.
;; .parameter parameter-validator! A checking procedure of two parameters - contents and the attribute property list. Must report errors explicitly by xml-check-error.\
;;                                 Defaults to a validator that always returns #t.
;; .parameter f-name The name of the abstraction - used for error message purposes only. A string.
;; .parameter language The name of the XML language to which the generated xml-in-laml abstraction belongs.\
;;                     If you wish the most precise warnings or error messages you should pass this parameter. A symbol.
;; .reference "LAML tutorial" "Web authoring with higher-order functions" "../../../tutorial/higher-order-authoring/html/index.html"
;; .misc See the LAML tutorial referenced above for additional discussion and examples. 
(define (xml-in-laml-abstraction f  . optional-parameter-list)
 (let ((parameter-validator! (optional-parameter 1 optional-parameter-list (lambda (co at) #t)))
       (f-name (optional-parameter 2 optional-parameter-list "ad hoc abstraction"))
       (language (optional-parameter 3 optional-parameter-list (guess-xml-language-if-possible)))
      )
  (lambda parameters 
    (let* ((ordered-parameters (xml-sort-superficially-tag-parameters parameters f-name language))
           (content-parameters (car ordered-parameters))
           (attribute-prop-list (cdr ordered-parameters))
          )
      (parameter-validator! content-parameters attribute-prop-list)
      (f content-parameters attribute-prop-list)))))


;; Generate a function with XML-in-LAML parameter passing rules, together with a number of required parameters before and after the XML-in-LAML parameters.
;; .form (xml-in-laml-positional-abstraction n m f [parameter-validator! f-name language])
;; .pre-condition n + m is less than or equal to the length of parameterlist of the generated function. (Condition checked by this function).
;; .parameter n  The number of positional parameters before the XML-in-LAML parameter section.
;; .parameter m  The number of positional parameters after the XML-in-LAML parameter section.
;; .parameter f A transformer function of n + 2 + m parameters. The two parameters in the middle are the contents and the attributes. The generated function returns the value of f applied on contents and attributes together with the positional parameters.
;; .parameter parameter-validator! A checking procedure of two parameters - contents and the attribute property list. Must report errors explicitly by xml-check-error. Cannot validate the positional parameters.
;; .parameter f-name The name of the abstraction - used for error message purposes only.
;; .parameter language The name of the XML language to which the generated xml-in-laml abstraction belongs.\
;;                     If you wish the most precise warnings or error messages you should pass this parameter. A symbol.
;; .internal-references "specialized function" "xml-in-laml-abstraction"
;; .internal-references "useful par. validator" "required-implied-attributes"
(define (xml-in-laml-positional-abstraction n m f  . optional-parameter-list)
 (let ((parameter-validator! (optional-parameter 1 optional-parameter-list (lambda (co at) #t)))
       (f-name (optional-parameter 2 optional-parameter-list "ad hoc abstraction"))
       (language (optional-parameter 3 optional-parameter-list (guess-xml-language-if-possible)))
      )
  (lambda parameters 
   (let ((lgt-parameters (length parameters)))
    (if (> (+ n m) lgt-parameters)
        (laml-error "Two few parameters passed to" f-name ":" parameters))
    (let* ((prefix-parameters (front-sublist parameters n))
           (rest-parameters (list-tail parameters n))
           (rest-length (- lgt-parameters n))
           (xml-in-laml-parameters (front-sublist rest-parameters (-  rest-length m)))
           (suffix-parameters (rear-sublist parameters m))
           (ordered-parameters (xml-sort-superficially-tag-parameters xml-in-laml-parameters f-name language))
           (content-parameters (car ordered-parameters))
           (attribute-prop-list (cdr ordered-parameters))
          )
      (parameter-validator! content-parameters attribute-prop-list)
      (apply f (append prefix-parameters (list content-parameters attribute-prop-list) suffix-parameters)))))))

;; Generate a function with XML-in-LAML parameter passing rules which sends its input to an ordinary function, f, with positional parameters via a parameter mediator.
;; The first parameter, f, is typically a web-related function with positional parameter correspondence (such as an existing, old-style 'conveninece function').
;; The parameter parameter-mediator is a function that generates a parameter list for f from
;; content-list and the attributes property list, which is produced by the function xml-sort-tag-parameters.
;; Thus, the parameter-mediator function translates the 'new parameter profile' to the 'old one', which is associated with f.
;; The optional procedure parameter-validator! validates the contents-list and the attribute-list by reporting problems via xml-check-error.
;; The optional f-name parameter is a string corresponding to the name of f (used for error message purposes only).
;; .form (xml-in-laml-parametrization f parameter-mediator [parameter-validator! f-name language])
;; .parameter f An 'old style' fuction web function with positional parameters. Is applied on the result of parameter-mediator.
;; .parameter parameter-mediator a function of two parameters (contents-list and attribute property list) which transforms the new style parameters to the old style. f is applied on the output of parameter-mediator!
;; .parameter parameter-validator! a procedure of two parameters (contents-list and attribute property list) which validates the input of the generated function.
;; .parameter f-name The name of the generated function. Solely used for error message purposes.
;; .parameter language The name of the XML language to which the generated xml-in-laml parametrization belongs. 
;; .internal-references "applied function" "xml-sort-tag-parameters"
;; .internal-references "similar function" "xml-in-laml-abstraction"
;; .internal-references "useful par. validator" "required-implied-attributes"
;; .reference "LAML tutorial" "Web authoring with higher-order functions" "../../../tutorial/higher-order-authoring/html/index.html"
;; .misc See the LAML tutorial referenced above for additional discussion and examples.
(define (xml-in-laml-parametrization f  parameter-mediator . optional-parameter-list)
 (let ((parameter-validator! (optional-parameter 1 optional-parameter-list (lambda (co at) #t)))
       (f-name (optional-parameter 2 optional-parameter-list "some xml-in-laml parametrization"))
       (language (optional-parameter 3 optional-parameter-list #f))
      )
  (lambda parameters 
    (let* ((ordered-parameters (xml-sort-tag-parameters parameters f-name language))
           (content-parameters (car ordered-parameters))
           (attribute-prop-list (cdr ordered-parameters))
          )
      (parameter-validator! content-parameters attribute-prop-list)
      (apply f (parameter-mediator content-parameters attribute-prop-list))))))

; Return the first and only language in (languages-in-use), or else #f.
(define (guess-xml-language-if-possible)
  (let ((language-list (languages-in-use)))
    (if (= 1 (length language-list))
        (first language-list)
        #f)))


;; A higher-order function which returns an attribute checker. Report problems via xml-check-error.
;; Check that all required-attribute-names are present and that the actual attributes are covered by
;; required-attribute-names and implied-attribute-names together.
;; The generated functions can be used as parameter valdiator procedures in xml-in-laml-abstraction and xml-in-laml-parametrization
;; .internal-references "relevant context of use" "xml-in-laml-abstraction" "xml-in-laml-parametrization"
;; .internal-references "error function" "xml-check-error"
;; .parameter required-attribute-names A list of attribute names (symbols) which are required.
;; .parameter implied-attribute-names A list of additional attribute names (symbols) which are allowed, or the list (*) - a singleton list with the symbol * - in the meaning of any attribute.
;; .form (required-implied-attributes required-attribute-names implied-attribute-names [tag-name])
(define (required-implied-attributes required-attribute-names implied-attribute-names . optional-parameter-list)
 (let ((tag-name (optional-parameter 1 optional-parameter-list "??")))
  (lambda (contents attributes)
   (let ((attribute-names (every-second-element attributes)))
    (xml-check-required-attributes! attribute-names required-attribute-names tag-name)
    (if (not (equal? implied-attribute-names (list '*)))
        (xml-check-for-attribute-existence! attribute-names (append required-attribute-names implied-attribute-names) tag-name))
    ))))


;;; AST traversal and AST transformation functions.
;;; The AST traversal functions extract information from AST by means of traversal and searching.
;;; The "find" functions use guided search, aided by the XML navigation information which is derived from the XML DTD.
;;; The "traverse-and-collect" functions are slightly more general (with a node-interesting? predicate), but they
;;; do not (and cannot) make use of the XML navigation information. Thus, these function do an exhaustive search.
;;; The "find" and "transverse-and-collect" functions in this section constitute a 'minimalistic transformation framework' in LAML, which is useful for
;;; target-controlled transformation of an AST.
;;; The function transform-ast is useful for comprehensive, recursive, source-controlled transformation of an AST (similar to a typical use of XSLT).
;;; The following section contains more sophisticated locator functions.
;;; .section-id ast-transf

;; Find and return a list of transformed sub ASTs of ast (first parameter) each with a root element of name el-name.
;; The search is guided by the XML navigation information, hereby pruning the tree traversal. 
;; The AST is traversed in pre-order.
;; The transformation of the resulting sub-ASTS is done by the optional ast-transformer.
;; If a sub-ast is returned as a part of the result then the sub-ast is not searched internally for recursive matches.
;; .form (find-asts ast el-name [ast-transformer])
;; .parameter ast An AST.
;; .parameter el-name A name of an element in the language of ast (a string or symbol).
;; .parameter ast-transformer An optional AST transformation function, which defaults to the identity function id-1.
;; .returns A list of transformed ASTs.
;; .internal-references "similar function" "traverse-and-collect-all-from-ast" "traverse-and-collect-first-from-ast" "find-first-ast"
;; .internal-references "related functions" "traverse-and-collect-first-from-ast" "find-first-ast"
(define (find-asts ast el-name . optional-parameter-list)
 (let ((ast-transformer (optional-parameter 1 optional-parameter-list id-1)))
  (cond ((equal? (ast-element-name ast) (as-string el-name)) (list (ast-transformer ast)))
        (else 
          (let* ((sub-asts (filter ast? (ast-subtrees ast)))
                 (possible-sub-asts 
                  (filter 
                   (lambda (sub-ast)
                     (can-have-element-constituent? sub-ast el-name)) ; static determination, from XML navigator derived from DTD
                   sub-asts))
                 )
            (flatten (map (lambda (sub-ast) (find-asts sub-ast el-name ast-transformer)) possible-sub-asts)))))))

;; Find and return a sub-AST of ast (first parameter) with a root element of name el-name.
;; The search is guided by the XML navigation information, hereby pruning the tree traversal.
;; The AST is traversed in pre-order. When an appropriate sub-AST is found, the search is terminated.
;; The transformation of the resulting sub-ASTS is done by the optional ast-transformer.
;; .form (find-first-ast ast el-name [ast-transformer])
;; .parameter ast An AST.
;; .parameter el-name A name of an element in the language of ast (a string or symbol).
;; .parameter ast-transformer An optional AST transformation function, which defaults to the identity function id-1.
;; .returns A transformed AST, or #f
;; .internal-references "similar function" "traverse-and-collect-first-from-ast"
;; .internal-references "related functions" "traverse-and-collect-all-from-ast" "find-asts"
(define (find-first-ast ast el-name . optional-parameter-list)
 (let ((ast-transformer (optional-parameter 1 optional-parameter-list id-1)))
  (call-with-current-continuation 
   (lambda (return)
    (find-first-ast-help ast el-name ast-transformer return)))))

(define (find-first-ast-help ast el-name ast-transformer return)
  (cond ((equal? (ast-element-name ast) (as-string el-name)) (return (ast-transformer ast)))
        (else 
          (let* ((sub-asts (filter ast? (ast-subtrees ast))))
            (for-each 
              (lambda (sub-ast)
                (if (can-have-element-constituent? sub-ast el-name)
                    (find-first-ast-help sub-ast el-name ast-transformer return)))
              sub-asts))))

  ; We did not find any AST:
  #f)

;; Return the value of the attribute name in ast, or in one of the subtrees of ast.
;; If, from a static consideration, the attribute is not unique in ast, a fatal error occurs. 
;; The default attribute value is only applied if the attribute could occur, but if it does NOT in the actual AST.
;; This function is useful for extraction of deep attributes given the
;; fact that they only can occur once in the document, according to both the actual document structure and the statically extracted XML navigation information.
;; The navigation towards a deep, unique attribute is efficient.
;; .form (unique-ast-attribute ast name [default-attribute-value])
;; .pre-condition Statically, only one occurence of attr-name can appear in ast.
;; .parameter ast The AST in which to look for the attribute
;; .parameter name The name of the attribute (symbol or string)
;; .parameter default-attribute-value The default value, used if no attribute of name is found, but only if it is allowed to occur. A string.
;; .internal-references "related function" "ast-attribute"
(define (unique-ast-attribute ast name . optional-parameter-list)
 (call-with-current-continuation 
  (lambda (return)
   (let ((default-attribute-value (optional-parameter 1 optional-parameter-list #f)))
    (unique-ast-attribute-help ast (as-symbol name) default-attribute-value return)
    (laml-error "unique-ast-attribute: The attribute named" name "is not found in AST, or it is not unique in AST."))))) 


(define (unique-ast-attribute-help ast name default-value return)
 (let* ((alist (propertylist-to-alist (ast-attributes ast)))
        (res (assq name alist))
        (can-be-in-ast (can-attribute-be-in-ast ast name))
        (candidate-sub-asts (sub-asts-with-possible-attribute ast name))
        (candidate-count (length candidate-sub-asts))
       )
   (cond ((and res can-be-in-ast (= 0 candidate-count)) (return (cdr res)))
         ((and (not res) can-be-in-ast (= 0 candidate-count)) (return default-value))
         ((= 1 candidate-count)
           (unique-ast-attribute-help (first candidate-sub-asts) name default-value return))
         (else 'do-nothing) ; wait for fall through to fatal error
   )
  )
)

; Return the list of sub ASTs of ast in which it is possible for an attribute of name to occur
; (from a statical consideration).
(define (sub-asts-with-possible-attribute ast name)
  (let ((sub-asts (filter ast? (ast-subtrees ast))))
    (filter (lambda (sub-ast) (can-attribute-be-in-ast sub-ast name)) sub-asts)))

; Can the attribute name be in one of the subtrees of ast according the statically derived
; XML navigation information from the DTD.
(define (can-attribute-be-in-one-of-subtrees? ast name)
 (let ((sub-asts (filter ast? (ast-subtrees ast))))
   (accumulate-right or-fn #f (map (lambda (ast) (can-attribute-be-in-ast ast name)) sub-asts))))

; Can the attribute name be in ast according the statically derived
; XML navigation information from the DTD.
(define (can-attribute-be-in-ast ast name)
 (turn-into-boolean
   (memq name (possible-attributes-rooted-by-element (ast-element-name ast) (ast-language ast)))))
   



;; Traverse all nodes of the AST ast-tree, and return transformed subtrees that satisfy the node-interesting? predicate.
;; No traversal takes place inside subtrees that sastify the node-interesting? predicate.
;; The transformation of the interesting subtree is done with ast-transformer (the third parameter).
;; Non-AST constituents of the AST (CDATA and white space markers) are not visited during the traversal,
;; and delayed procedural content items are not taken into account.
;; The traversal is done in pre-order.
;; .internal-references "useful as parameter" "ast-of-type?"
;; .internal-references "sibling function" "traverse-and-collect-first-from-ast"
;; .internal-references "similar function" "find-asts"
;; .parameter ast-tree The AST to be traversed.  May also be a content list (such as a list of ASTs) as returned by the function ast-subtrees.
;; .parameter node-interesting? The AST predicate that identifies the ASTs of interest.
;; .parameter ast-transformer The function which is applied on the ASTs identified by node-interesting?
;; .returns The mapping of the function ast-transformer on the list of interesting subtrees.
;; .misc The higher-order function ast-of-type? generates useful node-interesting? functions.
(define (traverse-and-collect-all-from-ast ast-tree node-interesting? ast-transformer)
  (cond ((and (terminal-ast-node? ast-tree) (node-interesting? ast-tree)) (list (ast-transformer ast-tree)))
        ((and (terminal-ast-node? ast-tree) (not (node-interesting? ast-tree))) '())
        ((ast? ast-tree)
           (let ((subtree-results
                   (map 
                    (lambda (subtr) (traverse-and-collect-all-from-ast subtr node-interesting? ast-transformer))
                    (ast-subtrees ast-tree))))
             (if (node-interesting? ast-tree)
                 (cons 
                   (ast-transformer ast-tree)
                   (flatten subtree-results))
                 (flatten subtree-results))))
        ((list? ast-tree)
            (flatten 
              (map 
               (lambda (tr) (traverse-and-collect-all-from-ast tr node-interesting? ast-transformer))
               (filter ast? ast-tree))))
        (else '())))


;; Traverse the AST ast-tree, and return a transformation of the first subtree which satisfies the predicate node-interesting.
;; The transformation of the interesting subtree is done with ast-transformer (the third parameter).
;; Non-AST constituents of the AST (CDATA and white space markers) are not visited during the traversal.
;; The traversal is done in pre-order.
;; Return #f in case no match is found.
;; In fact, return the transformed AST, applying ast-transformer on the returned tree.
;; .parameter ast-tree The AST to be traversed. May also be a content list (such as a list of ASTs) as returned by the function ast-subtrees.
;; .parameter node-interesting? The AST predicate that identifies the ASTs of interest.
;; .parameter ast-transformer The function which is applied on the ASTs identified by node-interesting?
;; .returns (ast-transformer TR), where TR is the first encounted tree matched by node-interesting? or #f if no tree is matched.
;; .internal-references "useful as parameter" "ast-of-type?"
;; .internal-references "sibling function" "traverse-and-collect-all-from-ast"
;; .internal-references "similar function" "find-first-ast"
;; .misc The higher-order function ast-of-type? generates useful node-interesting? functions.
(define (traverse-and-collect-first-from-ast ast-tree node-interesting? ast-transformer)
 (call-with-current-continuation
  (lambda (exit)
    (traverse-and-collect-first-from-ast-help ast-tree node-interesting? ast-transformer exit)))
 )


(define (traverse-and-collect-first-from-ast-help ast-tree node-interesting? ast-transformer exit)
  (cond ((and (terminal-ast-node? ast-tree) (node-interesting? ast-tree)) (exit (ast-transformer ast-tree)))
        ((ast? ast-tree)
           (if (node-interesting? ast-tree)
               (exit (ast-transformer ast-tree))
               (for-each
                 (lambda (subtr) (traverse-and-collect-first-from-ast-help subtr node-interesting? ast-transformer exit))
                 (ast-subtrees ast-tree))))
        ((list? ast-tree)
           (for-each
             (lambda (tr) (traverse-and-collect-first-from-ast-help tr node-interesting? ast-transformer exit))
              (filter ast? ast-tree)))
        (else #f))
  
  ; We did not find a tree:
  #f)


;; Transform source item(s) by means of transform-specs. 
;; The first applicable transformation in transform-specs is applied.  
;; Applies the function positive-to-negative-ast-spacing on any AST before the transformation process.
;; Recursive transformations, initiated in the transformation functions of transform-specs, should be done by means of the function apply-transformation-on.
;; .pre-condition ASTs in source are positive spaced. At least one transformation in the transform-spec MUST be applicable.
;; .parameter source-items A content item, a list of content items. This can be an AST, or a list of ASTs. It can also be CDATA (a text string or a list of text strings).
;; .parameter transform-spec A list of transformation specifications. Each transformation specification is itself a list of length 2: (input-predicate transformation-function).
;; .returns The transformed source (such as an AST or a list of ASTs).
;; .internal-references "recursive transformation" "apply-transformation-on"
;; .misc Notice that it is up to the transformation specifications to decide if recursive transformation is done.\
;;       Organize transformation-spec with the most specialized transformations first, and the most general transformations last.\
;;       Be sure that at least one transformation is applicable.\
;;       It is possible to to ignore 'non-matched source items' by having (list (lambda (x) #t) (lambda (source) '())) as the last element in the transformation specification.
;; .internal-references "applied function" "positive-to-negative-ast-spacing" 
(define (transform-ast transform-specs source-items)
 (let ((negated-source
          (cond ((ast? source-items) (positive-to-negative-ast-spacing source-items))
                ((list? source-items) 
                    (map (lambda (el) (if (ast? el) (positive-to-negative-ast-spacing el) el)) source-items))
                (else source-items))))
   (letrec ((real-transformer
	     (lambda (source)
	       (cond ((and (not (ast? source)) (list? source))  ; a list of source content items
                      (map apply-transformation-on source))
		     (else                                      ; A single source content item
                           (let ((transform-function (lookup-transform-spec source transform-specs)))
			     (if transform-function
				 (transform-function source)
				 '())))))))
     (set! apply-transformation-on real-transformer)
     (apply-transformation-on negated-source))))

;; Do recursive transformation on a content source item, or a list of content source items (such as an AST, or a list of ASTs).
;; At top level, always apply the function transform-ast. 
;; Within the transformation functions (of a transform-specs of transform-ast) perform recursive transformations by apply-transformation-on.
;; This function uses the same transformation specification as the original call of transform-ast.
;; .form (apply-transformation-on source-items)
;; .parameter source-items A content item, a list of content items. This includes an AST, and a list of ASTs. 
;; .internal-references "top-level transformation" "transform-ast"
(define apply-transformation-on #f)


;; Apply the transformation-specs on input-list and return a list of transformed input elements.
;; This function can, for instance, be applied on the subtree list of an AST.
;; A single transformation spec is a list of two element: A predicate and a transformation function.
;; A transformation function is applied if the corresponding predicate holds on the input element.
;; If no predicate holds on an input element, the input element is returned (not copied) without being transformed.
;; No recursive transformations are done by this function.
;; .parameter input-list Each element in input-list can be an AST, a string, a character reference, or a white space related marker
;; .parameter transform-spec A list of transformation specifications, each of which is a list of length 2: (input-predicate transformation-function)
;; .internal-references "similar, and better function" "transform-ast"
;; .internal-references "related functions" "traverse-and-collect-all-from-ast" "traverse-and-collect-first-from-ast"
;; .internal-references "usefully applied on results of" "ast-subtrees" "traverse-and-collect-all-from-ast"
;; .internal-references "useful predicate generator" "ast-of-type?"
;; .returns A list of the same length of input-list. Some elements in the returned list are transformed as requested by transform-spec.
;; .misc This is an older, ad-hoc transformation function. I recommend that you use the function transform-ast instead.
(define (transform-ast-list input-list . transform-specs)
 (if (null? input-list)
     '()
     (let* ((ast (car input-list))
            (transform-function (lookup-transform-spec ast transform-specs)))
      (cons 
        (if transform-function (transform-function ast) ast)
        (apply transform-ast-list (cdr input-list) transform-specs)))))


            

; Return the appropriate transformation function on ast in the flat list of transform-specs.
; Not used, because we have settled on a non-flat format of transformation specifications.
(define (lookup-transform-spec-flat source transform-specs)
  (cond ((null? transform-specs) #f)
        ((not (null? (cdr transform-specs))) ; at least two elements
           (let* ((pred (car transform-specs))
                  (transformer (cadr transform-specs)))
             (if (pred source) transformer (lookup-transform-spec-flat source (cddr transform-specs)))))
        (else (laml-error "lookup-transform-spec-flat: Odd length transform-spec passed."))))


; Return the appropriate transformation function on ast in the list of transform-specs.
(define (lookup-transform-spec ast transform-specs)
  (if (null? transform-specs)
      #f
      (let* ((single-transform-spec (car transform-specs))
             (pred (car single-transform-spec))
             (transformer (cadr single-transform-spec)))
        (if (pred ast) transformer (lookup-transform-spec ast (cdr transform-specs))))))


;; Given an AST with positive white spacing (such as an AST returned by mirror function).
;; Thus, the input AST may hold a number of explicit-space values (#t values), but no explicit-space-suppress values (no #f values).
;; Return a copy of the AST with negative white spacing, akin to the source forms of the mirror functions.
;; Thus, the output AST may hold a number of explicit-space-suppress values (#f values), but no explicit-space values (no #t values)
(define (positive-to-negative-ast-spacing ast)
  (let* ((subtree-list (ast-subtrees ast))
         (negative-subtree-list (positive-to-negative-subtree-list subtree-list)))
     (make-ast (ast-element-name ast)
               negative-subtree-list
               (ast-attributes ast)
               (ast-kind ast)
               (ast-language ast)
               (ast-internal-attributes ast))))

(define (positive-to-negative-subtree-list subtree-list)
  (cond ((null? subtree-list) '())    ; zero elements
        ((null? (cdr subtree-list))   ; exactly one element
           (let ((only-element (first subtree-list)))
             (list (if (ast? only-element) (positive-to-negative-ast-spacing only-element) only-element))))
        (else (positive-to-negative-subtree-list-1-2 (first subtree-list) (second subtree-list) (cdr subtree-list)))))

; next-e is first element of rest
; e is not supposed to be white space related.
(define (positive-to-negative-subtree-list-1-2 e next-e rest)
  (cond ((forced-white-space? next-e)
            (cons (if (ast? e) (positive-to-negative-ast-spacing e) e)
                  (positive-to-negative-subtree-list (cdr rest))))

        ((and (not (white-space-related? e)) (not (white-space-related? next-e)))
            (cons (if (ast? e) (positive-to-negative-ast-spacing e) e)
              (cons  explicit-space-suppress
                (positive-to-negative-subtree-list rest))))

        (else  ; should not happen
            (cons (if (ast? e) (positive-to-negative-ast-spacing e) e)
              (positive-to-negative-subtree-list rest)))))




;;; LAML location path and steps.  
;;; This section contains <a href = "http://www.w3.org/TR/xpath.html">Xpath</a> inspirred matching, extraction, and transformation functions.
;;; The definitions in this section make up an alternative to the minimalistic AST traversal and transformation functions in the previous section.
;;; Eventually, there will be additional documentation and examples of the LAML location path framework.
;;; .section-id location-path-step

;; Apply each function in locator-list sequentially on the list of ASTs resulting from previous location step.
;; Flatten the resulting lists, and ensure that a given content element is at most returned once.
;; A locator function is a function from content element (such as an AST) to a single content element, or (more typcially) to a list of content elements.
;; A locator function may, in fact, perform some tranformation on the list of content elements. 
;; This function sets the variable xml-root-node to ast, if the variable is not already assigned explitly.
;; If this function assigns xml-root-node, it also resets xml-root-node to value #f afterwards.
;; .parameter ast A single AST that represents a single XML document.
;; .parameter locator-list is a list of AST locator functions. Each such function works on a single content element (an AST for instance) and it returns a list of content elements. (For convenience, it is also possible to return a single contents element, which automatically is nested into a list). The function location-step is the primary generator of locator-path functions.
;; .returns A list of content elements (ASTs, for instance).
;; .internal-references "locator path generation function" "location-step"
(define (match-ast ast . locator-list)
 (let ((xml-root-node-assigned-on-before-hand #f))
   (if (eq? xml-root-node #f) (set! xml-root-node ast) (set! xml-root-node-assigned-on-before-hand #t))
   (let ((result (cond (#t (match-asts-1 (list ast) locator-list))
                       (else (laml-error "match-ast: Final or intermediate result must either be an AST:" ast)))))
     (if (not xml-root-node-assigned-on-before-hand) (set! xml-root-node #f))
     (remove-duplicates-by-predicate result eq?))))


;; The XML root node, used to determine some of the reverse axes.
;; It is possible to assign xml-root-node explicitly via set!. If you set xml-root-node explicitly, remember to reset its value to #f when you are done.
(define xml-root-node #f)

; Apply each function in locator-list on each content element in asts, and flatten the resulting lists of lists before applying the next function in the list.
; content-elements is always a list of content elements, among these ASTs.
; This function has no rest parameter, but a second parameter which is a function list.
(define (match-asts-1 content-elements locator-list)
  (cond ((null? locator-list)
           content-elements)
        (else
           (let* ((fu (first locator-list))
                  (res-list (map (compose ensure-list-result-xml-in-laml fu) content-elements)))
             (match-asts-1 (flatten res-list) (cdr locator-list))))))

;; Generates and returns a location step procedure for a given axis, name, and filtering.
;; Use of the axes following-sibling, preceding-sibling, following, preceding, ancestor, and parent requires that the xml-root-node is assigned in advance.
;; These axes correspond to the axes known from <a href = "http://www.w3.org/TR/xpath.html">Xpath</a>. 
;; .form (location-step axis [node-test filtering])
;; .parameter axis One of the axes self, child, descendant, attribute, following-sibling, preceding-sibling, following, preceding, ancestor, and parent (a symbol).
;; .parameter node-test A node test which is relevant in the context of the axis. Typcically a name of an XML element, or an attribute name.
;; .parameter filtering Some filtering applied realtive to axis and name. Typically a boolean function from (content-element node-position context-size). For the attribute axis, filtering may be a text string (the attribute value).  
(define (location-step axis . optional-parameter-list)
  (let ((node-test (optional-parameter 1 optional-parameter-list #f))
        (filtering (optional-parameter 2 optional-parameter-list #f)))

    (letrec ((sequence-location-step ; common support for sequential location steps. The parameter content-element is not used!
              (lambda (content-element offsprings)
                (cond ((and (string? node-test) (equal? node-test "*"))
                       (let* ((last (length offsprings)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) offsprings (number-interval 1 last))))
                      ((string? node-test)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (ast? child) (equal? (ast-element-name child) node-test))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'node 1) ; only (node)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (ast? child))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'node 2) ; (node "N")
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (ast? child) (equal? (ast-element-name child) (second node-test)))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'node 3) ; (node "L" "N")
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (ast? child)
                                                                                              (equal? (ast-element-name child) (second node-test))
                                                                                              (equal? (as-string (ast-language child)) (as-string (third node-test)))))
                                                                         offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'text 1) ; only (text)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (cdata? child))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'comment 1) ; only (comment)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (xml-comment? child))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'white-space 1) ; only (white-space)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (white-space-related? child) (forced-white-space? child))) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'processing-instruction 1) ; only (processing-instruction)
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (processing-instruction? child)) offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      ((node-test? node-test 'processing-instruction 2) ; (processing-instruction "N")
                       (let* ((relevant-children-after-node-test (filter (lambda (child) (and (processing-instruction? child)
                                                                                              (equal? (processing-instruction-target child) (second node-test))))
                                                                         offsprings))
                              (last (length relevant-children-after-node-test)))
                         (mapping-filter (lambda (child pos) (if (filtering child pos last) child #f)) relevant-children-after-node-test (number-interval 1 last))))
                      (else (laml-error "Unknown location step node test:" node-test))))))

      (cond 

            ; The attribute axis is special:

            ((and (eq? axis 'attribute) (not node-test) (not filtering))
             (lambda (content-element)
               (if (ast? content-element)
                   (let* ((attr-prop-list (ast-attributes content-element)))
                     (list attr-prop-list))
                   '())))

            ((and (eq? axis 'attribute) (or (string? node-test) (symbol? node-test)) (not filtering))
             (lambda (content-element)
               (if (ast? content-element)
                   (let* ((attr-name (as-symbol node-test))
                          (attr-prop-list (ast-attributes content-element)))
                     (if (find-in-property-list attr-name attr-prop-list)
                         (list (list (as-symbol node-test) (get-prop attr-name attr-prop-list)))
                         '()))
                   '())))

            ((and (eq? axis 'attribute) (or (string? node-test) (symbol? node-test)) filtering) ; filtering re-interpreted as attibute value! 
             (lambda (content-element)
               (if (ast? content-element)
                   (let* ((attr-name (as-symbol node-test))
                          (attr-value (as-string filtering)) ; renaming and special interpretation
                          (attr-prop-list (ast-attributes content-element)))
                     (if (and (find-in-property-list attr-name attr-prop-list) (equal? attr-value (get-prop attr-name attr-prop-list)))
                         (list (list (as-symbol node-test) (get-prop attr-name attr-prop-list)))
                         '()))
                   '())))


            ; Recursive dispatching in case one of node-test and filtering is not provided:

            ((and (not node-test) (not filtering)) ; all ancestors towards the root
             (apply location-step (list axis "*" (lambda (n p l) #t))))

            ((and node-test (not filtering))
             (apply location-step (list axis node-test (lambda (n p l) #t))))



            ; Remaining location steps with both axis, node-test and filtering:

            ((and (eq? axis 'self) node-test filtering)
             (lambda (content-element)
                (sequence-location-step content-element (list content-element))))


            ((and (eq? axis 'child) node-test (procedure? filtering)) ;  filtering: child x node-position x context-size -> boolean 
             (lambda (content-element)
               (if (ast? content-element)
                   (let ((children (ast-subtrees content-element)))
                     (sequence-location-step content-element children))
                   '())))

            ((and (eq? axis 'descendant) node-test (procedure? filtering)) ;  filtering: child x node-position x context-size -> boolean
             (lambda (content-element)
               (if (ast? content-element)
                   (let* ((descendants (all-children-recursively content-element)))
                     (sequence-location-step content-element descendants))
                   '())))

            ((and (eq? axis 'following-sibling) node-test filtering)
             (lambda (content-element)
               (let ((path (ast-path-from-to xml-root-node content-element)))
                 (if (and path (>= (length path) 2))
                     (let* ((parent (cadr (reverse path)))
                            (children-of-parent (ast-subtrees parent))
                            (pos (index-in-list-by-predicate children-of-parent content-element eq?)))
                       (sequence-location-step content-element (rear-sublist children-of-parent (- (length children-of-parent) (+ pos 1)))))
                     '()))))

            ((and (eq? axis 'preceding-sibling) node-test filtering)
             (lambda (content-element)
               (let ((path (ast-path-from-to xml-root-node content-element)))
                 (if (and path (>= (length path) 2))
                     (let* ((parent (cadr (reverse path)))
                            (children-of-parent (ast-subtrees parent))
                            (pos (index-in-list-by-predicate children-of-parent content-element eq?)))
                       (sequence-location-step content-element (front-sublist children-of-parent pos)))
                     '()))))

            ((and (eq? axis 'following) node-test filtering)
             (lambda (content-element)
               (sequence-location-step content-element (traverse-ast-start-after xml-root-node content-element))))

            ((and (eq? axis 'preceding) node-test filtering)
             (lambda (content-element)
               (sequence-location-step content-element (traverse-ast-interrupt-before-no-ancestors xml-root-node content-element))))

            ((and (eq? axis 'ancestor) node-test filtering)
             (lambda (content-element)
               (let ((path (ast-path-from-to xml-root-node content-element)))
                 (if path
                     (sequence-location-step content-element (cdr (reverse path)))
                     '()) )))

           ((and (eq? axis 'parent) node-test filtering)
             (lambda (content-element)
               (let ((path (ast-path-from-to xml-root-node content-element)))
                 (if (and path (>= (length path) 2))
                     (sequence-location-step content-element (list (cadr (reverse path))))
                     '()) )  ))

            (else (laml-error "location-step: axis not supported yet:" axis))
            )
      )
    ))

; Is the node test list nt of kind with a total of n constituents?
(define (node-test? nt kind n)
  (and 
     (list? nt)
     (cond ((= n 1) (and (= (length nt) 1) (eq? (first nt) kind)))
           ((= n 2) (and (= (length nt) 2) (eq? (first nt) kind)))  
           ((= n 3) (and (= (length nt) 3) (eq? (first nt) kind)))
           (else #f))))

(define (traverse-ast-start-after ast start-after-content-element)
 (let ((collecting? #f)
       (collected-content-elements '()))
  (letrec ((traverse-ast-start-after-1
            (lambda (cont-element)
              (if collecting? (set! collected-content-elements (cons cont-element collected-content-elements)))
              (if (ast? cont-element)
                  (for-each (lambda (child) (traverse-ast-start-after-1 child)) (ast-subtrees cont-element)))
              (if (eq? cont-element start-after-content-element) (set! collecting? #t)) )))
    (traverse-ast-start-after-1 ast)
    (reverse collected-content-elements))))

(define (traverse-ast-interrupt-before ast start-after-content-element)
 (let ((collecting? #t)
       (collected-content-elements '())
      )
  (letrec ((traverse-ast-interrupt-before-1
            (lambda (cont-element)
              (if (eq? cont-element start-after-content-element) (set! collecting? #f))
              (if collecting? (set! collected-content-elements (cons cont-element collected-content-elements)))
              (if (ast? cont-element)
                  (for-each (lambda (child) (traverse-ast-interrupt-before-1 child)) (ast-subtrees cont-element))) )))
    (traverse-ast-interrupt-before-1 ast)
    (reverse collected-content-elements))))

(define (traverse-ast-interrupt-before-no-ancestors ast start-after-content-element)
 (let ((root-to-start-path (ast-path-from-to ast start-after-content-element)))
   (list-difference (traverse-ast-interrupt-before ast start-after-content-element) root-to-start-path)))
  

(define (all-children-recursively ast)
  (if (ast? ast)
      (let ((children (ast-subtrees ast)))
        (append children (flatten (map all-children-recursively (filter ast? children)))))
      '()))

; Find the unique list of AST nodes from ast1 to ast2 via the parent-child relationship in the tree.
; The resulting list starts with ast1, and ends with ast2. In case ast2 is not a descendant of node1, return #f.
(define (ast-path-from-to ast1 ast2)
  (call-with-current-continuation 
    (lambda (returning)
      (ast-path-from-to-1 ast1 ast2 '() returning)
      #f))
)

(define (ast-path-from-to-1 ast1 ast2 path-list returning)
  (if (eq? ast1 ast2)
      (returning (reverse (cons ast2 path-list)))
      (if (ast? ast1)
          (let ((children (ast-subtrees ast1)))
            (for-each 
             (lambda (child)
               (ast-path-from-to-1 child ast2 (cons ast1 path-list) returning))
             children))))               ; (filter ast? children)
)

(define (ensure-list-result-xml-in-laml x)
  (cond ((ast? x) (list x))
        ((cdata? x) (list x))
        ((extended-contents-data? x) (list x))
        ((forced-white-space? x) (list x))
        ((delayed-procedural-contents-element? x) (list x))
        ((char-ref? x) (list x))
        ((xml-comment? x) (list x))
        ((cdata-section? x) (list x))
        ((processing-instruction? x) (list x))
        ((list? x) x)
        (else (laml-error "ensure-list-result-xml-in-laml: the parameter must be a single content item or a list of these: " (as-string x)))))

;; A higher-order function that negates a single location-step procedure. 
;; Negated steps are primarily useful in filtering contexts, because it is not possible to turn an empty result back to something useful.
(define (negate-step location-step-procedure)
  (lambda (content-element)
    (let ((result (location-step-procedure content-element)))
       (if (null? result)
           (list #t)  ; something
           '()))))

;; A higher-order function that conjuncts (ands) some location steps to a single location step.
;; Does each location step in location-step-procedure-list succeed? 
;; Returns a boolean-like result: If yes, return (list #t), else return ().
;; Only useful within the  filtering position in a location step.
(define (and-steps . location-step-procedure-list)
 (letrec ((and-fn (lambda (x y) (and x y))))
  (lambda (content-element)
    (let* ((result-list (map (lambda  (step) (step content-element)) location-step-procedure-list))
           (result-list-1 (map does-exist? result-list)))
      (if (accumulate-right and-fn #t result-list-1)
          (list #t)  ; success
          '()        ; failure
      )))))

;; A higher-order function that disjuncts (ors) some location steps to a single location step.
;; Does at least one location step in location-step-procedure-list succeed?
;; Returns a boolean-like result: If yes, return (list #t), else return ().
;; Only useful within the  filtering position in a location step.
(define (or-steps . location-step-procedure-list)
 (letrec ((or-fn (lambda (x y) (or x y))))
  (lambda (content-element)
    (let* ((result-list (map (lambda (step) (step content-element)) location-step-procedure-list))
           (result-list-1 (map does-exist? result-list)))
      (if (accumulate-right or-fn #f result-list-1)
          (list #t)  ; success
          '()        ; failure
      )))))

;; Compose the locatation steps in location-step-procedure-list to a single location step.
;; Useful in contexts where only a single location step is allowed.
(define (compose-steps . location-step-procedure-list)
  (lambda (content-element)
    (apply match-ast content-element location-step-procedure-list)))


;; Return the identifier function of the predicate p.
;; An identifier function returns x if p holds on x, else the it returns the empty list.
;; Via this function, a predicate can act as a location step.
(define (identifier-of-predicate p)
  (lambda (x) (if (p x) x '())))

;; Generates a location step that test if str is part of the immediate textual content. 
;; The generated procedure can be used as the last step in location path (a list of location steps).
(define (ast-text-containing str)
  (lambda (x)
    (cond ((cdata? x) (if (substring? x str) str '()))
          ((ast? x) (if (substring? (ast-text x) str) (ast-text x) '()))
          (else '()))))

;; Generates a location step that tests if str is part of the deep textual content. 
;; The generated procedure can be used as the last step in location path (a list of location steps).
(define (ast-text-deep-containing str)
  (lambda (x)
    (cond ((cdata? x) (if (substring? x str) str '()))
          ((ast? x) (if (substring? (ast-text-deep x) str) (ast-text-deep x) '()))
          (else '()))))

;; Generates a location step that tests for x being a node. 
(define (ast-node)
  (lambda (x)
     (if (ast? x) x '()))) 


 
; ---------------------------------------------------------------------------------------------------------------
; Generated node child axis filtering procedures. The generated procedures can be applied on by location-step.

; Generation of node test procedures.
; Node test procedure occur at the filtering position in location steps.
; Node test procedures filter the results of a location step.
; Basically, a node test procedure takes three parameters: (1) The node, (2) its position within its sibling context, and (3) the number of the last sibling.
; A node test procedure returns a boolean result.

;; A filtering function that tests if the node is the last in its context.
;; Filtering functions are used in location steps.
(define (nt:last)
  (lambda (n p l) (= p l)))

;; A filtering function that tests if the node is number q in its context.
;; Filtering functions are used in location steps.
(define (nt:child-number q)
  (lambda (n p l) (= p q)))

;; A filtering function that tests if the node has name.
;; Filtering functions are used in location steps.
(define (nt:child-name name)
  (lambda (n p l) (equal? name (ast-element-name n))))

;; A filtering function that tests if the node has an attribute name with the given value.
;; Filtering functions are used in location steps.
(define (nt:attribute name value)
  (lambda (n p l) 
   (if (ast? n)
    (let ((attr-prop-list (ast-attributes n)))
      (if (find-in-property-list name attr-prop-list)
          (equal? (as-string value) (get-prop name attr-prop-list))
          #f))
    #f)))

;; A filtering function which applies a list of location steps on the node, a passes the results of these steps to the predicate pred.
;; Operationally: Apply location steps on node in question (n), giving a list of results. Test if the result fulfills the predicate pred (which must be some list predicate).
;; Filtering functions are used in location steps.
(define (nt:for-which-predidate-holds pred . location-steps)
  (lambda (n p l)
    (let ((res (apply match-ast (cons n location-steps))))
      (pred res))))

;; A filtering function which test if the list of location steps leads success (leads to a non-empty result).
;; Filtering functions are used in location steps.
(define (nt:for-which . location-steps)
  (lambda (n p l)
    (let ((res (apply match-ast (cons n location-steps))))
      (does-exist? res))))

; Determines if x - the result of a location step - is considered as non-emtpy.
(define (does-exist? x)
  (cond ((list? x) (not (null? x)))
        ((string? x) (not (blank-string? x)))
        (else (laml-error "does-exist?: Unknown type of argument of:" x))))

; A filtering that tests if all location steps in the list of location paths succeed.
; Use and-steps on location-steps instead.
(define (nt:exist-all . location-path-list)
 (letrec ((and-fn (lambda (x y) (and x y))))
  (lambda (n p l)
    (let* ((res-list (map (lambda (location-step-list) (apply match-ast (cons n location-step-list))) location-path-list))
           (res-list-1 (map does-exist? res-list)))
      (accumulate-right and-fn #t res-list-1)))))

; A node test procedure that test if at least one of the location steps in the list of location paths succeeds.
; Use or-steps on location-steps instead.
(define (nt:exist-some . location-path-list)
 (letrec ((or-fn (lambda (x y) (or x y))))
  (lambda (n p l)
    (let* ((res-list (map (lambda (location-step-list) (apply match-ast (cons n location-step-list))) location-path-list))
           (res-list-1 (map does-exist? res-list)))
      (accumulate-right or-fn #f res-list-1)))))
          



;;; Element content models.
;;; The functions in this section make the content models of the XML elements available.
;;; As an example, this allows us to find out if an element is empty.
;;; The element content models are defined by the XML DTDs, and as such they are used for generation
;;; of the XML validation procedures.
;;; .section-id content-models

;; Returns the content model map (an association list) of a given XML language.
;; A content model map of an XML language is an sorted associative vector that maps 
;; element names (strings) to the parsed content models of the element, as provided by the
;; the LAML XML-DTD parser.
(define (content-model-map-of language)
 (defaulted-get (as-symbol language) xml-in-laml-content-model-structures #f))

; Selector functions of an entry in a content element map
(define elment-name-of-content-model-structure (make-selector-function 1 "elment-name-of-content-model-structure"))
(define content-model-of-content-model-structure (make-selector-function 2 "content-model-of-content-model-structure"))

;; Return the content model of the element named element-name in XML language.
;; The content model is the parsed content model, as delivered by the LAML XML-DTD parser.
;; If the content model is not available for some reason (unknown element-name, unknown XML language) return #f.
(define (content-model-of element-name language)
  (let* ((content-model-map (content-model-map-of language)))
    (if content-model-map
        (let ((content-model
                (binary-search-in-vector 
                  content-model-map (as-string element-name) elment-name-of-content-model-structure string=? string<=?)))
          (if content-model
              (content-model-of-content-model-structure content-model)
              #f)
        )
        #f)))


;; Register the content model structure for XML language. 
;; This function is called "automatically" when the Scheme mirror of the XML language is loaded. 
(define (register-xml-in-laml-content-models language content-model-structure)
 (set! xml-in-laml-content-model-structures (cons (cons language content-model-structure) xml-in-laml-content-model-structures))
)


; -----------------------------------------------------------------------------------------------------------------------------

;;; Action procedure map.   
;;; An action procedure map is a sorted associative vector that maps certain element names to 
;;; action procedures. 
;;; .section-id action-procedures

;; Return the action procedure map of the XML language.
(define (action-procedure-map-of language)
  (defaulted-get (as-symbol language) xml-in-laml-action-procedure-structures #f))

;; Return the action procedure of the XML element named element-name in the XML language.
;; If there is no action procedure associated with the element, or if the action procedure structure is not available
;; for language, return #f.
(define (action-procedure-of-language element-name language)
 (let* ((action-procedure-map (action-procedure-map-of language)))
    (if action-procedure-map
        (action-procedure-of-map element-name action-procedure-map)
        #f)))

;; Return the action procedure of the XML element named element-name relative to the action procedure map action-procedure-map.
(define (action-procedure-of-map element-name action-procedure-map)
 (let ((action-proc 
         (binary-search-in-vector 
          action-procedure-map (as-string element-name) element-name-of-action-procedure-entry string=? string<=?)))
   (if action-proc
       (action-procedure-of-action-procedure-entry action-proc)
       #f)))

;; Register the action procedure structure for XML language. 
;; An action procedure structure is a sorted, associative vector that maps XML elements (stings) to
;; their action procedures. Notice that this is only a partial mapping.
;; This function is called "automatically" when the Scheme mirror of the XML language is loaded. 
(define (register-xml-in-laml-action-procedures language action-procedure-structure)
 (set! xml-in-laml-action-procedure-structures (cons (cons language action-procedure-structure) xml-in-laml-action-procedure-structures))
)

; Selector functions of an entry in an action procedure map
(define element-name-of-action-procedure-entry (make-selector-function 1 "element-name-of-action-procedure-entry"))
(define action-procedure-of-action-procedure-entry (make-selector-function 2 "action-procedure-of-action-procedure-entry"))

;; Process the ast, the internal document representation, by means of the action
;; procedures in action-map. The action map is a sorted associative vector that maps element
;; names to action procedures. The default value of action-map
;; is (action-procedure-map-of given-language).
;; .internal-references "action procedure access" "action-procedure-map-of"
;; .form (process-ast! ast [given-language action-map])
;; .parameter ast The internal representation of the document to be processed.
;; .parameter given-language The language to which ast belongs. Bound at root level. Defaults to (ast-language ast). A symbol.
;; .parameter action-map The action map that maps elements in the XML language to action procedure. Defaults to (action-procedure-map-of given-language). A sorted associative vector. 
(define (process-ast! ast . optional-parameter-list)
 (let* ((given-language (optional-parameter 1 optional-parameter-list (ast-language ast)))
        (action-map (optional-parameter 2 optional-parameter-list (action-procedure-map-of given-language)))
       )
  (let* ((el-name (ast-element-name ast))
         (element-content-items (ast-subtrees ast))
         (action-proc-of-ast! (action-procedure-of-map el-name action-map))
        )

    (if action-proc-of-ast! 
        (action-proc-of-ast! ast))

    (for-each 
         (lambda (ast) (process-ast! ast given-language action-map)) ; Notice that given-language is bound at root level
         (filter ast? element-content-items)))))

; ----------------------------------------------------------------------------------------------------------------------------




;;; Expansion of delayed procedural content items.
;;; It is possible to embed a Scheme procedure (with a certain given signature) in an XML-in-LAML document.
;;; This allows for simple and direct handling of context-sensitive aspects of a document.
;;; The function expand-procedural-content-items-in-ast calls such procedures, hereby expanding the AST.

;; Expand all delayed procedural content items (recursively) inside the ast.
;; Traverses the entire ast and looks for procedural content item constituents.
;; Return an expanded AST (a copy of ast) in which procedural content elements have been activated 
;; and thereby expanded. If ast does not contain any delayed procedural content items, ast
;; is just returned (without copying it).
(define (expand-procedural-content-items-in-ast ast)
  (if (has-procedural-content-items-deep? ast)
      (expand-procedural-content-items-in-ast-1 ast ast)
      ast))

; Expand all delayed procedural content items (recursively) in ast relative to root-ast
(define (expand-procedural-content-items-in-ast-1 ast root-ast)
 (let ((ast-content-items  (ast-subtrees ast)))
   (if (null? (filter delayed-procedural-contents-element? ast-content-items))  ; NO delayed constituents
       (make-ast    ; just recursively expand sub asts
          (ast-element-name ast)
          (map (lambda (content-item)   ; recursively expand ASTs
                 (cond ((ast? content-item)
                         (expand-procedural-content-items-in-ast-1 content-item root-ast))
                       (else content-item)))
               ast-content-items)
          (ast-attributes ast)
          (ast-kind ast)
          (ast-language ast))
       (let* ((inversed-ast-content-items (inverse-laml-content-list-white-spacing ast-content-items)) ; Bring contents items back to original form (wrt. spacing) - as written in the document.
              (expanded-ast-content-items  ; Do the expansions of delayed procedural constituents
               (map (lambda (content-item)
                      (cond ((delayed-procedural-contents-element? content-item)
                               (content-item root-ast ast))
                            (else content-item)))
                    inversed-ast-content-items))
              (attributes (ast-attributes ast))
              (combined-contents-and-attributes  (append expanded-ast-content-items attributes))  ; mix content and attributes - preparing for 'resorting'
              (sorted-contents-attributes    ; re-sort, splitting the expanded and patially reconstructed content list 
                (xml-sort-tag-parameters combined-contents-and-attributes (ast-element-name ast) (ast-language ast)))
              (new-contents (car sorted-contents-attributes))   ; new content, including constributions from expansion  
              (new-recursively-expanded-content (map (lambda (content-item) ; recursively expand ASTs
                                                       (cond ((ast? content-item)
                                                                 (expand-procedural-content-items-in-ast-1 content-item root-ast)) 
                                                             (else content-item)))
                                                     new-contents)) 
              (new-attributes (cdr sorted-contents-attributes)) ; new attributes, including constributions from expansion  
             )

         ; Validate the expanded AST, which at this location is known to have had a real expansion of delayed procedural content items.
         (let* ((el-name (ast-element-name ast))
                (lang (ast-language ast))
                (validation-proc (validation-procedure-of el-name lang)))
           (validation-proc el-name new-attributes new-recursively-expanded-content xml-check-language-overlap?))

         (make-ast
          (ast-element-name ast)
          new-recursively-expanded-content
          new-attributes
          (ast-kind ast)
          (ast-language ast))))))

;; Is there any procedure content items in ast. Traverses the entire ast if necessary.
(define (has-procedural-content-items-deep? ast)
 (call-with-current-continuation 
  (lambda (return)
    (has-procedural-content-items-deep-1? ast return))))


(define (has-procedural-content-items-deep-1? ast return)
  (let ((sub-content-items (ast-subtrees ast)))
    (if (find-in-list delayed-procedural-contents-element? sub-content-items)
        (return #t)
        (for-each 
          (lambda (content-item)
            (if (ast? content-item)
                (has-procedural-content-items-deep-1? content-item return)))
          sub-content-items)))

  ; We did not find any procedure content item:
  #f)

;; Is there any procedure content items among the immediate constituents of x (usually an AST).
;; No AST traversal is involved to find out.
;; This function also work on a (flat) element content list x (of text, space, comment, CDATA etc).
(define (has-procedural-content-items? x)
 (cond ((ast? x)
          (let ((sub-content-items (ast-subtrees x)))
            (if (find-in-list delayed-procedural-contents-element? sub-content-items) #t #f)))
       ((list? x)
            (if (find-in-list delayed-procedural-contents-element? x) #t #f))
       (else (laml-error "has-procedural-content-items? must be called on an XML-in-LAML AST or a list of content items" x))))
     
     


; Inverse the white spacing in content-items.
; Assume that content-items has a number of explicit-space values among its members (#t).
; No  explicit-space-suppress values (#f) are allowed to occur.
; Return a similar content items, corresponding to the one being input by the user, in which 
; only explicit-space-suppress values occur.
; This function takes a input a content items list as produced by  xml-sort-tag-parameters.
; It returns a content items list similar to the surface form, given by the LAML document author.
(define (inverse-laml-content-list-white-spacing content-items)
  (cond ((null? content-items) '())
        (else (iwssm-start content-items '()))  ; start inverse white space state machine
  ))

; -----------------------------------------------------------------         
; Start state machine: Inverse white space state machine - iwssm

(define debug-inverse-space-state-machine? #f)

(define (iwssm-start lst res)
 (if debug-inverse-space-state-machine? (display-message "start"))
 (if (null? lst)
     (reverse res)
     (let ((el (first lst))  (rest (cdr lst)))
      (cond 
       ((equal? explicit-space el) (iwssm-start rest res))
       (else (iwssm-string-seen el rest res))))))

(define (iwssm-string-seen the-string lst res)
 (if debug-inverse-space-state-machine? (display-message "string-seen"))
 (set! res (cons the-string res))

 (if (null? lst)
     (reverse res)
     (let ((el (first lst))  (rest (cdr lst)))
      (cond ((equal? explicit-space el) (iwssm-string-space-seen rest res))
             (else (iwssm-string-string-seen el rest res))))))

(define (iwssm-string-space-seen lst res)
 (if debug-inverse-space-state-machine? (display-message "string-space-seen"))

 (if (null? lst)
     (reverse res)
     (let ((el (first lst))  (rest (cdr lst)))
      (cond ((equal? explicit-space el) (iwssm-string-space-seen rest res))
             (else (iwssm-string-seen el rest res))))))

(define (iwssm-string-string-seen the-string lst res) 
 (if debug-inverse-space-state-machine? (display-message "string-string-seen"))
 (set! res (cons the-string (cons explicit-space-suppress res)))

 (if (null? lst)
     (reverse res)
     (let ((el (first lst))  (rest (cdr lst)))
      (cond ((equal? explicit-space el) (iwssm-string-space-seen rest res))
            (else (iwssm-string-string-seen el rest res))))))

; End state machine
; -----------------------------------------------------------------         



;;; Other useful functions. 
;;; In this section there are non-mirror functions which are useful in in the context of XML and LAML.
;;; .section-id others-functions


;; Convert an abtract syntax tree to a parse tree.
;; Abstract syntax trees are produced by the validating mirror functions.
;; Parse trees are used as an internal format in the HTML and the XML pretty printing procedures.
;; You can use the function pretty-print-html-parse-tree on the parse tree returned by ast-to-parse-tree.
;; .pre-condition The LAML tool html-support (for parsing and pretty printing) must be loaded for this function to work.
;; .misc To load html-support: <kbd>(laml-tool-load "xml-html-support/html-support.scm")</kbd>
(define (ast-to-parse-tree ast)
 (let ((pt (ast-to-parse-tree-1 ast)))
   (make-final-parse-tree 'html-tree (list pt))))


; The function doing the real work in the ast to parse tree conversion.
(define (ast-to-parse-tree-1 ast)
 (letrec 
  ((subtree-transform     ; the function doing the overall ast to parse tree transformation
    (lambda (x) 
     (cond ((ast? x) (ast-to-parse-tree-1 x))
           ((cdata? x) x)
           ((char-ref? x) (xml-render-char-ref x))
           ((forced-white-space? x) " ")
           (else (laml-error "subtree-transform:" "Unknown subtree constituent" (as-string x)))))
    )

   (explicit-space-splicing  ; join explicit spaces, " ", to the left string constituent - to avoid empty lines after pretty printing.
     (lambda (parse-tree)
       (if (tree-entry? parse-tree)
           (let* ((node (root-of-parse-tree parse-tree))
                  (subtrees (subtrees-of-parse-tree parse-tree))
                  (new-subtrees (explicit-space-splicing-lst subtrees '()))
                 )
             (make-parse-tree node new-subtrees))
           parse-tree)
      ))

   (explicit-space-splicing-lst
     (lambda (subtree-list res-lst)
       (cond ((null? subtree-list)
                  (reverse res-lst))
             ((null? (cdr subtree-list))   ; only a single element left - get finished
                (explicit-space-splicing-lst (cdr subtree-list) (cons (car subtree-list) res-lst)))
             ((and (string? (car subtree-list)) (string? (cadr subtree-list)))   ; interesting case - there are two elements
                (if (equal? (cadr subtree-list) " ")    ; join car and cadr via string-append
                    (explicit-space-splicing-lst (cddr subtree-list) (cons (string-append (car subtree-list) (cadr subtree-list)) res-lst))
                    (explicit-space-splicing-lst (cdr subtree-list) (cons (car subtree-list) res-lst))))
             (else (explicit-space-splicing-lst (cdr subtree-list) (cons (car subtree-list) res-lst))))))

   (split-attribute-list  ; split in html and css attributes
     (lambda (attr-lst) (split-attribute-list-1 attr-lst '() '())))

   (split-attribute-list-1  ; iterative helping function
     (lambda (attr-lst html-attr-list css-attr-list)
       (cond ((null? attr-lst) (cons (reverse html-attr-list) (reverse css-attr-list)))
             ((xml-css-key? (car attr-lst)) 
                (split-attribute-list-1 (cddr attr-lst) html-attr-list (cons (cadr attr-lst) (cons (car attr-lst) css-attr-list))))
             (else 
                (split-attribute-list-1 (cddr attr-lst) (cons (cadr attr-lst) (cons (car attr-lst) html-attr-list)) css-attr-list)))))

   (to-css-string     ; linearize all css attributes to a single string
     (lambda (css-attr-list)
       (let ((css-attr-alist (propertylist-to-alist css-attr-list)))
        (string-append
         (list-to-string 
          (map 
           (lambda (css-key-val) (xml-linearize-attribute-pair-css (cdr css-key-val) (xml-css-key? (car css-key-val))))
           css-attr-alist)
          ";") ";"))))

   )
   (let* ((element-name (ast-element-name ast))
          (subtrees (ast-subtrees ast))
          (attr-lst (ast-attributes ast))  ; prop list format
          (attr-list-normal-css (split-attribute-list attr-lst))
          (html-attr-list (car attr-list-normal-css)) ; prop list format
          (css-attr-list (cdr attr-list-normal-css)) ; prop list format
          (attr-lst-result 
            (if (null? css-attr-list)
                html-attr-list
                (cons 'style (cons (to-css-string css-attr-list) html-attr-list))))
          (kind (ast-kind ast))
   
         )
     (cond ((eq? kind 'single)
              (make-tag-structure
               'start-end
               element-name 
               attr-lst-result))
           ((eq? kind 'double)
              (explicit-space-splicing
               (make-parse-tree 
                (make-tag-structure
                 'start
                 element-name 
                 attr-lst-result)
                (map subtree-transform subtrees))))
           (else (laml-error "ast-to-parse-tree-1:" "Unknown kind of ast" kind))))))


