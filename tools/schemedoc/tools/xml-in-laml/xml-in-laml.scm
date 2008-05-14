; With automaton generation. This version assumes that the content model is parsed with 
; tools/dtd-parser/dtd-parser-4.scm (version of March 2003).

;;;; This is the LAML tool that generates the language specific part
;;;; of an XML mirror in Scheme. From version 20 of LAML, this tool is able to generate a fully validating
;;;; mirror i Scheme of an XML language from a parsed XML DTD. <p>
;;;; For validation purposes, this tool generates deterministic final state automata from the parsed content models,
;;;; as delivered by the LAML XML DTD parser. These automata are embedded in the validation predicates which accompany
;;;; the mirror functions in Scheme of the XML elements.<p>
;;;; The generated mirrors rely heavily on the common part, which is shared for
;;;; all XML-in-LAML languages. The common part is found in lib/xml-in-laml/xml-in-laml.scm, and
;;;; a <a href="../../../lib/xml-in-laml/man/xml-in-laml.html">XML-in-LAML common library manual</a> is available.<p>
;;;; Let us denote the language by the symbol L. 
;;;; As input this tool takes a parsed DTD, L.dtd, in the Lisp representation
;;;; made by the LAML <a href="../../dtd-parser/man/dtd-parser.html">DTD parser</a>.
;;;; As output the LAML mirror generation tool makes a file L-mirror.scm in the mirror-target-dir.<p>
;;;; The tool is typically driven by a LAML script 
;;;; (such as <a href="../sample-mirror-generation/make-xhtml10-transitional-mirror.laml"> the script for generation of XHTML 1.0 transitional</a>).<p>
;;;; An easy way to use the mirror generation tool is via the procedure
;;;; <a href="../../../man/laml.html#generate-xml-mirror">generate-xml-mirror</a> in the fundamental LAML setup library, called laml.scm.<p>
;;;; The main procedure in LAML mirror generation tool is generate-mirror (see below). As such, the tool can be identified with the generate-mirror procedure.<p>
;;;; Please consult <a href = "../../../tutorial/xml-in-laml/xml-in-laml.html">"XML mirrors in Scheme: XML in LAML"</a> (section 3)
;;;; for a tutorial introduction to the use of the tool.<p>
;;;; From LAML version 26, attributes list corresponding to the same element name, are merged to a single attribute list.
;;;; (This was prompted by the processing of XHTML 1.1).
;;;; .title Reference Manual of the XML-in-LAML Mirror Generation Tool

(lib-load "file-read.scm")
(lib-load "final-state-automaton.scm")
(load (string-append laml-dir "tools/dtd-parser/element-attribute-access.scm"))

; ---------------------------------------------------------------------------------------------------
;;; Tool parameters - configuration constants.
;;; Please notice that most of these constants must be passed as parameters to the procedure generate-mirror.

;; The name of the generated mirror - a string without initial path and without extension.
;; Must be defined in the LAML script which parameterizes this tool.
(define mirror-name #f)

;; The full path to the parsed DTD file (a lsp file).
;; Must be defined in the LAML script which parameterizes this tool
(define parsed-dtd-path #f)

;; The full path of the directory in which to put the mirror.
;; Must be defined in the LAML script which parameterizes this tool
(define mirror-target-dir #f)

;; Is lib/xml-in-laml/xml-in-laml.scm loaded by the language specific mirror library?
(define auto-lib-loading #f)


; A variable that controls the naming scheme of the contents model validation predicates.
; A symbol, either old or normal.
; The old value is only used for existing stuff. Use normal.
(define validation-predicate-names 'normal)

;; The list of action elements (a list of symbols). 
;; The named mirror functions of action elements do not return an AST.
;; The mirror function accessed throught the language map do, however, always return ASTs.
;; Rather, they apply a procedure on the ast.
;; If the element name is N, the action procedure name becomes N!
(define action-elements '())

;; A boolean variable that controls how to pass action procedures to generate-xml-mirror-function.
;; If #f, only pass a boolean value. If #t, pass the action procedure itself. #f is recommeded.
;; The reason is that it allows us to load the mirror library and to define the action procedures in arbitrary order.
;; If #t, the action procedures must be defined before the mirror library is loaded.
;; If the value of this variable is #f, we pass #t to generate-xml-mirror-function if there is an action procedure of the element. 
;; It is then up to generate-xml-mirror-function to get the action procedure from the action procedure map
;; (as made by make-xml-action-procedure-structure).
(define pass-action-procedure? #f)

;; A boolean variable that controls the generation of name mirror functions.
;; If true (#t) named mirror functions will be defined.
;; If false (#f), the mirror functions are only available via the language map.
(define define-named-mirror-functions? #t)

;; A string prepended to all names of function names.
;; Defaults to the empty string (no mirror name prefix used).
;; If you wish to have a '-' og ':' in between the prefix and the proper name, you should add it as the last character of
;; the string bound to this variable.
;; You can use the mirror name prefix to ensure that two or more mirrors of XML functions can coexist without
;; name clashes. Notice that the use of language maps is an alternative way to handle name clashes.
(define mirror-name-prefix "")




; ---------------------------------------------------------------------------------------------------
; Scheme name collision.

;; Scheme names - a list of symbols.
;; Think of these as reserved names in Scheme, which we do not want to redefine
;; as mirror functions.
(define reserved-scheme-names (map symbol-of-scheme-knowledge (read-scheme-knowledge 5)))

;   '(define do or and else quasiquote begin if quote case lambda set! cond let unquote let* unquote-splicing 
;    delay letrec not boolean? eqv? eq? equal? pair? cons set-car! set-cdr! car cdr caar cadr cdar 
;    cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
;    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr null? list? list length append rerverse list-tail
;    list-ref memq memv member assq assv assoc symbol? symbol->string? string->symbol number? complex? real? rational? 
;    integer? exact? inexact? = < > <= >= zero? positive? negative? odd? even? max min + * - / abs quotient remainder
;    modulo gcd lcm floor ceiling truncate round number->string string->number char? char=? char<? char>? char<=? char>=?
;    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace? char-upper-case? 
;    char-lower-case? char->integer integer->char char-upcase char-downcase string? make-string string string-length
;    string-ref string-set! string=? string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>? string-ci<=?
;    string-ci>=? substring string-append string->list list->string string-copy string-fill! vector? make-vector vector
;    vector-length vector-ref vector-set! vector->list list->vector vector-fill! procedure? apply map for-each force
;    call-with-current-continuation call-with-input-file call-with-output-file input-port? output-port? current-input-port
;    current-output-port with-output-to-file with-input-from-file open-input-file open-output-file close-input-port 
;    close-output-port read read-char peek-char eof-object? char-ready? write display newline write-char load transcript-on
;    transcript-off rationalize exp log sin cos tan asin acos atan sqrt expt make-rectangular make-polar real-part
;    imag-part magnitude angle exact->inexact inexact->exact)

; Is ename (a string or symbol) a reserved scheme name?
(define (is-scheme-name? ename)
 (let ((ename-symbol (as-symbol ename)))
   (turn-into-boolean (memq ename-symbol reserved-scheme-names))))


; ---------------------------------------------------------------------------------------------------
;;; Default language properties.
;;; The default language properties are string-valued variables, which are intended to be set for
;;; each new mirror. The value of these variable are inserted into the generated mirror. This explains
;;; why all values must be strings. This may seem a little strange, but take a look in the beginning of
;;; one of the generated mirror files to see the effect. <p> Notice that lib/xml-in-laml/xml-in-laml.scm contains a number
;;; of configuration options too, together with a number of functions that access/mutate the properties given below.
;;; You should only set those that provide inappropriate defaults.
;;; It means that you typically only have to set of few of the variables in this section when you make a new mirror.


;; A boolean value which controls the default value of CDATA transliteration via the HTML/XML charater transformation table.
;; The default value is true.
;; The boolean value must be embedded in a string.
;; This value can be changed after the mirror is generated.
(define default-xml-transliterate-character-data? "#t")

;; The list of elements names (strings) for which the transliteration does not apply.
;; The list value must be embedded in a string.
;; The value of this variable is locked at mirror generation time.
(define default-xml-non-transliteration-elements "'()")

;; The list of elements names (strings) for which all white space content characters must be rendered. 
;; Typically the HTML pre element.
;; The list value must be embedded in a string.
;; The value of this variable is locked at mirror generation time.
(define default-xml-preformatted-text-elements "'()")

;; The default value of xml-char-transformation-table.
;; The default value is is "html-char-transformation-table".
;; The value must be a string.
(define default-xml-char-transformation-table "html-char-transformation-table")

;; Controls whether to explicitly pass the default dtd attributes in every instance of it.
;; Normally #f, which is the default value. If #t, it will most likely collide with the attribute validation (only
;; one instance of each attribute name is allowed).
;; The boolean value must be embedded a string. 
;; The value of this variable is locked at mirror generation time.
(define default-pass-default-dtd-attributes? "#f")

;; Controls if only string valued attributes are accepted.
;; The default value is true.
;; The boolean value must be embedded in a string.
(define default-xml-accept-only-string-valued-attributes? "#t")

;; Controls if numbers and characters are accepted as 'extended' element contents items.
;; Such items will be transformed to strings before further processing.
;; The default value is false.
;; The boolean value must be embedded in a string.
(define default-xml-accept-extended-contents? "#f")


;; The default value of document-type-declaration.
;; The value must be a string. The default default is the empty string with the meaning
;; 'no attribute type declaration'. Thus, if you do not give the default-document-type-declaration
;; there will be no document type declartion in your documents.
;; The value of this variable is locked at mirror generation time.
(define default-document-type-declaration "\"\"")


;; Controls the presence of white space markers in the ast.
;; The default value is true.
;; The boolean value must be represented as a string.
(define default-xml-represent-white-space "#t")

;; Controls the handling of duplicated attributes in XML attribute lists.
;; Possible values are keep-all, keep-first, and keep-last.
;; keep-all: the attribute list is not affected at all; All attributes are passed.
;; keep-first: only the first attribute name/value pair is passed for further processing.
;; keep-last: only the last attribute name/value pair is passed for further processing.
;; The default value is the symbol keep-all.
;; The symbol value must be represented as a string.
(define default-xml-duplicated-attribute-handling "'keep-all")


; ---------------------------------------------------------------------------------------------------
; Tool encapsulation functions:

; Return the name of the validation procedure of the element name in language.
(define (name-of-validation-procedure language-name name)
 (string-append (as-string name) "-" (as-string language-name) "-" "laml-validate" "!"))

; Return the name of the content model validation predicate.
; These are the predicates some of whic we need to write manually.
(define (name-of-content-validation-predicate language-name name)
 (cond ((eq? validation-predicate-names 'old) 
         (string-append (as-string name) "-" "checker"))
       ((eq? validation-predicate-names 'normal)
         (string-append (as-string name) "-" (as-string language-name) "-" "checker" "?"))
       (else (laml-error "name-of-content-validation-predicate: unknown value of validation-predicate-names: " validation-predicate-names))))

; Return the name of an action procedure given the element-name.
(define (action-procedure-name element-name)  
  (string-append (as-string element-name) "!"))


; ---------------------------------------------------------------------------------------------------

;;; The main tool procedure.
;;; The main tool procedure is generate-mirror. After setting the parameters call this procedure
;;; which will make the XML mirror.

; Global variables that hold the list of parsed elements and attributes:
(define element-list '())
(define attribute-list '())


;; The main tool procedure which makes the mirror of XML in LAML.
;; The procedure writes a text file in mirror-destination-path. 
;; .form (generate-mirror parsed-dtd-path mirror-destination-path language-name [file-inclusion-path])
;; .parameter parsed-dtd-path the full path to the parsed dtd (a lsp file).
;; .parameter mirror-destination-path full path to the file on which to write the mirror.
;; .parameter language-name the name of language which we mirror in LAML (a symbol).
;; .parameter file-inclusion-path An absolute path to a text file, which will be included at the rear end of the generated mirror library.\
;;  The setup of link checking is typically provided via such file inclusion.
;; .reference "About link checking in LAML" "Note" "../../../info/link-checking.html"
(define (generate-mirror parsed-dtd-path mirror-destination-path language-name . optional-parameter-list) 
 (let ((file-inclusion-path (optional-parameter 1 optional-parameter-list #f)))
   (display-message "Depending on the DTD of the XML language it may take long time to generate the mirror - maybe a few minutes.")
   (let* ((dtd-list (file-read parsed-dtd-path))
          (elements (filter (lambda (el) (eq? (car el) 'element)) dtd-list))
          (attributes (filter (lambda (el) (eq? (car el) 'attribute)) dtd-list))
          (merged-attributes (merge-attribute-list-for-same-element attributes))
          (sorted-attributes   ; attributes sorted in the same order as elements
            (sort-list
              merged-attributes
              (generate-leq (map (compose as-symbol cadr) elements) (compose as-symbol cadr))))
         )

     ; Make the parsed elements and attributes globally available in the tool. 
     ; Introduced to aid XML element navigation

     (set! element-list elements)
     (set! attribute-list attributes)
 
     (let ((xml-navigator-structure (make-xml-navigator-structure))  ; implicitly for the language whose DTD is loaded
           (xml-validator-structure (make-xml-validator-structure language-name))
           (xml-content-model-structure (make-xml-content-model-structure language-name))
           (xml-action-procedure-structure (make-xml-action-procedure-structure language-name))
          )
  
      (write-text-file
       (string-append
        "; This file is generated by an LAML script based on the LAML tool tools/xml-in-laml/xml-in-laml.scm. DO NOT EDIT!" CR CR
  
        (if auto-lib-loading
            (string-append
             ";;; Loading the XML-in-LAML stuff common for all languages:" CR
             "(load (string-append laml-dir \"lib/xml-in-laml/xml-in-laml.scm\"))" CR CR)
            (string-append
             "; lib/xml-in-laml/xml-in-laml.scm is not loaded here. " CR
             "; You must load it yourself prior to the loading of this file." CR CR))

        (language-property-definitions language-name)  CR CR
  
        "; Empty temporary language map" CR
        (as-string `(set! temp-language-map '())) CR CR
  
        CR CR 
        ";;; The validation procedures" CR CR
        (accumulate-right
         string-append
         ""
         (map2 
          (lambda (element attribute)
            (make-xml-validation-procedure element attribute language-name))
          elements sorted-attributes))
  
        ";;; Make and put XML mirror functions in the temporary language map:" CR
        (accumulate-right
         string-append
         ""
         (map2 
          (lambda (element attribute)
            (make-xml-mirror-function element attribute language-name))
          elements sorted-attributes))
  
        "; Register the name of the language:" CR
        (as-string `(register-xml-in-laml-language (quote ,language-name) temp-language-map))   CR CR
  
        "; Define the language variable" CR
        (as-string `(define ,language-name (activator-via-language-map (quote ,language-name))))   CR CR

        "; Register the XML navigator of the language:" CR
        (as-string `(register-xml-in-laml-navigator (quote ,language-name) (quote ,xml-navigator-structure)))   CR CR

        "; Register the validation procedures of the language" CR
        (as-string `(register-xml-in-laml-validators (quote ,language-name) (vector ,@xml-validator-structure)))   CR CR

        "; Register the content model map of the language." CR
        "; This makes the content model available for LAML at runtime." CR
        (as-string `(register-xml-in-laml-content-models (quote ,language-name) (quote ,xml-content-model-structure)))   CR CR

        "; Register the action procedure map of the language." CR
        (as-string `(register-xml-in-laml-action-procedures (quote ,language-name) (vector ,@xml-action-procedure-structure)))   CR CR

        (if file-inclusion-path
            (string-append
              "; Stuff included via the the following file-inclusion path:  " file-inclusion-path CR CR
              (read-text-file file-inclusion-path)
            )
            "")

  
        ) 
       mirror-destination-path)

      ; Generate an .info file side by side with the parsed dtd file.
      ; The .info file contains useful information about the generated mirror, represented as an a-list.
      (file-write
        (mirror-info)
        (string-append (file-name-initial-path parsed-dtd-path) (file-name-proper parsed-dtd-path) "." "info")
      )
  
     'DONE))))


; Merge attribute lists of same element, as prescribed in the XML1.0 specification:
; When more than one AttlistDecl is provided for a given element type, the contents of all those provided are merged.
(define (merge-attribute-list-for-same-element attributes)
  (let ((sorted-attributes (sort-list attributes (lambda (a1 a2) (string<=? (attribute-name-of a1) (attribute-name-of a2))))))
    (merge-attribute-list-for-same-element-1 sorted-attributes)))

(define (merge-attribute-list-for-same-element-1 sorted-attribute-list)
  (cond ((null? sorted-attribute-list) '())     ; empty list
        ((null? (cdr sorted-attribute-list)) sorted-attribute-list)  ; one element-list
        ((equal? (attribute-name-of (first sorted-attribute-list)) (attribute-name-of (second sorted-attribute-list)))  ; at least two elements   
           (let ((merged-first-and-second-attr 
                   (list 'attribute (attribute-name-of (first sorted-attribute-list)) (append (attribute-list-of (first sorted-attribute-list)) (attribute-list-of (second sorted-attribute-list))))))
              (merge-attribute-list-for-same-element-1 (cons merged-first-and-second-attr (cddr sorted-attribute-list)))))  ; re-merge, in case three or more attribute lists need to be merged
        (else (cons (car sorted-attribute-list) (merge-attribute-list-for-same-element-1 (cdr sorted-attribute-list))))))




(define (language-property-definitions language-name)
 (string-append
   (definition-for language-name "xml-transliterate-character-data?" default-xml-transliterate-character-data?)
   (definition-for language-name "xml-char-transformation-table" default-xml-char-transformation-table)
   (definition-for language-name "xml-non-transliteration-elements" default-xml-non-transliteration-elements)
   (definition-for language-name "xml-preformatted-text-elements" default-xml-preformatted-text-elements)

   (definition-for language-name "xml-pass-default-dtd-attributes?" default-pass-default-dtd-attributes?)
   (definition-for language-name "xml-accept-only-string-valued-attributes?" default-xml-accept-only-string-valued-attributes?)
   (definition-for language-name "xml-accept-extended-contents?" default-xml-accept-extended-contents?)
   (definition-for language-name "xml-document-type-declaration" default-document-type-declaration)
   (definition-for language-name "xml-represent-white-space?" default-xml-represent-white-space)
   (definition-for language-name "xml-duplicated-attribute-handling" default-xml-duplicated-attribute-handling)
 )
)



; Generate a textual definition, which can be inserted into the generated mirror.
(define (definition-for language-name variable-name value-string)
 (let ((SP " "))
  (string-append
    "(define" SP 
              (as-string language-name) "-" (as-string variable-name) SP
              value-string ")" CR)))
              


;; Construct and return a mirror function.
;; .parameter element    The parsed element structure, as produced by the LAML DTD parser.
;; .parameter attribute  The parsed attribute structure, as produced by the LAML DTD parser.
;; .parameter language-name The name of the XML language, to which this mirror function belongs.
(define (make-xml-mirror-function element attribute language-name)
 (let* ((ename (string-append mirror-name-prefix (el-name element)))
        (content-model (el-content-model element))
        (scheme-collision? (is-scheme-name? ename))
        (comment (el-comment element))
        (aname (at-name attribute))
        (alist (at-list attribute))
        (validation-procedure-name (name-of-validation-procedure language-name ename))
        (kind (cond ((single-tag? content-model) 'single)
                    ((double-tag? content-model) 'double)
                    (else (laml-error "make-xml-mirror-function: Neither single or double: " ename))))
        (action-element? (turn-into-boolean (memq (as-symbol ename) action-elements)))
        (default-dtd-attributes (extract-defaulted-attributes attribute))
       )
    (if scheme-collision?
        (display-warning 
         (string-append ename ": Element name collides with reserved Scheme name. No named mirror function defined.")))

    (if (eq? (as-symbol language-name) (as-symbol ename))
        (display-warning 
          (string-append ename ": Element name collides with the name of the XML language. Chose another language name!" CR
                               "No named mirror function available.")))

    (string-append 
     (make-xml-mirror-function-help kind ename default-dtd-attributes validation-procedure-name scheme-collision?
                                    language-name action-element?)
     CR CR)
  )
)
   

(define (make-xml-mirror-function-help kind ename default-dtd-attributes validation-procedure scheme-collision? language-name action-element?)
 (let* ((action-procedure (if action-element? (action-procedure-name ename) #f))
        (action-procedure-value (if pass-action-procedure? action-procedure (turn-into-boolean action-element?)))
       )
  (string-append
   (as-string-1
    `(set! temp-mirror-function
      (generate-xml-mirror-function ,validation-procedure ,(string-it ename) (quote ,default-dtd-attributes)
                                    (quote ,kind) (quote ,language-name) #f #f))) CR                          ; No action procedure!?

   (as-string
    `(set! temp-language-map (put-mirror-function temp-language-map ,(string-it ename) temp-mirror-function))) CR

   (if (and define-named-mirror-functions? (not scheme-collision?))
       (string-append
        (as-string-1
         `(set! temp-mirror-function
           (generate-xml-mirror-function ,validation-procedure ,(string-it ename) (quote ,default-dtd-attributes)
                                         (quote ,kind) (quote ,language-name) #t ,action-procedure-value))) 
           CR)
       "")

   (cond ((and define-named-mirror-functions? (not scheme-collision?))
            (as-string
             `(define ,ename temp-mirror-function)
             ))
         ((not define-named-mirror-functions?) 
             (string-append "; No named mirror functions defined for this mirror" ))
         (else 
          (string-append "; No mirror of " (as-string ename) " defined due to name clash with reversed Scheme name." )))

   )
  )
)

;; Make and return a validation procedure.
;; .parameter element    The parsed element structure, as produced by the LAML DTD parser.
;; .parameter attribute  The parsed attribute structure, as produced by the LAML DTD parser.
;; .parameter language-name The name of the XML language, to which this mirror function belongs.
(define (make-xml-validation-procedure element attribute language-name)
 (let* ((ename (el-name element))
        (parsed-content-model (el-content-model element))
        (comment (el-comment element))
        (aname (at-name attribute))
        (alist (at-list attribute))
        (val-proc-name (name-of-validation-procedure language-name ename))
       )
 (if (not (equal? ename aname))
     (error 
      (string-append 
        "make-html-basic-mirror-function: element name and attribute name must be the same. " (as-string ename) " " (as-string aname)  CR
        "For each element definition there must be an explicit attribute definition.")))
 
 (string-append 
     (cond 
           ((empty-element-rhs? parsed-content-model)
              (make-empty-element-xml-validation-procedure val-proc-name ename comment alist parsed-content-model language-name))
           ((any-element-rhs? parsed-content-model)
              (make-any-element-xml-validation-procedure val-proc-name ename comment alist parsed-content-model language-name))
           ((mixed-content-pcdata-only-rhs? parsed-content-model)
              (make-pcdata-xml-validation-procedure val-proc-name ename comment alist parsed-content-model language-name))
           ((mixed-content-rhs? parsed-content-model)
              (make-mixed-content-xml-validation-procedure val-proc-name ename comment alist parsed-content-model language-name))
           ((element-content-rhs? parsed-content-model)
             (make-element-content-xml-validation-procedure val-proc-name ename comment alist parsed-content-model language-name))

           (else (error (string-append "make-xml-validation-procedure: " ename ". Unknown kind of content model."))))
     CR CR)
 )
)

(define (make-pcdata-xml-validation-procedure val-proc-name tag-name comment attribute-list rhs language)
 (let* (
        (lc-tag-name tag-name)
        (lc-tag-name-string (string-it lc-tag-name))
        (attribute-list-1 (reorder-attributes attribute-list))
        (number-of-required-attributes (length (filter required-attribute? attribute-list-1))) ; could be made more efficient...
        (par (lambda(str) ""))
        (com-format (lambda(str) (par (if (empty-string? str) ""  (strip-initial-spaces str)))))
        (stringed-attribute-list (map (lambda (triple) (map string-it-1 triple)) attribute-list-1))

       )
  (string-append
   (as-string-1
    `(define (,val-proc-name el-name attributes contents overlap-check?)
       (let ((attributes-of-elements (quote ,stringed-attribute-list))
             (req-n ,number-of-required-attributes)
            )
         (if (and overlap-check? xml-check-language-overlap?)
             (check-language-overlap! (as-symbol ,(string-it lc-tag-name))))

         (if xml-check-attributes?
             (xml-check-attributes! attributes attributes-of-elements req-n ,(string-it lc-tag-name)))

         (if xml-validate-contents?  
             (validate-as-pcdata! contents ,(string-it lc-tag-name)))

     ))
    )
   )
 ))

(define (make-empty-element-xml-validation-procedure val-proc-name tag-name comment attribute-list rhs language)
 (let* (
        (lc-tag-name tag-name)
        (lc-tag-name-string (string-it lc-tag-name))
        (attribute-list-1 (reorder-attributes attribute-list))
        (number-of-required-attributes (length (filter required-attribute? attribute-list-1))) ; could be made more efficient...
        (stringed-attribute-list (map (lambda (triple) (map string-it-1 triple)) attribute-list-1))
       )
  (string-append
   (as-string
    `(define (,val-proc-name el-name attributes contents overlap-check?)  ; earlier (,val-proc-name . attributes)
       (let ((attributes-of-elements (quote ,stringed-attribute-list))
             (req-n ,number-of-required-attributes)
            )
         (xml-check-for-empty-contents! contents ,(string-it lc-tag-name))

         (if (and overlap-check? xml-check-language-overlap?)
             (check-language-overlap! (as-symbol ,(string-it lc-tag-name))))

         (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n ,(string-it lc-tag-name)))))
   )
  )
 )
)

(define (make-any-element-xml-validation-procedure val-proc-name tag-name comment attribute-list rhs language)
 (let* (
        (lc-tag-name tag-name)
        (lc-tag-name-string (string-it lc-tag-name))
        (attribute-list-1 (reorder-attributes attribute-list))
        (number-of-required-attributes (length (filter required-attribute? attribute-list-1))) ; could be made more efficient...
        (stringed-attribute-list (map (lambda (triple) (map string-it-1 triple)) attribute-list-1))
       )
  (string-append
   (as-string
    `(define (,val-proc-name el-name attributes contents overlap-check?)  ; earlier (,val-proc-name . attributes)
       (let ((attributes-of-elements (quote ,stringed-attribute-list))
             (req-n ,number-of-required-attributes)
            )

         (if (and overlap-check? xml-check-language-overlap?)
             (check-language-overlap! (as-symbol ,(string-it lc-tag-name))))

         (if xml-check-attributes? (xml-check-attributes! attributes attributes-of-elements req-n ,(string-it lc-tag-name)))))
   )
  )
 )
)

(define (make-element-content-xml-validation-procedure val-proc-name tag-name comment attribute-list rhs language)
 (let* (
        (lc-tag-name tag-name)
        (lc-tag-name-string (string-it lc-tag-name))
        (attribute-list-1 (reorder-attributes attribute-list))
        (number-of-required-attributes (length (filter required-attribute? attribute-list-1))) ; could be made more efficient...
        (par (lambda(str) ""))
        (com-format (lambda(str) (par (if (empty-string? str) ""  (strip-initial-spaces str)))))
        (stringed-attribute-list (map (lambda (triple) (map string-it-1 triple)) attribute-list-1))

        (simplified-rhs-syntax-tree (simplify-parsed-rhs rhs))
        (the-dfa (dfa-acceptor simplified-rhs-syntax-tree))

       )
  (string-append
   (as-string-1
    `(define (,val-proc-name el-name attributes contents overlap-check?)
       (let ((attributes-of-elements (quote ,stringed-attribute-list))
             (req-n ,number-of-required-attributes)
             (dfa (quote ,the-dfa))
            )
         (if (and overlap-check? xml-check-language-overlap?)
             (check-language-overlap! (as-symbol ,(string-it lc-tag-name))))

         (if xml-check-attributes?
             (xml-check-attributes! attributes attributes-of-elements req-n ,(string-it lc-tag-name)))

         (if xml-validate-contents?  
             (validate-contents-by-dfa!
               contents 
               dfa ,(string-it lc-tag-name))
             )

     ))
    )
   )
 )
)

(define (make-mixed-content-xml-validation-procedure val-proc-name tag-name comment attribute-list rhs language)
 (let* (
        (lc-tag-name tag-name)
        (lc-tag-name-string (string-it lc-tag-name))
        (attribute-list-1 (reorder-attributes attribute-list))
        (number-of-required-attributes (length (filter required-attribute? attribute-list-1))) ; could be made more efficient...
        (par (lambda(str) ""))
        (com-format (lambda(str) (par (if (empty-string? str) ""  (strip-initial-spaces str)))))
        (stringed-attribute-list (map (lambda (triple) (map string-it-1 triple)) attribute-list-1))

        (element-symbol-choice-list (map as-symbol (cddr (cadr rhs))))  ; rhs = (mixed-contents (choice pcdata "el1" "el2" ... "eln"))
       )
  (string-append
   (as-string-1
    `(define (,val-proc-name el-name attributes contents overlap-check?)
       (let ((attributes-of-elements (quote ,stringed-attribute-list))
             (req-n ,number-of-required-attributes)
            )
         (if (and overlap-check? xml-check-language-overlap?)
             (check-language-overlap! (as-symbol ,(string-it lc-tag-name))))

         (if xml-check-attributes?
             (xml-check-attributes! attributes attributes-of-elements req-n ,(string-it lc-tag-name)))

         (if xml-validate-contents?  
             (validate-mixed-contents-by-simple-means! contents (quote ,element-symbol-choice-list) ,(string-it lc-tag-name))
         )

     ))
    )
   )
 )
)

(define (string-or-quote-it x)
  (cond ((string? x) (string-it x))
        ((symbol? x) (list 'quote x))))



; Re-arrange attribute-list such that the required attributes comes first.
(define (reorder-attributes attribute-list)
  (let* (
         (required-attributes (filter required-attribute? attribute-list))
         (optional-attributes (filter optional-attribute? attribute-list)))
    (append required-attributes optional-attributes)))



; Element and attribute selectors - in relation to Lisp representation of DTD information.

(define el-name (make-selector-function 2))
(define el-start-tag (make-selector-function 3))
(define el-end-tag (make-selector-function 4))
(define el-content-model (make-selector-function 5))
(define el-comment (make-selector-function 6))

; Selectors on a parsed DTD attribute structure such as the list
; (attribute "el-name" (("id" "CDATA" "#IMPLIED") ("margin" "CDATA" "#IMPLIED")))
(define at-name (make-selector-function 2))
(define at-list (make-selector-function 3))

; Selectors on a single DTD attribute, such as 
; ("id" "CDATA" "#IMPLIED") or
; ("id" "CDATA" "xxx" "#FIXED") or
(define att-name (make-selector-function 1))
(define att-type (make-selector-function 2))
(define att-status-and-default (make-selector-function 3))

(define att-fixed? 
  (lambda (a) 
    (if (= (length a) 4)
        (equal? (fourth a) "#FIXED")
        #f)))


; -----------------------------------------------------------------------------
; Predicates that distinguish various kinds of parsed RHSes (as taken from a parsed DTD) from each other.
; Replicated in styles/manual/manual.scm for the sake of content model unparsing.

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

; ----------------------------------------------------------------------------------
; Single double tag distinctions.

; Does the content-model of an element indicate an empty element (single-tag). 
(define (single-tag? content-model)
  (empty-element-rhs? content-model))

; Does the content-model of an element indicate a non-empty element (double-tag).
(define (double-tag? content-model)
  (not (empty-element-rhs? content-model)))


; ----------------------------------------------------------------------------------------


(define (required-attribute? attr-structure)
  (equal? (att-status-and-default attr-structure) "#REQUIRED"))

(define (optional-attribute? attr-structure)
  (not (equal? (att-status-and-default attr-structure) "#REQUIRED")))

(define (defaulted-attribute? attr-structure)
 (let ((att-status-default (att-status-and-default attr-structure)))
  (not 
    (or (equal? att-status-default "#REQUIRED")
        (equal? att-status-default "#IMPLIED")))))

; Return an attribute list - a property list - of attribute name / attribute default value.
; .parameter dtd-attribute-structure the attribute structure of a parsed DTD structure.
(define (extract-defaulted-attributes dtd-attribute-structure)
 (let ((dtd-attr-list (at-list dtd-attribute-structure))
       (dtd-att-to-runtime-att 
            (lambda (dtd-att)
              (cons (att-name dtd-att) (string-it (att-status-and-default dtd-att)))))
      )
   (alist-to-propertylist (map dtd-att-to-runtime-att (filter defaulted-attribute? dtd-attr-list)))))
  



; A more general version of string-it, which is able to recursively apply string-it on lists of strings.
(define (string-it-1 x)
  (cond ((string? x) (string-it x))
        ((list? x) (map string-it-1 x))
        (else (error "string-it-1: Applied on unsupported element"))))

; A variant of as-string, which translates booleans to #t and #f instead of true and false
(define (as-string-1 x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) x)
        ((boolean? x) 
            (if x "#t" "#f"))   ; true and false in the normal version 
        ((char? x) (char->string x))
        ((list? x)
            (string-append "(" 
               (string-merge (map as-string-1 x) (make-list (- (length x) 1) " "))
               ")"))
        ((vector? x)
          (let ((lst (vector->list x)))
            (string-append "#(" 
               (string-merge (map as-string-1 lst) (make-list (- (length lst) 1) " "))
               ")")))
        ((pair? x)
            (string-append "(" 
               (apply string-append
                  (map (lambda (y) (string-append (as-string-1 y) " ")) (proper-part x))
               )
               " . " (as-string-1 (first-improper-part x))
               ")"))
        (else "??")))

; Move to general...
(define CR (as-string #\newline))


; ---------------------------------------------------------------------------------------------------------------
; DFA generation from parsed rhs (parsed regular expression). This is kernel of the
; automatic generation of validators, as available from LAML version 20.
; Involves a number of simplifications of the regular expression syntax tree.
; Depends on lib/final-state-automaton.scm.



; ---------------------------------------------------------------------------------------------------
; Predicate that select simplication constructs

(define (rhs-element-content-node? x)
  (and (list? x) (>= (length x) 2) (eq? (car x) 'element-content)))

(define (choice-one-or-more-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'choice) (eq? (cadr x) 'one-or-more)))

(define (choice-optional-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'choice) (eq? (cadr x) 'optional)))

(define (seq-one-or-more-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'seq) (eq? (cadr x) 'one-or-more)))

(define (seq-optional-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'seq) (eq? (cadr x) 'optional)))

(define (name-one-or-more-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'name) (eq? (cadr x) 'one-or-more)))

(define (name-optional-node? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'name) (eq? (cadr x) 'optional)))

; ---------------------------------------------------------------------------------------------------

; Simplifications that eliminate one-or-more and optional constructs.
(define (simplify-parsed-rhs parsed-rhs)
 (cond ((rhs-element-content-node? parsed-rhs) 
          (list 'element-content (eliminate-unary-seq (to-binary-seq (rhs-triple-to-tuple (simplify-parsed-rhs-1 (cadr parsed-rhs)))))))
       (else parsed-rhs)))

(define (simplify-parsed-rhs-1 element-content-node)
 (letrec ((rhs-copy    ; just a simple, recursive lisp copy function
            (lambda (x) 
             (cond ((pair? x) (cons (rhs-copy (car x)) (rhs-copy (cdr x))))
                   (else x)))))
  (cond ((choice-one-or-more-node? element-content-node)          ; (choice one-or-more CONTENT)       =>     (seq one (choice one CONTENT) (choice zero-or-more CONTENT))  OK
          (let* ((content (cddr element-content-node))
                 (const1 `(choice one ,@(simplify-parsed-rhs-1 (rhs-copy content))))
                 (const2 `(choice zero-or-more ,@(map simplify-parsed-rhs-1 (rhs-copy content))))
                )
            `(seq one ,const1 ,const2))
        )
        ((choice-optional-node? element-content-node)             ; (choice optional CONTENT)          =>     (choice one (choice one CONTENT) (empty one "")) OK
          (let* ((content (cddr element-content-node))
                 (const1 `(choice one ,@(map simplify-parsed-rhs-1 (rhs-copy content))))
                 (const2 `(empty one ""))
                )
            `(choice one ,const1 ,const2))
        )
        ((seq-one-or-more-node? element-content-node)             ; (seq one-or-more CONTENT)          =>     (seq one CONTENT (seq zero-or-more CONTENT))  OK
          (let* ((content (cddr element-content-node))
                 (const `(seq zero-or-more ,@(map simplify-parsed-rhs-1 (rhs-copy content))))
                )
            `(seq one ,@content ,const))
        )
        ((seq-optional-node? element-content-node)                ; (seq optional CONTENT)             =>     (choice one (empty one "") (seq one CONTENT))   
          (let* ((content (cddr element-content-node))
                 (const1 `(empty one ""))
                 (const2 `(seq one ,@(map simplify-parsed-rhs-1 (rhs-copy content))))
                )
            `(choice one ,const2 ,const1))
        )
        ((name-one-or-more-node? element-content-node)            ; (name one-or-more "name")          =>     (seq one (name one "name") (name zero-or-more "name"))  OK
          (let* ((the-name (caddr element-content-node))
                 (const1 `(name one ,the-name))
                 (const2 `(name zero-or-more ,the-name))
                )
            `(seq one ,const1 ,const2))
        )
        ((name-optional-node? element-content-node)               ; (name optional "name")             =>     (choice one (name one "name") (empty one ""))
          (let* ((the-name (caddr element-content-node))
                 (const1 `(name one ,the-name))
                 (const2 `(empty one ""))
                )
            `(choice one ,const1 ,const2))           
        )
        ((and (list? element-content-node) (>= (length element-content-node) 3))    ; another tripple
          (let* ((kind (car element-content-node))
                 (mult (cadr element-content-node))
                 (content (cddr element-content-node)))
            `(,kind ,mult ,@(map simplify-parsed-rhs-1 content)))
        )
        (else element-content-node)
   )))


; ---------------------------------------------------------------------------------------------------
; Going from triples to tuples, eliminating the multiplicty component, and introducing star nodes.

(define (rhs-triple-to-tuple element-content-node)
  (cond ((and (list? element-content-node) (>= (length element-content-node) 3) (eq? 'zero-or-more (cadr element-content-node)))
          (let ((kind (car element-content-node))
                (content (cddr element-content-node)))
            `(star (,kind ,@(map rhs-triple-to-tuple content)))))
        ((and (list? element-content-node) (>= (length element-content-node) 3) (eq? 'one (cadr element-content-node)))
          (let ((kind (car element-content-node))
                (content (cddr element-content-node)))
            `(,kind ,@(map rhs-triple-to-tuple content))))
        (else element-content-node)))

; ---------------------------------------------------------------------------------------------------
; Enforcing binary seq forms, instead of n-ary.
; Must be applied on a tuple form.
(define (to-binary-seq element-content-node)
  (cond ((and (list? element-content-node) (> (length element-content-node) 3) (eq? 'seq (car element-content-node))) ; non-binary seq node
          (let ((first-form (cadr element-content-node))
                (rest-forms (cddr element-content-node)))
             (list 'seq first-form (to-binary-seq (cons 'seq (map to-binary-seq rest-forms))))))
        ((and (list? element-content-node) (>= (length element-content-node) 2)) 
          (let ((kind (car element-content-node))
                (content (cdr element-content-node)))
            `(,kind ,@(map to-binary-seq content))))
        (else element-content-node)))

; Unnest and eliminate forms of the kind (seq X). Transform to X throughout.
; Works on the output of to-binary-seq.
(define (eliminate-unary-seq element-content-node)
  (cond ((and (list? element-content-node) (eq? 'seq (car element-content-node)) (= (length element-content-node) 2)) ; unary seq node
          (let ((seq-content (cadr element-content-node)))
             (eliminate-unary-seq seq-content)))
        ((and (list? element-content-node) (>= (length element-content-node) 2)) 
          (let ((kind (car element-content-node))
                (content (cdr element-content-node)))
            `(,kind ,@(map eliminate-unary-seq content))))
        (else element-content-node)))

; -------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Assign possitions to name nodes:
; Position n is represented as a cons pair with the name, such as in (name ("a" . n)).
; In a functional manner, return a syntax tree with positions on name nodes.
(define (syntax-tree-with-positions element-content-node)
   (set! next-position-number 0)
   (set! position-symbol-map '())
   (syntax-tree-with-positions-1 element-content-node))

(define next-position-number 0)
(define position-symbol-map '()) ; alist that maps positions to symbols (strings).

(define (syntax-tree-with-positions-1 element-content-node)
  (cond ((and (list? element-content-node) (= (length element-content-node) 2) (eq? 'name (car element-content-node)))
          (set! next-position-number (+ 1 next-position-number))
          (let ((the-name (cadr element-content-node)))
             (set! position-symbol-map (cons (cons next-position-number the-name) position-symbol-map))
             (list 'name (cons the-name next-position-number))))
        ((and (list? element-content-node) (>= (length element-content-node) 2)) 
          (let ((kind (car element-content-node))
                (content (cdr element-content-node)))
            `(,kind ,@(map syntax-tree-with-positions-1 content))))
        (else element-content-node)))
  

; -------------------------------------------------------------------------------------------------------------------------------------------------------------------
; nullable, firstpos, lastpos, followpos functions on element-content-nodes.
; Applied on binary seq nodes and n-ary choice nodes, as returned from to-binary-seq.

; Node precicates
(define (empty-element-content-node? ecn)
  (and (list ecn) (>= (length ecn) 2) (eq? (car ecn) 'empty)))

(define (name-element-content-node? ecn)
  (and (list ecn) (>= (length ecn) 2) (eq? (car ecn) 'name)))

(define (star-element-content-node? ecn)
  (and (list ecn) (>= (length ecn) 2) (eq? (car ecn) 'star)))

(define (seq-element-content-node? ecn)
  (and (list ecn) (>= (length ecn) 2) (eq? (car ecn) 'seq)))

(define (choice-element-content-node? ecn)
  (and (list ecn) (>= (length ecn) 2) (eq? (car ecn) 'choice)))

(define (position-number-of-name-node ecn)
  (cdr (cadr ecn)))

(define (or-fn x y ) (or x y))
(define (and-fn x y ) (and x y))

(define (normalize-positions pos-list)
  (sort-list (remove-duplicates-by-predicate pos-list =) <=))

(define (nullable? ecn)
 (cond ((empty-element-content-node? ecn)      #t)
       ((name-element-content-node? ecn)       #f)
       ((choice-element-content-node? ecn)     (accumulate-right or-fn #f (map nullable? (cdr ecn))))
       ((seq-element-content-node? ecn)        (accumulate-right and-fn #t (map nullable? (cdr ecn))))
       ((star-element-content-node? ecn)       #t)
       (else (laml-error "nullable: Unknown node type encountered: " ecn))))

(define (firstpos ecn)
 (cond ((empty-element-content-node? ecn)     '())
       ((name-element-content-node? ecn)      (list (position-number-of-name-node ecn)))
       ((choice-element-content-node? ecn)    (normalize-positions (accumulate-right append '() (map firstpos (cdr ecn)))))
       ((seq-element-content-node? ecn)       (let ((c1 (cadr ecn))  ; binary seq
                                                    (c2 (caddr ecn)))
                                                (if (nullable? c1)
                                                    (normalize-positions (append (firstpos c1) (firstpos c2)))
                                                    (firstpos c1))))
       ((star-element-content-node? ecn)      (let ((c1 (cadr ecn)))  ; unary star
                                                (firstpos c1)))
       (else (laml-error "firstpos: Unknown node type encountered: " ecn))))

(define (lastpos ecn)
 (cond ((empty-element-content-node? ecn)     '())
       ((name-element-content-node? ecn)      (list (position-number-of-name-node ecn)))
       ((choice-element-content-node? ecn)    (normalize-positions (accumulate-right append '() (map lastpos (cdr ecn)))))
       ((seq-element-content-node? ecn)       (let ((c1 (cadr ecn))  ; binary seq
                                                    (c2 (caddr ecn)))
                                                (if (nullable? c2)
                                                    (normalize-positions (append (lastpos c1) (lastpos c2)))
                                                    (lastpos c2))))
       ((star-element-content-node? ecn)      (let ((c1 (cadr ecn)))  ; unary star
                                                (lastpos c1)))
       (else (laml-error "lastpos: Unknown node type encountered: " ecn))))


; Return followpos of position i in the syntax tree rooted by the element content node ecn.
; Method: Traverse the syntax tree and collect the contributions for each star and seq node.
(define (followpos i ecn)
 (cond ((seq-element-content-node? ecn)       (let* ((c1 (cadr ecn))  ; binary seq
                                                     (c2 (caddr ecn))
                                                    )
                                                (if (member i (lastpos c1))
                                                    (normalize-positions (append (firstpos c2) (followpos i c1) (followpos i c2)))
                                                    (normalize-positions (append (followpos i c1) (followpos i c2))))))

       ((star-element-content-node? ecn)      (let ((c1 (cadr ecn)))
                                                (if (member i (lastpos ecn))
                                                    (normalize-positions (append (firstpos ecn) (followpos i c1)))
                                                    (normalize-positions (followpos i c1)))))

       ((choice-element-content-node? ecn)    (normalize-positions (accumulate-right append '() (map (lambda (n) (followpos i n)) (cdr ecn)))))

       ((empty-element-content-node? ecn)     '())
       ((name-element-content-node? ecn)      '())

       (else (laml-error "followpos: Unknown node type encountered: " ecn))))

; -------------------------------------------------------------------------------------------------------------------------------------------

; The symbol which is assumed to end all input to the dfa. 
; Must comply with the similar constant in lib/xml-in-laml/xml-in-laml.scm.
(define terminator-symbol 'terminator$$)


; DFA construction from follow pos on a given syntax tree.
; Augments the syntax tree with a terminator symbol, add positions to the syntax tree,
; find the followpos graph, and hereby a NFA. Use the subset construction to make af DFA.
; The syntax tree is on simplified and seq-binary form.
; Assumes as precondition that the final-state-automaton.scm stuff is loaded (not done here).
(define (dfa-acceptor rhs-syntax-tree)
 (let* ((rhs-syntax-tree-proper (cadr rhs-syntax-tree))  ; without (element-content ...) and (mixed-content ...)
        (augmented-rhs-syntax-tree-proper (list 'seq rhs-syntax-tree-proper (list 'name (as-string terminator-symbol))))
        (positioned-rhs-syntax-tree-proper (syntax-tree-with-positions augmented-rhs-syntax-tree-proper))
        (highest-position next-position-number) ; the global variable left by syntax-tree-with-positions
        (accepting-state highest-position)
        (init-states (firstpos positioned-rhs-syntax-tree-proper))
        (fresh-init-state 0) 
        (epsilon-transitions
         (map
          (lambda (edge) (make-automaton-transition (car edge) (as-symbol (get-by-predicate (cdr edge) position-symbol-map =)) (cdr edge)))
          (map (lambda (is) (cons 0 is)) init-states)))
        (followpos-edges ; a list of (x . y) meaning that there is a transition from x to y in the graph ala 3.43 page 140 Aho, Sethi, Ullman
          (flatten
           (map
            (lambda (i)
              (let ((followpos-i (followpos i positioned-rhs-syntax-tree-proper)))
                (map (lambda (fpi) (cons i fpi)) followpos-i)))
            (number-interval 1 highest-position))))
       )
  (let* ((nfa 
          (make-finite-state-automaton fresh-init-state (list accepting-state)
           (append epsilon-transitions
            (map (lambda (followpos-edge)
                   (let* ((i (car followpos-edge))
                          (j (cdr followpos-edge))
                          (the-symbol (get-by-predicate j position-symbol-map =))
                         )
                     (make-automaton-transition i (as-symbol the-symbol) j)))
                 followpos-edges))))
         (dfa (subset-construction nfa)))
    dfa)))
               
; --------------------------------------------------------------------------------------------------------------------------------------------
; XML Navigation Information.
; Assume here that element-list contains the list of elements (list structure).
; And assume similarly that attribute-list contains the list of elements (list structure).


       
; Make and return an XML navigator structure from the parsed element list, element-list, and from
; the parsed attribute list, attribtue-list. An XML navigator structure is a vector of element-tripples.
; An element-tripple is of the form (el element-navigator attribute-navigator), where
; el is a symbol. 
(define (make-xml-navigator-structure)
 (let ((elements-prepare 
          (lambda (el-list)
             (list->vector (map as-symbol (sort-list el-list string<=?)))))
       (attributes-prepare
          (lambda (attr-list)
             (list->vector (map as-symbol (sort-list attr-list string<=?)))))
      )

  (let* ((element-names (all-element-names element-list))
 
         (element-contr (map elements-prepare (map sub-element-names-of-element-name* element-names)))
         (attr-contr (map attributes-prepare (map attributes-of-element-name* element-names)))
        )

   (list 
     'xml-navigator
     (list->vector
      (sort-list
       (map (lambda (el-name el-structure attr-structure)
              (list (as-symbol el-name)
                    el-structure
                    attr-structure))
            element-names
            element-contr
            attr-contr)
       (lambda (e1 e2)
         (string<=? (as-string (car e1)) (as-string (car e2))))
      )
     )
   )
  )
 )
)

; Return a sorted vector that maps element names (strings) to 
; names of validator procedures. When used in the context of a generated mirror,
; this will map element names (strings) to validator procedures.
; Notice that this function a Scheme source generating function
(define (make-xml-validator-structure language-name)
 (let* ((element-names (all-element-names element-list))
        (validator-names (map (lambda (el-name) (name-of-validation-procedure language-name el-name)) element-names))
       )
   (sort-list
       (map (lambda (el-name val-name)
              (list 'list (string-it (as-string el-name))
                    (as-symbol val-name)))
            element-names
            validator-names)
       (lambda (e1 e2)
         (string<=? (cadr e1) (cadr e2)))
      )))


; Return a sorted vector that maps element names (strings) to their LAML parsed content models in language.
(define (make-xml-content-model-structure language-name)
 (let* ((sorted-element-list 
          (sort-list element-list (lambda (e1 e2) (string<=? (as-string (element-name-of e1)) (as-string (element-name-of e2))))))
        (element-names (all-element-names sorted-element-list))
        (content-models (map rhs-of-element sorted-element-list))
       )
   (list->vector
     (map2 (lambda (n cm) (list (string-it n) cm)) element-names content-models))))


; Make an action procedure structure for the language named language-name.
; An action procedure structure is a sorted list (to be made into a sorted associative vector) that
; maps element names to action procedures. Notice that this is a partial map, because in the typical
; case only few elements have action procedures.
(define (make-xml-action-procedure-structure language-name)
 (let* ((action-element-names (map as-string action-elements))
        (action-procedure-names (map (lambda (el-name) (action-procedure-name el-name)) action-element-names))
       )
   (sort-list
       (map (lambda (el-name action-proc-name)
              (list 'list (string-it (as-string el-name))
                    `(lambda (ast) (,(as-symbol action-proc-name) ast))
              )
            )
            action-element-names
            action-procedure-names)
       (lambda (e1 e2)
         (string<=? (cadr e1) (cadr e2)))
      ))
)


; Return an a-list with useful information about the mirror library, which is generated.
; This information can be presented in a SchemeDoc manual.
(define (mirror-info)
  (list
    (cons 'mirror-name mirror-name)
    (cons 'auto-lib-loading auto-lib-loading)
    (cons 'action-elements action-elements)
    (cons 'define-named-mirror-functions? define-named-mirror-functions?)
    (cons 'mirror-name-prefix mirror-name-prefix)
    (cons 'default-xml-transliterate-character-data? default-xml-transliterate-character-data?)
    (cons 'default-xml-non-transliteration-elements default-xml-non-transliteration-elements)
    (cons 'default-xml-preformatted-text-elements default-xml-preformatted-text-elements)
    (cons 'default-xml-char-transformation-table default-xml-char-transformation-table)
    (cons 'default-pass-default-dtd-attributes? default-pass-default-dtd-attributes?)
    (cons 'default-xml-accept-only-string-valued-attributes? default-xml-accept-only-string-valued-attributes?)
    (cons 'default-xml-accept-extended-contents? default-xml-accept-extended-contents?)
    (cons 'default-xml-represent-white-space default-xml-represent-white-space)
    (cons 'default-xml-duplicated-attribute-handling default-xml-duplicated-attribute-handling)
  )
) 