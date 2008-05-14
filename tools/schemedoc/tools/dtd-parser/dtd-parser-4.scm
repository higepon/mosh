;=>man/dtd-parser.sdoc

;;;; This tool parses an XML Document Type Definition (DTD). The parsed result is represented as a flat list of element, attribute,
;;;; and entity descriptors. <p>
;;;;
;;;; This version of the XML DTD parser also parses the content models of the XML elements. This is the basis for the fully automatic synthesis of
;;;; finite state automata for validation of XML-in-LAML documents at document generation time.<p>
;;;;
;;;; Another tool - <a href="../../xml-in-laml/man/xml-in-laml.html">the XML-in-LAML mirror generation tool</a> -
;;;; is able to produce a Scheme mirror of the XML language. The mirror generation tool takes as input the data structures, which are produced by the DTD parser.<p>
;;;;
;;;; There is some <a href="http://www.cs.aau.dk/~normark/scheme/tools/dtd-parser/doc/dtd-parser.html"> internal elucidative documentation</a>
;;;; of the DTD parser at the LAML development site.<p>
;;;;
;;;; Please consult <a href = "../../../tutorial/xml-in-laml/xml-in-laml.html">"XML mirrors in Scheme: XML in LAML"</a> (section 2)
;;;; for a tutorial introduction to the use of the parser.<p>
;;;;
;;;; The DTD parser is not perfect, but we steadily improve it when it is needed. See the README file in the directory of the source files
;;;; for details and progress.<p>
;;;; 
;;;; An earlier (and now obsolete) version of this tool was used to parse the HTML4.01 DTD (which is a non-XML DTD).
;;;; .title Reference Manual of the XML Document Type Definition Parser

;;; Introduction and usage.
;;; The easiest way to parse an XML DTD is to use the procedure
;;; <a href="../../../man/laml.html#xml-dtd-parse">xml-dtd-parse</a>
;;; located in laml.scm in the root of LAML distribution.
;;; This procedure can be called from a LAML prompt.<p>
;;;
;;; Here is another slightly more low-level way to use the parser.
;;; <ol>
;;; <li> Start a Scheme interpreter, such as SCM or MzScheme.
;;; 
;;; <li> Define the variable laml-dir to the LAML directory path on your computer.
;;; 
;;; <li> Load this file (dtd-parser-4.scm in tools/dtd-parser) into a Scheme interpreter, for instance from the directory 
;;; in which the dtd-parser.scm source file resides.
;;; 
;;; <li> Call the function (parse-dtd "dtd-file-without-extension"). Additional parameters of parse-dtd
;;; are interpreted as non-expanding entities.
;;; A call of the function will give you a similar Lisp parsed file with lsp extension. In addition the 
;;; variables element-list, attribute-list, and entity-list will be defined
;;; by the parser. The resulting file contains a "flat" concatenation of these three lists.
;;; </ol>
;;; We now describe the format of an entity, an attribute, and an element in the parsed result.
;;; Each unit in the result of the parsing is either an element, an attribute, or an entity.
;;; An element describes, in popular terms, "a single tag" in the markup language. An attribute describes the attribute
;;; of an element. Thus, in general, there is a one-to-one correspondance between the element list and the attribute-list.
;;; The list of entities are macro expansions, which have been applied to achieve a clean and parsable dtd file.
;;; The entity list is of minor importance once the file has been parsed.<p>
;;; The first element in each of the top level form of the parsed result distinguishes this element from each others
;;; (tagging).<p>
;;;
;;; <ul>
;;; <li> Element format: <br>
;;;      (element name start-tag-status end-tag-status content-model comment)<p>
;;; </ul>
;;; The start-tag-status and end-tag-status are left overs from SGML and HTML4, see <a href="http://www.w3.org/TR/html401/intro/sgmltut.html#h-3.3">the HTML4.01 definition</a>.
;;; These two status fields are not used for XML. The content model is a symbol (any or empty) or a list prefixed with either mixed-content or element-content.<p>
;;;
;;; Mixed contents list have the form
;;; <pre>
;;;   (mixed-content pcdata)
;;;   (mixed-content (choice zero-or-more pcdata NAME-STRING NAME-STRING ... NAME-STRING))
;;; </pre>
;;; 
;;; Element contents lists have the form
;;; <pre> 
;;;   (element-contents CONT)
;;; </pre>
;;; where <p>
;;;    CONT ::=  ( KIND MULTIPLICITY DATA+ )                              <br>
;;;    KIND ::=  name | seq | choice | empty                              <br>
;;;    MULTIPLICITY ::= one | optional | zero-or-more | one-or-more       <br>
;;;    DATA ::= NAME-STRING | CONT                                        <p>
;;; and where NAME-STRING is a placeholder for a string constant.<p>
;;;
;;; Here is information about the parsed formats of attributes and entities:
;;; <ul>
;;; <li> Attribute format: <br>
;;;      (attribute name list-of-attribute-triples). <br>
;;;      An attribute triple consist of
;;;      attribute name, attribute type (most basic type, no entities), and "availability status".<p>
;;;
;;; <li> Entity name: <br> 
;;;      (entity name entity-expansion).
;;; </ul>
;;;



; A list of entities not to expand.
(define non-expand-entities '())

;;; The main parsing function.
;;; There is only one function, parse-dtd, to learn. This function represents and "is" the tool.

;; Parses a dtd file to Lisp expressions and writes the parsed result to another file.
;; The parameter file is without extension. The second parameter is a list
;; of entity names (strings without leading percent chars); no entity in the list will be expanded.
;; This function also defines the lists element-list, attribute-list, and entity-list.
;; The parsed result is the appending of element-list, attribute-list, and entity-list.
;; It is assumed that the DTD file resides on file.dtd.
;; This function writes the result to the file.lsp.
(define (parse-dtd file . non-expanding-entities)
   (reset-dtd-parser)
   (set! non-expand-entities non-expanding-entities)
   (let* ((input-port (open-input-file (string-append file "." "dtd"))))
      (set! ip input-port)

      ; defines element-list, attribute-list, and entity-list.
      (parse-dtd-ip)
      (close-input-port ip)
   )
 
   ; Reversing entity list in order to 'let the first occurrence win'.
   ; It is important to get the first entity, which is the one in effect.
   (set! entity-list (reverse entity-list))

   ; @i
   (display-message "EXPLODING ELEMENTS")
   (set! element-list 
    (explode-element-list ; @a
      (map (compose expand-element-entities bring-to-old-form) element-list)))

   (display-message "PARSING ELMENT CONTENTS:")
   ; replace rhs string with parsed representation.
   (set! element-list 
     (map parse-rhs-constituent element-list))

   (display-message "EXPLODING ATTRIBUTES:")
   ; @j
   (set! attribute-list 
     (explode-attribute-list (map expand-attribute-entities attribute-list)))

   (display-message "ADDING SUFFICIENT EMTPY ATTRIBUTE LIST DECLARATIONS")
   (add-sufficient-empty-attributes)

   (file-write
     (append (reverse element-list) (reverse attribute-list) entity-list) ; @k
      (string-append file "." "lsp"))

   ; (report-on-rhs-parsing! element-list)

   (display-message (string-append "DONE. The result is in the file " file ".lsp" " and in the variables element-list, attribute-list, and entity-list"))

  )



(load (string-append laml-dir "laml.scm"))
(lib-load "general.scm")
(lib-load "collect-skip.scm")

;;; Variables which control the parser.

;; A boolean variable which controls whether the parser should report on the progress while parsing. Default #t.
(define dtd-parse-verbose #t)

;; A boolean variable which controls whether to parse ignored marked sections anyway. Default #t.
(define forced-inclusion-of-marked-sections #f)

;;; Variables which is assigned to parser output.

;; A list of entities defined by the parser
(define entity-list '())

;; A list of elements defined by the parser
(define element-list '())

;; A list of attributes defined by the parser
(define attribute-list '())

;; A list of notations defined by the parser
(define notation-list '())

; ========================================================================================================================
; DTD specific part of parser

(define (skip-comment)
  (if dtd-parse-verbose (display-message "Skipping comment"))
  (ensure-look-ahead 4)
  (let ((res (match-look-ahead? "<!--")))
     (if res (read-a-string 4) (error "Skip comment not in front of a comment"))
     (skip-rest-of-comment)
     'comment-skip-done))

(define (skip-rest-of-comment)
  (skip-while (lambda (ch) (not (eqv? ch #\-))))
  (ensure-look-ahead 3)
  (if (match-look-ahead? "-->")
      (read-a-string 3)
      (begin 
        (read-a-char)  ; the -
        (skip-rest-of-comment))))

(define (skip-white-space)
  (skip-while is-white-space?))

; ---------------------------------------------------------------------------------------------------------------

(define (type-of-construct-ahead)
 (ensure-look-ahead 4)
  (cond ((equal? (look-ahead-prefix 4) "<!--") 'comment)
	((equal? (look-ahead-prefix 4) "<!AT") 'attribute)
	((equal? (look-ahead-prefix 4) "<!EN") 'entity)
	((equal? (look-ahead-prefix 4) "<!EL") 'element)
	((equal? (look-ahead-prefix 1) "%") 'entity-occurence)
	((equal? (look-ahead-prefix 3) "<![") 'marked-section)
        ((equal? (look-ahead-prefix 4) "<!NO") 'notation-declaration)
        ((equal? (look-ahead-prefix 2) "<?") 'processing-instruction)
	(else (error "type-of-construct-ahead: Unknown construct"))))

(define (parse-entity-occurence)
  (if dtd-parse-verbose (display-message "Parsing top level element occurence"))
  (let* ((entity-name (collect-until-string ";" #t))
         (clean-entity-name (clean-entity entity-name))
         (entity-val (lookup-entity-expansion clean-entity-name entity-list)))
   (if dtd-parse-verbose (display-message (string-append "  " entity-name)))
   (if entity-val
       (put-back-a-string entity-val 'read-end)
       (error "Encountered undefined character entity at top level"))))

; Take away the leading "%" and the trailing ";" in an entity name.
(define (clean-entity ent)
  (if (>= (string-length ent) 2)
      (substring ent 1 (- (string-length ent) 1))
      (error "clean-entity: Entity name must hold more than one character")))

(define (parse-unit)
  (skip-white-space)
  (let ((ahead (type-of-construct-ahead)))
    (cond ((eq? ahead 'comment) (skip-comment))
          ((eq? ahead 'attribute) (parse-attribute))
          ((eq? ahead 'entity) (parse-entity))
          ((eq? ahead 'element) (parse-element))
          ((eq? ahead 'entity-occurence) (parse-entity-occurence))
          ((eq? ahead 'marked-section) (parse-marked-section))
          ((eq? ahead 'notation-declaration) (parse-notation-declaration))
          ((eq? ahead 'processing-instruction) (parse-processing-instruction))
          (else (error "parse-unit: Unknown construct ahead")))))


(define (parse-notation-declaration)
  (if dtd-parse-verbose (display-message "Parsing Notation Declaration"))
  (skip-string "<!NOTATION" "<!NOTATION declaration expected")
  (skip-white-space)
  (let* ((name (collect-until is-white-space?))
         (white-1 (skip-white-space))
         (public-or-system (collect-until is-white-space?))
         (white-2 (skip-white-space))
         (literal (collect-quoted-string))
         (white-3 (skip-white-space))
        )

     (ensure-look-ahead 1)
     (cond ((equal? (look-ahead-prefix 1) ">")
             (skip-string ">" "> Expected")
             (let ((res (list 'notation name public-or-system literal)))
	       (set! notation-list (cons res notation-list))))
           ((or (equal? (look-ahead-prefix 1) "\"") (equal? (look-ahead-prefix 1) "'"))
             (let ((literal-extra (collect-quoted-string))
                   (white-4 (skip-white-space)))
               (ensure-look-ahead 1)
               (if (equal? (look-ahead-prefix 1) ">")
                   (begin
                      (skip-string ">" "'>' Expected")
                      (let ((res (list 'notation name public-or-system literal literal-extra)))
			(set! notation-list (cons res notation-list))))
                   (laml-error "During parsing of notation declaration: '>' expected"))))
          (else (laml-error "Unexpected ending of notation declaration")))

 )
)
                

(define (parse-processing-instruction)   ; Simply ignoring - reading through a processing instruction. 
  (if dtd-parse-verbose (display-message "Parsing processing instruction - ignoring it"))
  (skip-string "<?" "<? expected")
  (skip-white-space)
  (let* ((name (collect-until is-white-space?))
         (white-1 (skip-white-space))
        )
     (skip-until-string "?>" #t)))

(define (collect-quoted-string)
 (let ((kind-of-quote #f))
  (skip-white-space)
  (ensure-look-ahead 1)
  (let ((col (cond ((eqv? #\' (look-ahead-char))
                      (set! kind-of-quote 'single) (read-a-char) (collect-until-string "'"))
                   ((eqv? #\" (look-ahead-char))
                      (set! kind-of-quote 'double) (read-a-char) (collect-until-string "\""))
                   (else (laml-error "Single or double quoted string expected" (look-ahead-char))))))
    (cond ((eq? kind-of-quote 'double)
             (skip-string "\"" "String double quote expected"))
          ((eq? kind-of-quote 'single)
             (skip-string "'" "String single quote expected"))
          (else (laml-error "collect-quoted-string: should not happen")))
    col)))
      

(define (parse-marked-section)
  (if dtd-parse-verbose (display-message "Parsing marked section"))
  (skip-string "<![" "<![ expected in marked entity")
  (skip-white-space)
  (let* ((entity-name (collect-until is-white-space-or-left-bracket))
         (expanded-entity-name (expand-entities entity-name)))
    (cond ((or (equal? expanded-entity-name "INCLUDE") forced-inclusion-of-marked-sections)
             (skip-white-space)
             (skip-string "[" "[ expected in marked entity")
             (skip-white-space)
             (parse-included-marked-section) ; @j
             (skip-string "]]>" "]]> expected in marked entity")
             (if dtd-parse-verbose (string-append "  " entity-name " "
                                   (if forced-inclusion-of-marked-sections "(forced inclusion) " ""))))
          ((equal? expanded-entity-name "IGNORE")   ; skipping entire marked section.
             (skip-until-string "]]>" #t))
          (else
             (error "parse-marked-section: First word must be or expand to either IGNORE or INCLUDE"))))

  )

(define (is-white-space-or-left-bracket ch)
  (or (is-white-space? ch) (eqv? ch #\[)))

(define (parse-included-marked-section)
  (skip-white-space)
  (ensure-look-ahead 3)
  (if (not (equal? (look-ahead-prefix 3) "]]>"))
      (begin
         (parse-unit)
         (parse-included-marked-section))))

(define (parse-entity)
  (if dtd-parse-verbose (display-message "Parsing entity"))
  (skip-string "<!ENTITY" "<!ENTITY expected in entity defintion")
  (skip-white-space)

  (ensure-look-ahead 1)
  (if (equal? (look-ahead-prefix 1) "%")
      (skip-string "%" "% expected in entity definition")
      'do-nothing)

  (skip-white-space)
  (let ((name (collect-until is-white-space?))
        (white (skip-white-space)))
    (ensure-look-ahead 6)
    (cond ((equal? (look-ahead-prefix 1) (as-string #\"))  ; a string next
	     (let ((start-quote (skip-string (as-string #\") "Quote expected"))
		   (entity-definition (collect-until (char-predicate #\")))
		   (end-quote (skip-string (as-string #\") "Quote expected"))
		   (white-1 (skip-white-space)))


					; skip trailing stuff:
	       (ensure-look-ahead 2)
	       (let ((next-1 (look-ahead-prefix 1))
		     (next-2 (look-ahead-prefix 2)))
		 (cond ((equal? next-1 ">") ; done 
			(read-a-char))   
		       ((and (equal? next-1 "-") (equal? next-2 "--")) ; @a start comment
			(read-a-string 2) (skip-until-string "--" #t) (skip-until-string ">" #t))
		       (else (if dtd-parse-verbose (display-message "???!!!")) (skip-until-string ">" #t) )))

	       (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
	       (let ((res (list 'entity name entity-definition)))
		 (set! entity-list (cons res entity-list))
		 res)))
          ((equal? (look-ahead-prefix 1) (as-string #\'))  ; a single quoted string next
	     (let ((start-quote (skip-string (as-string #\') "Single quote expected"))
		   (entity-definition (collect-until (char-predicate #\')))
		   (end-quote (skip-string (as-string #\') "Single quote expected"))
		   (white-1 (skip-white-space)))


					; skip trailing stuff:
	       (ensure-look-ahead 2)
	       (let ((next-1 (look-ahead-prefix 1))
		     (next-2 (look-ahead-prefix 2)))
		 (cond ((equal? next-1 ">") ; done 
			(read-a-char))   
		       ((and (equal? next-1 "-") (equal? next-2 "--")) ; @a start comment
			(read-a-string 2) (skip-until-string "--" #t) (skip-until-string ">" #t))
		       (else (if dtd-parse-verbose (display-message "???!!!")) (skip-until-string ">" #t) )))

	       (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
	       (let ((res (list 'entity name entity-definition)))
		 (set! entity-list (cons res entity-list))
		 res)))
          ((equal? (look-ahead-prefix 6) "PUBLIC")  ; @b
             (skip-until-string ">" #t)
             (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
             (let ((res (list 'entity name "")))
   		     (set! entity-list (cons res entity-list))
		     res)))))

(define (parse-element)
  (if dtd-parse-verbose (display-message "Parsing element"))
  (skip-string "<!ELEMENT" "<!ELEMENT expected in element defintion")
  (skip-white-space)
  (let* ((name (collect-until is-white-space?))
         (white-1 (skip-white-space))
         (ensure-look-ahead 2)
         (right-hand-side-plus-comment (collect-until (char-predicate #\>))) ; @a
         (right-hand-side (purge-comment right-hand-side-plus-comment))
         (right-hand-side-1 (purge-trailing-spaces right-hand-side)) ; @b
         (comment (extract-comment right-hand-side-plus-comment))
         (rest (read-a-char)))
   (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
   (let ((res (list 'element name right-hand-side-1)))
            (set! element-list (cons res element-list))
            res)))

(define (purge-comment str)
  (let* ((start (substring-index str 0 "--"))
         (end   (substring-index str (if start (+ start 2) 0) "--")))
    (if (and start end) ; get rid of this interval
        (string-append (substring str 0 start) (substring str (+ end 2) (string-length str)))
        str)))

(define (extract-comment str)
  (let* ((start (substring-index str 0 "--"))
         (end   (substring-index str (if start (+ start 2) 0) "--")))
    (if (and start end) ; get rid of this interval
        (substring str (+ start 2) end)
        "")))

(define (purge-all-comments str)
  (let ((comment-there? (substring-index str 0 "--")))
    (if comment-there?
        (purge-all-comments (purge-comment str))
        str)))

(define (purge-trailing-spaces str)
 (let ((lgt (string-length str)))
  (if (or (eqv? #\space (string-ref str (- lgt 1)))
          (eqv? (as-char 10) (string-ref str (- lgt 1)))
          (eqv? (as-char 13) (string-ref str (- lgt 1))))
      (purge-trailing-spaces (substring str 0 (- lgt 1)))
      str)))

(define (purge-leading-spaces str)
 (let ((lgt (string-length str)))
  (if (or (eqv? #\space (string-ref str 0))
          (eqv? (as-char 10) (string-ref str 0))
          (eqv? (as-char 13) (string-ref str 0)))
      (purge-leading-spaces (substring str 1 lgt))
      str)))

; Old:
(define (parse-attribute)
  (if dtd-parse-verbose (display-message "Parsing attribute"))
  (skip-string "<!ATTLIST" "<!ATTLIST expected in attribute definition")
  (skip-white-space)
  (let* ((name (collect-until is-white-space?)) ; @a
         (white-1 (skip-white-space))
         (raw-attributes (collect-until (char-predicate #\>))) ; @b
         (raw-attributes-1 (purge-all-comments raw-attributes))
         (rest (read-a-char)))
   (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
   (let ((res (list 'attribute name raw-attributes-1)))
            (set! attribute-list (cons res attribute-list))
            res)))

(define (parse-attribute)
  (if dtd-parse-verbose (display-message "Parsing attribute"))
  (skip-string "<!ATTLIST" "<!ATTLIST expected in attribute definition")
  (skip-white-space)
  (let* ((name (collect-until (lambda (x) (or (is-white-space? x) (eqv? x #\>))))))
    (ensure-look-ahead 1)
    (if (match-look-ahead? ">")
        (let ((res (list 'attribute name "")))
           (read-a-char) ; read '>'
           (set! attribute-list (cons res attribute-list))
           res)
        (let* ((white-1 (skip-white-space))
	       (raw-attributes (collect-until (char-predicate #\>))) ; @b
	       (raw-attributes-1 (purge-all-comments raw-attributes))
 	       (rest (read-a-char)))
	  (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
	  (let ((res (list 'attribute name raw-attributes-1)))
            (set! attribute-list (cons res attribute-list))
            res)))))

; If applied on a string of the form "(x|y|z)" it returns a list ("x" "y" "z").
; If applied on a string not following this syntax, it returns the empty list.
(define (alternative-string-to-list str)
  (let* ((start (substring-index str 0 "("))
         (start1 (if start start (substring-index str 0 "|")))
         (end (substring-index str (if start1 (+ start1 1) 0) "|"))
         (end1 (if end end (substring-index str (if start1 start1 0) ")"))))
    (if (and start1 end1)
        (cons (purge-leading-spaces ; @a
                (purge-trailing-spaces
                  (substring str (+ start1 1) end1)))
              (alternative-string-to-list  ; @b
                (substring str end1 (string-length str))))
        '())))

; ---------------------------------------------------------------------------------------------------------------    

(define (parse-dtd-ip)
  (if (not end-of-file?) (skip-white-space))
  (if (not end-of-file?)
      (begin
         (parse-unit)
         (parse-dtd-ip))
      (if dtd-parse-verbose (display-message "EOF encountered")))

)

(define (expand-element-entities element)
  (if dtd-parse-verbose (display-message "expanding element "))
  (let* ((name (second element))
         (name-0 (expand-entities name))
         (name-1 (alternative-string-to-list name-0))
         (name-2 (if (null? name-1) name-0 name-1))
         (start (third element))
         (end   (fourth element))
         (rhs (fifth element))
         (comment (sixth element))
         (res (list 'element name-2 start end (expand-entities rhs) comment))
        )
    (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
    res))

(define (explode-element-list element-list)
  (accumulate-right append '() (map explode-element element-list)))

; return a list of elements created by exploding element on the second element.
; Example: (element ("a" "b") - - -) ->
;          ((element "a" - - -) (element "b" - - -))
(define (explode-element element)
  (let ((name-constituent (cadr element)))
    (cond ((and (list? name-constituent) (null? name-constituent)) '())
          ((and (list? name-constituent) (not (null? name-constituent)))
              (cons
                (list 'element (car name-constituent) (third element) (fourth element) (fifth element) (sixth element))
                (explode-element
                   (list 'element (cdr name-constituent) (third element) (fourth element) (fifth element) (sixth element)))))
          (else (list element)))))

(define (explode-attribute-list attribute-list)
  (accumulate-right append '() (map explode-attribute attribute-list)))

(define (explode-attribute attribute)
  (let ((name-constituent (cadr attribute)))
    (cond ((and (list? name-constituent) (null? name-constituent)) '())
          ((and (list? name-constituent) (not (null? name-constituent)))
              (cons
                (list 'attribute (car name-constituent) (third attribute))
                (explode-attribute
                   (list 'attribute (cdr name-constituent) (third attribute)))))
          (else (list attribute)))))
                     

(define (expand-attribute-entities attribute)
  (if dtd-parse-verbose (display-message (string-append "expanding attribute " (as-string (second attribute)))))
  (let* ((name (second attribute))
         (name-1 (expand-entities name))  
         (name-2 (alternative-string-to-list name-1)) ; @b
         (name-3 (if (null? name-2) name-1 name-2)) ; @c
         (raw-attr (trailing-cr (third attribute)))
         (res     (list 'attribute 
                        name-3
                        (parse-attribute-string (purge-all-comments (expand-entities raw-attr))))) ; @d
        )
    ; (if dtd-parse-verbose (display-message (string-append "  " (as-string name))))
    (if dtd-parse-verbose (display-message "OK"))
    res))


; str represents consequtive attributes (name, type, required/...)
; example of a single attribute:   
;  "a CDATA   #IMPLIED
;   b (x|y|z) #IMPLIED
;   c CDATA   #REQUIRED
;   d (x|y|z) "x"
;   e (x|y|z) 'x'
;   f (x|y|z) #FIXED 'x'
;   g (x|y|z) #FIXED "x"
;   h NOTATION (x) #IMPLIED
; which is parsed to the following strings resp.
;  (("a" "CDATA" "#IMPLIED")
;   ("b" ("x" "y" "z") "#IMPLIED")
;   ("c" "CDATA" "REQUIRED")
;   ("d" ("x" "y" "z") "x")
;   ("e" ("x" "y" "z") "x")
;   ("f" ("x" "y" "z") "x" "#FIXED")
;   ("g" ("x" "y" "z") "x" "#FIXED")
;   ("h" "CDATA" "#IMPLIED")   ; loosing information about notations - 

(define (parse-attribute-string str) 
 (let ((very-first (first-word str)))
  (if very-first
    (let* ((is-fixed-keyword? (lambda (x) (equal? "#FIXED" x)))
           (dequote-string 
             (lambda (str)
              (if (>= (string-length str) 2)
                  (let ((f (string-ref str 0))
                        (l (string-ref str (- (string-length str) 1))))
                   (cond ((and (eqv? f #\") (eqv? l #\")) (substring str 1 (- (string-length str) 1)))
                         ((and (eqv? f #\') (eqv? l #\')) (substring str 1 (- (string-length str) 1)))
                         (else str)))
                  str)))

           (first very-first)
           (rest-1 (but-first-word str first))
           (second (first-word rest-1))
           (notation? (equal? second "NOTATION"))
           (rest-1 (if notation? (but-first-word rest-1 second) rest-1))  ; re-binding rest-1
           (second (if notation? (first-word rest-1) second))             ; re-binding second
           (second-1 (if (eqv? #\( (string-ref second 0)) ; @k
                         (first-parenthesized-expr rest-1)
                         second))
           (second-2 (if (eqv? #\( (string-ref second 0))  ; @l
                         (alternative-string-to-list second-1)
                         second-1)) 
           (rest-2 (but-first-word rest-1 second-1))  
           (third (first-word rest-2))
           (rest-3 (but-first-word rest-2 third))
           
           (fourth (if (is-fixed-keyword? third) (first-word rest-3) #f))
           (rest-4 (if (is-fixed-keyword? third) (but-first-word rest-3 fourth) rest-3))

          )
      (cons
       (if notation?   ; loosing information about the notation. In a future version we may easily introduced a parsed repr. of notations in attributes.
           (list first "CDATA" third)
           (if fourth
               (list first second-2 (dequote-string fourth) third)
               (list first second-2 (dequote-string third))
               ))
        (parse-attribute-string rest-4)))
    '())))

(define (first-parenthesized-expr str)
  (let ((str1 (strip-initial-spaces str)))
    (string-append (collect-in-string-until (char-predicate #\)) str1) ")")))

; Return the first word of str, without initial and trailing spaces.
; Recogizes '...' and "..." in the sense that the quoted portion is considered the first word.
(define (first-word str)
  (let ((str1 (strip-initial-spaces str)))
    (cond ((= 0 (string-length str1)) #f)
          ((eqv? (string-ref str1 0) #\') (collect-in-string-until (char-predicate #\') str1 1 #t)) 
          ((eqv? (string-ref str1 0) #\") (collect-in-string-until (char-predicate #\") str1 1 #t)) 
          (else (collect-in-string-until is-white-space? str1)))))

; Collect and return a prefix of str. Collection stops when p does not hold
; on a char c. As the default, c is not included in the returned string.
; As the first optional-parameter, you can control where to start applying the predicate. Default is 0.
; In any case, alwas collect from the start of str.
; As the second optional parameter, you can ask for that the character on which p returns #t, is included.
; Notice that it is possible to have an 'index out of bounds' error, if the predicate never becomes false.
(define (collect-in-string-until p str . optional-parameter-list)
 (let ((start-index (optional-parameter 1 optional-parameter-list 0))
       (include-match (optional-parameter 2 optional-parameter-list #f))
      ) 
  (let ((idx (check-string-indexes p str start-index)))
    (if include-match
        (substring str 0 (+ idx 1))
        (substring str 0 idx)))))
  

; Return the first index i in str for which (p (string-ref str i)) holds
(define (check-string-indexes p str i)
  (if (p (string-ref str i))
      i
      (check-string-indexes p str (+ i 1))))

; Return a suffix of str excluding initial space and word
(define (but-first-word str word)
  (let* ((str1 (strip-initial-spaces str)))
    (substring str1 (string-length word) (string-length str1))))
  


; ---------------------------------------------------------------------------------------------------------------
; Entity expansion in strings

; Expands all entities - recursively - in str relative to entity-list
(define (expand-entities str)
  (let* ((entity-occurence (entity-in-string str)))
    (if entity-occurence
        (let* ((clean-entity-occurence (clean-entity entity-occurence))
               (entity-value (lookup-entity-expansion clean-entity-occurence entity-list)))
          (if entity-value 
              (if (member clean-entity-occurence non-expand-entities) ;@e
                  (expand-entities (expand-entity entity-occurence clean-entity-occurence str))
                  (expand-entities (expand-entity entity-occurence entity-value str)))
              (begin 
                 (display-message (string-append "Encountered undefined entity occurence: " entity-occurence))
                 str)))
        str)))

; Return the first entity in str. If none is found, return #f
(define (entity-in-string str)
  (let* ((start-point (substring-index str 0 "%"))
         (end-point   (substring-index str (if start-point start-point 0) ";"))
         (spaces      (if (and start-point end-point)
                          (substring-index (substring str start-point end-point) 0 " ")
                          #f)))
    (if (and start-point end-point (not spaces))
        (substring str start-point (+ end-point 1))
        #f)))

; Expands a single entity in str. If none is found, just return str
(define (expand-entity ent expansion str)
  (let* ((start-point (substring-index str 0 ent)))
    (if (and start-point expansion)
        (string-append 
           (substring str 0 start-point)
           expansion
           (substring str (+ start-point (string-length ent)) (string-length str)))
        str)))

; Return the expansion of entity-name (without % and ;) relative to expansion-list.
; Expansion list has the same format as entity-list. If no expansion can be found,
; return #f.
(define (lookup-entity-expansion entity-name expansion-list)
  (let ((res (find-in-list 
                (lambda (el) (equal? entity-name (cadr el)))
                expansion-list)))
    (if res (caddr res) #f)))
      
  
; Prepare for a new parsing, overwriting the result of the previous one  
(define (reset-dtd-parser)
  (reset-look-ahead-buffer)  

  (set! entity-list '())
  (set! element-list '())
  (set! attribute-list '()))

; --------------------------------------------------------------------------------

; bring an element from the new form to the old form.
; The two "-" elements act as dummies.
(define (bring-to-old-form new-element-list)
  (list 'element (second new-element-list) "-" "-" (third new-element-list) ""))

(define (trailing-cr x)
  (string-append x (as-string #\newline)))


; ---------------------------------------------------------------------------------------------------

; ::rhs-parsing::
;
; OLD stuff - not used any more. 
; 
; (define state-list '()) ; for debugging purposes
; (define debugging-rhs-parsing #f)

; Attempt to parse rhs-str. We only support parsing of rhs's of the form:
;   (x | y ... | z)*
;   (x | y ... | z)+
;   (x, y, ...., z)
;   (x?, y?, ..., z?)
;   mixes of (x, y, ...., z) and (x?, y?, ..., z?)
;   (#PCDATA)
;   EMPTY
; Return the list (zero-or-more "x" "y" ... "z"), (one-or-more "x" "y" ... "z"),
; (sequence-with-optionals "x" ("y") ... "z"),
; the symbol 'pcdata-checker, or
; the symbol 'empty. Return the orginal input in case we attempt to parse another rhs form.
; Return the list (zero-or-more "x" "y" ... "z"), (one-or-more "x" "y" ... "z"),
; the symbol 'pcdata-checker, or
; the symbol 'empty. Return the orginal input in case we attempt to parse another rhs form.
; (define (old-parse-rhs-string rhs-str)
;  (call-with-current-continuation
;   (lambda (exit)
;    (parse-rhs-string-1 rhs-str exit))))
; 
; (define (parse-rhs-string-1 rhs-str give-up)
;  (cond ((equal? "EMPTY" (upcase-string rhs-str)) "EMPTY")
;        ((pcdata-only? rhs-str) 'pcdata-checker)
;        ((hope-for-alternative? rhs-str) (parse-possible-alternatives rhs-str give-up))
;        ((hope-for-sequence? rhs-str) (parse-possible-sequence rhs-str give-up))
;        (else (give-up rhs-str))))
; 
; ; Does str contains a trace of alternatives?
; (define (hope-for-alternative? str)
;   (turn-into-boolean (or (find-in-string str #\| 0) (find-in-string str #\* 0) (find-in-string str #\+ 0))))
; 
; ; Does str contains a trace of a sequence.
; (define (hope-for-sequence? str)
;   #t) ; earlier: (turn-into-boolean (find-in-string str #\, 0))
; 
; 
; ; Is x of the form (#PCDATA) - with or without spaces?
; (define (pcdata-only? x)
;  (let ((x1 (transliterate x #\space "")))
;    (equal? x1 "(#PCDATA)"))) 
; 
; ; ----------------------------------------------------------
; ; Alternatives parsing
; ; OLD stuff - not used any more.
; 
; (define (parse-possible-alternatives str give-up-continuation)
;  (let* ((strlgt (string-length str))
;         )
;   (set! state-list '())
;   (parse-possible-alternatives-1 str 0 (string-length str) '() "" 'start give-up-continuation)))
; 
; (define (parse-possible-alternatives-1 instr inptr inlength outlst word current-state give-up-cont)
;   (cond ((and (eq? current-state 'finish) (>= inptr inlength)) ((alternative-operator word) (reverse outlst)))
;         ((>= inptr inlength) (give-up-cont instr))
;         (else (let* ((inch (string-ref instr inptr))
; 		     (trans-res (parse-alternative-transition current-state inch word))
; 		     (next-state (car trans-res))
; 		     (new-word (cdr trans-res))
; 		    )
; 		(if debugging-rhs-parsing
; 		    (set! state-list (cons (cons (as-string inch) next-state) state-list)))
;                 (if (eq? next-state 'error) (give-up-cont instr))
; 
; 		(parse-possible-alternatives-1 
; 		 instr (+ 1 inptr) inlength
; 		 (if (or (eq? next-state 'word-found) (eq? next-state 'word-found-outside-par) (eq? next-state 'word-found-after-bar))
; 		     (cons word outlst)
; 		   outlst)
; 		 new-word next-state give-up-cont)))))
; 
; 
; (define (alternative-operator word)
;  (lambda (lst)
;    (cond ((equal? word "star") (cons 'zero-or-more lst))
;          ((equal? word "plus") (cons 'one-or-more lst))
;          (else (laml-error "alternative-operator: Unknown word: " word)))))
; 
; ; states 
; ;  start: star state
; ;  error:  We give up
; ;  inside-par: just passed start parenthesis
; ;  inside-word: we are collecting chars in a word
; ;  word-found: located end of word
; ;  word-found-outside-par: located end of word and also passed end parenthesis
; ;  word-found-after-bar: located end of word and bar. Waiting for new word.
; ;  between-word-before-bar: waiting for par or ")" to appear
; ;  outside-par: We have passed end parenthesis
; ;  finish: We have seen trailing star or plus
; 
; (define (parse-alternative-transition in-state ch word)
;  (let ((char (as-string ch)))
;    (cond 
;          ((and (symbol? in-state) (eq? in-state 'start))
;             (cond ((memq ch white-space-char-list)                        (cons 'start ""))
;                   ((equal? "(" char)                                      (cons 'inside-par ""))
;                   (else                                                   (cons 'error ""))))
; 
; 
;          ((and (symbol? in-state) (eq? in-state 'inside-par))
;             (cond ((memq ch white-space-char-list)                        (cons 'inside-par ""))
;                   ((eq? ch #\|)                                           (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'inside-word))
;             (cond ((memq ch white-space-char-list)                        (cons 'word-found word))
;                   ((eq? ch #\|)                                           (cons 'word-found-after-bar word))
;                   ((eq? ch #\))                                           (cons 'word-found-outside-par word))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word (string-append word char)))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found))
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-bar ""))
;                   ((eq? ch #\|)                                           (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   ((eq? ch #\))                                           (cons 'outside-par ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'between-word-before-bar))
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-bar ""))
;                   ((eq? ch #\|)                                           (cons 'inside-par ""))
;                   ((eq? ch #\))                                           (cons 'outside-par ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-after-bar))
;             (cond ((memq ch white-space-char-list)                        (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'outside-par))
;             (cond ((memq ch white-space-char-list)                        (cons 'outside-par ""))
;                   ((eq? ch #\*)                                           (cons 'finish "star"))
;                   ((eq? ch #\+)                                           (cons 'finish "plus"))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-outside-par))
;             (cond ((memq ch white-space-char-list)                        (cons 'outside-par ""))
;                   ((eq? ch #\*)                                           (cons 'finish "star"))
;                   ((eq? ch #\+)                                           (cons 'finish "plus"))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'finish))
;             (cond ((memq ch white-space-char-list)                        (cons 'finish word))
;                   (else                                                   (cons 'error ""))))
; 
;          (else                                                            (laml-error "parse-alternative-transition error 1:" in-state))
; 
;   )))
; 
; ; End alternatives parsing
; ; ---------------------------------------------------------------------------------------------------
; ; Sequence parsing
; ; OLD stuff - not used any more.
; 
; (define (parse-possible-sequence str give-up-continuation)
;  (let* ((strlgt (string-length str))
;         )
;   (set! state-list '())
;   (parse-possible-sequence-1 str 0 (string-length str) '() "" 'start give-up-continuation)))
; 
; (define (parse-possible-sequence-1 instr inptr inlength outlst word current-state give-up-cont)
;   (cond ((and (or (eq? current-state 'finish) (eq? current-state 'word-found-outside-par))
;               (>= inptr inlength))
;            ((sequence-operator) (reverse outlst)))
;         ((>= inptr inlength) (give-up-cont instr))
;         (else (let* ((inch (string-ref instr inptr))
; 		     (trans-res (parse-sequence-transition current-state inch word))
; 		     (next-state (car trans-res))
; 		     (new-word (cdr trans-res))
; 		    )
; 		(if debugging-rhs-parsing
; 		    (set! state-list (cons (cons (as-string inch) next-state) state-list)))
;                 (if (eq? next-state 'error) (give-up-cont instr))
; 
; 		(parse-possible-sequence-1 
; 		 instr (+ 1 inptr) inlength
; 		 (cond ((or (eq? next-state 'word-found) (eq? next-state 'word-found-after-comma)
;                             (eq? next-state 'word-found-outside-par) (eq? next-state 'word-found-before-comma))
;    		          (cons (as-string word) outlst))
;                        ((or (eq? next-state 'word-found-after-question))
;    		          (cons (as-symbol word) outlst))
; 		       (else outlst))
; 		 new-word next-state give-up-cont)))))
; 
; 
; (define (sequence-operator)
;  (lambda (lst)
;    (cons 'sequence-with-optionals lst))) 
; 
; (define (downcase-string-or-symbol x)
;   (cond ((string? x) (downcase-string x))
;         ((symbol? x) (as-symbol (downcase-string (as-string x))))
;         (else (laml-error "downcase-string-or-symbol: Does only accept string or symbol: " x))))
; 
; ; states 
; ;  start: star state - before "("
; ;  error:  We give up
; ;  inside-par: just passed start parenthesis - or we are 'in the middle'.
; ;  inside-word: we are collecting chars in a word
; ;  word-found: located end of word
; ;  between-word-before-comma
; ;  word-found-after-comma
; ;  word-found-after-question
; ;  optional-word-found: located end of optional word
; ;  word-found-outside-par: located end of word and also passed end parenthesis
; ;  optional-word-found-outside-par: located end of optional-word and also passed end parenthesis
; 
; ;  between-word-before-comma: waiting for word or ")" to appear
; ;  finish: We have seen ")"
; 
; ; (x?, y?, ..., z?), (x, y, ..., z) or mixes
; 
; ; There is an error in this function: 
; ; An rhs with two spaces or CR before the final parenthesis causes a duplicate element in the list. 
; ; Until further, avoid extra white space before final par.
; 
; (define (parse-sequence-transition in-state ch word)
;  (let ((char (as-string ch)))
;    (cond 
;          ((and (symbol? in-state) (eq? in-state 'start))
;             (cond ((memq ch white-space-char-list)                        (cons 'start ""))
;                   ((equal? "(" char)                                      (cons 'inside-par ""))
;                   (else                                                   (cons 'error ""))))
; 
; 
;          ((and (symbol? in-state) (eq? in-state 'inside-par))
;             (cond ((memq ch white-space-char-list)                        (cons 'inside-par ""))
;                   ((eq? ch #\,)                                           (cons 'inside-par ""))   ; ???
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'inside-word))
;             (cond ((memq ch white-space-char-list)                        (cons 'word-found-before-comma word))
;                   ((eq? ch #\,)                                           (cons 'word-found-after-comma word))
;                   ((eq? ch #\?)                                           (cons 'word-found-after-question word))
;                   ((eq? ch #\))                                           (cons 'word-found-outside-par word))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word (string-append word char)))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found))             ; NEVER ENTERED
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-comma ""))
;                   ((eq? ch #\,)                                           (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   ((eq? ch #\))                                           (cons 'finish ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-before-comma))
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-comma ""))  ; Before Dec 30, 2002: word-found-before-comma
;                   ((eq? ch #\,)                                           (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'error ""))
;                   ((eq? ch #\))                                           (cons 'finish ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'between-word-before-comma))
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-comma ""))
;                   ((eq? ch #\,)                                           (cons 'inside-par ""))
;                   ((eq? ch #\))                                           (cons 'finish ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-after-comma))
;             (cond ((memq ch white-space-char-list)                        (cons 'inside-par ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-after-question))
;             (cond ((memq ch white-space-char-list)                        (cons 'between-word-before-comma ""))
;                   ((eq? ch #\,)                                           (cons 'inside-par ""))
;                   ((eq? ch #\))                                           (cons 'finish ""))
;                   ((memq ch constituent-char-list)                        (cons 'inside-word char))
;                   (else                                                   (cons 'error ""))))
; 
; 
;          ((and (symbol? in-state) (eq? in-state 'word-found-outside-par))
;             (cond ((memq ch white-space-char-list)                        (cons 'finish ""))
;                   ((eq? ch #\*)                                           (cons 'error ""))
;                   ((eq? ch #\+)                                           (cons 'error ""))
;                   (else                                                   (cons 'error ""))))
; 
;          ((and (symbol? in-state) (eq? in-state 'finish))
;             (cond ((memq ch white-space-char-list)                        (cons 'finish word))
;                   (else                                                   (cons 'error ""))))
; 
;          (else                                                            (laml-error "parse-sequence-transition error 1:" in-state))
; 
;   )))
; 
; End sequence parsing - old stuff
; ---------------------------------------------------------------------------------------------------
; 
; 
; 
; ; A list of char of which rhs symbols are formed
; (define constituent-char-list
;  (map as-char
;   (append 
;     (number-interval (as-number #\a) (as-number #\z))
;     (number-interval (as-number #\A) (as-number #\Z))
;     (number-interval (as-number #\0) (as-number #\9))
;     (list (as-number #\#) (as-number #\-)))))
;   

; 
; 
; ; Check existence of elements in parsed-rhs
; (define (check-rhs! tag possibly-parsed-rhs)
;  (if (list? possibly-parsed-rhs)
;      (check-rhs-1! tag possibly-parsed-rhs)))
; 
; ; This procedure is prepared for richer right hand sides - this is the reason behind the cond.
; (define (check-rhs-1! tag parsed-rhs)
;  (cond ((eq? (car parsed-rhs) 'sequence-with-optionals)
;           (for-each (lambda (rhs-el) (check-rhs-element! rhs-el tag)) (cdr parsed-rhs)))
;        ((eq? (car parsed-rhs) 'zero-or-more)
;           (for-each (lambda (rhs-el) (check-rhs-element! rhs-el tag)) (cdr parsed-rhs)))
;        ((eq? (car parsed-rhs) 'one-or-more)
;           (for-each (lambda (rhs-el) (check-rhs-element! rhs-el tag)) (cdr parsed-rhs)))
;        (else (laml-error "check-rhs-1: Unknown operator in parsed rhs element" (car parsed-rhs)))))
; 
; ; Does rhs-el exist in element-list.
; ; Host-el-name is a name used for error message purposes
; (define (check-rhs-element! rhs-el host-el-name)
;  (if (not (equal? rhs-el "#PCDATA"))
;    (let ((rhs-el-exists? (find-in-list (lambda (el) (equal? (as-string rhs-el) (as-string (cadr el)))) element-list)))
;      (if (not rhs-el-exists?)
; 	 (display-warning (string-append "In " (as-string host-el-name) ": " "Cannot find rhs element " (as-string rhs-el)))))))   
;   
; 
; ; ---------------------------------------------------------------------------------------------------
; 
; ; Tell how many rhs' we managed to parse
; ; Also: Omit pcdata functions.
; (define (report-on-rhs-parsing! element-list)
;  (let ((rhs fifth))
;   (let* ((non-empty-elements  (filter (lambda (e) (not (and (string? (rhs e)) (equal? "EMPTY" (rhs e))))) element-list))
;          (non-empty-non-pcdata-elements (filter (lambda (e) (not (and (symbol? (rhs e)) (eq? 'pcdata-checker (rhs e))))) non-empty-elements))
; 	 (rhs-non-parsed-elements (filter (lambda (e) (string? (rhs e))) non-empty-elements))
;         )
;     (display-message (string-append "Elements in total: " (as-string (length element-list))))
;     (display-message (string-append "Non-empty elements: " (as-string (length non-empty-elements))))
;     (display-message (string-append "Elements with non-parsed element content models: " (as-string (length rhs-non-parsed-elements))))
;     (display-message 
;       (string-append "The elements with non-parsed element content models are: " (list-to-string  (map (compose as-string second) rhs-non-parsed-elements) ", ")))  
; 
; )))
; 
; End OLD rhs stuff

; ---------------------------------------------------------------------------------------------------
; Utility functions:
;
;(define (non-trivial-element? e)
;  (and (eq? (first e) 'element)
;       (string? (fifth e))
;       (not (equal? (fifth e) "EMPTY"))))



; --------------------------------------------------------------------------------------------------------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------------------------------------------
; New RHS parsing stuff (as of march, 2003).
; The stuff in this section is the important addition relative to the earlier version of the tool.
; The full RHS content model (content specification) parser.
; RHS means right hand side, as in a context free grammar.
; A better word would be XML element content specification parser.

; replace the rhs field in element with the parsed representation - only simple cases are parsed.
; The remaining are left as string, unparsed.
(define (parse-rhs-constituent element)
 (if dtd-parse-verbose (display-message "Parsing content specification of " (as-string (second element)) ))
 (let* ((tag (second element))
        (x (third element))
        (y (fourth element))
        (rhs (fifth element))
        (parsed-rhs (parse-rhs-string rhs)) 
        (comment (sixth element))
       )
  (if dtd-parse-verbose (display-message "OK"))
  (list 'element tag x y parsed-rhs comment)
))



;; Top level rhs parser function.
(define (parse-rhs-string rhs-str)
 (call-with-current-continuation
  (lambda (exit)
   (parse-rhs-string-1 rhs-str exit))))

; Handling of EMPTY and ANY, and additional parsing
(define (parse-rhs-string-1 rhs-str give-up)
 (cond ((blank-string? rhs-str) (give-up rhs-str))
       ((equal? "EMPTY" (upcase-string rhs-str)) 'empty)
       ((equal? "ANY" (upcase-string rhs-str)) 'any)
       (else (parse-rhs-string-2 rhs-str give-up))))

; Handle both mixed content and element content. Setup text collection and skipping for rhs-string.
; Dispatches to mixed-content-parse or element-content-parse
(define (parse-rhs-string-2 rhs-str give-up)
  (reset-look-ahead-buffer)
  (set! ip rhs-str)
  (cond ((stream-has-mixed-content? rhs-str) (mixed-content-parse rhs-str))
        (else (element-content-parse rhs-str))))


; Parse non-mixed element contents, called children in the XML specification
(define (element-content-parse rhs-str)

  ; Restart the 'text collecting and skipping' administration
  (reset-look-ahead-buffer)
  (set! ip rhs-str)

  (skip-while is-white-space?)
  (ensure-look-ahead 1)

  ;; too broad, actually - does also accept a single name
  (list 'element-content (cp-parse))  
)

; Precondition: rhs-string is a mixed content string, such as ( #PCDATA ) OR (#PCDATA | x | y).
; Parse it and return parsed structure
(define (mixed-content-parse rhs-str)

  ; Restart the 'text collecting and skipping' administration
  (reset-look-ahead-buffer)
  (set! ip rhs-str)

  ; Match front end until #PCDATA 
  (skip-while is-white-space?)
  (match-look-ahead? "(") (read-a-char)
  (skip-while is-white-space?)
  (collect-until-string "#PCDATA" #t)
  (skip-while is-white-space?)
  (ensure-look-ahead 1)
  (cond ((match-look-ahead? ")")
           (list 'mixed-content 'pcdata))
        ((match-look-ahead? "|")
           (list 'mixed-content (cons 'choice (cons 'pcdata (collect-mixed-content-alternatives rhs-str)))))
        (else (laml-error "Mixed content parse error. '|' expected:" rhs-str))))


; Pre-condition: The input 'stream' is not blank.
(define (stream-has-mixed-content? rhs-str)
  (skip-while is-white-space?)
  (ensure-look-ahead 1)
  (if (match-look-ahead? "(")
      (begin
        (read-a-char)
        (skip-while is-white-space?)
        (ensure-look-ahead 7)
        (match-look-ahead? "#PCDATA")
      )
      #f))


; Bring a mixed contents form, as parsed, to a form which is compatible with an element-contents form
(define (normalize-mixed-content mc)
  (list 'star 
        (cons
         (car mc)
         (map (lambda (n) (list 'name (as-string n))) (cdr mc)))))


; Just passed bar. Assume that a string like "| sym1 | sym2 ... | symi )" or ahead
; Return ("sym1" "sym2" ... "symi").
(define (collect-mixed-content-alternatives rhs-str)
  (ensure-look-ahead 1)
  (cond ((match-look-ahead? ")") 
            '())
        ((match-look-ahead? "|")
          (read-a-char) ; bar
          (skip-while is-white-space?)
          (let ((symbol-string (collect-until (lambda (x) (or (is-white-space? x) (eqv? x #\)) (eqv? x #\|)))))
               )
            (skip-while is-white-space?)
            (cons symbol-string (collect-mixed-content-alternatives rhs-str))))))



; The recursive content particle - cp - parser. Recursive descent.
; The most serious parse work is done in this function.
(define (cp-parse) 
 (let ((kind 'seq))
  (skip-while is-white-space?)
  (ensure-look-ahead 1)
  (cond ((match-look-ahead? "(")
           (read-a-char) 
           (skip-while is-white-space?)
           (do ((parsed-cp-list '() (cons (cp-parse) parsed-cp-list))
               )
            ((match-look-ahead? ")")
              (read-a-char) 
              (let ((rep (parse-rep)))
                 (skip-while is-white-space?)
                 (cons kind (cons rep (reverse parsed-cp-list)))) ; result here
            )
              (cond ((match-look-ahead? "|")
                      (read-a-char)
                      (set! kind 'choice))
                    ((match-look-ahead? ",")
                      (read-a-char)
                      (set! kind 'seq)))
              (skip-while is-white-space?)
           ))
        (else 
          (let ((name (collect-until (lambda (x) (or (generic-eof-object? x) (is-white-space? x) (is-rhs-punctuation? x)))))
		(rep (parse-rep)))
            (if (equal? "#PCDATA" (upcase-string name))
                (laml-error "DTD parse error: Encountered '#PCDATA' in element-content construct")) 
	    (list 'name rep name))))))

; parse repetition: *, +, ?.
(define (parse-rep)
  (skip-while is-white-space?) ; not needed or wanted?
  (if (or (not (generic-at-eof?)) (not (empty-string? (max-look-ahead-prefix))))
      (begin
	(ensure-look-ahead 1)
	(cond ((match-look-ahead? "?") (read-a-char) (skip-while is-white-space?) 'optional)
	      ((match-look-ahead? "+") (read-a-char) (skip-while is-white-space?) 'one-or-more)
	      ((match-look-ahead? "*") (read-a-char) (skip-while is-white-space?) 'zero-or-more)
	      (else (skip-while is-white-space?) 'one))
	)
    'one))

; Is ch a punctuation character, as such chars appear in a rhs string.
(define (is-rhs-punctuation? ch)
  (if (eof? ch) 
      #t
      (let ((n (as-number ch)))
        (turn-into-boolean (member n '(44 40 41 63 42 43 124))))))

(define (ip-string-setup str)
 (reset-look-ahead-buffer)
 (set! ip str))

; ---------------------------------------------------------------------------------------------------------------

; Add an empty attribute to attribute-list for these element that does not
; have an explictly declared attribute.
(define (add-sufficient-empty-attributes)
  (for-each
    (lambda (el)
     (let ((el-name (cadr el)))
      (let ((at-of-el (find-in-list (lambda (at) (equal? el-name (cadr at))) attribute-list)))
        (if (not at-of-el)        ; no declared attribute found 
            (begin
              (set! attribute-list (cons (make-empty-attribute el-name) attribute-list))
              (if dtd-parse-verbose (display-message el-name))))))
    )
    element-list))

(define (make-empty-attribute at-name)
  (list 'attribute (as-string at-name) '()))
 