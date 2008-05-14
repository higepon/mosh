; The Scheme Elucidator 2 - with version support - using the XML-in-LAML compliant XHTML libraries.

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 1999-2004 Kurt Normark, normark@cs.auc.dk.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

; ---------------------------------------------------------------------------------------------------
; Message function, version, and verbosity variables

; A variable used for internal debuggig purposes.
(define the-problem #f)

(define elucidator-version "2")

;; If #t a number of messages are written on the output when processing is done.
;; If #f, nothing is written.
(define elucidator-verbose-mode #t)

(define (display-warning . messages)
  (if elucidator-verbose-mode
      (begin 
        (display (string-append (laml-aggregate-messages messages))) (newline))))

(define start-run-time (current-time))

;---------------------------------------------------------------------------------------------------
;;; Directory setup. 
;;; A number of variables and functions which gives information about relevant directories.

;; The directory in which all Scheme programs and libraries are located
(define software-base-directory laml-dir)

;; The LAML library relative to software-base-library
(define scheme-library "lib")

;; The directory in which the Scheme elucidator 2 software is located
(define elucidator-software-directory (string-append software-base-directory "styles/xml-in-laml/elucidator-2/"))

;; The directory in which the documentation source is locacted.
;; Must be assigned . Ends with a slash.
(define source-directory #f)

(define elucidator-source-destination-delta "html/")

; The fragment of a file path which distinguishes the source-directory and the HTML destination directory.
; As default, the html files are placed in a subdirectory of the source directory named 'html'.
; You can redefine this function after the elucidator software is loaded in order to provide for another html destination.
(define (relative-source-html-destination-path-fragment)
  elucidator-source-destination-delta)

;; The directory in which the generated html files are located. Depends on source-directory
(define (html-directory)
  (string-append source-directory (relative-source-html-destination-path-fragment)))

; Return the full path to the file name in the internal directory. 
; The parameter name includes a possible extension
(define (internal-file name)
 (string-append source-directory "internal/" name))

; Return the full path to the file name in the documentation source directory. 
; The parameter name includes a possible extension
(define (documentation-source-file name)
   (string-append source-directory  name))

; A variable that controls how to access CSS stylesheets in the Scheme Elucidator. The value is a symbol.
; local: access CSS stylesheets in a local stylesheets subdirectory.
; central: access CSS stylesheets in css-stylesheets/elucidator/ of laml-dir
(define ep-stylesheet-approach 'local)

(define (in-elucidator-stylesheet-directory css-file-name)
  (cond ((eq? ep-stylesheet-approach 'local) (string-append "stylesheets/" css-file-name))
        ((eq? ep-stylesheet-approach 'central) 
           (string-append (laml-dir-prefix (html-directory)) "css-stylesheets/elucidator/" css-file-name))
        (else (laml-error "in-elucidator-stylesheet-directory: Unknown value of ep-stylesheet-approach:" ep-stylesheet-approach))))


;---------------------------------------------------------------------------------------------------
; LOADING LIBRARRIES AND TOOLS:

(lib-load "xml-in-laml/xml-in-laml.scm")

; ----------------------------------------------------------------------
; ELUCIDATOR MIRROR LOADING AND ACTION PROCEDURES
(lib-load "xml-in-laml/mirrors/xhtml10-frameset-mirror.scm")

(define (elucidator-front-matters! front-matters-ast)                    
   (do-elucidator-front-matters! front-matters-ast))

(define (begin-documentation! ast) (do-begin-documentation!))            
(define (documentation-intro! ast) (do-documentation-intro! ast))
(define (documentation-section! ast) (do-documentation-section! ast))
(define (documentation-entry! ast) (do-documentation-entry! ast))
(define (documentation-from! ast) (do-documentation-from! ast))
(define (end-documentation! ast) (do-end-documentation!))                

; ---------------------------------------------------------------------


(define xhtml-frameset:frameset (xhtml10-frameset 'frameset))
(define xhtml-frameset:frame    (xhtml10-frameset 'frame))
(define xhtml-frameset:html     (xhtml10-frameset 'html))
(define xhtml-frameset:title    (xhtml10-frameset 'title))
(define xhtml-frameset:link    (xhtml10-frameset 'link))
(define xhtml-frameset:head    (xhtml10-frameset 'head))

(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")

(load (string-append elucidator-software-directory "mirror/elucidator2-mirror.scm"))

; To avoid excessive language overlap detection between XHTML transitional and frameset.
(set! xml-check-language-overlap? #f)

; Accept non-strings as attribute values in documentation text
(set-xml-accept-only-string-valued-attributes-in 'elucidator2 #f)

(lib-load "xhtml1.0-convenience.scm")

(lib-load "time.scm")
(lib-load "color.scm")
(lib-load "file-read.scm")

(load (string-append laml-dir "tools/schemedoc-extractor/schemedoc-extractor.scm"))

; (display-message "Loading elucidator software")

; Extraordinarily allow the elucidator reference elements within XHTML p and li elements.
(define (extraordinary-allow-element? element-name context-element-name)
  (and 
   (memq element-name '(strong-prog-ref weak-prog-ref  typographic-prog-ref doc-ref source-marker))
   (memq context-element-name '(p li em b kbd))))
        

; -------------------------------------------------------------------------------------------------
; R4RS/R5RS KNOWLEDGE

; Read the scheme knowledge list. Assigned in do-elucidator-front-matters!
(define scheme-syntax-procedure-list '())

; The names of Scheme procedures and syntax forms.
; A list of symbols.
; Assigned in do-elucidator-front-matters!
(define scheme-syntax-procedure-names '())

; The URL prefix to the directory with the R4RS/R5RS Scheme Report. Assigned in do-elucidator-front-matters!
(define rnrs-url-prefix #f)

; -------------------------------------------------------------------------------------------------
; SUPPORT FOR LINKING DIFFERENT ELUCIDATORS TOGETHER

; The URL of the designated elucidator home. 
; Can be used as the root of a number of related elucidators.
; Relative to the html directory. 
; This URL Has nothing to do with the elucidator home page.
; If #f, no home-url is provided for.
(define elucidator-home-url #f)

; Do we use previous or next elucidator navigation? 
(define previous-next-elucidators #f)

; The URLs of the previous and the next elucidator.
; Used in contexts where several elucidators are connected.
; These URLs are only used if previous-next-elucidators is #t.
; Relative to the html directory. 
; If #f, but previous-next-elucidators is #t, we show 'empty' previous/next icons.
(define elucidator-previous-url #f)
(define elucidator-next-url #f)

; ---------------------------------------------------------------------------------------------------
;;; Controlling the amount of processing. 
;;; There are a number of variables which control the amount of elucidator processing

; A boolean variable that controls if defined names are stored in an internal file.
(define store-defined-names? #t)

(define make-large-source-files? #t)

; Link definitions to entries in the cross reference index?
(define link-definitions-to-cross-reference-index? #t)

;; A variable which controls whether to copy image icons from the software directory to the source (documentation) directory.
(define copy-image-files? #t)

;; A variable that controls the generation of a defined name tables. 
;; One of the symbols per-letter, aggregated, or none.
;; Assigned in do-elucidator-front-matters! 
(define defined-name-index-support #f)

;; A variable that controls the generation of cross reference tables. 
;; One of the symbols per-letter, aggregated, or none.
;; Assigned in do-elucidator-front-matters! 
(define cross-reference-index-support #f)

;; A boolean variable that controls the generation of a duplication name index.
(define make-duplicated-name-index? #t)

; Is the current elucidator an internal LAML resource.
; Assigned from front-matters attribute in do-elucidator-front-matters! 
; Affects the linking to R4RS/R5RS resources.
(define is-laml-resource #f)

; The version of the scheme report to use for the actual elucidative program.
; One of the symbols r4rs, r5rs, or none.
(define the-scheme-report-version #f)


; A constant that controls the generation of links from an elucidative program to a
; SchemeDoc manual. Only used (only true - #t) in the cases where an elucidative program is 
; made from SchemeDoc (via the scheme-source-linking attribute).
(define schemedoc-back-linking? #f)

; ---------------------------------------------------------------------------------------------------
; OVERALL ELUCIDATOR SET UP, INTERNAL VARIABLES AND SET-FUNCTIONS.

; The overall documentation approach in Scheme elucidator 2:
; Either textual or laml (a symbol).
; A textual approach means that documentation is handled with the documentation-from clause.
; An LAML approach means that the documentation is given by documentation-intro, documentation-section, and documentation-entry forms.
; This variable is assigned in the action procedures do-documentation-from! and do-documentation-intro!
(define documentation-approach #f)

; How to deal with the program(/manual) menu of the Scheme Elucidator.
; Possible values: none, inline-table, separate-frame (a symbol).
(define program-menu-approach #f)  ; initialized later. 

; A boolean variable that tells whether to underline links in program files.
; Deprecated in Elucidat 2. Should be under control of CSS stylesheet.
(define underline-program-links #f)

; A boolean variable that tells whether to underline links in documentation files.
; Deprecated in Elucidat 2. Should be under control of CSS stylesheet.
(define underline-documentation-links #f)

; Determination of the default font size as applied on program pages. Either large or small (symbols).
; If the value is large, we enforce generation of small as well as large programs.
; This takes considerable more processing time.
(define default-program-font-size 'small)

; Determination of which kind of 'table of contents' to show: Either overall or detailed (symbols).
(define default-table-of-content 'overall)

; Defines how to handle comments. 
; Possible values are syntactical and lexcical.
; With syntactical comment handling the comments are turned into syntactic constituents before
; the elucidator program procesing.
; NOT USED ANYMORE.
(define comment-handling 'syntactical)

(define syntactical-comment-designator (as-string SYNTACTICAL-COMMENT-SYMBOL))  ; constant from the SchemeDoc extraction tool 

; A boolean variable which controls whether to show a sectional comment name (within ::...::) in
; the rendering of a comment
(define show-sectional-comment-name #t)

; The most syntactical recent comment level. 
(define the-comment-level #f)

; Which kind of source markers to use in documentation: one of the symbols as-text, as-colored-text, as-image.
(define source-marker-kind 'as-image)

; The character used to mark detailed places in a program, and the corresponding one character string.
(define elucidator-marker-char #\@)
(define elucidator-marker-char-string (as-string elucidator-marker-char))

; A variable that controls the handling of source markers in the source programs.
; show-all: Both documented and undocumented source markers are shown in the program.
; show-documented: Only those source markers actually mentioned in the documentation are included in the program.
; ignore: No source markers are shown in the program.
(define source-marker-handling-in-program 'show-documented)

; The character used to escape characters with special interpretation
(define elucidator-escape-char #\\)
(define elucidator-escape-char-string (as-string elucidator-escape-char))


; Global variables holding title, author, affiliation, email, and abstract information.
(define documentation-filename-without-extension #f)
(define documentation-title #f)
(define documentation-author #f)
(define documentation-email #f)
(define documentation-affiliation #f)
(define documentation-abstract #f)

; A list of program sources of this program bundle.
; An element of this variable is an association list of key, file-location, version, and language pairs.
(define program-source-list '())

; A map (association list) of source program keys to the highest version number wrt. the source key.
(define program-source-key-version-map '())

; Return the highest version number dealt with in respect to source-key.
; Return #f if the highest version number cannot be located.
(define (highest-version-number source-key)
 (let ((res (assoc source-key program-source-key-version-map)))
   (if res 
       (cdr res)
       #f)))
  

; Is key a Scheme source file key?
(define (source-file-qualification? key)
  (find-in-list
    (lambda (source-list-entry)
      (equal? (as-string key) (car (get 'key source-list-entry))))
    program-source-list))



; A list of manual sources of this elucidation batch.
; Contributions to this list are made by the manual-source procedure
(define manual-source-list '())

(define (find-manual-source-list-entry manual-source-list key)
  (find-in-list
    (lambda (manual-entry)
      (equal? (as-string key) (car (get 'key manual-entry))))
    manual-source-list))

; Is key a manual key?
(define (manual-file-qualification? key)
  (find-manual-source-list-entry manual-source-list key))


; A list of list parsed source forms from all new version source files in this documentation batch.
(define source-list-list-process '())

; A list of list parsed source forms from all older version source files in this documentation batch.
(define source-list-list-old-versions-process '())

; A list of all source keys of this documentation batch
(define source-key-list '())

; --------------------------------------------------------------------------------------------------------------------

; A list of all defining name occurrences - defined name entries - of all source files in this documentation batch.
; Each element is a list of the form (name source-key version). In earlier version: (name . source-key).
; The meaning is that name is defined in a file with the source key, and of version version.
; name is a symbol, source-key is a string, and version is an integer.
(define defining-name-occurences '())

; Constructor and selectors of defined name entries:
(define (make-defined-name-entry name source-key version) (list (as-symbol name) (as-string source-key) (as-number version)))
(define defined-name-of (make-selector-function 1 "defined-name-of"))
(define source-key-of (make-selector-function 2 "source-key-of"))
(define version-of (make-selector-function 3 "version-of"))  

; Comparison on entries in a defining name entry (elements of defining-name-occurences).
; Compare the name constituent of x and y. 
(define (name-entry-leq? x y)
  (string<=?  (as-string (defined-name-of x))  (as-string (defined-name-of y))))


; --------------------------------------------------------------------------------------------------------------------

; A list of documentation source marker relations. 
; A list of tripples:  (program-id doc-id source-mark)
(define documentation-source-marker-occurences '())

; A list of name pairs of the form (applied-name . defined-name)
; The meaning is that the applied-name is used in a form: (define (defined-name...) ...).
(define defined-applied-names '())


; A list of (program-name doc-id weak/strong version) tuples - documented name entries.
; The first three of these constituents are symbols. The last an integer or boolean.
; This list represents the relation between the documentation sections/entries in which certain program definitions are
; explained/mentioned either strongly or weakly.
(define documented-name-occurences '())

; Constructors and selectors. 
; prog-name is a name of a definition i a source file. A symbol.
; doc-id is the identification of the (sub)section in the documentation, where the anchored link occurs. A symbol.
; kind is one of symbols weak or strong (meaning either a weak or a strong reference from doc to prog). A symbol.
; If version is a number, we have explicitly asked for documentation of a given version of a definition. If it is #f, we have not. 
(define (make-documented-name-entry prog-name doc-id kind version)
  (list (as-symbol prog-name) (as-symbol doc-id) (as-symbol kind)
        (cond ((boolean? version) version)
              ((number? version) version)
              ((and (string? version) (numeric-string? version)) (as-number version)) 
              (else (laml-error "make-documented-name-entry: version should be boolean, numeric string, or numeric (integral)" version)))))

(define name-of-documented-name-entry (make-selector-function 1 "name-of-documented-name-entry"))
(define doc-id-of-documented-name-entry (make-selector-function 2 "doc-id-of-documented-name-entry"))
(define doc-kind-of-documented-name-entry (make-selector-function 3 "doc-kind-of-documented-name-entry"))
(define version-of-documented-name-entry (make-selector-function 4 "version-of-documented-name-entry"))



; An alist which relates documentation-id to titles of sections and entries
; The documentation-ids are symbols.
(define documentation-key-title-alist '())

; An alist which relates documentation-id to the hierarcical numbers of sections and entries
; The documentation-ids are symbols.
(define documentation-key-numbering-alist '())

; A list of documentation elements, either sections or entries, kind-taged with 'section or 'entry resp.
(define documentation-elements '())


; An association list of names a full path manual (html) file names.
; Used to locate the relevant manual entry of a given name.
(define manual-name-file-map '())


;; The width (in pixels) of the browser
(define browser-pixel-width 1100)

;; The height of the top control frame in pixels
(define control-frame-pixel-height 130)

; The number of columns in the detailed table of contents
(define toc-columns-detail 3)

; The number of columns in the overall table of contents
(define toc-columns-overall 3)

;; The prefix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(define p-link-prefix-char "{")  

;; The suffix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(define p-link-suffix-char "}")


;; The prefix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(define d-link-prefix-char "[")  

;; The suffix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(define d-link-suffix-char "]")

;; Controls whether to present the identification of sections and entries, hidden using the background color.
(define present-hidden-ids? #f)

;; The character which defines strong linking from documentation to program. Is supposed to follow the p-link-prefix-char.
(define strong-link-char #\*)

;; The character which defines weak linking from documentation to program. Is supposed to follow the p-link-prefix-char.
;; As a convention, a link is also a weak link if there is no particular link-type modifier after the p-link-prefix-char.
(define weak-link-char #\+)

;; The character which defines a program word documentation to program. Is supposed to follow the p-link-prefix-char.
;; A program word is not linked, by type set in kbd font.
(define none-link-char #\-)

;; The default kind of linking in case a link from the documentation to program does not start with
;; either strong-link-char, weak-link-char, or none-link-char.
;; The value must be one of the symbols strong, weak, or none.
(define default-program-link 'weak)


;; The frame to use for manual entries refered from the documentation.
;; A string. Either "program-frame", "documentation-frame", or another string.
(define manual-frame-from-documentation "program-frame" )

;; The frame to use for manual entries and RnRS Scheme manual information refered from the program.
;; A string. Either "program-frame", "documentation-frame", or another string.
;; Can also be boolean false (#f) in which case no specific target is used. 
;; I.e., the manual information is shown in window that holds the anchor of the link that leads to the manual.
(define manual-frame-from-program "documentation-frame")

; An enumeration of all elucidator icons. These icons are copied from the images directory of the software-directory
; to the images directory of the source-directory.
(define elucidator-image-files 
  (list "cross-index.gif" "doc-left.gif" "doc-left-weak.gif"   "doc-left-point.gif" "doc-left-weak-point.gif"
         "index.gif" 
        "question-left-arrow.gif" "question-right-arrow.gif" "small-square.gif" "three-frames-horizontal.gif"
        "three-frames.gif" "home.gif"
        "contents.gif" "overall-contents.gif"
        "xx.gif" "small-green-up-triangle.gif"

        "source-mark-black.gif" "source-mark-grey.gif" "source-mark-silver.gif"
        "source-mark-maroon.gif" "source-mark-red.gif" "source-mark-purple.gif"
        "source-mark-green.gif" "source-mark-lime.gif" "source-mark-olive.gif"
        "source-mark-yellow.gif" "source-mark-navy.gif" "source-mark-blue.gif"
        "source-mark-tetal.gif" "source-mark-aqua.gif" "source-mark-fuchsia.gif"   
 
        "small-up.gif" "small-up-blind.gif"    "small-next.gif" "small-next-blind.gif"
        "small-prev.gif" "small-prev-blind.gif"

        "nav-left.gif" "nav-right.gif"  "nav-left-empty.gif" "nav-right-empty.gif"

        "laml-mini-icon-1.gif"
        "old-version-45.gif"
        "gray-left-arrow.gif" "gray-right-arrow.gif" "gray-left-arrow-large.gif" "gray-left-arrow-large-blank.gif"
        "gray-right-arrow-large.gif"
        "no-pass-sign.gif" "new.gif"  "updated.gif" "renamed.gif" "moved.gif"
        "vers-m-n.gif" "vers-n-n.gif" "vers-1-3.gif" "vers-2-3.gif" "vers-3-3.gif" "vers-2-2.gif" "vers-1-2.gif"
        "16-16-ep.ico"
))





;; A boolean variable that controls the initial content of the program frame.
;; If true - #t - no initial program is shown in the program frame.
;; If false - #f - the first program in the program source list is shown in the program frame.
;; Instead, a brief informative text is shown.
(define blank-initial-program? #f)

; The color scheme of the program windows. 
; An alist mapping group strings to colors, or #f in case
; no color scheme is defined. Redefine in LAML setup file.
(define elucidator-color-scheme #f)

;; The number of empty lines in the bottom of an html file,
;; in order to allow navigation to bottom stuf
(define end-file-empty-lines 25)



; Return the interal anchor name of id.
; id can be a string or symbol.
(define (internal-reference id)
  (string-append (as-string id)))

; Boolean constants that control the checking of documentation source markers. 
(define warn-if-missing-source-marker-in-documentation #f)
(define warn-if-ambiguous-source-markers-in-documentation #t)

; ---------------------------------------------------------------------------------------------------
; Color settings:
; Many colors are really determined by the CSS stylesheets. Thus, the
; color constants do not necssarily have effect any more. 

;; apply black and white coloring if you make hard copies for black and white proceedings or articles
(define black-and-white-coloring #f) 

(define defined-color (make-color 255 0 0))
; (define comment-color (make-color 0 100 0)) 
(define comment-color (make-color 112 168 0)) 
(define applied-color (make-color 0 0 128)) 
(define documentation-section-color (make-color 0 204 255))
(define documentation-entry-color (make-color 0 204 255))
(define documentation-program-link-color red)
(define documentation-program-link-color-weak applied-color)
(define documentation-documentation-link-color blue)
(define none-reference-color (make-color 70 70 70))
(define rnrs-scheme-color brown)
(define manual-name-color (make-color 0 90 0)) ; dark green

(define default-background-color white)

(define documentation-background-color (make-color 255 236 217))

(define program-background-color-1     white)                     ; white
(define program-background-color-2     (make-color 221 255 221))  ; very light green
(define program-background-color-3     (make-color 198 226 255))  ; very light blue  (make-color 222 222 239)
(define program-background-color-4     (make-color 255 230 230))  ; very light red
(define program-background-color-5     (make-color 226 226 199))  ; very light brown
(define program-background-color-6     (make-color 255 255 193))  ; very light yellow
(define program-background-color-7     (make-color 224 224 224))  ; very light grey
(define program-background-color-8     (make-color 255 210 255))  ; very light purple
(define program-background-color-9     (make-color 155 255 255))  ; another very light blue
(define program-background-color-10    (make-color 255 214 193))  ; very light orange

;; Re-assigns the colors in order to provide for good printing in black and white
(define (apply-black-and-white-hardcopy-colors!)
  (set! defined-color black)
  (set! comment-color black) 
  (set! applied-color black) 
  (set! documentation-section-color (make-color 0 204 255))
  (set! documentation-entry-color (make-color 0 204 255))
  (set! documentation-program-link-color black)
  (set! documentation-program-link-color-weak black)
  (set! documentation-documentation-link-color black)
  (set! none-reference-color black)
  (set! underline-program-links #f)
  (set! underline-documentation-links #t)
  (set! rnrs-scheme-color black)
)

(if black-and-white-coloring (apply-black-and-white-hardcopy-colors!))

; ---------------------------------------------------------------------------------------------------
; CSS. Both assigned with attribute values in the action procedure do-elucidator-front-matters!

; Proper name of the CSS documentation stylesheet. 
(define the-css-documentation-stylesheet #f)

; Proper name of the CSS program stylesheet. 
(define the-css-program-stylesheet #f)

; ---------------------------------------------------------------------------------------------------
; File name handling

; Return the proper name (without path and without extension) of a program source file, described by source-descriptor.
(define (proper-source-file-with-syntactic-comment source-descriptor)
  (string-append (get-value 'key source-descriptor) "-" (as-string (get-value 'version source-descriptor))))

; Return the proper name (without path and without extension) of a HTML program source file.
; The file is characterized by source-key, source-version and size (the symbol large or small).
(define (source-file-name-html-file source-key source-version size)
  (string-append source-key "-" (as-string source-version) (if (eq? size 'large) "-LARGE" "")))

; The name of Elucidator Error page.
(define error-page-name "error-page")

; ---------------------------------------------------------------------------------------------------

; make an absolute path out of path
(define abs-path   
 (lambda (path)
   (if (absolute-file-path? path) path (in-startup-directory path))))

; Dig out program-source and version-group clauses, and return a flat list of entries to the program-source-list.
(define (find-program-source-asts source-files-ast)
  (let* ((source-asts (filter ast? (ast-subtrees source-files-ast))) ; a list of program-source, version-group, and manual-source clauses
         (prog-vers-source-ast   ; only program-source and version-group ASTs.
          (filter 
            (lambda (ast) (or (ast-of-type? 'element-name "program-source") (ast-of-type? 'element-name "version-group")))
            source-asts))
        )
     (prog-vers-transform prog-vers-source-ast)))

; Process recursively program-source and version-group clauses as encountered in a source-files clause.
; It is important to preserve the order or program-source and version-group clauses.
(define (prog-vers-transform ast-list)
  (cond ((null? ast-list) '())
        (((ast-of-type? 'element-name "program-source") (car ast-list))
           (cons 
             (program-source-transform (car ast-list) starting-version)
             (prog-vers-transform (cdr ast-list))))
        (((ast-of-type? 'element-name "version-group") (car ast-list))
           (append
              (reverse (version-group-transform (car ast-list)))  ; reverse: To enforce new = high version entries first
              (prog-vers-transform (cdr ast-list))))
        (((ast-of-type? 'element-name "manual-source") (car ast-list))    ; drop manual-source entries in this context
           (prog-vers-transform (cdr ast-list)))
        (else
           (begin 
             (set! the-problem (car ast-list)) 
             (laml-error "prog-vers-source: Met unknown source-file constituent.")))))

; Return an association list of source keys and the highest version number wrt that source key.
; The returned value is intended to be assigned to the global variable program-source-key-version-map.
(define (count-versions-in source-files-ast)
 (letrec ((number-of-program-sources 
            (lambda (version-group-ast) 
              (length (find-asts version-group-ast "program-source")))))
  (let* ((source-asts (filter ast? (ast-subtrees source-files-ast))) ; a list of program-source, version-group, and manual-source clauses
         (program-source-asts (filter (ast-of-type? 'element-name "program-source") source-asts))
         (version-group-asts (filter (ast-of-type? 'element-name "version-group") source-asts))
        )
    (append
      (map (lambda (psa) (cons (ast-attribute psa 'key) 1)) program-source-asts)
      (map (lambda (vga) (cons (ast-attribute vga 'key) (number-of-program-sources vga))) version-group-asts)))))         



; Transform program-source-ast to an entry of the program-source-list. 
; vers is the version number, where 0 is the newest number.
(define (program-source-transform program-source-ast vers)
 (let ((key (ast-attribute program-source-ast 'key))
       (group (ast-attribute program-source-ast 'group "program")) ; new default group 
       (file-path (ast-attribute program-source-ast 'file-path))
       (language (ast-attribute program-source-ast 'language "scheme"))
       (process? (as-boolean (ast-attribute program-source-ast 'process "true")))
      )
   (list (list 'key key) (list 'file-location (abs-path file-path)) (list 'language language)
         (list 'group group) (list 'version vers) (list 'process process?)
         (list 'friendly-name (ast-text program-source-ast))
   )))



; Transform version-group-ast to a list of entries, which can be added to program-source-list.
(define (version-group-transform version-group-ast)
  (let* ((version-group-key-attr (ast-attribute version-group-ast 'key))
         (version-group-group-attr (ast-attribute version-group-ast 'group "program"))
         (program-source-asts (find-asts version-group-ast "program-source")))
    (map (lambda (program-source-ast vers)
           (let ((group (ast-attribute program-source-ast 'group "program")) ; new default group 
                 (file-path (ast-attribute program-source-ast 'file-path))
                 (language (ast-attribute program-source-ast 'language "scheme"))
                 (process? (as-boolean (ast-attribute program-source-ast 'process "true")))
                )
             (list (list 'key version-group-key-attr) (list 'file-location (abs-path file-path)) (list 'language language)
                   (list 'group version-group-group-attr) (list 'version vers) (list 'process process?)
                   (list 'friendly-name (ast-text program-source-ast))
             )
         ))
         program-source-asts
         (number-interval 1 (length program-source-asts)) )))
       

; Assign the global variables documentation-title, documentation-author, documentation-email, documentation-affiliation,
; and documentation-abstract. All of the, apart from documentation-abstract, are textual. 
; The variable documentation-abstract holds an AST.
(define (do-documentation-intro! intro-ast)
  (let* 
     ((doc-title-text (find-first-ast intro-ast "doc-title" ast-text))
      (doc-author-text (find-first-ast intro-ast "doc-author" ast-text))
      (doc-email-text (find-first-ast intro-ast "doc-email" ast-text))
      (doc-affiliation-text (find-first-ast intro-ast "doc-affiliation" ast-text))
      (doc-abstract-ast (find-first-ast intro-ast "doc-abstract"))
     )
  (set! documentation-approach 'laml) 

  (set! documentation-title doc-title-text)
  (set! documentation-author doc-author-text)
  (set! documentation-email doc-email-text)
  (set! documentation-affiliation doc-affiliation-text)
  (set! documentation-abstract doc-abstract-ast)
 )
)

; This is the action procedure of LAML form documentation-section.
; Collects documentation section information in the global variables documentation-elements,
; documentation-key-title-alist, and documentation-key-numbering-alist. 
; This procedure does also affect the section numbering variables.
(define (do-documentation-section! documentation-section-ast)
 (set! section-number (+ section-number 1))
 (set! subsection-number 0)
 (let ((id (ast-attribute documentation-section-ast 'id))
       (program-version (ast-attribute documentation-section-ast 'program-version #f))
       (title (find-first-ast documentation-section-ast "section-title" ast-text))
       (section-body-ast (find-first-ast documentation-section-ast "section-body"))
       (numbering (section-numbering))
       (raw-numbering (list section-number subsection-number)) ; always 0 as subsection-nuber
      )
  (check-that-id-is-unique! id)
  (set! documentation-elements
        (cons
          (make-associations
                     (list 'kind      'numbering 'raw-numbering 'id 'doc-title 'body-ast  'program-version)
                     (list 'section   numbering   raw-numbering  id  title  section-body-ast program-version)
                  )
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons (as-symbol id) title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons (as-symbol id) numbering) documentation-key-numbering-alist))
 )
)

; This is the action procedure of LAML form documentation-entry.
; Collects documentation entry information in the global variables documentation-elements,
; documentation-key-title-alist, and documentation-key-numbering-alist. 
; This procedure does also affect the section subnumbering variable.
(define (do-documentation-entry! documentation-entry-ast)
 (set! subsection-number (+ subsection-number 1))
 (let ((id (ast-attribute documentation-entry-ast 'id))
       (program-version (ast-attribute documentation-entry-ast 'program-version #f))
       (title (find-first-ast documentation-entry-ast "entry-title" ast-text))
       (entry-body (find-first-ast documentation-entry-ast "entry-body"))
       (numbering (subsection-numbering))
       (raw-numbering (list section-number subsection-number))
      )
  (set! documentation-elements
        (cons
          (make-associations
                     (list 'kind    'numbering   'raw-numbering 'id 'doc-title 'body-ast 'program-version)
                     (list 'entry    numbering   raw-numbering   id  title entry-body     program-version)
                  )
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons (as-symbol id) title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons (as-symbol id) numbering) documentation-key-numbering-alist))
 )
)


; ---------------------------------------------------------------------------------------------------
; Textual counterparts of do-documentation-entry! and do-documentation-section!
; These procedures are called via eval by define-unit!

; Define a textual-documentation-section. 
; Internally, this function collect information about a documentation section
(define (process-textual-documentation-section elements)
 (set! section-number (+ section-number 1))
 (set! subsection-number 0)
 (let ((id (get-value 'id elements))
       (title (get-value 'doc-title elements))
       (numbering (section-numbering))
       (raw-numbering (list section-number subsection-number)) ; always 0 as subsection-nuber
      )
  (set! documentation-elements
        (cons
          (append (make-associations
                     (list 'kind    'numbering 'raw-numbering)
                     (list 'section numbering   raw-numbering))
                  elements)
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons id title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons id numbering) documentation-key-numbering-alist))
 ))

; Define a textual documentation entry.
; Internally, this function collects information about a documentation entry.
(define (process-textual-documentation-entry elements)
 (set! subsection-number (+ subsection-number 1))
 (let ((id (get-value 'id elements))
       (title (get-value 'doc-title elements))
       (numbering (subsection-numbering))
       (raw-numbering (list section-number subsection-number))
      )
  (set! documentation-elements
        (cons
          (append (make-associations
                     (list 'kind    'numbering   'raw-numbering)
                     (list 'entry    numbering   raw-numbering))
                  elements)
          documentation-elements))
  (set! documentation-key-title-alist
        (cons (cons id title) documentation-key-title-alist))
  (set! documentation-key-numbering-alist
        (cons (cons id numbering) documentation-key-numbering-alist))
)) 


; ---------------------------------------------------------------------------------------------------

; Make an a list, associating with list (not cons).
(define (make-associations keys values)
  (pair-up keys (map list values)))

;; Begin the documentation part. This ends the preamble section.
(define (do-begin-documentation!)

  ; only make html directory in the simple case where it resides as a subdirectory in the source-directory
  (if (and (equal? "html/" (relative-source-html-destination-path-fragment))
           (not (directory-exists? (string-append source-directory "html/"))))
      (make-directory-in-directory source-directory "html"))

  (ensure-directory-existence! (string-append source-directory (relative-source-html-destination-path-fragment)) "images")

  (ensure-directory-existence! source-directory "internal")
)

;;; Handling of the elucidator 2 front matters - XML-in-LAML.

(define (do-elucidator-front-matters! front-matters-ast)

 (reset-schemedoc-extractor!)

 (if elucidator-verbose-mode 
     (begin
       (display-message "Welcome to the Scheme Elucidator 2 with the XHTML backend." )
       (display-message "Elucidating" (source-filename-without-extension))
       (display-message "Copyright (c) Kurt Normark (normark@cs.auc.dk), Aalborg University, Denmark")
       (display-message "Loading libraries and the schemeDoc tool")))

 (set! source-directory (startup-directory))
 (letrec
   (
     (manual-source-transform
      (lambda (manual-source-ast)
        (let* ((key (ast-attribute manual-source-ast 'key "no-key")) ; !!!
               (file-path (ast-attribute manual-source-ast 'file-path))
               (url (ast-attribute manual-source-ast 'url))
               (manual-source-destination-delta (get-manual-source-destination-delta file-path source-directory))
               (url-derived 
                  (cond ((absolute-file-path? file-path) (string-append "file://" file-path manual-source-destination-delta))  ; MUST be relative. So illegal
                        (else (string-append (inverse-return-path elucidator-source-destination-delta source-directory) (file-name-initial-path file-path) 
                                             manual-source-destination-delta (file-name-proper file-path) "." "html"))))
              )
         (list (list 'key key) (list 'file-location (abs-path file-path)) (list 'url-location url-derived)
               (list 'friendly-name (ast-text manual-source-ast))
         ))))
   )
  (let*
    ((laml-resource-attr (as-boolean (ast-attribute front-matters-ast 'laml-resource "false")))
     (table-of-contents-attr (as-symbol (ast-attribute front-matters-ast 'table-of-contents "detailed")))
     (shallow-table-of-contents-columns-attr (as-number (ast-attribute front-matters-ast 'shallow-table-of-contents-columns 3)))
     (detailed-table-of-contents-columns-attr (as-number (ast-attribute front-matters-ast 'detailed-table-of-contents-columns 2)))
     (source-marker-presentation-attr (as-symbol (ast-attribute front-matters-ast 'source-marker-presentation "image")))
     (source-marker-char-attr (as-char (ast-attribute front-matters-ast 'source-marker-char "@")))
     (source-markers-in-program-attr (as-symbol (ast-attribute front-matters-ast 'source-markers-in-program "show-documented")))
     (browser-pixel-width-attr (as-number (ast-attribute front-matters-ast 'browser-pixel-width 1100)))
     (control-frame-pixel-height-attr (as-number (ast-attribute front-matters-ast 'control-frame-pixel-height 130)))
     (home-url-attr (ast-attribute front-matters-ast 'home-url #f))
     (next-url-attr (ast-attribute front-matters-ast 'next-url #f))
     (previous-url-attr (ast-attribute front-matters-ast 'previous-url #f))
     (scheme-report-version-attr (as-symbol (ast-attribute front-matters-ast 'scheme-report-version "r5rs")))
     (rnrs-url-attr (if (or (eq? 'r4rs scheme-report-version-attr) (eq? 'r5rs scheme-report-version-attr))
                        (ast-attribute front-matters-ast 'rnrs-url 
                           (string-append "http://www.cs.aau.dk/~normark/scheme/distribution/laml/"
                                          (as-string scheme-report-version-attr)"/"))
                        #f))
     (cross-reference-index-attr (as-symbol (ast-attribute front-matters-ast 'cross-reference-index "per-letter")))
     (defined-name-index-attr (as-symbol (ast-attribute front-matters-ast 'defined-name-index "per-letter")))
     (duplicated-name-index-attr (as-boolean (ast-attribute front-matters-ast 'duplicated-name-index "true")))
     (initial-program-frame-attr (as-symbol (ast-attribute front-matters-ast 'initial-program-frame "first-source-file")))
     (large-font-source-file-attr (as-boolean (ast-attribute front-matters-ast 'large-font-source-file "false")))
     (small-font-source-file-attr (as-boolean (ast-attribute front-matters-ast 'small-font-source-file "true")))
     (default-source-file-font-size-attr (as-symbol (ast-attribute front-matters-ast 'default-source-file-font-size "small")))
     (program-menu-attr (as-symbol (ast-attribute front-matters-ast 'program-menu "separate-frame")))
     (manual-frame-from-program-attr (ast-attribute front-matters-ast 'manual-frame-from-program "documentation-frame"))
     (manual-frame-from-documentation-attr (ast-attribute front-matters-ast 'manual-frame-from-documentation "program-frame"))
     (documentation-escape-char-attr (as-char (ast-attribute front-matters-ast 'documentation-escape-char "\\")))
     (program-link-prefix-char-attr (as-string (ast-attribute front-matters-ast 'program-link-prefix-char "{")))
     (program-link-suffix-char-attr (as-string (ast-attribute front-matters-ast 'program-link-suffix-char "}")))
     (documentation-link-prefix-char-attr (as-string (ast-attribute front-matters-ast 'documentation-link-prefix-char "[")))
     (documentation-link-suffix-char-attr (as-string (ast-attribute front-matters-ast 'documentation-link-suffix-char "]")))
     (strong-link-char-attr (as-char (ast-attribute front-matters-ast 'strong-link-char "*")))
     (weak-link-char-attr (as-char (ast-attribute front-matters-ast 'weak-link-char "+")))
     (none-link-char-attr (as-char (ast-attribute front-matters-ast 'none-link-char "-")))
     (default-program-link-attr (as-symbol (ast-attribute front-matters-ast 'default-program-link "weak")))
     (author-mode-attr (as-boolean (ast-attribute front-matters-ast 'author-mode "false")))
     (processing-mode-attr (as-symbol (ast-attribute front-matters-ast 'processing-mode "verbose")))
     (source-destination-delta-attr (ast-attribute front-matters-ast 'source-destination-delta "html/"))
     (css-documentation-stylesheet-attr (ast-attribute front-matters-ast 'css-documentation-stylesheet "documentation"))
     (css-program-stylesheet-attr (ast-attribute front-matters-ast 'css-program-stylesheet "program"))

     (warn-if-no-doc-source-marker-attr (as-boolean (ast-attribute front-matters-ast 'warn-if-no-doc-source-marker "false")))
     (warn-if-multiple-doc-source-markers-attr (as-boolean (ast-attribute front-matters-ast 'warn-if-multiple-doc-source-markers "true")))

     (color-scheme-ast (find-first-ast front-matters-ast "color-scheme"))
     (source-files-ast (find-first-ast front-matters-ast "source-files"))
     (program-source-contribution (find-program-source-asts source-files-ast))  ; specialized ast finding
     (manual-source-contribution (find-asts source-files-ast "manual-source"))
   )

  (set! scheme-syntax-procedure-list (if (or (eq? 'r4rs scheme-report-version-attr) (eq? 'r5rs scheme-report-version-attr))
                                         (read-scheme-knowledge scheme-report-version-attr)
                                         '()))

  (set! scheme-syntax-procedure-names (map first scheme-syntax-procedure-list))

  ; Color Scheme:
  (letrec
        ((make-rgb-color-from-color-ast
           (lambda (color-ast)
             (let ((el-name (ast-element-name color-ast)))
             (cond ((equal? el-name "new-rgb-color")
		      (list (as-number (ast-attribute color-ast 'red)) (as-number (ast-attribute color-ast 'green)) (as-number (ast-attribute color-ast 'blue))))
                   ((equal? el-name "predefined-color")
                      (eval-cur-env (as-symbol (ast-text color-ast))))
                   (else (laml-error "Unknown color in color scheme"))))))
                   
        (transform-color-entry   ; return cons pair of group string and rgb-color
           (lambda (color-entry-ast) 
             (let* ((group-attr (ast-attribute color-entry-ast 'group))
                    (color-ast (car (ast-subtrees color-entry-ast)))) ; either a new-rgb-color or a predefined color
                (cons group-attr (make-rgb-color-from-color-ast color-ast))))))
   (set! elucidator-color-scheme 
	 (if color-scheme-ast
	     (find-asts color-scheme-ast "color-entry" transform-color-entry)
	     #f)))

  (set! is-laml-resource laml-resource-attr)
  (set! the-scheme-report-version scheme-report-version-attr)

  (set! elucidator-source-destination-delta source-destination-delta-attr)

  ; Program and manual source lists:
  (set! program-source-list program-source-contribution)
  (set! program-source-key-version-map (count-versions-in source-files-ast))

  (set! manual-source-list 
     (filter  ; only those contributions which are present in the current LAML distribution
              ; Rationale: The slim version does not include the LAML manuals.
       (lambda (mse)
         (let ((file-loc (get-value 'file-location mse)))
          (file-exists?
            (string-append (file-name-initial-path file-loc) (file-name-proper file-loc) ".manlsp"))))
       (map manual-source-transform manual-source-contribution)))

  ; Attribute settings:    
  (set! default-table-of-content 
    (case table-of-contents-attr
      ((detailed) 'detailed)
      ((shallow)  'overall)
      (else (laml-error "elucidator-front-matters: Unexpected value of table-of-contents attribute:" table-of-contents-attr))))

  (set! toc-columns-detail detailed-table-of-contents-columns-attr)
  (set! toc-columns-overall shallow-table-of-contents-columns-attr)  

  (set! source-marker-kind 
     (case source-marker-presentation-attr
        ((image) 'as-image)
        ((text)  'as-text)
        ((colored-text) 'as-colored-text)))
  (set! elucidator-marker-char source-marker-char-attr)
  (set! elucidator-marker-char-string (as-string elucidator-marker-char))
  (set! source-marker-handling-in-program source-markers-in-program-attr)

  (set! browser-pixel-width browser-pixel-width-attr)
  (set! control-frame-pixel-height control-frame-pixel-height-attr)

  (set! elucidator-home-url home-url-attr)
  (set! elucidator-previous-url previous-url-attr)
  (set! elucidator-next-url next-url-attr)
  (set! previous-next-elucidators (or elucidator-previous-url elucidator-next-url))

  (set! rnrs-url-prefix
        (cond ((and laml-resource-attr 
                    (or (equal? "development" (laml-version-kind)) (equal? "full" (laml-version-kind)))) 
                (string-append 
		 (laml-dir-prefix (string-append (startup-directory) (relative-source-html-destination-path-fragment)))	
                                                                             ; relative or absolute path to LAML root dir
		 (as-string scheme-report-version-attr) "/"
		 ))

              ((and (not laml-resource-attr) 
                    (or (equal? "development" (laml-version-kind)) (equal? "full" (laml-version-kind)))) 
                (string-append 
		 (laml-local-url-prefix (string-append (startup-directory) (relative-source-html-destination-path-fragment)))	
                                                                             ; relative or absolute path to LAML root dir
		 (as-string scheme-report-version-attr) "/"
		 ))
              
              (else rnrs-url-attr))   ; defaulted to absolute URL to cs.auc.dk LAML distribution.
  )

  (set! cross-reference-index-support cross-reference-index-attr)
  (set! defined-name-index-support defined-name-index-attr)
  (set! make-duplicated-name-index? duplicated-name-index-attr)

  (set! blank-initial-program?
     (case initial-program-frame-attr
       ((blank) #t)
       ((first-source-file) #f)
       (else (laml-error "elucidator-front-matters: Unexpected value of initial-program-frame attribute:" initial-program-frame-attr))))

  (set! make-large-source-files? large-font-source-file-attr)
;  (set! make-small-source-files? small-font-source-file-attr)
  (set! default-program-font-size default-source-file-font-size-attr)

  (set! program-menu-approach program-menu-attr) 

  (set! manual-frame-from-program manual-frame-from-program-attr)
  (set! manual-frame-from-documentation manual-frame-from-documentation-attr)

  (set! elucidator-escape-char documentation-escape-char-attr)
  (set! elucidator-escape-char-string (as-string elucidator-escape-char))
  (set! p-link-prefix-char program-link-prefix-char-attr)
  (set! p-link-suffix-char program-link-suffix-char-attr)
  (set! d-link-prefix-char documentation-link-prefix-char-attr)
  (set! d-link-suffix-char documentation-link-suffix-char-attr)
  (set! strong-link-char strong-link-char-attr)
  (set! weak-link-char weak-link-char-attr)
  (set! none-link-char none-link-char-attr)
  (set! default-program-link default-program-link-attr)

  (set! present-hidden-ids? author-mode-attr)
  (set! elucidator-verbose-mode (eq? processing-mode-attr 'verbose))

  (set! the-css-documentation-stylesheet css-documentation-stylesheet-attr)
  (set! the-css-program-stylesheet css-program-stylesheet-attr)

  (set! warn-if-missing-source-marker-in-documentation warn-if-no-doc-source-marker-attr)
  (set! warn-if-ambiguous-source-markers-in-documentation warn-if-multiple-doc-source-markers-attr)
  )
 )
)



;; End of documentation part.
;; Makes all the html stuff. Until now we have collected stuff. Here we generate html files
;; based on the collected stuff.
(define (do-end-documentation!)

  ; make the help page in the actual html directory
  (if elucidator-verbose-mode (display-message "Making the help page"))
  (make-elucidator-help-page)

  ; copy image files from the software directory to the html directory
  (if copy-image-files?    
      (begin
        (if elucidator-verbose-mode (display-message "Copying image files"))
        (copy-files 
          elucidator-image-files
          (string-append elucidator-software-directory "images/")
          (string-append source-directory (relative-source-html-destination-path-fragment) "images/") )))

  (store-lisp-expression 
    (map emacs-protect-alist program-source-list)
    (internal-file "program-source-list"))  ; @a

  ; Store yet another version, appropriate for the Emacs EP tool. Only information about the newest versions are included here.
  (store-lisp-expression 
    (map emacs-protect-alist (only-newest-versions program-source-list))
    (internal-file "editor-program-source-list"))

  (set! manual-name-file-map (pre-process-manual-lsp-files manual-source-list))

  (set! documentation-elements (reverse documentation-elements))

  ; @b save the list of documentation keys. This is for communication with the editor part (Emacs support) of the elucidator.
  (store-lisp-expression (reverse (map car documentation-key-title-alist)) (internal-file "documentation-ids"))  

  ; @n Pre-process lexical comments.
  (let ((program-source-list-process 
          (filter must-process-source? program-source-list))
        (program-source-list-non-process
          (filter (negate must-process-source?) program-source-list))
       )

    ; Pre-processing comments in source files, thereby defining new source files in the internal directory.
     (if #t  ; always pre-process source file, to achieve syntactic comments.
        (begin 
          (if elucidator-verbose-mode (display-message "Pre-processing lexical comments in source files"))
          (pre-process-comments-in-files! program-source-list-process)
        )
        (if elucidator-verbose-mode (display-message "NO Pre-processing lexical comments in source files"))
     )

    ; parse source files - both new and old versions, but only those to be processed -
    ; and assign the list of results to source-list-list-process.
    (if elucidator-verbose-mode (display-message "Parsing source files"))
    (set! source-list-list-process
          (map read-source   ; @c
               (map source-file-with-syntactic-comment program-source-list-process)
               (map (lambda (ps) (get-value 'key ps)) program-source-list-process)
               (map (lambda (ps) (get-value 'version ps)) program-source-list-process)
          ))


    ; Read the sources of the files, that we do not process. 
    ; This may seem a little contradictory, but we need the sources of older versions in order to facilitate version comparions.
    ; Reading the sources causes the source to be cached, and hereby the source is availble, for instance
    ; when we compare the source versions with definition-updated?
    (map read-source 
	 (map source-file-with-syntactic-comment program-source-list-non-process)
	 (map (lambda (ps) (get-value 'key ps)) program-source-list-non-process)
	 (map (lambda (ps) (get-value 'version ps)) program-source-list-non-process)
	 )

    (set! source-key-list
          (map (lambda (ps) (get-value 'key ps)) program-source-list))

    (let ((source-key-list-process (map (lambda (ps) (get-value 'key ps)) program-source-list-process))
          (source-key-list-non-process (map (lambda (ps) (get-value 'key ps)) program-source-list-non-process))
         )
   
     ; collect all defining names from all source files @d
     ; each element is an entry of the form (name source-key version).
     ; store newly calculcated defined names in -.name files
     (if elucidator-verbose-mode (display-message "Collecting and reading defining name occurences"))

     (set! defining-name-occurences (make-defining-name-occurences program-source-list))

     ; @g make documentation file. Hereby the global variable documented-name-occurences is assigned.

     (cond ((eq? documentation-approach 'textual)
             (if elucidator-verbose-mode (display-message "Presenting and resolving links in the textual documentation"))
             (let ((of (html-destination "documentation")))
	       (if (file-exists? of) (delete-file of))
	       (let* ((op (open-output-file of))
		      (color-attributes (bg-text-link-vlink-colors (color-of-group "doc") black black black))
  
                      (body-ast (body color-attributes "dummy"))
                      (html-ast (html (head (title "dummy")) body-ast))
		     )

                 (render-start-tag-to-output-port html-ast op)
		 (render-to-output-port 
		  (head 
		   (link 'href (in-elucidator-stylesheet-directory "documentation.css")
                         'rel "stylesheet" 'title "documentation" 'type "text/css")
		   (title "documentation"))
		  op)
                 (render-start-tag-to-output-port body-ast op)
		 (textual-documentation-contents! op)
                 (render-end-tag-to-output-port body-ast op)
                 (render-end-tag-to-output-port html-ast op)

		 (close-output-port op)
		 )))
           ((eq? documentation-approach 'laml)
             (if elucidator-verbose-mode (display-message "Presenting LAML documentation"))
             (let ((of (html-destination "documentation"))) ; absolute path
	       (if (file-exists? of) (delete-file of))
	       (let* ((op (open-output-file of))
		      (color-attributes (bg-text-link-vlink-colors (color-of-group "doc") black black black))

                      (body-ast (body color-attributes "dummy"))
                      (html-ast (html (head (title "dummy")) body-ast))
		      )

                 (render-start-tag-to-output-port html-ast op)
		 (render-to-output-port 
		  (head 
		   (link 'href (in-elucidator-stylesheet-directory "documentation.css")
                         'rel "stylesheet" 'title "documentation" 'type "text/css")
		   (title "documentation"))
		  op)
                 (render-start-tag-to-output-port body-ast op)
		 (laml-documentation-contents! op of)
                 (render-end-tag-to-output-port body-ast op)
                 (render-end-tag-to-output-port html-ast op)

		 (close-output-port op)
		 ))
           )
           (else (laml-error "do-end-documentation: Unknown documentation-approach:" documentation-approach)))

     ; save documented-name-occurences. This is soley for communication with the Emcas editor part of the elucidator
     (store-lisp-expression (map emacs-protect-documented-name-entry (reverse documented-name-occurences)) (internal-file "documented-names"))

     ; reverse documentation-source-marker-occurences such that documentation source markers are encountered in the right sequence
     ; when we process the program source files.
     (set! documentation-source-marker-occurences (reverse documentation-source-marker-occurences))

     ; make program files @h:
     (if elucidator-verbose-mode (display-message "Making program source files"))
     (for-each 
        (lambda(ps source-list)
          (if elucidator-verbose-mode (display-message (string-append "  " (get-value 'key ps))))
          (make-source-program-file (get-value 'key ps) (get-value 'version ps) (defaulted-get-value 'group ps "program")
                                    (source-file-with-syntactic-comment ps) (get-value 'language ps) 
                                    source-list defining-name-occurences documented-name-occurences 'small))
        program-source-list-process
        source-list-list-process
     )

     ;@i
     (if (or make-large-source-files? (eq? default-program-font-size 'large))
       (begin
       ; make large source files:
       (if elucidator-verbose-mode (display-message "Making LARGE program source files"))
       (for-each 
         (lambda(ps source-list)
           (if elucidator-verbose-mode (display-message (string-append "  " (get-value 'key ps))))
           (make-source-program-file (get-value 'key ps) (get-value 'version ps) (defaulted-get-value 'group ps "program")
                                     (source-file-with-syntactic-comment ps) (get-value 'language ps) 
                                     source-list defining-name-occurences documented-name-occurences 'large))
         program-source-list-process
         source-list-list-process
       )))


     ; @j make control file:
     (if elucidator-verbose-mode (display-message "Making the control file"))
     (write-html 'raw
        (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
          (html 
            (head (title "control"))
            (body color-attributes
             (icon-bar)
             (elucidator-when-generated))))
        (html-destination "control"))

     ; @k
     (if make-duplicated-name-index?
      (begin
       ; make duplicate report:
       (if elucidator-verbose-mode (display-message "Making the duplicate report"))
       (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
	   (html 
            (head (title "Duplicate report"))
            (body color-attributes
              (icon-bar)
              (present-duplicated-definitions))))    
          (html-destination "duplicate-report"))
       )
       (if elucidator-verbose-mode (display-message "NO duplicated name index is being generated"))
     )

     ; make defined names index:
     (if (not (eq? defined-name-index-support 'none))      ;@l
         (let* ((unversioned-defining-name-occurences (filter new-version-name? defining-name-occurences))
                (sorted-defining-name-occurences (sort-list unversioned-defining-name-occurences name-entry-leq?)))
           (if elucidator-verbose-mode (display-message "Making index of defined names"))
           (if elucidator-verbose-mode
               (display-message 
		(if (eq? defined-name-index-support 'per-letter) 
		    "  alphabetically broken"
		    "  as one large index")))

           (if (eq? defined-name-index-support 'per-letter)
               (let* ((splitted-defining-name-occurences (split-defining-name-occurences sorted-defining-name-occurences))
                      (alphabet (map downcase-string 
                                     (map first-letter-of (map (compose defined-name-of car) splitted-defining-name-occurences)))))
                 (map2 (lambda (dan letter)
                         (make-defining-name-index dan letter alphabet))
                       splitted-defining-name-occurences
                       alphabet)
                 (make-overall-defining-name-index alphabet))
             (begin
              (write-html 'raw
               (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
		 (html 
		  (head (title "Alphabetic index of defined names"))
		  (body color-attributes
			(icon-bar)  
			(present-defined-name-index sorted-defining-name-occurences)
			)))
               (html-destination "defining-name-index")))))
       (if elucidator-verbose-mode (display-message "NO index of defined names is being generated"))
     )

     ; Make cross reference index. This involves extracting of applied-defined name pairs from the source files.
     (if (not (eq? cross-reference-index-support 'none))

      (let ((unversioned-defining-name-occurences (filter new-version-name? defining-name-occurences)))
       (if elucidator-verbose-mode (display-message "Extracting applied-defined name pairs from parsed source files"))
       (set! defined-applied-names      ; A list of name pairs of the form (applied-name . defined-name)
          (applied-names-multiple-sources
             (append
                source-list-list-process  ; the list of sources processed in this run
                (map read-source          ; together with the list of sources NOT processed in this run
                     (map (lambda (ps) (get-value 'file-location ps)) program-source-list-non-process)
                     (map (lambda (ps) (get-value 'key ps)) program-source-list-non-process)
                     (map (lambda (ps) (get-value 'version ps)) program-source-list-non-process)
                   )   ; the list of sources that need to be read
             )))
                
       ; make index: cross references involving applied names
       ;@o
       (if elucidator-verbose-mode (display-message "Presenting the extracted cross reference index")) 
       (if elucidator-verbose-mode 
           (display-message 
	    (if (eq? cross-reference-index-support 'per-letter) 
		"  alphabetically broken"
		"  as one large index")))
       (let ((extended-defined-applied-names    ; A list of pairs: (applied-name . defined-name)
              (merge-defined-and-defined-applied-lists
                defined-applied-names
                (sort-list 
                 (map (lambda (dno) (cons (defined-name-of dno) #f)) unversioned-defining-name-occurences)
                 (lambda (x y) (string<=? (as-string x) (as-string y)))))))

                
         (if (eq? cross-reference-index-support 'per-letter) 
             (let* ((sdan (split-defined-applied-names extended-defined-applied-names))
                    (alphabet (map downcase-string  (map first-letter-of (map caar sdan)))))   
               (map2 (lambda (dan letter)
                       (make-cross-reference-index dan letter alphabet))
                     sdan 
                     alphabet)
               (make-overall-cross-reference-index alphabet)  ; with the alphabet navigator
               )
           (write-html 'raw
            (let ((color-attributes (bg-text-link-vlink-colors  (color-of-group "index") black black black)))
	      (html 
	       (head (title "Alphabetic cross reference index"))
	       (body color-attributes
		     (icon-bar)  
		     (present-cross-reference-index
		      extended-defined-applied-names)
		     )))
            (html-destination "cross-reference-index")))))
       (if elucidator-verbose-mode (display-message "NO cross reference index is being generated"))
      )

      (if elucidator-verbose-mode (display-message "Presenting overall documentation table of contents"))
      (write-html 'raw
          (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black )))
	    (html 
	     (head (title "Documentation table of contents"))
	     (body color-attributes
              (icon-bar)  
              (present-documentation-contents documentation-elements 'overall)
              (elucidator-when-generated))))
          (html-destination "documentation-toc-overall"))

      (if elucidator-verbose-mode (display-message "Presenting detailed documentation table of contents"))
      (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
	   (html 
	    (head (title "Documentation table of contents"))
	    (body color-attributes
		  (icon-bar)  
		  (present-documentation-contents documentation-elements 'detail)
		  (elucidator-when-generated)              
		  )))
          (html-destination "documentation-toc-detail"))

     ; Making program menu frame:
     (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
	   (html 
            (head (title "Program Menu"))
             (body color-attributes
              (if (> (length manual-source-list) 0)
                  (left-right-banner "" (a 'href "manual-menu.html" (font 'size "1" "Show Manual Menu")))
                  "")
              (source-file-links-for-program-menu (only-newest-versions program-source-list))              
		  )))
         (html-destination "program-menu"))

     ; Making manual menu frame:
     (if (> (length manual-source-list) 0)
         (write-html 'raw
                     (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
                       (html 
                        (head (title "Manual Menu"))
                        (body color-attributes
                              (left-right-banner ""  (a 'href "program-menu.html" (font 'size "1" "Show Program Menu")))  
                              (manual-links-for-menu manual-source-list)              
                              )))
                     (html-destination "manual-menu")))

     ; @m make frame files, in which the program is the first mentioned program source
     (if elucidator-verbose-mode (display-message "Making frame files"))


     (make-frame-file-in-html-dir
        "Scheme Elucidator"
        (elucidator-frame 
           (documentation-toc-name) 
           (program-or-manual-menu) ; "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           default-program-font-size
           ""  ; in html directory
        )
        "index")

     (if elucidator-verbose-mode (display-message (string-append "The Elucidator result is available in " (source-filename-without-extension scheme-system) ".html,")))
     (if elucidator-verbose-mode (display-message (string-append "which is located in the same directory as the setup and documentation files")))

      ; Also make a frame file in the source directory, for easy and convenient start of the browsing:
     (make-frame-file-in-source-dir
        "Scheme Elucidator"
        (elucidator-frame 
           (documentation-toc-name)
           (program-or-manual-menu)  ; "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           default-program-font-size
           (relative-source-html-destination-path-fragment)
        )
        (source-filename-without-extension scheme-system))
     
     (make-frame-file-in-html-dir
        "Scheme Elucidator"
        (elucidator-frame-horizontal   ; always in html-dir
           (documentation-toc-name) 
           (program-or-manual-menu)  ; "program-menu"
           "documentation"
           (initial-program-page program-source-list) ; program starting point
           default-program-font-size
           ""
        )
        "index-horizontal")


     (let ((program-frame-content
            (con
              (vertical-space 1)
              (center (font-1 6 grey "The Scheme Elucidator 2"))
              (center (font-1 6 grey "Program Frame"))
              (vertical-space 1)
              (center (narrow-with-pixels 100 
                        (con
                         (p (font-1 4 grey "Scheme source programs are shown here when they are selected in the documentation frame."))
                         (p
                           (case program-menu-approach
                             ((inline-table) (font-1 4 grey "You can also select the programs in the upper control frame."))
                             ((separate-frame) (font-1 4 grey (string-append "You can also select the programs in the upper right menu frame. "
                                                                              "(If necessary, first activate " (string-it "Show Program Menu") " in this frame)." )))
                             ((none) ""))
                           )))))) 
          )
      (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors white black black black)))
	   (html 
            (head (title "Blank Initial Program"))
            (body color-attributes
               program-frame-content  
		  )))
         (html-destination "blank-initial-program"))

      (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors white black black black)))
	   (html 
            (head (title "Blank Initial Program"))
            (body color-attributes
              program-frame-content  
		  )))
         (html-destination "blank-initial-program-LARGE")))


     (if elucidator-verbose-mode (display-message "Copying CSS stylesheets"))

     ; Create the stylesheets directory within the html directory.
     (ensure-directory-existence! (string-append source-directory (relative-source-html-destination-path-fragment)) "stylesheets")

     ; Copy CSS stylesheets:
     (if #t
	  (let ((documentation-source-css-filepath 
                      (string-append source-directory "stylesheets/" the-css-documentation-stylesheet ".css"))
                (documentation-ep-software-css-filepath 
                      (string-append elucidator-software-directory "stylesheets/" the-css-documentation-stylesheet ".css"))
		(documentation-target-css-filepath 
                      (string-append source-directory (relative-source-html-destination-path-fragment) "stylesheets/" "documentation.css"))

                (program-source-css-filepath
                      (string-append source-directory "stylesheets/" the-css-program-stylesheet ".css"))
                (program-ep-software-css-filepath 
                      (string-append elucidator-software-directory "stylesheets/" the-css-program-stylesheet ".css"))
		(program-target-css-filepath (string-append source-directory 
							 (relative-source-html-destination-path-fragment) "stylesheets/" "program.css")) 
	       )
	    (write-text-file
              (string-append
                (read-text-file-if-exists documentation-ep-software-css-filepath) CR CR
                (read-text-file-if-exists documentation-source-css-filepath))
 	      documentation-target-css-filepath
	    )
	    (write-text-file
              (string-append (read-text-file-if-exists program-ep-software-css-filepath) CR CR
                (read-text-file-if-exists program-source-css-filepath))
 	      program-target-css-filepath
	    )
          )
     )

     (write-html 'raw
         (let ((color-attributes (bg-text-link-vlink-colors white black black black)))
	   (html 
            (head (title "Elucidator Error Page"))
            (body color-attributes

              (h1 "The Elucidator Error Page")

              (p "This page describes various errors that may occur in an elucidative program.")

              (a 'name "program-reference-error")
              (h3 "Linking to unknown abstractions") 

              (p 
                 "The author of the documentation of the elucidative program has accidentally referred to a name that does not exist in the source program(s), 
                  in the SchemeDoc manual file, or in the R4RS/R5RS Scheme Report." (br)
                 "The documentation should be corrected and reprocessed.")

              (div (vertical-space end-file-empty-lines))
                
	  )))
         (html-destination error-page-name))

     (if elucidator-verbose-mode (display-message (string-append "Total processing time: " (present-time-interval (- (current-time) start-run-time)))))
     (if elucidator-verbose-mode (display-message CR))

     (end-laml)
   
 )))

; Return either "manual-menu" or "program-menu" depending on the number of source program and manuals in this documentation bundle.
(define (program-or-manual-menu)
 (let* ((effective-program-source-list (only-newest-versions program-source-list))
        (effective-program-source-list-lgt (length effective-program-source-list))
        (manual-source-list-lgt (length manual-source-list)))
  (cond ((and (<= effective-program-source-list-lgt 1) (> manual-source-list-lgt 1)) "manual-menu")
        ((and (<= manual-source-list-lgt 1) (> effective-program-source-list-lgt 1)) "program-menu")
        (else "program-menu"))))


; Filter the program-source-list such that only the newest (highest) versions are included in the list.
(define (only-newest-versions program-source-list)
 (filter 
   (lambda (psl-entry)
     (let ((source-key (get-value 'key psl-entry))
           (source-version (get-value 'version psl-entry)))
        (= source-version (highest-version-number source-key))))
   program-source-list))


; Return the list of defined names, as to be assigned to defining-name-occurences.
; Also, during the process, and as a side effect, store the defined names on internal files.
(define (make-defining-name-occurences program-source-list)
  (cond ((null? program-source-list) '())
        ((must-process-source? (car program-source-list))
           (let* ((program-source-entry (car program-source-list))
                  (source-key (get-value 'key program-source-entry))
                  (version (get-value 'version program-source-entry))
                  (file-location (get-value 'file-location program-source-entry))
                  (list-of-forms (read-source file-location source-key version))
                  (def-names (defined-names list-of-forms))
                 )
              (if store-defined-names? (store-defined-names program-source-entry def-names))  ; side effect
              (append 
                 (map (lambda (dn) (make-defined-name-entry dn source-key version)) def-names)
                 (make-defining-name-occurences (cdr program-source-list)))))
        (else 
           (let* ((program-source-entry (car program-source-list)))
              (append 
                 (restore-defined-names program-source-entry) 
                 (make-defining-name-occurences (cdr program-source-list)))))))
                

(define (read-text-file-if-exists file-path)
  (if (file-exists? file-path)
      (read-text-file file-path)
      ""))

; Return the proper (HTML) file name of the initial content of the program frame.
; Always small sized.
(define (initial-program-page program-source-list)
 (let* ((source-key (get-value 'key (car program-source-list)))
        (source-version (highest-version-number source-key))
        (size default-program-font-size)
       )
   (if blank-initial-program?
       "blank-initial-program"
       (source-file-name-html-file source-key source-version 'small))))

(define (pre-process-manual-lsp-files manual-source-list)
 (flatten 
  (map pre-process-manual-lsp-file
       (map (lambda (entry) (car (get 'file-location entry))) manual-source-list)
       (map (lambda (entry) (car (get 'url-location entry))) manual-source-list))))

; Return a contribution to the manual map.
; full-manual-file-path can be with or without lsp or manlsp extension.
; We actually force the manlsp extension if such a file exists, else a lsp extension.
(define (pre-process-manual-lsp-file full-manual-file-path manual-url)
 (let* ((full-manual-file-path-manlsp
           (string-append (file-name-initial-path full-manual-file-path) (file-name-proper full-manual-file-path) "." "manlsp"))
        (full-manual-file-path-lsp 
            (string-append (file-name-initial-path full-manual-file-path) (file-name-proper full-manual-file-path) "." "lsp"))
       )
  (letrec ((manual-page? (lambda (lsp-entry) (equal? "manual-page" (car (get 'kind lsp-entry)))))
	   (get-symbol-name-of-lsp-entry (lambda (lsp-entry) (car (get 'title lsp-entry))))
          )

    (cond ((file-exists? full-manual-file-path-manlsp)
	     (let ((lsp-structure (cdr (file-read full-manual-file-path-manlsp)))) ; cdr: Skip manual meta information. 
	       (map (lambda (name)
		      (cons name manual-url))
		    (map get-symbol-name-of-lsp-entry (filter manual-page? lsp-structure)))))
          ((file-exists? full-manual-file-path-lsp)
	     (let ((lsp-structure (file-read full-manual-file-path-lsp)))
	       (map (lambda (name)
		      (cons name manual-url))
		    (map get-symbol-name-of-lsp-entry (filter manual-page? lsp-structure)))))
	  (else (laml-error "Cannot locate LAML manual file: " full-manual-file-path-lsp full-manual-file-path-manlsp))))))



; Return either "documentation-toc-overall" or "documentation-toc-detail" depending
; on the global variable "documentation-toc-overall"
(define (documentation-toc-name)
 (cond ((eq? default-table-of-content 'overall) "documentation-toc-overall")
       ((eq? default-table-of-content 'detailed) "documentation-toc-detail")
       (else (laml-error "documentation-toc-name: Unknown default-table-of-content: " default-table-of-content))))



; Return the name of the source file to parse, given source descriptor and the global variable comment-handling
; Not used any more. One of source-file-with-syntactic-comment or original-source-file are used instead.
(define (source-file-determinator source-descriptor)
  (cond ((eq? comment-handling 'syntactical) (internal-syntactic-commented-file (get-value 'key source-descriptor)))
        ((eq? comment-handling 'lexical) (get-value 'file-location source-descriptor))
        (else (error "source-file-determinator: Unknown kind of comment-handling"))))


; Return the full path to the internal source file with syntactic comments.
(define (source-file-with-syntactic-comment source-descriptor)
  (internal-syntactic-commented-file
    (proper-source-file-with-syntactic-comment source-descriptor)))

            
; Return the name of the original source file (with lextical comments).
(define (original-source-file source-descriptor)
  (get-value 'file-location source-descriptor))

; Return the name of the file holding the comment transformed source file (with syntactic comments).
(define (internal-syntactic-commented-file name)
 (internal-file (string-append (as-string name) "-syntactical-comments")))

; Pre-process all source files in source-file-list, which is a source file descriptor
(define (pre-process-comments-in-files! source-file-list)
  (map pre-process-comments! source-file-list))

; Pre-process a single source-file-descriptor. 
; This defines a file in the internal directory
(define (pre-process-comments! source-file-descriptor)
  (let* ((input-file (get-value 'file-location source-file-descriptor))
         (output-file (internal-syntactic-commented-file (proper-source-file-with-syntactic-comment source-file-descriptor))))
    (lexical-to-syntactical-comments! input-file output-file)))

(define (elucidator-when-generated)
 (let* ((dt (date-time (current-time)))
        (date (car dt))
        (time (cadr dt)))
  (font-1 2 red (span (when-generated)))))


; Store the lisp expression exr on the file with full path file-path.
(define (store-lisp-expression expr file-path)
 (if (file-exists? file-path) (delete-file file-path))
 (with-output-to-file file-path
       (lambda () (write expr))))


(define (icon-bar)
 (left-right-banner
  (table-3 0 (append (if previous-next-elucidators (list 30 30 30) '())
                     (if elucidator-home-url (list 30 30) '())
                     (list 30 30   30    30 30 30    30    30 30  30  30 30   60  1000)) 
           (list
            (append
              (if previous-next-elucidators
                  (list
                   (if elucidator-previous-url 
                       (a-tag-target elucidator-previous-url (image "nav-left.gif" "Go to previous elucidator") "_top")
                       (image "nav-left-empty.gif" ""))
 
                   (if elucidator-next-url 
                       (a-tag-target elucidator-next-url (image "nav-right.gif" "Go to next elucidator") "_top")
                       (image "nav-right-empty.gif" ""))

                   " "
                  )
                  '())

              (if elucidator-home-url
                  (list 
                    (a-tag-target elucidator-home-url (image "home.gif" "Go home") "_top")
                    " ")
                  '())
             (list 

	      (a-tag-target "index.html" (image "three-frames.gif" "Reset Elucidator to vertical layout") "_top")
	      (a-tag-target "index-horizontal.html" (image "three-frames-horizontal.gif" "Reset Elucidator to horizontal layout") "_top")
	      " "

	      (if (not (eq? defined-name-index-support 'none)) 
	          (a-tag-target "defining-name-index.html" 
			    (image "index.gif" "Alphabetic index of defined names in the program") "control-frame")
                  "")  

	      (if (not (eq? cross-reference-index-support 'none)) 
                  (a-tag-target "cross-reference-index.html" (image "cross-index.gif" "Cross reference index") "control-frame")
                  "")
 
	      (if make-duplicated-name-index?
                  (a-tag-target "duplicate-report.html" (image "xx.gif" "Duplicated definitions") "control-frame")
                  "")

	      " "
	      (a-tag-target "documentation-toc-detail.html" (image "contents.gif" "Detailed documentation table of contents") "control-frame")
	      (a-tag-target "documentation-toc-overall.html" (image "overall-contents.gif" "Overall documentation table of contents") "control-frame")

	      " "
	      (a-tag-target "elucidator-help.html" 
			    (image "question-left-arrow.gif" "Elucidator Help Page to be shown in the documentation frame")
			    "documentation-frame")

	      (a-tag-target "elucidator-help.html"
			    (image "question-right-arrow.gif" "Elucidator Help Page to be shown in the program frame")
			    "program-frame")


	      " "

	      (if (eq? program-menu-approach 'inline-table) (source-file-links program-source-list) "")
	      )))
           'middle
           )

   (if is-laml-resource
       (laml-home-button 0 "laml-home.gif" (string-append source-directory elucidator-source-destination-delta))
       (laml-power-icon 0 'small))
  )
)


; Read the list of defined names (list of (name . source-key)) from file
; If no file found, return the empty list
(define (restore-defined-names program-source-entry)
  (let* ((restore-filename (defining-names-file program-source-entry))
         (source-key (get-value 'key program-source-entry))
         (version    (get-value 'version program-source-entry))
        )
    (if (file-exists? restore-filename)
        (let* ((ip (open-input-file restore-filename))
               (res (read ip)))
          (if elucidator-verbose-mode (display-message (string-append " Restoring defined names from " source-key ".names")))
          (close-input-port ip)
          res  ; earlier we returned a copy of res.
        )
        (begin
          (display-warning (string-append "No defining names stored for " source-key))
          '()))))

; Write the list of defined names, in terms of defined name entries, to an internal file.
(define (store-defined-names program-source-entry defined-names)
  (let* ((store-filename (defining-names-file program-source-entry))
         (source-key (get-value 'key program-source-entry))
         (version (get-value 'version program-source-entry))
         (keyed-names (map (lambda (dn) (make-defined-name-entry (as-string dn) source-key version)) defined-names))
        )
    (if (file-exists? store-filename) (delete-file store-filename))
    (with-output-to-file store-filename
       (lambda () (write keyed-names)))))


; return the file name (full path) of the name file with program-source-entry (an entry in ...).
(define (defining-names-file program-source-entry)
  (string-append source-directory "internal/" (proper-source-file-with-syntactic-comment program-source-entry) "." "names"))

; source file links - inline version for icon-bar
(define (source-file-links program-source-list)
 (let* ((source-key-list   (map (lambda (ps) (get-value 'key ps)) program-source-list))   ; extract keys from program-source-list
        (source-group-list (map (lambda (ps) (defaulted-get-value 'group ps "program")) program-source-list)) ; similarly extract the groups
        (source-file-list (map (lambda (ps) (get-value 'file-location ps)) program-source-list)) ; similarly extract the file-locations
        (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
       )
  (table-1 1 
    (map (lambda (sk) (* (string-length sk) 7)) source-key-list)
    (map color-of-group source-group-list)
    (list
     (map2
      (lambda (sk sf)
         (a
          (font-size 2 sk)
          'href (add-file-extension (string-append sk size-string) "html")
          'title sf
          'target "program-frame"
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         )
      )
      source-key-list
      source-file-list)))))

; source file links - version of program menu frame
(define (source-file-links-for-program-menu program-source-list)
 (let ((source-group-list 
          (map (lambda (ps) (defaulted-get-value 'group ps "program")) program-source-list))) ; similarly extract the groups
  (table-4 1 
    (list 240)
    (map color-of-group source-group-list)
    (map
      (lambda (ps)
       (let* ((sk (get-value 'key ps))
              (friendly-source-program-name (get-value 'friendly-name ps))
              (source-program-name (if (blank-string? friendly-source-program-name) sk friendly-source-program-name))
              (vers (get-value 'version ps))
              (sf (get-value 'file-location ps)))
       (list
         (a
          (font source-program-name 
                'size "2" 'color (if (older-version-source-program-entry? ps) (rgb-color-encoding 'grey) (rgb-color-encoding 'black)))
          'href (add-file-extension (source-file-name-html-file sk vers default-program-font-size) "html")
          'title sf
          'target "program-frame"
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         )
       ))
      )
      program-source-list))))


(define (manual-links-for-menu manual-source-list)
  (table 'border "1"
    (map
      (lambda (manual-source-entry)
       (let* ((sk (get-value 'key manual-source-entry))
              (url (get-value 'url-location manual-source-entry))
              (friendly-name (get-value 'friendly-name manual-source-entry))
              (actual-name (if (blank-string? friendly-name) sk friendly-name))
             )
       (list
         (tr 'bgcolor (rgb-color-encoding default-background-color) (td 'width "240px"
         (a
          (font-size 2 actual-name)
          'href url
          'title "Manual"
          'target "program-frame"
          'style (if underline-program-links "{text-decoration: underline;}" "{text-decoration: none;}")
         ))))))
      manual-source-list)))
    



; ---------------------------------------------------------------------------------------------------

; Get the key value from elements. Elements is assumed to be a list of double-lists, such as ((key1 val1) (key2 val2) (key3 val3)).
; Key comparison is done by the function equal?, by means of assoc.
; In this context, a double list is a list of length two.
(define (get-value key elements)
 (let ((res (assoc key elements)))
   (if (and (list? res) (> (length res) 1))
       (cadr res)
       (laml-error "get-value in elucidator: Problems accessing a value of a syntax element:" key elements res))))

; As get-value, but instead of a fatal error we return default-value if key is not in elements.
; Key comparison is done by the function equal?, by means of assoc.
; In this context, a double list is a list of length two.
(define (defaulted-get-value key elements default-value)
 (let ((res (assoc key elements)))
   (if (and (list? res) (> (length res) 1))
       (cadr res)
       default-value)))

; Get the list of values of an element
(define (get-values key elements)
  (cdr (assoc key elements)))


; General functions

(define (html-destination filename)
  (string-append (html-directory) filename ".html"))

(define (source-destination filename)
  (string-append source-directory filename ".html"))




; ---------------------------------------------------------------------------------------------------
(define image-file-access 'sub-directory)

(define (image file-name help-text) (img 'src (image-file file-name) 'title help-text 'alt "" 'border "0"))

; ---------------------------------------------------------------------------------------------------
;;; Scheme source file reading and chacing.

; A map from source-key and version to the list of top-level forms, as read from the source file (with syntactic comments).
; Serves as a chache.
(define source-file-map '())

; Source file contribution constructor
(define (make-source-file-contribution source-key source-version source-list)
  (list source-key source-version source-list))

; Source file contribution selectors
(define source-key-of-source-file-contribution (make-selector-function 1 "source-key-of-source-file-contribution"))
(define version-of-source-file-contribution (make-selector-function 2 "version-of-source-file-contribution"))
(define source-list-of-source-file-contribution (make-selector-function 3 "source-list-of-source-file-contribution"))

; Does the source file entry have source-key and source-version?
(define (source-file-equal? entry source-key source-version)
  (and (equal? (as-string source-key) (as-string (source-key-of-source-file-contribution entry)))
       (= (as-number source-version) (as-number (version-of-source-file-contribution entry)))))

; Ad an entry to the source file map
(define (add-to-source-file-map source-key source-version source-list)
  (set! source-file-map
     (cons (make-source-file-contribution source-key source-version source-list)
           source-file-map)))
  
; Get an entry from the source file map. Return #f if it does not exist.
(define (get-source-list-from-source-file-map source-key source-version)
 (let ((entry (find-in-list
               (lambda (entry)
                 (source-file-equal? entry source-key source-version))
               source-file-map)))
   (if entry
       (source-list-of-source-file-contribution entry)
       #f)))       

;; Read the file (a lisp source file) and return a list of the lisp expressions found in the source file.
;; Take the source list from the cache if it is there, and put it into the the cache if it is not.
(define (read-source file key version)
 (let ((result (get-source-list-from-source-file-map key version)))
   (if result
       result
       (let* ((ip (open-input-file file))
              (read-result (reverse (read-source-1 ip '()))))
         (close-input-port ip)
         (add-to-source-file-map key version read-result)
         read-result))))

(define (read-source-1 input-port source-list)
  (if (eof-object? (peek-char input-port))
      source-list
      (read-source-1 input-port (cons (read input-port) source-list))))

; ---------------------------------------------------------------------------------------------------
;;; Syntactic predicates.

(define (function-define-form? x)
  (and (list? x) 
       (> (length x) 2)
       (eq? (car x) 'define)
       (or (and (symbol? (cadr x)) (pair? (caddr x)) (eq? 'lambda (car (caddr x))))  ; (define N (lambda ...))
                (pair? (cadr x)))))

; A slightly broader predicate than function-define-form.
; Captures everyting started with 'define-', including define syntax and macros applications that start the 'define-'.
(define (is-define-form? x)
  (and (list? x) 
       (> (length x) 1)
       (or (eq? (car x) 'define)
	   (and (symbol? (car x))
		(let ((s (symbol->string (car x))))
		  (and (> (string-length s) 6)
		       (string=? (downcase-string (substring s 0 7)) "define-")))))))

(define (lambda-form? x)
  (and (list? x) 
       (> (length x) 2)
       (and (symbol? (car x)) (eq? (car x) 'lambda))
       (parameter-variable-list? (cadr x))))

(define (parameter-variable-list? x)
  (or (symbol? x)
      (null? x)
      (and (pair? x)
           (symbol? (car x)) 
           (or (symbol? (cdr x))
               (parameter-variable-list? (cdr x))))))

(define (quote-form? x)
  (and (list? x) 
       (>= (length x) 2)
       (eq? (car x) 'quote)))

(define (unquote-form? x)
  (and (list? x) 
       (>= (length x) 2)
       (or (eq? (car x) 'unquote) (eq? (car x) 'unquote-splicing))))

(define (quasiquote-form? x)
  (and (list? x) 
       (>= (length x) 2)
       (eq? (car x) 'quasiquote)))

(define (let-form? x)
  (and (list? x) 
       (> (length x) 2)
       (or (memq (car x) (list 'let 'let* 'letrec 'let-syntax 'letrec-syntax)))))

; is the let form a named-let
; precondition: let-form satisfies let-form?
(define (named-let? let-form)
  (and (eq? (car let-form) 'let) (symbol? (cadr let-form))))

; Return the formal parameter names of a lamba construct. Always returns a list.
; The paramter lambda-form must be a lambda expression.
(define (lambda-names lambda-form)
  (let ((par-list (cadr lambda-form)))
    (cond ((list? par-list) par-list)
          ((symbol? par-list) (list par-list))
          ((pair? par-list) (append (proper-part par-list) (list (first-improper-part par-list))))
          (error "lambda name: unknown kind of the lambda form's parameter list"))))



(define (syntax-rules-form? x)
  (and (list? x) (> (length x) 2) (eq? (car x) 'syntax-rules)))

(define (syntactical-comment? x)
  (and (list? x) 
       (not (null? x))
       (eq? (car x) (as-symbol syntactical-comment-designator))))




; ---------------------------------------------------------------------------------------------------
;;; Extraction of top level defined names from parsed Scheme expressions. 

;; Return the list of top-level defined names in the source list. Preserve the order of the source file occurrences of the defined names.
(define (defined-names source-list)
  (defined-names-1 source-list '()))

(define (defined-names-1 source-list res)
  (if (null? source-list) 
      (reverse res)
      (let ((form (car source-list)))
        (if (is-define-form? form)
            (defined-names-1 (cdr source-list) (cons (defined-name form) res))
            (if (syntactical-comment? form)
                (let ((section-name (section-name-comment? (comment-string-of-syntactical-comment form))))
                   (if section-name 
                       (defined-names-1 (cdr source-list) (cons (as-symbol section-name) res))
                       (defined-names-1 (cdr source-list) res)))
                (defined-names-1 (cdr source-list) res))))))
          

; syntactical comment selectors:

(define comment-string-of-syntactical-comment (make-selector-function 3 'comment-string-of-syntactical-comment))
(define comment-level-of-syntactical-comment (make-selector-function 2 'comment-level-of-syntactical-comment))

; This function takes the string of a syntactical comment and returns whether
; it is a section name comment. A positive answer returns the sectional comment name (a string without double colons).
(define (section-name-comment? comment-string)
  (let ((p1 (skip-chars-in-string comment-string white-space-char-list 0)))
    (if (looking-at-substring? comment-string p1 "::")
        (let ((p2 (find-in-string comment-string  #\: (+ p1 2))))    ; finding first colon at the end of name
          (if p2
              (substring comment-string (+ p1 2) p2)  ; returning portin of string between double colons
              #f))
        #f)))

; Return the defined name in x, given that x is a define form.
(define (defined-name x)
  (if (pair? (cadr x))
      (car (cadr x))
      (cadr x)))

; Return the body of def, given that def is a definition.
; Expected form of def: (define n body) or (define (n ...) body)
(define (body-of-definition def)
  (cddr def))

; (define (N pars-list) ...) or (define N (lambda (par-list) ...)) or (define N (lambda par-list ...))
; Return par-list, of #f in case N happens not to be a function definition.
; Type of return value is either list or symbol.
; Precondition: def is a defintion.
(define (parameters-of-definition def)
  (cond ((pair? (second def)) (cdr (second def)))
        ((symbol? (cadr def))
             (if (lambda-form? (third  def))
                 (second (third def))
                 #f))
        (else #f)))
  

; Return a list of expressions bound to names in let-form
(define (let-vals let-form)
  (let ((binding-forms (if (named-let? let-form)
                           (caddr let-form)
                           (cadr let-form))))
    (accumulate-right append '() (map cdr binding-forms))))

; Return a list of names bound in let-form
(define (let-names let-form)
  (let ((binding-forms (if (named-let? let-form)
                           (caddr let-form)
                           (cadr let-form))))
    (accumulate-right append '() (map (lambda (b) (list (car b))) binding-forms))))


; Return the bounded names of the form f.
; This function works on an arbitrary form.
; As a peculiarity, this function does not recognize the name in (define n ...) as a bound name.
; But the names (x y z) are bound in (define (n x y z) ...)
(define (bounded-names x)
 (let ((rinsed-x (no-syntactic-comments x)))
  (cond ((is-define-form? rinsed-x) (parameter-names-of-define rinsed-x))  
        ((and (let-form? rinsed-x) (named-let? rinsed-x)) (cons (second x) (let-names rinsed-x)))
        ((let-form? rinsed-x) (let-names rinsed-x))
        ((lambda-form? rinsed-x) (lambda-names rinsed-x))
        ((syntax-rules-form? rinsed-x) (syntax-rules-bounded-names rinsed-x))
        (else '()))))

(define (syntax-rules-bounded-names x)
  (let ((literals (cadr x))
        (syntax-rules-list (cddr x)))
    (if syntax-rules-list
        (let ((macro-name (caaar syntax-rules-list)))
           (cons macro-name literals))
        literals)))
   

; Eliminate syntactic comments from f
(define (no-syntactic-comments f)
 (cond ((list? f)
          (let ((rinsed-f (filter (negate syntactical-comment?) f)))
            (map no-syntactic-comments rinsed-f)))
       (else f)))


(define (parameter-names-of-define x)
  ; Return the bounded names in x, which is a define form
  ; Assume as a pre-condition that x is a define form.
    (cond ((pair? (cadr x))
           (let ((call-form (cadr x)))
             (cond ((list? call-form) (cdr call-form))
                   ((pair? call-form) 
                      (cond ((pair? (cdr call-form)) (append (proper-part (cdr call-form)) (list (first-improper-part (cdr call-form)))))
                            ((symbol? (cdr call-form)) (list (cdr call-form))))
                      ))))
;           ((symbol? (cadr x))     ; Sept. 14, 2004. These names are captured at lambda level.
;            (if (> (length x) 2)
;                (let ((y (caddr x)))  ; possible lambda form
;                  (if (and (pair? y) (eq? (car y) 'lambda))
;                      (let ((par (cadr y)))
;                        (cond ((symbol? par) (list par))
;                              ((list? par) par)
;                              ((pair? par) (append (proper-part par) (list (first-improper-part par))))))
;                      '()))
;                '()))
         (else '())))

; It is assumed as a precondition that f is a define form (weak assumption, which is actually checked for stability).
; Return a local definition comment. If no such comment exists, return #f.
(define (get-definition-comment-of-define-form f)
  (if (is-define-form? f)
      (let ((candidate (third f)))
        (if (syntactical-comment? candidate)
            candidate
            #f))
      #f))
  
  

; ---------------------------------------------------------------------------------------------------

; Piece the URL together from the url prefix and the url suffix.  Return #f if something is missing.
(define (url-of-scheme-knowledge entry)
  (if (and rnrs-url-prefix (>= (length entry) 4) (not (eq? 'none the-scheme-report-version)))
      (string-append rnrs-url-prefix (url-suffix-of-scheme-knowledge entry the-scheme-report-version))
      #f))



; ---------------------------------------------------------------------------------------------------

; Global variable that hold information about the source key and version for the currently processed source file.
; These are currently used for informational purposes.
(define actual-source-key #f)
(define actual-source-version #f)

;; Decorate the Scheme source-file with anchors and links.
;; Source-path is the name of the internal file with the Scheme source text - the one with syntactic comments (full path and extension).
;; Destination-path is the name of the html file with where the decorated Scheme source is to be written (full path and extension).
;; Source-list is the list of Scheme expressions in the source file - the parsed source file.
;; Defined-names is a list of name-definitions to which we link applied names when elucidating the program.
;; documented-names is a list of name descriptors, which are documented in the elucidated program.
(define (elucidate-program-source source-path destination-path source-list defined-names documented-names size source-key 
                                  source-version source-group)
 (let ((of destination-path))
  (if (file-exists? of) (delete-file of))
  (let* ((ip (open-input-file source-path))
         (op (open-output-file of))
         (source-program-colors (bg-text-link-vlink-colors (color-of-group source-group) black black black))
         (highest-version (highest-version-number source-key))
         (old-version-attributes (list 'css:background (rgb-color-encoding (color-of-group source-group))
                                       'css:background-position "top right"
                                       'css:background-repeat "no-repeat" 
                                       'css:background-attachment "fixed"
                                       'css:background-image (version-graphics-url source-version highest-version)))
        )
    
    ; Set global variables.
    (set! actual-source-key source-key)
    (set! actual-source-version source-version)

    (let ((html-ast (html (head (title "dummy")) (body "dummy")))
          (body-ast (body (bg-text-link-vlink-colors (color-of-group source-group) black black black)
                          (if (> highest-version starting-version) old-version-attributes '()) 
                          "dummy-body"))
          (font-ast (font 'size (as-string (if (eq? size 'small) 2 3)) "dummy-string"))
          (pre-ast (pre "dummy-string"))) 

      (render-start-tag-to-output-port html-ast op)
      (render-to-output-port
       (head 
        (link 'href (in-elucidator-stylesheet-directory "program.css") 'rel "stylesheet" 'title "program" 'type "text/css")
        (title "Source file"))
       op)

      (render-start-tag-to-output-port body-ast op)

      ; Version navigation at overall source file level
      (if (or (> source-version starting-version) (< source-version highest-version))
       (render-to-output-port
        (span
         (if (> source-version starting-version)
             (a (image "gray-left-arrow-large.gif" 
                       (string-append "The previous version" " (" (as-string (- source-version 1)) ")"))
                'href (string-append (source-file-name-html-file source-key (- source-version 1) size)
                                     ".html"))
             (image "gray-left-arrow-large-blank.gif" "")) ; entirely transparent - just for positioning purposes of the right arrow
         (horizontal-space 1)
         (if (< source-version highest-version)
             (a (image "gray-right-arrow-large.gif" 
                       (string-append "The next version" " (" (as-string (+ source-version 1)) ")"))
                'href (string-append (source-file-name-html-file source-key (+ source-version 1) size)
                                     ".html"))
             "")
         ) op))

      (render-start-tag-to-output-port font-ast op) (render-start-tag-to-output-port pre-ast op) 
      (elucidate-program-source-1 ip op source-list defined-names documented-names size source-key source-version #f (length source-list))
      (render-end-tag-to-output-port pre-ast op) (render-end-tag-to-output-port font-ast op)
      (render-to-output-port (div (vertical-space end-file-empty-lines)) op)
      (render-end-tag-to-output-port body-ast op)
      (render-end-tag-to-output-port html-ast op)

      (close-input-port ip)
      (close-output-port op) ))))


(define (version-graphics-url m n)
  (string-append 
   "url('images/vers-"
   (if (and (<= n 3) (<= m 3)) (string-append (as-string m) "-") (if (= n m) "n-" "m-"))
   (if (and (<= n 3) (<= m 3)) (string-append (as-string n)) "n")
   ".gif')"))

(define (add-file-extension f ext)
  (string-append f "." ext))

; Elucidate each program source file in source-list. source-length is the length of source-list.
(define (elucidate-program-source-1 ip op source-list defined-names documented-names size source-key source-version raw? source-length)
 (set! last-define-a-name #f)
 (skip-white-space ip op)
 (if (not (eof-object? (peek-char ip)))
     (let ((form (car source-list))
           (next-form (if (> source-length 1) (cadr source-list) #f)))
       (elucidate-program-form ip op form next-form defined-names '() documented-names size source-key source-version raw? #t)
       (elucidate-program-source-1 ip op (cdr source-list) defined-names documented-names size source-key source-version raw? (- source-length 1)))
     ))

; The name of the definition, in which we currently are located.
(define enclosing-definition-name #f)

; The name of the last definition from which an anchor name has been written to the output port
; Set imperatively by elucidate-program-form
(define last-define-a-name #f)

; The central elucidation function. ip and op are input and output ports. 
; f is the form to be elucidated.  nf is the next form, or #f if no such form exist,
; or of the next form is unknown (not important for the processing).
; defined-names and documented-names are lists. 
; defined-names is the list of names defined in the context of f. Names defined locally in f shadow the defined names.
; shadowing-names is a simple list of names, which shadows the defined names as seen from f.
; size is 'small or 'large.
; Source-key is the source-key of the file, we are elucidating.
; at-top? is true for top-level stuff.
; raw? is true for quoted forms, and from forms which do not need any linking or awareness of Scheme details. 
; The optional parameter trailing-parenthesis applies only for pairs; It controls whether to match a trailing parenthesis.
; The value is no in case we deal with elements in a proper list.
(define (elucidate-program-form ip op f nf defined-names shadowing-names documented-names size source-key source-version raw? at-top? . optional-parameter-list)

 (let ((trailing-parenthesis (optional-parameter 1 optional-parameter-list 'yes)))
  (cond ((null? f)
          (if (eq? trailing-parenthesis 'yes)
              (begin
                (skip-white-space ip op)
                (match-start-parenthesis ip op)
                (skip-white-space ip op)
                (match-end-parenthesis ip op)
                (skip-white-space ip op)
               )))

        ((quote-in-input? ip f)
          (begin
           (write-char #\' op)
           (elucidate-program-form ip op (cadr f) #f '() shadowing-names documented-names size source-key source-version #t #f)  ; Passing #t as the 10th parameter prevents formatting of quoted form.
           (skip-white-space ip op)))

        ; We need to pass defined-names to the recursive call of elucidate-program-form, but 
        ; linkings to defined-names should only be in effect within unquotings. This is not yet made.
        ((backquote-in-input? ip f) ; @d
          (begin
           (write-char #\` op)
           (elucidate-program-form ip op (cadr f) #f defined-names shadowing-names documented-names size source-key source-version #t #f)   ; Passing #t as the 10th parameter prevents formatting of quoted form.
           (skip-white-space ip op)))

        ((unquote-in-input? ip f)   ; @e handles both unquote and and unquote-splicing
          (begin
           (write-char #\, op)
           (let ((ch (peek-char ip)))
             (if (eqv? #\@ ch)
                 (begin
                   (read-char ip)
                   (write-char #\@ op)
                   (elucidate-program-form ip op (cadr f) #f defined-names  shadowing-names documented-names size source-key source-version #f #f)
                   )
                 (begin
		   (elucidate-program-form ip op (cadr f) #f defined-names shadowing-names documented-names size source-key source-version #f #f)
                 )))
           (skip-white-space ip op)
          )
        )
         
        ((eof-object? f) ; nothing
        )

        ((symbol? f) (if raw?
                         (match-simple-symbol f ip op)
                         (match-symbol f ip op defined-names shadowing-names size source-version)) 
                     (skip-white-space ip op))

        ((string? f) (match-string f ip op)
                     (skip-white-space ip op))

        ((number? f) (match-number f ip op)
                     (skip-white-space ip op))

        ((char? f) (match-char f ip op)
                   (skip-white-space ip op))

        ((and (eq? scheme-system 'gosh) (regexp? f)) (match-regexp f ip op)
                   (skip-white-space ip op))

        ((boolean? f) (match-boolean f ip op)
                      (skip-white-space ip op))

        ((unquote-form? f)  ; unquote or unquote-splicing
           (skip-white-space ip op)
           (match-start-parenthesis ip op)
           (skip-white-space ip op)
           
           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; the unquote or unqute-splice name
           (skip-white-space ip op)

           (elucidate-program-form ip op (cadr f) #f defined-names shadowing-names documented-names
                                   size source-key source-version #f #f)  ; the unquoted form
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op))

        ((and (or (quote-form? f) (quasiquote-form? f)) (not raw?))
           (skip-white-space ip op)
           (match-start-parenthesis ip op)
           (skip-white-space ip op)
           
           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; the quote name
           (skip-white-space ip op)

           (elucidate-program-form ip op (cadr f) #f defined-names shadowing-names documented-names 
                                   size source-key source-version #t #f)  ; the quoted form
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op))

        ((syntactical-comment? f)  ; @h   ; before aug 12, 2005:  (and (syntactical-comment? f) (not raw?)). There may be comments in raw forms.
           (let ((sectional-comment (section-name-comment? (comment-string-of-syntactical-comment f)))
                 (comment-level (comment-level-of-syntactical-comment f))
                )
            (if sectional-comment 
                (set! enclosing-definition-name sectional-comment))
            (set! the-comment-level comment-level)
            (match-syntactical-comment-without-output ip)
            (read-char ip)  ; eats the empty after each syntactical comment
                            ; compensates for this accedential (and wrong) behaviour of lexical-to-syntactical-comments! in SchemeDoc

            (if sectional-comment 
                (begin
                  (render-to-output-port (total-doc-navigator (as-symbol sectional-comment) documented-names size source-key source-version) op)
                  ; (render-to-output-port (br) op)
            ))

            ; write anchor name of next defined form before the rendering of the comment
            (if (is-define-form? nf)
                (let ((def-name (defined-name nf)))
                  (render-to-output-port (a-name (as-string def-name)) op)
                  (set! last-define-a-name def-name)))

            ; render the comment
            (render-syntactical-comment! (comment-string-of-syntactical-comment f) comment-level op)
           )
        )

        ((and (is-define-form? f) (not raw?)) ;@a           Both ordinary define and define-syntax, and anything with (define-... ...)
         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn)))

           (if at-top?
               (if elucidator-verbose-mode (display-message (defined-name f))))

           (set! enclosing-definition-name (defined-name f)) ;@b

           (skip-white-space ip op)
           (if (not (eq? last-define-a-name (defined-name f)))  ; in case there was no comment before the define form
               (render-to-output-port (a-name (as-string (defined-name f))) op))
           (set! last-define-a-name #f) ; forget about the last written anchor name

           (if at-top?
               (let* ((local-definition-comment (get-definition-comment-of-define-form f))
                      (comment-level (cond (the-comment-level the-comment-level)
                                           (local-definition-comment (comment-level-of-syntactical-comment local-definition-comment))
                                           (else #f)))
                     ) 
                 (render-to-output-port 
                    (total-doc-navigator (defined-name f) documented-names size source-key source-version comment-level)
                    op)
                 ; (render-to-output-port (br) op)
           ))

           (match-start-parenthesis ip op)
           (skip-white-space ip op)

           ; The define 'keyword'
           (match-symbol (car f) ip op '() shadowing-names size source-version)

           (skip-white-space ip op)

;          Elided November 22, 2005
;            (if (memq (cadr f) shadowing-names)  ; a very special case, and probably questionable
;                                                 ; introduced for the sake of defines in syntax rules
;                (elucidate-restricted-define-form ip op (cadr f) size)
;                (begin
;                    (write-string-to-port (start-tag-of (span 'class "signature")) op)
;                      ; make sure that only the next form (no comments) is matched here:
;                      (elucidate-restricted-define-form ip op (cadr f) size)    
;                    (write-string-to-port (end-tag-of (span 'class "signature")) op)))

           ; the defined element - the second element of a define form. A symbol or a pair.
           (cond ((and (symbol? (cadr f)) (memq (cadr f) shadowing-names)) ; a very special case, and probably questionable
                                                                           ; introduced for the sake of defines in syntax rules
                    (elucidate-restricted-define-form ip op (cadr f) size))
                 ((and (pair? (cadr f)) (eq? (car (cadr f)) 'quote))       ; Again a very, very special case (define 'x ...). 
                                                                           ; Requested by newton@mit.edu in November 2005
                      (elucidate-program-form ip op (cadr f) (cddr f)
                                              defined-names shadowing-names documented-names size source-key source-version raw? #f)
                  ) 
                 (else  ; NORMAL CASES (define symbol ...) or (define (symbol ...) ...)
                     (begin
		      (write-string-to-port (start-tag-of (span 'class "signature")) op)
 					; make sure that only the next form (no comments) is matched here:
 		      (elucidate-restricted-define-form ip op (cadr f) size)    
 		      (write-string-to-port (end-tag-of (span 'class "signature")) op)))
            )                    

           (skip-white-space ip op)
           (for-each 
              (lambda (sf nf) 
                 (skip-white-space ip op)
                 (elucidate-program-form 
                     ip op sf nf 
                     reduced-defined-names ;@c
                     (append shadowing-names bn)
                     documented-names size source-key source-version raw? #f))
              (cddr f)
              (if (null? (cddr f)) '() (append (cdddr f) (list #f)))  ; next forms, of same length as (cddr f) because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (set! the-comment-level #f)
           (skip-white-space ip op)))

        ((and (lambda-form? f) (not raw?)) 
         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn)))

           (skip-white-space ip op)

           (match-start-parenthesis ip op)
           (skip-white-space ip op)

           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; lambda symbol

           (skip-white-space ip op)

           (elucidate-lambda-parameters ip op (cadr f) size) ; lambda parameters

           ; lambda body
           (skip-white-space ip op)  
           (for-each 
              (lambda (sf nf) 
                 (skip-white-space ip op)
                 (elucidate-program-form 
                     ip op sf nf 
                     reduced-defined-names 
                     (append shadowing-names bn)
                     documented-names size source-key source-version raw? #f))
              (cddr f)
              (if (null? (cddr f)) '() (append (cdddr f) (list #f)))  ; next forms, of same length as (cddr f) because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op)))

        ((and (let-form? f) (named-let? f) (not raw?))   ; comments within the let form causes problems - THINK!
                           
         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn))
               )

           (skip-white-space ip op)

           (match-start-parenthesis ip op)
           (skip-white-space ip op)

           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; let symbol

           (skip-white-space ip op)
           (write-string-to-port (start-tag-of (span 'class "local-name-binding")) op)
             (match-simple-symbol (cadr f) ip op)                   ; the let name
           (write-string-to-port (end-tag-of (span 'class "local-name-binding")) op)    
           (skip-white-space ip op)

           (elucidate-let-bindings ip op (caddr f) defined-names
                 shadowing-names 
                 documented-names size source-key source-version 'let raw?)

           ; let body
           (skip-white-space ip op)  
           (for-each 
              (lambda (sf nf) 
                 (skip-white-space ip op)
                 (elucidate-program-form 
                     ip op sf nf 
                     reduced-defined-names 
                     (append shadowing-names bn)   ; includes the name of the let
                     documented-names size source-key source-version raw? #f))
              (cdddr f)
              (if (null? (cdddr f)) '() (append (cdr (cdddr f)) (list #f)))  ; next forms, of same length as (cddr f)
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op)))

        ((and (let-form? f) (not raw?))   ; comments within the let form causes problems - THINK!

         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn))
                (let-kind (car f))
               )

           (skip-white-space ip op)

           (match-start-parenthesis ip op)
           (skip-white-space ip op)

           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; let symbol

           (skip-white-space ip op)

           (elucidate-let-bindings ip op (cadr f) defined-names
                 (if (eq? let-kind 'letrec) (append bn shadowing-names) shadowing-names) 
                 documented-names size source-key source-version let-kind raw?)

           ; let body
           (skip-white-space ip op)  
           (for-each 
              (lambda (sf nf) 
                 (skip-white-space ip op)
                 (elucidate-program-form 
                     ip op sf nf 
                     reduced-defined-names 
                     (append shadowing-names bn)
                     documented-names size source-key source-version raw? #f))
              (cddr f)
              (if (null? (cddr f)) '() (append (cdddr f) (list #f)))  ; next forms, of same length as (cddr f) because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op)))

        ;let-syntax and letrec-syntax here

       ((and (syntax-rules-form? f) (not raw?))   
                           
         (let* ((bn (bounded-names f))
                (reduced-defined-names (list-difference-2 defined-names bn))
               )

           (skip-white-space ip op)

           (match-start-parenthesis ip op)
           (skip-white-space ip op) 

           (match-symbol (car f) ip op '() shadowing-names size source-version)  ; the syntax-rules 'keyword'

           (skip-white-space ip op)
           (elucidate-list-simple ip op (cadr f)  size)  ; the literal list
           (skip-white-space ip op)   

           ; the pattern-template list. May in fact include syntactical comments which we handle by a recursive call to elucidate-program-form
           (for-each 
              (lambda (pat-templ nf) 
                 (skip-white-space ip op) 
                 (if (syntactical-comment? pat-templ)    
                     (elucidate-program-form ip op pat-templ nf reduced-defined-names (append shadowing-names bn) documented-names size source-key source-version raw? #f)
                     (elucidate-pattern-template-form ip op pat-templ reduced-defined-names (append shadowing-names bn) documented-names size source-key source-version raw? #f))
                 (skip-white-space ip op))
              (cddr f)
              (if (null? (cddr f)) '() (append (cdddr f) (list #f)))  
	      )  

           (skip-white-space ip op) 
           (match-end-parenthesis ip op)
           (skip-white-space ip op)))


        ((pair? f) ; @h
          (let* ((bn '()) ; (bounded-names f)
                 (reduced-defined-names (list-difference-2 defined-names bn))
                )
            (skip-white-space ip op)
            
            (if (eq? trailing-parenthesis 'yes) (match-start-parenthesis ip op))

            ; Process (car f)
            (elucidate-program-form
               ip op (car f) #f   ; no next form passed - is that safe?
               reduced-defined-names shadowing-names documented-names size source-key source-version raw? #f)
            (skip-white-space ip op)

            ; Process (cdr f)
            (let ((next-ch (peek-char ip)))
              (if (eqv? next-ch #\.)  ; full dot notation, perhaps improper list
                  (begin
                    ; handle the dot on input and look what is next
                    (read-char ip)
                    (let ((next-ch (peek-char ip)))
                      (if (white-space? next-ch) 

                          (begin                         ; dot notation
                           (write-char #\. op)  
                           (skip-white-space ip op)
                           (elucidate-program-form
			    ip op (cdr f) #f 
			    reduced-defined-names shadowing-names documented-names size source-key source-version raw? #f))

                          (begin                         ; initial dot in lexeme, such as .56 or ...

                            ; do not write the dot.
                            ; The subsequent (read ip) reads the rest of the lexeme, but is actually not used.
                            ; The textual rendering in the elucidator is controlled by (cdr f)

                            (elucidate-program-form
			     ip op (cdr f) #f   
			     reduced-defined-names shadowing-names documented-names size source-key source-version raw? #f 'no)
                          )
                       )
                      )
                  )
                  (let ((rest (cdr f)))
                    (elucidate-program-form
                      ip op rest #f   
                      reduced-defined-names  shadowing-names documented-names size source-key source-version raw? #f 'no)
                  )
               )
            )

            (skip-white-space ip op)
            (if (eq? trailing-parenthesis 'yes) (match-end-parenthesis ip op))
            (skip-white-space ip op)
           )
        )

        ((vector? f) ; @i
          (let* ((lf (vector->list f))
                 (bn '()) ; (bounded-names f)
                 (reduced-defined-names (list-difference-2 defined-names bn)))
           (match-number-sign ip op)
;           (skip-white-space ip op)
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf nf)
                 (skip-white-space ip op)
                 (elucidate-program-form 
                       ip op sf nf
                       reduced-defined-names 
                       shadowing-names
                       documented-names
                       size source-key source-version raw? #f))
              lf
              (if (null? lf) '() (append (cdr lf) (list #f)))  ; next forms, of same length as f because of trailing #f
           )
           (skip-white-space ip op)
           (match-end-parenthesis ip op)
           (skip-white-space ip op))
        )

        (else (error (string-append "elucidate-program-form: unknown kind of expression" (as-string f))))))
  )

; Not used - experimental.
(define (dot-notation-ahead? ip)
 (let ((ch1 (peek-char ip)))
  (if (eqv? ch1 #\.)
      (let ((ch2 (peek-char ip)))  ; WRONG - only look ahead of length one
         (white-space? ch2)
      )
      #f)))
      


; A specialized procdures which reads through a syntactical comment on ip without
; outputting anyting on op
(define (match-syntactical-comment-without-output ip)
  (read-char ip)  ;  read start-parenthesis
  (read ip)       ;  read comment symbol
  (read ip)       ;  read comment level
  (read ip)       ;  read comment string 
  (read-char ip)       ;  read end-parenthesis which follows right next to the string
)

; ---------------------------------------------------------------------------------------------------
; Processing of a syntactical comment string via a state machine.

; An internal variable in which we register whether the comment string parameter of
; render-syntactical-comment is considered a sectional-comment.
(define indeed-section-comment #f)

; Render comment-string, at comment-level to the output port op.
; This is the 'main function' for these purposes which uses some helping functions,
; most notably the state machine in do-render-syntactical-comment!.
(define (render-syntactical-comment! comment-string comment-level op)
 (let ((sectional-comment (section-name-comment? comment-string)))
  (set! indeed-section-comment sectional-comment)
  (let ((comment-string-1 (strip-trailing-characters (list #\newline #\return) comment-string)))
    (set! state-list '())
    (cond (sectional-comment
	   (write-string-to-port (start-tag-of (div 'class "sectional-comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )

          ((= comment-level 1)
	   (write-string-to-port (start-tag-of (span 'class "comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )

          ((= comment-level 2)
	   (write-string-to-port (start-tag-of (div 'class "schemedoc-definition-comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )

          ((= comment-level 3)
	   (write-string-to-port (start-tag-of (div 'class "schemedoc-section-comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )

          ((= comment-level 4)
	   (write-string-to-port (start-tag-of (div 'class "schemedoc-abstract-comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )

	  (else    ; higher comment levels
	   (write-string-to-port (start-tag-of (div 'class "comment")) op)
	   (write-string-to-port (make-string comment-level #\;) op)
           (write-string-to-port (as-string #\space) op)
          )
    )

    (do-render-syntactical-comment!
     comment-string-1 comment-level 0 (string-length comment-string-1)
     'normal "" op)

    ; Similar end tag: Coupled to the conditional above
    (let ((a-div (div "dummy"))
          (a-span (span "dummy")))
      (cond (sectional-comment
             (write-string-to-port (end-tag-of a-div) op))
            ((= comment-level 1)
             (write-string-to-port (end-tag-of a-span) op)
             (render-to-output-port (br) op)
             )
            ((= comment-level 2)
             (write-string-to-port (end-tag-of a-div) op))
            (else                       ; higher comment levels
             (write-string-to-port (end-tag-of a-div) op))
            ))

    )))

(define debugging-syntactical-comment-rendering #f)
(define state-list '())

(define (do-render-syntactical-comment! c-str c-lev inptr inlength current-state collected-str op)
  (if (= inptr inlength)
      (if (and (eq? current-state 'source-char) (> (string-length collected-str) 0))  ; pending source marker at end of line
            (render-to-output-port (render-source-char collected-str) op))
      (let* ((inch (string-ref c-str inptr))
             (trans-res (syntactical-comment-transition current-state inch collected-str c-lev))
             (next-state (car trans-res))
             (toput (cadr trans-res))
             (collected-str (caddr trans-res))
            )
       (if debugging-syntactical-comment-rendering
            (set! state-list (cons (list (as-string inch) next-state collected-str) state-list)))
       (cond ((ast? toput) (render-to-output-port toput op))
             ((string? toput) (write-string-to-port toput op))
             (else (laml-error "do-render-syntactical-comment!: Either AST of string expected.")))

       (do-render-syntactical-comment! c-str c-lev (+ 1 inptr) inlength next-state collected-str op)
  )))


(define sectional-comment-char #\:)
(define sectional-comment-char-string (as-string sectional-comment-char))
(define elucidator-marker-char-string (as-string elucidator-marker-char))

; A simple version of html-protect which only work at singleton character strings (strings of length 1).
(define (hp single-string-char)
 (cond ((equal? single-string-char "<") "&lt;")
       ((equal? single-string-char ">") "&gt;")
       (else single-string-char)))

(define (syntactical-comment-transition in-state ch collected-str c-level)
 (let ((char (as-string ch))
       (expl (string-append "A link to a program source marker in " (as-string previous-strong-program-word))))
   (cond 
        ((and (symbol? in-state) (eq? in-state 'normal))
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-initial-1 "" ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign "" ""))
                  ((equal? char (as-string #\newline))                 (list 'newline "" ""))
                  (else                                                (list 'normal (hp char) collected-str)))) 

        ((and (symbol? in-state) (eq? in-state 'colon-initial-1))
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-initial-2 "" ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign (as-string sectional-comment-char) ""))
                  ((equal? char (as-string #\newline))                 (list 'newline
                                                                                 (string-append (as-string sectional-comment-char)) ""))
                  (else                                                (list 'normal
                                                                              (string-append (as-string sectional-comment-char) (hp char)) 
                                                                              collected-str))))

        ((and (symbol? in-state) (eq? in-state 'colon-initial-2))
            (cond ((equal? char sectional-comment-char-string)         (error "syntactical-comment-transition: more than two colons not allowed"))
                  ((equal? char elucidator-marker-char-string)         (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                 (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                (list 'within-section-name "" 
                                                                             (string-append collected-str char)))))

        ((and (symbol? in-state) (eq? in-state 'within-section-name))
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-after-1 "" collected-str))
                  ((equal? char elucidator-marker-char-string)         (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                 (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                (list 'within-section-name "" 
                                                                                 (string-append collected-str char)))))

        ((and (symbol? in-state) (eq? in-state 'colon-after-1))
            (cond ((equal? char sectional-comment-char-string)         (list 'normal (render-sectional-comment collected-str) ""))
                  ((equal? char elucidator-marker-char-string)         (error "syntactical-comment-transition: @ in section name not allowed"))
                  ((equal? char (as-string #\newline))                 (error "syntactical-comment-transition: newline not allowed in section name"))
                  (else                                                (list 'within-section-name "" 
                                                                             (string-append collected-str sectional-comment-char-string char)))))

        ((and (symbol? in-state) (eq? in-state 'colon-after-2))  ; blind
            (cond ((equal? char sectional-comment-char-string)         (error "syntactical-comment-transition: three colons not allowed"))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign (render-sectional-comment collected-str) ""))
                  ((equal? char (as-string #\newline))                 (list 'newline (render-sectional-comment collected-str) ""))
                  (else                                                (list 'normal (span (render-sectional-comment collected-str) (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'at-sign))
            (cond ((equal? char sectional-comment-char-string)         (error "syntactical-comment-transition: colon after source mark char not allowed"))
                  ((equal? char elucidator-marker-char-string)         (error "syntactical-comment-transition: double @ not allowed"))
                  ((equal? char (as-string #\newline))                 (error "syntactical-comment-transition: newline after @ not allowed"))
                  (else                                                (list 'source-char "" char))))

        ((and (symbol? in-state) (eq? in-state 'source-char))
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-initial-1 elucidator-marker-char-string ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign elucidator-marker-char-string ""))
                  ((equal? char (as-string #\space))                   (list 'normal
                                                                                 (render-source-char collected-str) ""))
                  ((equal? char (as-string #\return))                  (list 'source-char "" collected-str))  ; just eat the return - char 13
                  ((equal? char (as-string #\newline))                 (list 'newline 
                                                                                 (render-source-char collected-str) ""))
                  (else                                                (list 'normal (string-append elucidator-marker-char-string (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'space-after-source-char))  ;blind
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-initial-1 (render-source-char collected-str) ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign (render-source-char collected-str) ""))
                  (else                                                (list 'normal (span (render-source-char collected-str) (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'newline))
            (cond ((equal? char sectional-comment-char-string)         (list 'colon-initial-1 (comment-glyph c-level) ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign (comment-glyph c-level) ""))
                  ((equal? char (as-string #\space))                   (list 'newline-and-spaces "" char))
                  ((equal? char (as-string #\newline))                 (list 'newline (comment-glyph c-level) ""))
                  (else                                                (list 'normal (string-append (comment-glyph c-level) " " (hp char)) ""))))

        ((and (symbol? in-state) (eq? in-state 'newline-and-spaces))
            (cond ((equal? char (as-string #\space))                   (list 'newline-and-spaces "" (string-append collected-str char)))
                  ((equal? char sectional-comment-char-string)         (list 'colon-initial-1
                                                                              (string-append (comment-glyph c-level collected-str)) ""))
                  ((equal? char elucidator-marker-char-string)         (list 'at-sign
                                                                              (string-append (comment-glyph c-level collected-str)) ""))
                  ((equal? char (as-string #\newline))                 (list 'newline 
                                                                             (string-append (comment-glyph c-level collected-str)) ""))
                  (else                                                (list 'normal 
                                                                             (string-append 
                                                                                (comment-glyph c-level collected-str) " " (hp char)) ""))))

        (else                                                          (error (string-append 
                                                                                   "syntactical-comment-transition error: unknown state "
                                                                                   (as-string in-state)))
        )

  )))

;(define (comment-glyph comment-level . in-between-newline-and-semicolon)
; (let ((in-between (if (null? in-between-newline-and-semicolon) #f (car in-between-newline-and-semicolon))))
;  (string-append 
;    (as-string #\newline)
;    (if in-between in-between "")
;    (make-string comment-level #\;)
;  )))

(define (comment-glyph comment-level . in-between-semicolon-and-txt)
 (let ((in-between (if (null? in-between-semicolon-and-txt) #f (car in-between-semicolon-and-txt))))
  (string-append 
    (as-string #\newline)
    (make-string comment-level #\;)
    (if in-between in-between "")
  )))

(define (render-sectional-comment section-name)
  (if indeed-section-comment
      (begin
       (set! indeed-section-comment #f)  ; such that no other section names in this comment are rendered as sectioin comments
       (span
        (a-name section-name)
        (if show-sectional-comment-name
	    (b (font-color red section-name))
	    ""))
      )
      (string-append 
       (as-string sectional-comment-char) (as-string sectional-comment-char)
       section-name
       (as-string sectional-comment-char) (as-string sectional-comment-char))))
      

(define (render-source-char source-char-string)
  (span 
   (a-name 
    (string-append
     (as-string enclosing-definition-name) ;@i
     "-@" source-char-string)) 
   (doc-source-marker-link              ;@a
    documentation-source-marker-occurences
    source-char-string
    enclosing-definition-name)
  ))


; End state machine and processing of syntactical comment string
; ---------------------------------------------------------------------------------------------------
  

; Return a link to the documentation frame. NOT USED.
(define (doc-navigator name documented-names)
  (let ((res (assq name documented-names)) )
    (if res
        (let* ((res-docid (cadr res))
               (weak-strong (caddr res))
               (res-doc-title (cdr (assq res-docid documentation-key-title-alist)))
              )
          (span (a-tag-target (string-append "documentation.html" "#" (as-string res-docid))
                             (cond ((eq? strong-weak 'strong) (image "doc-left.gif" title))
                                   ((eq? strong-weak 'weak) (image "doc-left-weak.gif" title))
                                   (else (error "doc-link: problems determining strong or weak documentation link")))
                             "documentation-frame"
               )
               (br)))
        "")))


(define (total-doc-navigator name documented-names size source-key source-version . optional-parameter-list)
 (let ((comment-level (optional-parameter 1 optional-parameter-list #f))
       (br-necessary? #f)   ; becomes #t per assignment if a trailing newline (br) is necessary.
      )
  (let* ((doc-entries 
            (filter (lambda (doc-name-entr) (eq? name (name-of-documented-name-entry doc-name-entr))) documented-names))
         (reversed-doc-entries (reverse doc-entries))
         (unique-reversed-doc-entries-0
           (remove-duplicates-by-predicate
             reversed-doc-entries
             (lambda (x y) (and (eq? (cadr x) (cadr y)) (eq? (caddr x) (caddr y))) )))
         (unique-reversed-doc-entries (remove-redundant-weak-entries unique-reversed-doc-entries-0))
        )
   (span   ;@a

    ; Size navigation:
    (if (or make-large-source-files? (eq? default-program-font-size 'large))
     (begin
      (set! br-necessary? #t)
      (span
       (if (eq? size 'small) 
           (a-tag (string-append (source-file-name-html-file source-key source-version 'large) ".html" "#" (as-string name))
                  (image "small-square.gif" "Show source file in large font"))
           (a-tag (string-append (source-file-name-html-file source-key source-version 'small) ".html" "#" (as-string name))
                  (image "small-square.gif" "Show source file in small font"))
           )
       ))
     '())

    ; Cross reference navigation:
    (if (not (eq? cross-reference-index-support 'none))
     (begin 
      (set! br-necessary? #t)
      (let* ((name-string (as-string name))
             (name-first-letter (as-string (string-ref name-string 0))))
        (span
         (a-tag-target
          (if (eq? cross-reference-index-support 'per-letter)
              (string-append "cross-reference-index" "-" (hygienic-file-character (downcase-string name-first-letter)) 
                             ".html" "#" name-string)
              (string-append "cross-reference-index" ".html" "#" name-string))
          (image "small-green-up-triangle.gif" 
                 (string-append "In " source-key ": " "Link from " name-string " to it's cross reference table entry"))
          "control-frame")
         )))
     '())

    ; Documentation navigation:
    (if (not (null? unique-reversed-doc-entries))
        (begin
         (set! br-necessary? #t)
         (map 
             (lambda (de)
               (let* ((doc-id (doc-id-of-documented-name-entry de))
                      (strong-weak (doc-kind-of-documented-name-entry de))
                      (given-version (version-of-documented-name-entry de))
                      (number (cdr (assq doc-id documentation-key-numbering-alist)))
                      (doc-entry-title (cdr (assq doc-id documentation-key-title-alist))))
                 (cond ((and given-version (= given-version source-version))
                         (doc-link name doc-id (string-append number ". " doc-entry-title) strong-weak given-version))
                       ((and given-version (not (= given-version source-version)))  
                         "")                                                  ; suppress linking from versions different from given-version
                       (else 
                         (doc-link name doc-id (string-append number ". " doc-entry-title) strong-weak given-version)))
               )       
             )
             unique-reversed-doc-entries))
        '())

     ; Version navigation:
     (let ((highest-version (highest-version-number source-key)))
       (if (> highest-version starting-version)
           (begin
            (set! br-necessary? #t)
            (span
             (if (> source-version starting-version)  ; need for version back navigation
                 (let ((exists-prev-version? (find-defining-name name source-key (- source-version 1))))
                   (if exists-prev-version?
                       (span
                         
                         (if (definition-updated? name source-key source-version)
                             (span 
                               (a (image "gray-left-arrow.gif" 
                                         (string-append "The previous version" " (" (as-string (- source-version 1)) ")"))
                                  'href (string-append (source-file-name-html-file source-key (- source-version 1) size)
                                                       ".html" "#" (as-string name)))
                               (image "updated.gif" (string-append "Updated compared with version " (as-string (- source-version 1))))
                             )
                             (a (image "gray-left-arrow.gif" 
                                       (string-append "The previous version" " (" (as-string (- source-version 1)) ") "
                                                      "which is identical with the current version."))
                                'href (string-append (source-file-name-html-file source-key (- source-version 1) size)
                                                     ".html" "#" (as-string name))))
                       )
                       (let ((renamed (is-definition-renamed name source-key source-version)))
                           (if renamed
                               (span
				(a (image "gray-left-arrow.gif" 
					  (string-append "The previous version (" (as-string (- source-version 1)) ")"
                                                         " named " (as-string renamed) ))
				   'href (string-append (source-file-name-html-file source-key (- source-version 1) size)
							".html" "#" (as-string renamed)))
				(image "renamed.gif" 
                                 (string-append "Probably a renaming of " (as-string renamed) " from version "
                                                (as-string (- source-version 1))))
				)
                               (let* ((moved (is-definition-moved name source-key source-version)))
                                 (if moved
                                     (let ((moved-from-key (car moved))
                                           (moved-from-version (cdr moved)))
                                       (span
                                        (a (image "gray-left-arrow.gif" 
                                                  (string-append "The version that was moved: " (as-string name) " in "  
                                                     moved-from-key " version " (as-string (- source-version 1)) ))
                                           'href (string-append (source-file-name-html-file moved-from-key moved-from-version size)
                                                                ".html" "#" (as-string name)))
                                        (image "moved.gif" 
                                               (string-append "Probably moved from " (as-string moved-from-key) ", version "
                                                              (as-string moved-from-version)))))
                                     (image "new.gif" (string-append "New in this version" " (" (as-string source-version) ")"))))))
                   )    
                 )
                 '()
             )
 
             (if (< source-version highest-version)   ; need for version forward navigation
                 (let ((exists-next-version? (find-defining-name name source-key (+ source-version 1))))
                   (if exists-next-version?
                       (a (image "gray-right-arrow.gif" (string-append "The next version" " (" (as-string (+ source-version 1)) ")"))
                          'href (string-append (source-file-name-html-file source-key (+ source-version 1) size)
                                               ".html" "#" (as-string name)))
                       (image "no-pass-sign.gif" 
                              (string-append "Dead End - Not in next version" " (" (as-string (+ source-version 1)) ")"))
                   )
                 )
                 '()
             )
           ))
           '()))
   
     ; SchemeDoc back linking
     (if (and schemedoc-back-linking? (number? comment-level) (= comment-level 2))   
         (begin
           (set! br-necessary? #t)
           (a (image "small-prev.gif" "SchemeDoc Manual entry")
              'href (string-append "../" source-key ".html" "#" (as-string name))))
         '())

     (if br-necessary?
         (br)
         '())
           
  ))))


; Is the definition behind (the defining name) name updated in source-version compared with
; (- source-version 1).
(define (definition-updated? name source-key source-version)
  (let ((new-definition (find-definition-in-source-file-map name source-key source-version))
        (older-definition  (find-definition-in-source-file-map name source-key (- source-version 1)))
       )
   (not (definitions-equal? new-definition older-definition))))

; Is the definition behind (the defining name) name (in source-key of version source-version)
; a renaming of another abstraction in version (-source-version 1).
; If it is, return the name of it as of (-source-version 1).
; As a precondition it is assumed that version (-source-version 1) exists and is well-defined. 
(define (is-definition-renamed name source-key source-version)
  (let* ((definition-behind-name (find-definition-in-source-file-map name source-key source-version))
         (is-definition-new-version? (function-define-form? definition-behind-name))
         (parameters-new-version (if is-definition-new-version? (parameters-of-definition definition-behind-name) #f))
         (body-of-definition-new-version (if is-definition-new-version? (body-of-definition definition-behind-name) #f))
         (similar-definition-old-version 
             (find-in-list
              (lambda (old-version-form)
                (if (and (function-define-form? old-version-form) is-definition-new-version?)
                    (and (bodies-equal? (body-of-definition old-version-form) body-of-definition-new-version)
                         (parameters-of-definition old-version-form) parameters-new-version  ; tests if we deal with function definitions  
                         (parameters-equal? (parameters-of-definition old-version-form) parameters-new-version))
                    #f))
              (get-source-list-from-source-file-map source-key (- source-version 1)))))
    (if similar-definition-old-version
        (defined-name similar-definition-old-version)
        #f)))

; Is the definition behind (the defining name) name (in source-key of version source-version)
; moved from a definition of the same name (and structurally equal parameters and bodies) in another source file.
; Return the source-key and source-version of the located definition of name, as a cons pair.
; Prefer source-version as high as possible.
; Return #f if not moved.
(define (is-definition-moved name source-key source-version)
  (let* ((definition-behind-name (find-definition-in-source-file-map name source-key source-version))
         (candidates (find-definitions-named name))
         (candidates-not-self (filter (lambda (c) (not (equal? (as-string source-key) (car c))))
                                      candidates))
         (equal-candidates  ; those candidates with the exact same defintion as definition-behind-name
           (filter
             (lambda (c)
               (let* ((candidate-form (find-definition-in-source-file-map name (car c) (cdr c))))
                 (definitions-equal? candidate-form definition-behind-name)))
           candidates-not-self))
        )
    (if (null? equal-candidates)
        #f
        (let ((ranked-candidates 
                  (sort-list candidates-not-self (lambda (c1 c2) (>= (cdr c1) (cdr c2)))))) ; sort after version. Highets first
                                                                                            ; maybe accross source files!!!
          (car ranked-candidates)))))


; Find all definitions named name in all source files and in all versions.
; Return a list of cons cells with (source-key . source-version) in which name is located. 
; Return the empty list if no candidates can be found
; We assume that a source file does not hold more than one definition of the same name. - But we cannot really be sure. 
; We loose accuracy in the Elucidator if this does not hold.
(define (find-definitions-named name) 
 (let ((find-definition-single-file 
         (lambda (a-source-file-map)
           (let* ((source-key (source-key-of-source-file-contribution a-source-file-map))
                  (source-version (version-of-source-file-contribution a-source-file-map))
                  (source-list (source-list-of-source-file-contribution a-source-file-map))
                  (res (find-in-list
                        (lambda (x) (and (is-define-form? x) (eq? (as-symbol name) (defined-name x))))
                        source-list)))
             (if res (cons (as-string source-key) source-version) #f)))))
   (filter (lambda (x) (pair? x))   ; only those cons cell contributions
           (map find-definition-single-file source-file-map))))


(define (find-definition-in-source-file-map name source-key source-version)
 (let ((source-list (get-source-list-from-source-file-map source-key source-version)))
   (find-in-list
     (lambda (x) (and (is-define-form? x) (eq? (as-symbol name) (defined-name x))))
     source-list)))

(define (definitions-equal? def-1 def-2)
  (equal? def-1 def-2))

(define (bodies-equal? body-1 body-2)
  (equal? body-1 body-2))

(define (parameters-equal? par-list-1 par-list-2)
  (equal? par-list-1 par-list-2))
    


; Return a given defining name occurrence that matches name, source-key and version.
; If no such entry exists, return #f.
(define (find-defining-name name source-key version)
  (find-in-list
    (lambda (dno)
      (and (eq? name (defined-name-of dno))
           (equal? source-key (source-key-of dno))
           (= version (version-of dno))))
    defining-name-occurences))


(define (remove-redundant-weak-entries entries)
  ; Entries is a subset of documented-name-occurenes. In this function we remove possible weak entries
  ; for which also a strong entry exist in the list of entries.
 (remove-redundant-weak-entries-1 entries entries '()))

(define (remove-redundant-weak-entries-1 all-entries entries res)
  (letrec ((redundant-weak-entry? 
            (lambda (e1 e2) 
               (and (not (equal? e1 e2)) (eq? 'weak (caddr e1)) (eq? (cadr e1) (cadr e2))))))
   (cond ((null? entries) (reverse res))
         ((member-by-predicate (car entries) all-entries redundant-weak-entry?)
            (remove-redundant-weak-entries-1 all-entries (cdr entries) res))
         (else (remove-redundant-weak-entries-1 all-entries (cdr entries) (cons (car entries) res))))))
             

(define (documentation-url doc-id)
  (string-append "documentation.html" "#" (as-string doc-id)))

; Return a link to the documentation frame given name (a name in the program frame)
; a doc-id (the identification of a section or unit in the documentation frame) and
; title (the title of the section or unit in the documentation frame).
; strong-weak is a symbol (strong or weak) which tells whether to insert a strong or a weak documentation reference
; given-version is #f if no specific version is asked for in the documentation. Else it is a number.
(define (doc-link name doc-id title strong-weak given-version)
  (a-tag-target 
    (documentation-url doc-id) 
    (if given-version
        (cond ((eq? strong-weak 'strong) (image "doc-left-point.gif" title))
              ((eq? strong-weak 'weak) (image "doc-left-weak-point.gif" title))
              (else (error "doc-link: problems determining strong or weak documentation link")))
        (cond ((eq? strong-weak 'strong) (image "doc-left.gif" title))
              ((eq? strong-weak 'weak) (image "doc-left-weak.gif" title))
              (else (error "doc-link: problems determining strong or weak documentation link"))))
    "documentation-frame"))


; A specialized version of list-difference, where the first parameter is a list of pairs (name . key),
; and the second parameter is a simple list of names
; Returns a list of pairs (a subset of defined-name-pairs).
(define (list-difference-2 defined-name-pairs bounded-names)
  (list-difference-3 defined-name-pairs bounded-names '()))

(define (list-difference-3 lst1 lst2 res)
  (cond ((null? lst1) (reverse res))
        ((memq (caar lst1) lst2) (list-difference-3 (cdr lst1) lst2 res))
        (else (list-difference-3 (cdr lst1) lst2 (cons (car lst1) res)))))

; Elucidate a list of symbols, f, without attempting any linking from the involved symbols.
; Does not handle improper lists.
(define (elucidate-list-simple ip op f size)
  (cond ((symbol? f) (match-simple-symbol f ip op))
        ((list? f) 
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf)
                 (skip-white-space ip op)
                 (elucidate-list-simple ip op sf size))
              f)
           (skip-white-space ip op)
           (match-end-parenthesis ip op))
  )
)


; match the cadr symbol of a define form f, without matching trailing comments
(define (elucidate-restricted-define-form ip op f size)
  (cond ((symbol? f) (match-simple-symbol f ip op))
        ((list? f) 
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf)
                 (skip-white-space ip op)
                 (elucidate-restricted-define-form ip op sf size))
              f)
           (skip-white-space ip op)
           (match-end-parenthesis ip op))
        ((pair? f)
           (let ((p1 (proper-part f))
                 (p2 (first-improper-part f)))
            (skip-white-space ip op)
            (match-start-parenthesis ip op)
            (for-each 
              (lambda (sf)
                 (skip-white-space ip op)              
                 (elucidate-restricted-define-form ip op sf size))
              p1)
            (skip-white-space ip op)
            (match-dot ip op)
            (skip-white-space ip op)
            (elucidate-restricted-define-form ip op p2 size)
            (skip-white-space ip op)
            (match-end-parenthesis ip op)
          ))))

(define (elucidate-lambda-parameters ip op f size)
  (cond ((symbol? f) (begin
                       (write-string-to-port (start-tag-of (span 'class "local-name-binding")) op)
                         (match-simple-symbol f ip op)
                       (write-string-to-port (end-tag-of (span 'class "local-name-binding")) op)
                     ))
        ((list? f) 
           (match-start-parenthesis ip op)
           (for-each 
              (lambda (sf)
                 (skip-white-space ip op)
                 (elucidate-lambda-parameters ip op sf size))
              f)
           (skip-white-space ip op)
           (match-end-parenthesis ip op))
        ((pair? f)
           (let ((p1 (proper-part f))
                 (p2 (first-improper-part f)))
            (skip-white-space ip op)
            (match-start-parenthesis ip op)
            (for-each 
              (lambda (sf)
                 (skip-white-space ip op)              
                 (elucidate-lambda-parameters ip op sf size))
              p1)
            (skip-white-space ip op)
            (match-dot ip op)
            (skip-white-space ip op)
            (elucidate-lambda-parameters ip op p2 size)
            (skip-white-space ip op)
            (match-end-parenthesis ip op)
          ))))

; Elucidate let-binding-form, which is the second element of a let form - the one following the 'keyword'.
(define (elucidate-let-bindings ip op let-binding-form defined-names shadowing-names documented-names size
                                source-key source-version let-kind raw?)
  (skip-white-space ip op)
  (match-start-parenthesis ip op)
  (elucidate-let-bindings-internal ip op let-binding-form defined-names shadowing-names documented-names size 
                                   source-key source-version let-kind raw?)
  (skip-white-space ip op)
  (match-end-parenthesis ip op))

; Elucidate let-binding-form. Some name bindings can, confusingly,  turn out to be a syntactical comment, because comments
; are allowed in between name bindings.
(define (elucidate-let-bindings-internal ip op let-binding-form defined-names shadowing-names documented-names size
                                         source-key source-version let-kind raw?)
 (let ((name-just-bound #f)) ; assigned later
  (if (not (null? let-binding-form))
      (begin
        (if (syntactical-comment? (car let-binding-form))
            (begin
               (skip-white-space ip op)
               (elucidate-program-form ip op (car let-binding-form) #f defined-names shadowing-names documented-names size
                                       source-key source-version raw? #f)
               (skip-white-space ip op)
            )
            (begin
              (elucidate-single-let-binding
               ip op (car let-binding-form) defined-names shadowing-names documented-names size source-key source-version let-kind raw?)
              (set! name-just-bound (first (car let-binding-form)))
            ))
        (cond ((eq? let-kind 'let*)
                 (elucidate-let-bindings-internal ip op (cdr let-binding-form) defined-names 
                                         (if name-just-bound (cons name-just-bound shadowing-names) shadowing-names) 
                                         documented-names size source-key source-version let-kind raw?))
              (else
                 (elucidate-let-bindings-internal ip op (cdr let-binding-form) defined-names shadowing-names
                                         documented-names size source-key source-version let-kind raw?)))))))
               

; Elucidate a single let binding, such as (x 5) in (let ((x 5) (y 6)) (+ x y)).
; In principle, var-init-list is a list of length 2, but due to the syntactic comments, it may be longer.
; This complicates the elucidation of this form.
(define (elucidate-single-let-binding ip op var-init-list defined-names shadowing-names documented-names size
                                      source-key source-version let-kind raw?)
  (skip-white-space ip op)
  (match-start-parenthesis ip op)
  (skip-white-space ip op)
  (elucidate-binding-constituents 1 ip op var-init-list defined-names shadowing-names documented-names size
                                  source-key source-version let-kind raw?)
  (skip-white-space ip op)
  (match-end-parenthesis ip op))

; Iterative processing of constituent-list
; position = 1: The next non-comment form is the bound name.
; position = 2: The next non-comment-form is the init form.
(define (elucidate-binding-constituents position  ip op constituent-list defined-names shadowing-names documented-names size
                                        source-key source-version let-kind raw?)
  (skip-white-space ip op)
  (cond ((null? constituent-list)
           (skip-white-space ip op)) 
        ((syntactical-comment? (car constituent-list))
           (skip-white-space ip op)
           (elucidate-program-form ip op (car constituent-list) #f defined-names shadowing-names documented-names size
                                   source-key source-version raw? #f)
           (skip-white-space ip op)
           (elucidate-binding-constituents position  ip op (cdr constituent-list)  defined-names shadowing-names documented-names size
                                             source-key source-version let-kind raw?))
        ((= position 1)   ; The bound name
           (skip-white-space ip op)
           (write-string-to-port (start-tag-of (span 'class "local-name-binding")) op)
             (match-simple-symbol (car constituent-list) ip op) ; the bound name
           (write-string-to-port (end-tag-of (span 'class "local-name-binding")) op)
           (elucidate-binding-constituents 2 ip op (cdr constituent-list)  defined-names shadowing-names documented-names size
                                             source-key source-version let-kind raw?))
        ((= position 2)   ; The init form
           (skip-white-space ip op)

           ; the init form:
           (elucidate-program-form ip op (car constituent-list) #f defined-names shadowing-names documented-names size
                                   source-key source-version raw? #f)

           (skip-white-space ip op)
           (elucidate-binding-constituents 3 ip op (cdr constituent-list) defined-names shadowing-names documented-names size
                                             source-key source-version let-kind raw?))

        (else (laml-error "elucidate-binding-constituents: Let binding malformed."))))

(define (elucidate-pattern-template-form ip op pat-templ reduced-defined-names shadowing-names documented-names size
                                         source-key source-version raw? top-level?)

 ; pat-templ may in fact be a comment. Handle it!?

 (let* ((pattern (car pat-templ))
        (template (cadr pat-templ))
        (pattern-variables (collect-pattern-variables (no-syntactic-comments pattern)))) 
   (skip-white-space ip op)
   (match-start-parenthesis ip op)
   (skip-white-space ip op)

   (elucidate-program-form ip op pattern #f '() (append shadowing-names scheme-syntax-procedure-names) documented-names size
                           source-key source-version raw? top-level?)
   (skip-white-space ip op)
   (elucidate-program-form ip op template #f reduced-defined-names (append shadowing-names pattern-variables) documented-names size
                           source-key source-version raw? top-level?)

   (skip-white-space ip op)
   (match-end-parenthesis ip op)
   (skip-white-space ip op)
 )
)

; Return the list of identifiers in pattern
; Not 100% accurate. Does not elimiate literals (from syntax-rules). Does not elimiate _ or ...
(define (collect-pattern-variables pattern)
  (cond ((symbol? pattern) (list pattern))
        ((null? pattern) '())
        ((pair? pattern) 
            (append (collect-pattern-variables (car pattern))
                    (collect-pattern-variables (cdr pattern))))
        ((vector? pattern) (flatten (map collect-pattern-variables (as-list pattern))))
        (else '())))


; ---------------------------------------------------------------------------------------------------------------

(define (quote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\' ch)  (and (list? form) (> (length form) 1) (eq? (car form) 'quote)) )
        (begin 
          (read-char ip)
          #t)
        #f)))

(define (backquote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\` ch) (and (list? form) (> (length form) 1) (eq? (car form) 'quasiquote)))
        (begin 
          (read-char ip)
          #t)
        #f)))

(define (unquote-in-input? ip form)
  (let ((ch (peek-char ip)))
    (if (and (eqv? #\, ch) (and (list? form) (> (length form) 1) (or (eq? (car form) 'unquote) (eq? (car form) 'unquote-splicing))))
        (begin 
          (read-char ip)
          #t)
        #f)))

; Match symbol sym, and generate appropriate anchored links from it. 
; First attemp linking to defined name in the documentation bundle. Then try link to a SchemeDoc manual entry.
; Next attempt linking to RnRS Scheme name. If none of these linkings are possible, just output the symbol on op.
; In this case symbol most like represents a local name. 
; shadowing-names is a simple list of names which shadow for both defined names and the RnRS Scheme names.
(define (match-symbol sym ip op defined-names shadowing-names size version)
  (read ip)
  (let* ((source-key-version-pair (name-memq-version sym defined-names version))
         (symbol-string (as-string sym))
        )
    (if source-key-version-pair
        (let* ((source-key (car source-key-version-pair))
               (source-version (cdr source-key-version-pair))
               (high-version (highest-version-number source-key))
               (old? (if (and (number? source-version) (number? high-version))
                         (< source-version high-version)
                         #f)) 
              )
          (render-to-output-port ; applied name linked to its defining name occurrence.
           (a
            symbol-string
            'class "applied-name"
            'href (string-append (source-file-name-html-file source-key source-version size) ".html" "#" (as-string sym))
            'title (string-append source-key (if old? (string-append ", version " (as-string source-version)) ""))
            )
           op))
         (let ((man-entry (find-manual-entry symbol-string)))    
           (if (and man-entry (not (memq sym shadowing-names)))
               (let ((url (string-append (cdr man-entry) "#" symbol-string)))    ; name that can be linked to a schemedoc manual name
                 (render-to-output-port
                  (a
                   symbol-string 
                   'class "schemedoc-name"
                   'href url
                   (if manual-frame-from-program (list 'target manual-frame-from-program) '())
                   'title (string-append "Manual: " (file-name-proper url)) 
                   )
                  op))
               (let ((entry (scheme-knowledge-entry sym)))
                 (if (and entry (not (memq sym shadowing-names)))
                     (let ((url (url-of-scheme-knowledge entry)))     ; name that can be linked to a RnRs Scheme word
                       (if url
                           (render-to-output-port
                            (a
                             symbol-string
                             'class 
                             (case (category-of-scheme-knowledge entry)
                               ((syntax) "scheme-syntax-name")
                               ((procedure) "scheme-procedure-name")
                               (else "scheme-misc"))
                             'href url
                             (if manual-frame-from-program (list 'target manual-frame-from-program) '())
                             'title (string-append (upcase-string (as-string the-scheme-report-version)) " " "Scheme form")
                             )
                            op)
                           (render-to-output-port 
                            (span 'class
                                  (case (category-of-scheme-knowledge entry)
                                    ((syntax) "scheme-syntax-name")
                                    ((procedure) "scheme-procedure-name")
                                    (else "scheme-misc-name"))
                                  symbol-string)
                            op)
                           )
                       )
                     (display symbol-string op)
                     )))))))

; Match symbol sym without any linking or decoration.
(define (match-simple-symbol sym ip op)
  (read ip)
  (let* ((symbol-string (as-string sym))
         (protected-symbol-string (html-protect symbol-string))
        )
    (display protected-symbol-string op)))


;; The parameter entry is a an entry from scheme-syntax-procedure-list.
;; Return the a one-argument procedure, with which to decorate a kind symbol in the program presentation.
(define (scheme-syntax-procedure-decorate entry)
  (cond ((eq? 'syntax (category-of-scheme-knowledge entry)) b)
        ((eq? 'procedure (category-of-scheme-knowledge entry)) brown-normal)
        (else id-1)))

(define (brown-normal txt)
  (font-color rnrs-scheme-color (if black-and-white-coloring (em txt) txt)))

; The identify function
(define (id-1 x) x)

; Return an entry in scheme-syntax-procedure-list, if symbol is found in that list, or else #f
; The function converts symbol to a symbol, in case it is a string.
(define (scheme-knowledge-entry symbol)
  (scheme-knowledge-entry-1 (as-symbol symbol) scheme-syntax-procedure-list))

(define (scheme-knowledge-entry-1 symbol lst)
  (cond ((null? lst) #f)
        ((eq? (symbol-of-scheme-knowledge (car lst)) symbol)
           (car lst))
        (else (scheme-knowledge-entry-1 symbol (cdr lst)))))

; Return the source-key/source-version component of the matching sym in defined-names.
; What is actually retunred is a cons pair of source-key and source-version.
; defined-names is a list defined name entries, see the global variable defining-name-occurences. 
; If no match, return #f
(define (name-memq sym defined-names)
  (cond ((null? defined-names) #f)
        ((eq? sym (defined-name-of (car defined-names))) (cons (source-key-of (car defined-names)) (version-of (car defined-names)))) 
        (else (name-memq sym (cdr defined-names)))))

; As name-memq, but also ensure that the source-key/source-version is indeed of version vers.
(define (name-memq-version sym defined-names vers)
  (cond ((null? defined-names) #f)
        ((and (eq? sym (defined-name-of (car defined-names)))
              (= vers (version-of (car defined-names))))
            (cons (source-key-of (car defined-names)) (version-of (car defined-names)))) 
        (else (name-memq-version sym (cdr defined-names) vers))))

; Return an entry in manual-name-file-map matching sym.
; Returns #f if not found
; Name is supposed to be a string.
(define (find-manual-entry name)
 (find-in-list
   (lambda (name-file-pair)
     (equal? (car name-file-pair) name))
   manual-name-file-map))


(define (match-string str ip op)
  (read ip)
  (write-char #\" op)  ; write start quote of string
  (write-chars-in-string str 0 (string-length str) op)
  (write-char #\" op)  ; write end quote of string
)

; A specialized string printing functions for the elucidator.
; Takes special care of backslash and quotes within the string.
; Transforms the characters through the html char transformation table.
(define (write-chars-in-string str i str-lgt op)
  (if (< i str-lgt)
      (let* ((ch (string-ref str i))
             (ch-n (as-number ch)))
        (cond ((= ch-n 10) (newline op))
              ((= ch-n 13) 'do-nothing)
              ((= ch-n 34) (write-char #\\ op) (write-char #\" op))    ; "
              ((= ch-n 92) (write-char #\\ op) (write-char #\\ op))    ; \
              (else (display (html-char-transform ch) op)))
        (write-chars-in-string str (+ i 1) str-lgt op))))

(define (match-char ch ip op)
  (read ip)
  (write ch op))

(define (match-regexp ch ip op)
  (read ip)
  (write ch op))

(define (match-number n ip op)
  (read ip)
  (write n op))

(define (match-boolean b ip op)
  (read ip)
  (write b op))

(define (match-start-parenthesis ip op)
  (let ((ch (read-char ip)))
    (if (or (eqv? ch #\() (eqv? ch #\[)) 
	(write-char ch op)
	(error (string-append "match error: start parenthesis expected:" (as-string ch))))))

(define (match-end-parenthesis ip op)
  (let ((ch (read-char ip)))
    (if (or (eqv? ch #\)) (eqv? ch #\]))
        (write-char ch op)
        (error "match error: end parenthesis expected:" (as-string ch)))))

(define (match-dot ip op)
  (let ((ch (read-char ip)))
    (if (eqv? ch #\.)
        (write-char #\. op)
        (error "match error: dot expected. Problems if we deal with unnormlized dotted forms"))))

(define (match-number-sign ip op)
  (let ((ch (read-char ip)))
    (if (eqv? ch #\#)
        (write-char #\# op)
        (error "match error: number sign expected:" (as-string ch)))))

(define (skip-white-space ip op)
  (let ((ch (peek-char ip)))
    (cond ((white-space? ch) (begin (read-char ip) (write-char ch op) (skip-white-space ip op)))
          ((comment-begin? ch) (begin  (skip-comment ip op) (skip-white-space ip op)))
          (else 'empty))))

(define (white-space? ch)
 (if (eof-object? ch)
     #f
    (let ((n (char->integer ch)))
      (or (eqv? n 32) (eqv? n 9) (eqv? n 10) (eqv? n 12) (eqv? n 13)))))

(define (comment-begin? ch)
  (eqv? #\; ch))

(define (skip-comment ip op)
  ; skip rest of line.
  (write-string-to-port (start-tag-of (font 'color (rgb-string-list comment-color))) op)
  (skip-comment-1 ip op)
  (write-string-to-port (end-tag-of (font "dummy")) op))


(define (report-ambiguous-doc-source-markers amb-list)
  (let ((doc-sections
          (map (lambda (pid-did-sm)
                   (let ((doc-id (cadr pid-did-sm)))
                     (cdr (assq doc-id documentation-key-numbering-alist))))
               amb-list)))
    (string-append
       CR "The relation is ambiguous." CR
       (if (= 1 (length amb-list)) "The other relevant section is " "The other relevant sections are ")
       (string-merge doc-sections (make-list (- (length amb-list) 1) ", " )))))

; Return the link from a program source marker to the documentation.
; We are in the context of processing a syntactical comment in a Scheme source program.
; The syntactical comment contains a source marker. 
; Issue a warning in cases of ambiguities or a non-existing relation.
(define (doc-source-marker-link documentation-source-marker-occurences mark-char enclosing-definition-name)
 (let* ((relevant-occurences 
         (filter (lambda (pid-did-sm)
                   (let ((pid (car pid-did-sm))
                         (sm (caddr pid-did-sm)))
                     (and (equal? (as-string pid) (as-string enclosing-definition-name))
                          (equal? (as-string sm) (as-string mark-char)))))
                 documentation-source-marker-occurences))
        (lgt (length relevant-occurences)))

  ; possible warning side effect
  (cond ((and warn-if-missing-source-marker-in-documentation (= lgt 0)) (display-warning 
                                    "In Program:" actual-source-key "vers." (string-append (as-string actual-source-version) ":")
                                    "Encountered source marker" 
                                    (string-append "'" (as-string mark-char) "'") "in" 
                                    (as-string enclosing-definition-name)
                                    "which is not present in the documentation"
                                    ))
        ((and warn-if-ambiguous-source-markers-in-documentation (> lgt 1)) (display-warning 
                                    "In Program:" actual-source-key "vers." (string-append (as-string actual-source-version) ":")
                                    "Encountered source marker" 
                                    (string-append "'" (as-string mark-char) "'") "in" 
                                    (as-string enclosing-definition-name)
                                    "which occur" (as-string lgt) "times in the documentation. Using the first one."
                                    ))
        (else "")) ; no warning 
  
  (cond ((and (>= lgt 1) (or (eq? source-marker-handling-in-program 'show-all) (eq? source-marker-handling-in-program 'show-documented)))
          (let* ((used-occ (car relevant-occurences))
                 (doc-id (cadr used-occ))
                 (num (cdr (assq doc-id documentation-key-numbering-alist)))
                 (sec-title (cdr (assq doc-id documentation-key-title-alist)))
                 (ambiguous? (if (> lgt 1) (report-ambiguous-doc-source-markers (cdr relevant-occurences)) ""))  ;@o
                 (explanation 
                    (string-append "A linked program source marker to section " num ":"  CR (string-it-single sec-title)  ambiguous? CR
                                   "Mark char: " (as-string mark-char) ))
                )
            (a-tag-target (string-append "documentation.html" "#" (as-string doc-id) "-" "@" (as-string mark-char))
                          (source-marker-image mark-char explanation)
                          "documentation-frame")))
        ((and (= lgt 0) (eq? source-marker-handling-in-program 'show-all))
                (source-marker-image mark-char "A program source marker WITHOUT a link to the documentation"))
        (else (string-append (as-string elucidator-marker-char) (as-string mark-char))))))

  
(define (skip-comment-1 ip op)
  ; skip rest of line.
  (let ((ch (read-char ip)))
    (cond ((eof-object? ch) #f)  ; do nothing.
          ((eol? ch) (write-char ch op))
          ((eqv? ch #\<) (write-string-to-port "&lt;" op) (skip-comment-1 ip op))
          ((eqv? ch #\>) (write-string-to-port "&gt;" op) (skip-comment-1 ip op))
          ((eqv? ch elucidator-marker-char)  
             (let ((source-marker-char (read-char ip))  ; assume not eof
                   (next-char (read-char ip))           ; assume not eof
                  )
               (if (is-white-space? next-char)
                   (write-string-to-port
                       (string-append 
                         (a-name 
                          (string-append
                            (as-string enclosing-definition-name)  ;@i
                            "-@" (as-string source-marker-char))) 
                         (doc-source-marker-link ;@a
                            documentation-source-marker-occurences
                            source-marker-char
                            enclosing-definition-name)
                         (as-string next-char)) 
                       op)
                   (write-string-to-port
                       (string-append (as-string elucidator-marker-char) (as-string source-marker-char) (as-string next-char))
                       op))
               (skip-comment-1 ip op)))
          (else (begin (write-char ch op) (skip-comment-1 ip op))))))

(define (eol? ch)
  (eqv? ch #\newline))  


; ---------------------------------------------------------------------------------------------------


; Overall frame setup in terms of the control, documentation, and program file names.
; Directory prefix is added in front of all three frames of the elucidator.
; program-size is either small or large (a symbol).
(define (elucidator-frame control-filename menu-filename documentation-filename program-filename program-size directory-prefix)
  (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html"))))
           (sized-frame-file 
            (lambda (f) (frame-file (string-append f (if (eq? program-size 'large) "-LARGE" "")))))
          )
   (xhtml-frameset:frameset 
          (control-frame control-filename menu-filename directory-prefix) 
          (xhtml-frameset:frameset
              (xhtml-frameset:frame 'name "documentation-frame" 'src (frame-file documentation-filename) 'scrolling "yes")
              (xhtml-frameset:frame 'name "program-frame" 'src (sized-frame-file program-filename) 'scrolling "yes")
              'cols "50%,50%")
    'rows (string-append (as-string control-frame-pixel-height) ",*") )))

(define (elucidator-frame-horizontal control-filename menu-filename documentation-filename program-filename program-size directory-prefix)
  (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html"))))
           (sized-frame-file 
            (lambda (f) (frame-file (string-append f (if (eq? program-size 'large) "-LARGE" "")))))
          )
   (xhtml-frameset:frameset
     (control-frame control-filename menu-filename directory-prefix)
     (xhtml-frameset:frame 'name "documentation-frame" 'src (frame-file documentation-filename) 'scrolling "yes")
     (xhtml-frameset:frame 'name "program-frame" 'src (sized-frame-file program-filename) 'scrolling "yes")
     'rows (string-append (as-string control-frame-pixel-height) ",360,*") )))


; Return a control frame or frameset. 
(define (control-frame control-filename menu-filename directory-prefix)
 (letrec ((frame-file (lambda (f) (string-append directory-prefix (add-file-extension f "html")))))
  (if (eq? program-menu-approach 'separate-frame)
      (xhtml-frameset:frameset
            (xhtml-frameset:frame 'name "control-frame" 'src (frame-file control-filename) 'scrolling "auto")
            (xhtml-frameset:frame 'name "program-menu"  'src (frame-file menu-filename) 'scrolling "auto")
            'cols "*,240")
      (xhtml-frameset:frame 'name "control-frame" 'src (frame-file control-filename) 'scrolling "auto"))))


(define (make-frame-file-in-html-dir title frames filename)
  (write-html 'raw
    (xhtml-frameset:html
       (xhtml-frameset:head 
         (xhtml-frameset:title documentation-title)
         (xhtml-frameset:link 'rel "SHORTCUT ICON" 'href (string-append "images/" "16-16-ep.ico"))
       )
       frames)
    (html-destination filename)))

(define (make-frame-file-in-source-dir title frames filename)
  (write-html 'raw
    (xhtml-frameset:html
      (xhtml-frameset:head 
        (xhtml-frameset:title documentation-title)
        (xhtml-frameset:link 'rel "SHORTCUT ICON" 'href (string-append elucidator-source-destination-delta "images/" "16-16-ep.ico"))
      )
      frames)
    (source-destination filename)))

; Render the body of the documentation page to the open output port op.
; This function uses the global variables: documentation-title, documentation-author, documentation-email, documentation-affiliation,
; documentation-abstract,  and documentation-elements.
(define (textual-documentation-contents! op)
  (set-xml-transliterate-character-data-in 'xhtml10-transitional #f)  ; temporary elimination of HTML character transliteration - to allow textual HTML tags in abstract etc.
  (render-to-output-port (a 'name "START") op)
  (render-to-output-port (h 1 (font-color blue (guard-text documentation-title))) op)
  (render-to-output-port (present-author-info (map guard-text (list documentation-author documentation-email documentation-affiliation))) op)
  (render-to-output-port (p) op)
  (render-to-output-port (present-abstract (guard-text documentation-abstract)) op)
  (render-to-output-port (div (vertical-space 1)) op)
  (set-xml-transliterate-character-data-in 'xhtml10-transitional #t) ; resume normal HTML character transliteration. 

  ; @a
  (for-each (lambda (de) (present-documentation-element! de op)) documentation-elements)

  (render-to-output-port (div (vertical-space end-file-empty-lines)) op)
)

(define (present-documentation-element! doc-el op)
  (let ((kind (get-value 'kind doc-el)))
    (cond ((eq? kind 'section) (present-documentation-section! doc-el op))
          ((eq? kind 'entry) (present-documentation-entry! doc-el op))
          (else (error "present-documentation-element!: unknown kind of documentation element")))))

(define section-number 0)
(define subsection-number 0)

(define (section-numbering)
  (string-append (as-string section-number)))

(define (subsection-numbering)
  (string-append (as-string section-number) "." (as-string subsection-number)))

; Present a documentation section, doc-el, imperatively on the open output port op.
(define (present-documentation-section! doc-el op)
 (let* ((title (get-value 'doc-title doc-el))
        (section-numbering (get-value 'numbering doc-el))
        (section-number (car (get-value 'raw-numbering doc-el))) ; an integer
        (title-1 (span section-numbering (horizontal-space 2) title))
        (body (get-value 'doc-body doc-el))
        (id (get-value 'id doc-el))
        (hidden-id-pres (font-1 2 documentation-entry-color (as-string id)))
        (subsection-elements (filter (subsections? section-number) documentation-elements))
       )
   (write-string-to-port (start-tag-of (div 'class "elucidator-section")) op)   
    ; Sectional front matters:
    (render-to-output-port (a 'name (internal-reference id)) op)
    (render-to-output-port (div (elucidator-section-navigation-banner doc-el) (horizontal-space 1) (if present-hidden-ids? hidden-id-pres "") (br)) op)
    (render-to-output-port (div (b (font-size 5 title-1) )) op)

    ; The substantial sectional documentation is made here:
    (do-program-link-documentation! body id op)

    ; Subsection link table: 
    (render-to-output-port  (indent-pixels 10 (brl (map present-documentation-subsection-element subsection-elements))) op)
   (write-string-to-port (end-tag-of (div "dummy") ) op)))


(define (present-documentation-entry! doc-el op)
 (let* ((title (get-value 'doc-title doc-el))
        (entry-numbering (get-value 'numbering doc-el))
        (title-1 (span entry-numbering (horizontal-space 2) title))
        (body (get-value 'doc-body doc-el))
        (id (get-value 'id doc-el))
        (hidden-id-pres (font-1 2 documentation-entry-color (as-string id)))
       )
    (write-string-to-port (start-tag-of (div 'class "elucidator-entry")) op)

      ; Entry front matters:
      (render-to-output-port (a 'name (internal-reference id)) op)
      (render-to-output-port 
        (div  'class "elucidator-entry-head" 
           (elucidator-section-navigation-banner doc-el)
           (if present-hidden-ids? hidden-id-pres "") (br) ;@i
	   (b (font-size 4 title-1))) op)

      ; The substantial entry documentation is made here: 
      (do-program-link-documentation! body id op)
    (write-string-to-port (end-tag-of (div "dummy")) op) ))


; return a predicate which return #t on entries in section n
(define (subsections? n)
  (lambda (doc-el)
    (let ((kind (get-value 'kind doc-el))
          (raw-num (get-value 'raw-numbering doc-el)))
      (and (eq? kind 'entry) (eqv? n (car raw-num))))))

; return a more general predicate which returns #t on entry n.m
; n.0 means section n
(define (section-subsection? n m)
  (lambda (doc-el)
    (let ((raw-num (get-value 'raw-numbering doc-el)))
      (and (eqv? n (car raw-num)) (eqv? m (cadr raw-num))))))

(define (present-author-info au)
 (let ((au1 (if (not (null? au)) (cons (copyright-owner (car au)) (cdr au)) au)))
   (h 3 (map (lambda (e) (list e (horizontal-space 1))) au1) )))


(define (present-abstract abstr)
  (div 'class "elucidator-abstract" 
   (em (b "Abstract. ") abstr)))

(define (guard-text str)
  (if str str "???"))


(define (make-source-program-file source-key source-version source-group source-file language source-list defining-names documented-names size)
  (elucidate-program-source
    source-file
    (string-append (html-directory) (source-file-name-html-file source-key source-version size) ".html")
    source-list
    defining-names
    documented-names
    size
    source-key
    source-version
    source-group))


; ---------------------------------------------------------------------------------------------------

; Transform words surrounded by curly brackets (or more correctly, p-link-prefix-char and p-link-suffix-char)
; to links to one of the source programs. Use the information in the global variable
; defining-name-occurences to do so.

(define linking-output-factor 10)

(define (do-program-link-documentation! str doc-id op)
 (set! state-list '())
 (let* ((strlgt (string-length str)))
  (do-program-link-documentation-1! doc-id str 0 strlgt 'normal-text "" op)))


(define state-list '()) ; for debugging purposes
(define debugging-program-linking #f)


(define (do-program-link-documentation-1! doc-id instr inptr inlength current-state collected-word op)
  (if (< inptr inlength)
      (let* ((inch (string-ref instr inptr))
             (trans-res (program-linking-transition current-state inch collected-word doc-id))
             (next-state (car trans-res))
             (toput (cadr trans-res))
             (collected-word (caddr trans-res))
            )
       (if debugging-program-linking
            (set! state-list (cons (list (as-string inch) next-state collected-word) state-list)))
       (if (string? toput)
           (write-string-to-port toput op)
           (render-to-output-port toput op))
       (do-program-link-documentation-1! doc-id instr (+ 1 inptr) inlength
                                        next-state collected-word op))))


; STATES 
; normal-text:          We are outside a name from which to link
; inside-marker:        We have just seen a program source mark
; end-marker:           About to output marker or mark literal
; inside-p-link-word:   We are inside a word from which to link to program
; entering-p-link-word
; leaving-p-link-word
; inside-d-link-word:   We are inside a word from which to link to another section in the documentation
; entering-d-link-word
; leaving-d-link-word

(define (program-linking-transition in-state ch collected-word doc-id)
 (let ((char (as-string ch))
       (expl (string-append "A link to a program source marker in " (as-string previous-strong-program-word))))
   (cond ((and (symbol? in-state) (eq? in-state 'normal-text))
            (cond ((equal? char p-link-prefix-char)                        (list 'entering-p-link-word "" collected-word))
                  ((equal? char d-link-prefix-char)                        (list 'entering-d-link-word "" collected-word))
                  ((equal? char p-link-suffix-char)  (display-warning "Misplaced end-of-link char") 
                                                                         (list 'normal-text "" collected-word))
                  ((equal? char elucidator-marker-char-string)           (list 'inside-marker "" ""))
                  ((equal? char elucidator-escape-char-string)           (list 'normal-text-escape "" collected-word))
                  (else                                                  (list 'normal-text char collected-word))))

         ((and (symbol? in-state) (eq? in-state 'inside-marker))        ; char identifies the marker
            (cond ((or (equal? char p-link-suffix-char) (equal? char p-link-prefix-char)
                       (equal? char d-link-prefix-char) (equal? char d-link-suffix-char))
                                                     (display-warning "Unexpected marker char")
                                                                         (list 'normal-text (string-append elucidator-marker-char-string char)  collected-word))

                  (else                                                  (list 'normal-text
                                                                                (begin
                                                                                  (source-mark-register previous-strong-program-word doc-id char)
                                                                                  (span (source-mark-anchor (source-marker-glyph char expl) char) _   ;@a
                                                                                        (a-name (string-append (as-string doc-id) "-" "@" (as-string char)))))
                                                                                collected-word))
            ))

         ((and (symbol? in-state) (eq? in-state 'normal-text-escape)) 
            (cond (else                                                  (list 'normal-text char collected-word))))


         ((and (symbol? in-state) (eq? in-state 'entering-p-link-word))
            (cond ((equal? char p-link-suffix-char)  (display-warning "Empty link word")
                                                                         (list 'leaving-p-link-word ""  collected-word))
                  ((equal? char p-link-prefix-char)  (display-warning "Misplaced begin-of-link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  ((or (equal? char d-link-prefix-char) (equal? char d-link-prefix-char))  (display-warning "Misplaced documentation link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  (else                                                  (list 'inside-p-link-word "" char))))

         ((and (symbol? in-state) (eq? in-state 'entering-d-link-word))
            (cond ((equal? char d-link-suffix-char)  (display-warning "Empty link word")
                                                                         (list 'leaving-d-link-word ""  collected-word))
                  ((equal? char d-link-prefix-char)  (display-warning "Misplaced begin-of-link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  ((or (equal? char p-link-prefix-char) (equal? char p-link-prefix-char))  (display-warning "Misplaced program link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  (else                                                  (list 'inside-d-link-word "" char))))

         ((and (symbol? in-state) (eq? in-state 'inside-p-link-word))
            (cond ((equal? char p-link-suffix-char)                        (list 'leaving-p-link-word (linking-from-doc-to-prog collected-word doc-id) ""))
                  ((equal? char p-link-prefix-char)  (display-warning "Misplaced begin-of-link prog char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  ((or (equal? char d-link-prefix-char) (equal? char d-link-prefix-char))  (display-warning "Misplaced documentation link char") 
                                                                         (list 'inside-p-link-word "" collected-word))
                  (else                                                  (list 'inside-p-link-word "" (string-append collected-word char)))))

         ((and (symbol? in-state) (eq? in-state 'inside-d-link-word))
            (cond ((equal? char d-link-suffix-char)                        (list 'leaving-d-link-word (linking-from-doc-to-doc collected-word doc-id) ""))
                  ((equal? char d-link-prefix-char)  (display-warning "Misplaced begin-of-link doc char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  ((or (equal? char p-link-prefix-char) (equal? char p-link-prefix-char))  (display-warning "Misplaced program link char") 
                                                                         (list 'inside-d-link-word "" collected-word))
                  (else                                                  (list 'inside-d-link-word "" (string-append collected-word char)))))

         ((and (symbol? in-state) (eq? in-state 'leaving-p-link-word))
            (cond ((equal? char p-link-suffix-char)  (display-warning "Misplaced end-of-link prog char")
                                                                         (list 'leaving-p-link-word "" collected-word))
                  ((equal? char p-link-prefix-char)                      (list 'inside-p-link-word "" collected-word))  ; ??
                  ((equal? char d-link-prefix-char)                      (list 'inside-d-link-word "" collected-word))  ; ?? 
                  (else                                                  (list 'normal-text char collected-word))))

         ((and (symbol? in-state) (eq? in-state 'leaving-d-link-word))
            (cond ((equal? char d-link-suffix-char)  (display-warning "Misplaced end-of-link doc char")
                                                                         (list 'leaving-p-link-word "" collected-word))
                  ((equal? char p-link-prefix-char)                      (list 'inside-p-link-word "" collected-word))  ; ??
                  ((equal? char d-link-prefix-char)                      (list 'inside-d-link-word "" collected-word))  ; ?? 
                  (else                                                  (list 'normal-text char collected-word))))

         (else                                                           (error "program-linking-transition error: unknown state"))

  )))

; Add an entry to the variable documentation-source-marker-occurences.
; Clean
(define (source-mark-register previous-strong-program-word doc-id char)
  (set! documentation-source-marker-occurences
     (cons (list (as-string previous-strong-program-word) (as-symbol doc-id) (as-string char))
           documentation-source-marker-occurences)))
                                                                                        
;; This function is called during the traversal of a documentation body.
;; It returns the a-tag'ed and fonted link word, which links to another place in the documentation
;; Clean
(define (linking-from-doc-to-doc collected-word doc-id)
  (let* ((ass-number (assq (as-symbol collected-word) documentation-key-numbering-alist))
         (ass-title (assq (as-symbol collected-word) documentation-key-title-alist))
         (ref-number (if ass-number (cdr ass-number) #f))
         (ref-title (if ass-title (cdr ass-title) #f))  ; the title of the section referred
         (url (if ref-number (string-append "documentation.html" "#" collected-word) #f)))
   (if url 
       (a
          ref-number
          'class "documentation-reference"
          'href url
          'target "documentation-frame"
          'title (if ref-title ref-title ""))
    (begin
      (display-warning (string-append "Cannot find a linking target of the documentation linking word: " collected-word))
      collected-word))))

; Previous strong word relation in the documentation. Includes also source-key, and source version
; Assigned by the procedure linking-from-doc-to-prog.
(define previous-strong-program-word #f)
(define previous-strong-source-key #f)
(define previous-strong-source-file-version-number #f)

;; This function is called during the textual traversal of a documentation body.
;; The parameter word is a a linking-word, as found in the textual documentation, for instance *qual$ref or qual$ref.
;; The function returns the a-tag'ed and fonted link word.
;; As a side-effect, it collects the documented names in the list documented-name-occurences, and it assigns
;; the variable the variable previous-strong-program-word.
(define (linking-from-doc-to-prog word doc-id)
  (let* ((kind (kind-of-program-link? word))
         (qualification (qualified-program-link? word))
         (qualification-1 (if qualification qualification "")) ; the qualification, or the empty string if none.
         (word-1 (proper-linking-word word qualification))
        )
   (destructured-linking-from-doc-to-prog kind qualification-1 (highest-version-number qualification) word-1 doc-id "" "")))

;; This function does the bulk of the work of the procedure linking-from-doc-to-prog.
;; The function returns the a-tag'ed and fonted link word.
;; In a documentation section/entry with doc-id, we use qualified link-word of link-kind to link to an entity in a source program.
;; As an additional property, this function does not rely on properties of the textual documentation format.
;; Therefore, it can be used for the processing of LAML documentation as well.
;; link-kind is either the symbol strong or weak.
;; qualification is the key of the source file or manual file. It is the the empty string if no qualifiction (file attr) is supplied.
;; given-source-file-version is the version explicitly asked for - either in the prog-ref clause on via the program-version attribute
;; of documentation-section/documentation-entry - (a string) or else #f.
;; source-file-version may also be #f.
;; to which the linking is done. A string or boolean. May be #f or the empty string.
;; link-word is the name of the program entity which is referred. A string.
;; file-part denotes a part of the qualification, such as a section in a manaul. A string. Empty if not applied.
;; file-part is only used for whole manual references.
;; source-anchor-name is an explicitly passed source anchor name, indented to be used as the content of the generated a element.
;; As a side-effect, this assigns the variables previous-strong-program-word and documented-name-occurences
(define (destructured-linking-from-doc-to-prog link-kind qualification given-source-file-version
                                               link-word doc-id-0 file-part source-anchor-name)
  (let* (; (source-file-version-number (as-number source-file-version))
         (doc-id (as-symbol doc-id-0))
         (strong? (eq? link-kind 'strong))
         (strong-weak-symbol (if strong? 'strong 'weak))
         (link-targets
            (filter (lambda (dno-entry) (equal? link-word (as-string (defined-name-of dno-entry)))) defining-name-occurences))
         (size default-program-font-size)
        )
       
     (cond ((eq? link-kind 'none)   ; no linking, only fonting
              (span 'class "none-reference" link-word))  

           ((and (empty-string? link-word) (not (empty-string? qualification))      ; link to a the whole source file
                 (source-file-qualification? qualification) )
              (let* ((source-key qualification)
                     (high-version (highest-version-number source-key))
                     (source-file-version-number
                         (cond (given-source-file-version  (as-number given-source-file-version))
                               (high-version high-version)
                               (else 1)))  ; else: should not happen
                     (old? (< source-file-version-number high-version)) 
                   )
                 (a
                   (if (empty-string? source-anchor-name) source-key source-anchor-name)
                   'class (file-prog-ref-class strong? old?)
                   'href (string-append (source-file-name-html-file source-key source-file-version-number size)  ".html")
                   'target "program-frame"
                   'title (string-append "Link to program file: " source-key
                             (if old? (string-append ", version " (as-string source-file-version-number)) "")
                          )
                 )
              ))

           ((and (empty-string? link-word) (not (empty-string? qualification))     ; link to the whole SchemeDoc manual
                 (manual-file-qualification? qualification))
              (let* ((manual-key (as-string qualification))
                     (man-entry (find-manual-source-list-entry manual-source-list manual-key))
                    )
                 (if man-entry
                     (let ((manual-url (car (get 'url-location man-entry))))

                       (a
                        (if (empty-string? source-anchor-name) manual-key source-anchor-name)
                        'class (if strong? "manual-file-reference-strong" "manual-file-reference-weak")
                        'href (if (empty-string? file-part) manual-url (string-append manual-url "#" file-part))
                        'target "program-frame"
                        'title (string-append "Link to SchmeDoc manual: " manual-key)
                        ))
                     (laml-error "Problems locating manual entry for key: " manual-key))
              ))

           ((or (= (length link-targets) 0)                       ; no matching defined name. Attempt manual linking or RnRS linking
                (and (not (empty-string? qualification)) (manual-file-qualification? qualification)))
                
             (let ((man-entry (find-manual-entry link-word))
                  )
	       (if man-entry                                ; attempt linking to a manual entry - disregarding qualification
		   (let ((url (string-append (cdr man-entry) "#" link-word)))
		      (a
		       (if (empty-string? source-anchor-name) link-word source-anchor-name)
                       'class "manual-reference"
		       'href url
		       'target manual-frame-from-documentation
                       'title (string-append "Manual: " (file-name-proper url)) 
		       ))
                   (let ((rnrs-entry (scheme-knowledge-entry link-word)))
                      (if rnrs-entry                       ; attempt linking to Revised Scheme Report - disregarding qualification
                          (let ((url (url-of-scheme-knowledge rnrs-entry)))
                             (a
			      (if (empty-string? source-anchor-name) link-word source-anchor-name)
			      'class (case (category-of-scheme-knowledge rnrs-entry)
				       ((syntax) "scheme-syntax-name")
				       ((procedure) "scheme-procedure-name")
				       (else "scheme-misc"))
			      'href url
			      'target manual-frame-from-documentation
			      'title (string-append (upcase-string (as-string the-scheme-report-version)) " " "Scheme form") 
			      ))    
   		          (begin 
			    (display-warning (string-append "Documentation to program linking: Cannot find linking target of " link-word))
			    (a 'href (string-append (string-append error-page-name ".html") "#program-reference-error" ) 'target manual-frame-from-documentation 'class "program-reference-error"
                               'title "This name is not recognized. Click for details." link-word))))))
              )

           ((= (length link-targets) 1)                           ; exactly one matching defined name 
              (let* ((source-key (source-key-of (car link-targets)))
                     (source-version (version-of (car link-targets)))
                     (high-version (highest-version-number source-key))
                     (source-file-version-number
                         (cond (given-source-file-version  (as-number given-source-file-version))
                               (source-version source-version)  ; should always be applicable
                               (high-version high-version)
                               (else 1)))
                     (old? (< source-version high-version))
                    )
                 (if strong?
                    (begin
                      (set! previous-strong-program-word link-word)  ;@i
                      (set! previous-strong-source-key source-key)
                      (set! previous-strong-source-file-version-number source-file-version-number)))
                 
                 (set! documented-name-occurences   ; @b
                        (cons (make-documented-name-entry link-word doc-id strong-weak-symbol given-source-file-version)
                              documented-name-occurences))
                 (if (and qualification (not (empty-string? qualification)) (not (equal? qualification source-key)))
                     (display-warning "In documentation: Disregarding the file qualification of" link-word ":" qualification ))
                 (a
                   (if (empty-string? source-anchor-name) link-word source-anchor-name)
                   'class (prog-ref-class strong? old?)
                   'href (string-append (source-file-name-html-file source-key source-file-version-number size) ".html" "#" link-word)
                   'target "program-frame"
                   'title (string-append source-key 
                             (if old? (string-append ", source-version " (as-string source-file-version-number)) "")
                          )
                 )
              ))

           ((and (> (length link-targets) 1) (not (empty-string? qualification)))  ; @a    Explicitly qualified reference. More than one
              (let* ((possible-source-keys (map source-key-of link-targets))
                     (qualification-ok (member qualification possible-source-keys))
                     (source-key (if qualification-ok qualification (source-key-of (car link-targets))))
                     (high-version (highest-version-number source-key))
                     (source-file-version-number
                         (cond (given-source-file-version  (as-number given-source-file-version))
                               (high-version high-version)
                               (else (version-of (car link-targets))))) ; else: should not happen
;                     (source-version (if qualification-ok source-file-version-number (version-of (car link-targets))))  ; Is then case ok?

                     (old? (< source-file-version-number high-version))
                    )
                 (if strong?
                    (begin
                      (set! previous-strong-program-word link-word)
                      (set! previous-strong-source-key source-key)
                      (set! previous-strong-source-file-version-number source-file-version-number)))  
                 (set! documented-name-occurences 
                       (cons (make-documented-name-entry link-word doc-id strong-weak-symbol given-source-file-version)
                             documented-name-occurences))

                 (if (not qualification-ok)
                     (display-warning "In Documentation: Illegal qualification of " link-word ":"
                                      (if (empty-string? qualification) "<empty>" qualification) 
                     )
                 )

                 (a
                   (if (empty-string? source-anchor-name) link-word source-anchor-name)
                   'class (prog-ref-class strong? old?)
                   'href (string-append (source-file-name-html-file source-key source-file-version-number size) ".html" "#" link-word)
                   'target "program-frame"
                   'title (string-append 
                            source-key
                            (if old? (string-append ", version " (as-string source-file-version-number)) ""))
                 )
              ))

           ((> (length link-targets) 1)   ; non-qualified. More than one.
              (let* ((possible-source-keys (map source-key-of link-targets))
                     (unique-possible-source-keys (remove-duplicates possible-source-keys))
                     (source-key (source-key-of (car link-targets)))
                     (high-version (highest-version-number source-key))
                     (source-file-version-number
                         (cond (given-source-file-version  (as-number given-source-file-version))
                               (high-version high-version)
                               (else (version-of (car link-targets)))))
                     (old? (< source-file-version-number high-version))
                    )
                (if strong?
                    (begin ;@h
                      (set! previous-strong-program-word link-word)  
                      (set! previous-strong-source-key source-key)
                      (set! previous-strong-source-file-version-number (as-number source-file-version-number))))
                 (set! documented-name-occurences
                       (cons (make-documented-name-entry link-word doc-id strong-weak-symbol given-source-file-version)
                             documented-name-occurences))

                 (if (> (length unique-possible-source-keys) 1)   ; not just differenct versions, but real ambiguity
                     (display-warning "In Documentation: Multiple targets of the prog. ref. " link-word 
                                  "Using that in " source-key source-file-version-number))
                 (a
                   (if (empty-string? source-anchor-name) link-word source-anchor-name) 
                   'class (prog-ref-class strong? old?)
                   'href (string-append (source-file-name-html-file source-key source-file-version-number size) ".html" "#" link-word)
                   'target "program-frame"
                   'title (string-append 
                            source-key
                            (if old? (string-append ", version " (as-string source-file-version-number)) ""))
                 )
              ))
     )))

; Return the CSS class of a doc-prog anchor - to a definition.
(define (prog-ref-class strong? old?)
  (if strong? 
      (if old? "program-reference-old-strong" "program-reference-strong")
      (if old? "program-reference-old-weak" "program-reference-weak")))

; Return the CSS class of a doc-prog anchor - to a file.
(define (file-prog-ref-class strong? old?)
  (if strong? 
      (if old? "program-reference-old-strong" "program-file-reference-strong")
      (if old? "program-reference-old-weak" "program-file-reference-weak")))

; Return the qualification (source key) of word, or #f if there
; is no qualification. The parameter word is of the form 
; *qual$ref, qual$ref, or just ref (loosely speaking).
; A qualification must match one of the source
; keys in source-key-list to qualify.
(define (qualified-program-link? word)
  (let ((end-qual (find-in-string word #\$)))
    (if end-qual
        (let* ((init-char (string-ref word 0))
               (start-qual 
                (cond ((eqv? init-char strong-link-char) 1)
                      ((eqv? init-char weak-link-char) 1)
                      ((eqv? init-char none-link-char) 1)
                      (else 0)))
               (candidate-qual (substring word start-qual end-qual)))
          (if (member candidate-qual source-key-list)
              candidate-qual
              #f))
        #f)))

; Return the proper linking word from word (without qualification and kind-information).
(define (proper-linking-word word qualification)
  (if qualification
      (substring word (+ 1 (find-in-string word #\$)) (string-length word))
      (let* ((init-char (string-ref word 0))
             (start (cond ((eqv? init-char strong-link-char) 1)
                          ((eqv? init-char weak-link-char) 1)
                          ((eqv? init-char none-link-char) 1)
                          (else 0))))
        (substring word start (string-length word)))))

         

; does the program link word start with a strong-link-char
(define (strong-program-link? word)
  (if (>= (string-length word) 1) 
      (eqv? (string-ref word 0) strong-link-char)
      #f))

; Return a symbol which classifies the linking word.
; Possible results are the symbols strong, weak, none.
; In case word does not begin with either strong-link-char, weak-link-char, or none-link-char
; the value of default-program-link is used.
; If word is empty, return #f
(define (kind-of-program-link? word)
  (if (>= (string-length word) 1)
      (let ((ch (string-ref word 0)))
        (cond ((eqv? ch strong-link-char) 'strong)
              ((eqv? ch weak-link-char) 'weak)
              ((eqv? ch none-link-char) 'none)
              (else default-program-link)))
      #f))

; Disregard the initial star of star-word. Assume that there is an initial star in star-word
(define (linking-word-of-strong-link star-word)
  (substring star-word 1 (string-length star-word)))

; Happens to be identical to linking-word-of-strong-link
(define (linking-word-of-other-link link-word)
  (substring link-word 1 (string-length link-word)))


         
; ---------------------------------------------------------------------------------------------------

; Return a list of duplicates in name-def-list.
; The value returned is a list of defining name occerences, like in the global list defining-name-occurences.
; name-def-list is a list of defining name occerences.
(define (duplicated-definitions name-def-list)
  (let* ((sorted-names (sort-list name-def-list name-entry-leq?))   ; defining name occurrences
         (paired-names (if (null? sorted-names) '() (pair-up sorted-names (cdr sorted-names))))
         (filtered-pairs (filter (lambda (p) (eq? (defined-name-of (car p)) (defined-name-of (cdr p)))) paired-names))
         (duplicate-names (map (compose defined-name-of car) filtered-pairs)))
    (filter (lambda (ne) (memq (defined-name-of ne) duplicate-names)) sorted-names)))

(define (present-duplicated-definitions)
  (let ((dd (duplicated-definitions (filter new-version-name? defining-name-occurences))))  ; duplicates in older versions diregarded
    (con
      (indent-pixels 10 
        (multi-column-list 4 (map present-a-duplicate dd) browser-pixel-width)) 

      (font-size 1 (em "General notice: Navigation to duplicates, which both occur in the same source file, is not supported")))))

; Present a single duplicate. d is a defining name entry, as occuring in the list defining-name-occurences.
(define (present-a-duplicate d)
 (let ((source-key (source-key-of d))
       (version (version-of d))
       (defined-name (defined-name-of d)))
   (con
    (a-tag-target
     (string-append (source-file-name-html-file source-key version 'small) ".html" "#" (as-string defined-name))
     (font-size 2 (as-string defined-name))
     "program-frame")
    (font-size 2 (span " in file " source-key)))))
      

; ---------------------------------------------------------------------------------------------------

;; Index support: total index of all defining name occurences.

(define (present-defined-name-index sorted-defining-name-occurences)
  (indent-pixels 10 
     (multi-column-list 6
       (map present-a-defining-name-entry sorted-defining-name-occurences) browser-pixel-width)))

; Present a defining name entry. The entry is of the same form as the elements in the global list defining-name-occurences.
(define (present-a-defining-name-entry dno)
 (let ((name (defined-name-of dno))
       (sourcefile (source-key-of dno))
       (version (version-of dno))
       (size default-program-font-size)
       (size-string (if (eq? default-program-font-size 'large) "-LARGE" ""))
      )
   (a
       (font-1 2 defined-color (as-string name))  
       'href (string-append (source-file-name-html-file sourcefile version size) ".html" "#" (as-string name))
       'target "program-frame"
       'title sourcefile)))



; ---------------------------------------------------------------------------------------------------

;; Index support: cross references involving both applied and defining name occurences

(define (applied-names-multiple-sources source-list-list)
 (letrec ((applied-name-entry-leq? (lambda (x y) (string<=? (as-string (car x)) (as-string (car y))))))
  (sort-list
   (accumulate-right
     append
     '()
     (map applied-names source-list-list))
   applied-name-entry-leq?)))

(define (applied-names source-list)
  (applied-names-1 source-list '()))

(define (applied-names-1 source-list res)
  (cond ((null? source-list) res)

        ((is-define-form? (car source-list)) 
           (let* ((define-form (car source-list))
                  (def-name (defined-name define-form))
                  (this-contribution (map (lambda (appl-name) (cons appl-name def-name)) (applied-names-one-form define-form))))
             (applied-names-1 (cdr source-list) (append this-contribution res))))

        (else (applied-names-1 (cdr source-list) res)) ;drop (car source-list) because it is a non-define form
))

(define (applied-names-one-form f)
 (cond ((eof-object? f)                 ; nothing
        )
       ((symbol? f) (if (defining-in-batch? f) (list f) '()))
       ((string? f) '())
       ((number? f) '())
       ((char? f) '())
       ((boolean? f) '())
       ((vector? f) (applied-names-one-form (vector->list f)))
       ((and (list? f) (null? f)) '())

       ; special processing of forms with defining names:
       ((and (list? f) (function-define-form? f)) (applied-names-one-form (cddr f)))
       ((and (list? f) (quote-form? f)) '())
       ((and (list? f) (unquote-form? f)) (applied-names-one-form (cadr f)))
       ((and (list? f) (quasiquote-form? f)) (applied-names-quasiquoted f))
       ((and (list? f) (is-define-form? f)) (applied-names-one-form (cddr f)))
       ((and (list? f) (lambda-form? f)) (applied-names-one-form (cddr f)))
       ((and (list? f) (let-form? f)) (append (applied-names-one-form (let-vals f)) (applied-names-one-form (cddr f))))
       ; ADD syntax-rules

       ((list? f) (append (applied-names-one-form (car f)) (applied-names-one-form (cdr f))))
       ((pair? f)                       ; improper list 
        (let ((p1 (proper-part f))
              (p2 (first-improper-part f)))
          (append (applied-names-one-form p1) (applied-names-one-form p2))
        ))
       (else (error (string-append "applied-names-one-form: unknown kind of expression" (as-string f))))))

(define (applied-names-quasiquoted qqf)
 (let ((unquoted-forms 
         (traverse-cons-cells 
           (lambda (x) (and (pair? x) (or (eq? (car x) 'unquote) (eq? (car x) 'unquote-splicing))))
           qqf)))
  (flatten (map applied-names-one-form  unquoted-forms))))


(define (defining-in-batch? name)
  (if (assq name defining-name-occurences) #t #f))


; ---------------------------------------------------------------------------------------------------
; Presentation of cross references

(define (present-cross-reference-index appl-def-name-list-1)
 (let* ((appl-def-name-sublisted ; @a
           (sublist-by-predicate appl-def-name-list-1 (lambda (x y n) (not (eq? (car x) (car y))))))
        (appl-def-name-sublisted-1 ; @b
           (map (lambda (sublist) 
                   (remove-duplicates-by-predicate
                     sublist
                     (lambda (x y) (eq? (cdr x) (cdr y)))))
           appl-def-name-sublisted))
       )
  (indent-pixels 5 
    (table-3 0 
             (list 200 1000)
             (map present-applied-sublist appl-def-name-sublisted-1)))))


(define (present-applied-sublist sl)
  (let* ((sorted-sl (sort-list sl (lambda (x y) (string<=? (as-string (cdr x)) (as-string (cdr y))))))
         (appl-name (car (car sl)))     ; take the first element of an arbitrary entry, the first
         (def-table 
           (multi-column-list 5
                              (map present-defined-entry sorted-sl) (- browser-pixel-width 200)))
         (sourcefile (source-key-of-defining-name appl-name))
        )
   (list (div  
          (a-name (as-string appl-name)) ; name this entry in the cross reference index, allows direct access to entry from program.
          (box                           ; box it in order to allign with def-table
           (a
            (b (font-1 2 defined-color (as-string appl-name)))  ; earlier: html-protect
            'href (string-append (source-file-name-html-file sourcefile (highest-version-number sourcefile) default-program-font-size)
                                 ".html" "#" (as-string appl-name))
            'target "program-frame"
            'title sourcefile)))
          def-table)))


(define (present-defined-entry appl-def-entry)
 (let* ((appl-name (car appl-def-entry))
        (def-name (cdr appl-def-entry))
        (sourcefile (source-key-of-defining-name def-name))
       )
   (if def-name
     (a
       (font-size 2 (as-string def-name))  ; earlier: html-protect
       'href (string-append (source-file-name-html-file sourcefile (highest-version-number sourcefile) default-program-font-size)
                            ".html" "#" (as-string def-name))
       'target "program-frame"
       'title sourcefile)
     (font-size 2 (em "top level")))))


; lookup the source key (file name information) of the name in defining-name-occurences.
(define (source-key-of-defining-name name)
 (let ((res (filter (lambda (dno-entry) (eq? name (defined-name-of dno-entry)))  defining-name-occurences)))
   (cond ((= (length res) 0) "??")            ; question mark, leading to undefined link
         ((= (length res) 1) (source-key-of (car res))) ; the normal case
         ((> (length res) 1) (source-key-of (car res))) ; we take the first
   )))

; Merge the def-applied-list and the def-list to a single list.
; Both def-applied-list and def-list are alists with entries of the form (name . name-of-definition).
; def-list is just a list of pairs of the form (name . #f) reflecting all the definitions in the documentation bundle.
; The resulting list is also sorted.
(define (merge-defined-and-defined-applied-lists def-applied-list def-list)
   (merge-defined-and-defined-applied-lists-1 def-applied-list def-list '()))

; The special purpose asymmetric merge used above.
(define (merge-defined-and-defined-applied-lists-1 lst1 lst2 res)
 (letrec ((lt-cars? (lambda (x y) (string<? (as-string (car x)) (as-string (car y)))))
          (eq-cars? (lambda (x y) (eq? (car x) (car y))))
         )
  (cond ((and (null? lst1) (null? lst2)) (reverse res))
        ((null? lst1) (append (reverse res) lst2))
        ((null? lst2) (append (reverse res) lst1))
        ((eq-cars? (car lst1) (car lst2)) ; normal case
           (merge-defined-and-defined-applied-lists-1 (cdr lst1) (cdr lst2) (cons (car lst1) res)))
        ((lt-cars? (car lst1) (car lst2)) ; should not happen
           (merge-defined-and-defined-applied-lists-1 (cdr lst1) lst2 (cons (car lst1) res)))
        ((lt-cars? (car lst2) (car lst1)) ; if there is a defined name which is not applied
           (merge-defined-and-defined-applied-lists-1 lst1 (cdr lst2) (cons (car lst2) res)))
        (else (error "merge-defined-and-defined-applied-lists-1: should not happen!")))))
        
; Present the doc-elements in a two column list. If kind is 'detail show toc entries for both
; sections and entries. If kind is 'overall only show sections.
(define (present-documentation-contents doc-elements kind)
  (let ((doc-elements-1
          (cond ((eq? kind 'detail) doc-elements)
                ((eq? kind 'overall) (filter (lambda (e) (eq? (get-value 'kind e) 'section)) doc-elements)))))
  (n-column-list 
    (if (eq? kind 'detail) toc-columns-detail toc-columns-overall)
    (map present-documentation-content-element doc-elements-1)
     browser-pixel-width)))

(define (present-documentation-content-element element)
  (let ((kind (get-value 'kind element))
        (doc-id (get-value 'id element))
        (n (get-value 'numbering element))
        (ttl (get-value 'doc-title element)))
   (font-size 2
    (span 
     (cond ((eq? kind 'entry) (horizontal-space 4))
           ((eq? kind 'section)  "")
           (else (error "present-documentation-content-element: unknown kind of documentation element")))
     n (horizontal-space 2)
     (a-tag-target (string-append "documentation.html" "#" (as-string doc-id))
                       ttl
                       "documentation-frame"
                       )))))

; Return a string which represents an entry in a local table of contents
; within a documentation section
(define (present-documentation-subsection-element element)
  (let ((doc-id (get-value 'id element))
        (n (get-value 'numbering element))
        (ttl (get-value 'doc-title element)))
   (font-size 2
    (span 
     n (horizontal-space 2)
     (a-tag-target (string-append "documentation.html" "#" (as-string doc-id))
                       (font-color black ttl)
                       "documentation-frame"
                       )))))



; ---------------------------------------------------------------------------------------------------
;;; Support of a simple, line-base documentation text format. 
;;; This is an alternative to the lisp format based on documentation-entry and documentation-section
;;; forms.

; The format is

; .SECTION eee
; .TITLE ttt
; .BODY
; Section text
; More section text
; .END
; -----------------------------------------------------------------------------
; 
; .ENTRY eee
; .TITLE ttt
; .BODY
; Entry text
; More entry text
; .END
; -----------------------------------------------------------------------------

; A line starting with -- is a comment
; The lines formed of dashes are comments, and thus ignored.


;; Extract and parse documentation from a simple text file, addressed
;; the src attribute of doc-from-ast. 
;; The src attribute can either be absolute, or relative to the source-directory.
;; Translate to the documentation-section and -entry forms, and evaluate these.
;; Thus, a documentation-from clause is equivalent to the sequence of documentation-section
;; and documentation-entry forms.
(define (do-documentation-from! doc-from-ast)
  (set! documentation-approach 'textual)
  (let* ((file (ast-attribute doc-from-ast 'src))
         (file-path (if (absolute-file-path? file) file (string-append source-directory file)))
        )
   (if elucidator-verbose-mode (display-message (string-append "Parsing the textual documentation file")))
   (reset-collection)
   (let* ((ip (open-input-file file-path)))
      (documentation-intro-from-port ip)
      (documentation-units-from-port ip)
      (close-input-port ip))))

(define (documentation-intro-from-port ip)
  (let* ((skip1 (skip-while white-space-or-separator? ip))
         (intro (accept-documentation-intro ip)))
    (define-documentation-intro! intro)))


(define (documentation-units-from-port ip)
 (let* ((skip1 (skip-while white-space-or-separator? ip))
        (unit (accept-documentation-unit ip))
        (separator-skip (skip-while white-space-or-separator? ip))
       )
    (define-unit! unit)
    (if (not (eof-object? next-doc-char))
        (documentation-units-from-port ip))))


; As a contrast to elucidator-1, this procedures does not any longer evaluates any Scheme function, by means of eval.
(define (define-unit! unit)
  (let ((doc-form (make-documentation-form unit)))
    (cond ((eq? (car doc-form) 'textual-documentation-section)
             (process-textual-documentation-section (cdr doc-form)))
          ((eq? (car doc-form) 'textual-documentation-entry)
             (process-textual-documentation-entry (cdr doc-form)))
          (else (laml-error "define-unit!: Unknown kind of unit" (car doc-form))))))

(define (define-documentation-intro! intro-list)
 (set-documentation-constituents!
    (first intro-list) (second intro-list) (third intro-list)
    (fourth intro-list) (fifth intro-list)))

;; Define the title, affiliation, author, affiliation, and the abstract
(define (set-documentation-constituents! title author email affiliation abstract)
  (set! documentation-title title)
  (set! documentation-author author)
  (set! documentation-email email)
  (set! documentation-affiliation affiliation)
  (set! documentation-abstract abstract))


; Transform a unit, as extracted from the documentation, to a documentation-entry or documentation-section Lisp form
(define (make-documentation-form unit)
  (let* ((kind-string (car (car unit)))
         (kind (cond ((equal? kind-string ".ENTRY") 'textual-documentation-entry)
                     ((equal? kind-string ".SECTION") 'textual-documentation-section)
                     (else (error "make-documentation-form: Unknown documentation kind"))))
         (id (as-symbol (cadr (car unit))))
         (title (cadr unit))
         (body (caddr unit)))
   (list kind
         (list 'id id)
         (list 'doc-title title)
         (list 'doc-body body))))
        

(define (accept-documentation-unit ip)
 (let* ((id   (accept-doc-id ip))
        (ttl  (accept-doc-title ip))
        (bd   (accept-doc-body ip)))
    (list id ttl bd)))

(define (accept-documentation-intro ip)  ; title, author, email, affiliation, and abstract
 (let* ((ttl    (accept-doc-title ip))
        (aut    (accept-doc-author ip))
        (email  (accept-doc-email ip))
        (af     (accept-doc-affiliation ip))
        (abstr  (accept-doc-abstract ip))
       )
    (list ttl aut email af abstr)))



(define (accept-doc-author ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".AUTHOR") ".AUTHOR expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-email ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".EMAIL") ".EMAIL expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-affiliation ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".AFFILIATION") ".AFFILIATION expected"))
         (skip1 (skip-while is-white-space? ip))
         (res (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   res))

(define (accept-doc-abstract ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".ABSTRACT") ".ABSTRACT expected"))
         (skip1 (skip-while is-white-space? ip))
         (body (accept-body-text ip)))
   body))


; assume the next char is the dot in ENTRY or SECTION
(define (accept-doc-id ip)
  (let* ((unit (collect-until is-white-space? ip))
         (res (doc-check (or (equal? unit ".ENTRY") (equal? unit ".SECTION")) ".ENTRY or .SECTION expected"))
         (skip1 (skip-while is-white-space? ip))
         (id (collect-until is-white-space? ip))
         (skip2 (skip-while is-white-space? ip)))
    (list unit id)))

(define (accept-doc-title ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".TITLE") ".TITLE expected"))
         (skip1 (skip-while is-white-space? ip))
         (ttl (collect-until end-of-line? ip))
         (skip2 (skip-while is-white-space? ip)))
   ttl))

(define (accept-doc-body ip)
  (let* ((keyword (collect-until is-white-space? ip))
         (res (doc-check (equal? keyword ".BODY") ".BODY expected"))
         (skip1 (skip-while is-white-space? ip))
         (body (accept-body-text ip)))
   body))

(define (accept-body-text ip)
  (let* ((body-list (reverse (accept-body-text-1 ip '())))
         (cr-list (make-list (- (length body-list) 1) CR-string)))
   (string-merge 
     body-list
     cr-list)))

(define CR-string (as-string #\newline))


(define (accept-body-text-1 ip res)
 (let* ((line (collect-until end-of-line? ip))
        (skip1 (eat-eol-chars ip)))
    (cond ((end-unit? line) res)
           (else (accept-body-text-1 ip (cons line res))))))

(define (doc-check condition error-text)
  (if (not condition)
      (error (string-append "Line " (as-string doc-line-number) ": " error-text))))

(define (end-unit? line)
  (if (< (string-length line) 4)
      #f
      (equal? ".END" (substring line 0 4))))

; (define (end-unit? line)
;   (if (< (string-length line) 4)
;       #f
;       (let ((res (equal? ".END" (substring line 0 4))))
;         (if res
;             (display "end unit")
;             (display (string-append "not end unit: " line))
;         )
;         res)))
             
 


; Collection and skipping functions: Functions to read characters from an input port.
; All functions (as used above) are linebased. As such we can relatively safely assume that there is an upper
; limit on the amount of characters to be collected (although, of course, lines can be long...)

; Collection state variables and constants:
(define buffer-length 10000)
(define collection-buffer (make-string buffer-length #\space))
(define next-doc-char #f)
(define doc-line-number 1)



(define (reset-collection)
  (set! collection-buffer (make-string buffer-length #\space))
  (set! next-doc-char #f)
  (set! doc-line-number 1))

; return the string collected from the input port ip.
; collection stops when the predicate p holds holds on the character read.
; The last read character is putted back in the variable next-doc-char
(define (collect-until p ip)
  (collect-until-1 p ip collection-buffer 0)
)

(define (collect-until-1 p ip buffer next)
 (let ((ch (read-next-doc-char ip)))
   (if (or (p ch) (eof-object? ch))
       (begin
          (set! next-doc-char ch)
          (substring buffer 0 next))
       (begin 
         (string-set! buffer next ch)
         (collect-until-1 p ip buffer (+ 1 next))))))



(define (read-next-doc-char ip)
  (if next-doc-char
      (let ((res next-doc-char))
        (set! next-doc-char #f)
        res)
      (let ((ch (read-char ip)))
         (if (and (not (eof-object? ch)) (= 10 (char->integer ch))) (set! doc-line-number (+ doc-line-number 1)))
         ch)))


; skip characters on ip while p holds
(define (skip-while p ip)
 (let ((ch (read-next-doc-char ip)))
   (if (p ch)
       (skip-while p ip)
       (set! next-doc-char ch))))

; Situation: an eol character (13 (CR) on a PC) is in next-doc-char.
; Drop the buffer, and read a 10 char (LF) if it is there
; Should also work on UNIX
(define (eat-eol-chars ip)
  (let ((ch (read-char ip)))
    (cond ((eof-object? ch) (set! next-doc-char ch))        ; allow the eof condition to be rediscovered by the context
          ((= 10 (as-number ch)) (set! next-doc-char #f))  ; force real reading from from ip next time. Buffer is empty
          (else (set! next-doc-char ch)))))                ; put ch in buffer such that it will be read again
  
; not used
(define (skip-once p ip)
 (let ((ch (read-next-doc-char ip)))
   (if (p ch)
       (let ((ch (read-next-doc-char ip)))
         (set! next-doc-char ch))
       (set! next-doc-char ch))))


; Useful predicates

(define (is-white-space? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
        (or (eqv? n 32) (eqv? n 9) (eqv? n 10) (eqv? n 12) (eqv? n 13)))))

(define (white-space-or-separator? ch)
  (if (eof? ch) 
      #f
      (or (is-white-space? ch) (eqv? #\- ch))))

(define (end-of-line? ch)
  (if (eof? ch) 
      #f
      (let ((n (as-number ch)))
         (or  (eqv? n 10) (eqv? n 13)))))

(define (eof? ch)
  (eof-object? ch))



; ---------------------------------------------------------------------------------------------------
; Procedure making the elucidator help file
(define (make-elucidator-help-page)
 (let ((kn-email "normark@cs.auc.dk")
       (kn-www "http://www.cs.aau.dk/~normark/")
       (help-image (lambda (image-name) (image image-name (string-append "Icon name: " image-name))))
      )
  (letrec ((an-entry (lambda (x y) (li (font-color red (b x)) (br) y))))
   (write-html 'raw
    (let ((color-attributes (bg-text-link-vlink-colors white black blue blue)))
      (html 
       (head (title "Elucidator help page"))
       (body color-attributes
       (h 1 (font-color blue "The Elucidator Help Page"))

       (p (em "Revised December 2004"))

       (p "The " 
        (a-tag-target "http://www.cs.aau.dk/~normark/elucidative-programming/index.html" "elucidative programming home page" "elu-home") " and the " 
        (a-tag-target "http://dopu.cs.auc.dk" "DOPU page" "dopu-home") " are the primary places to find information
        about elucidative programming.")

       (p "The pages shown in this browser are the result of processing a number of programs
           and a documentation file with the Scheme Elucidator. The main purpose is to present" 
          (em "internal program documentation")
          "side by side with a number of source programs. The leftmost window shows the documentation,
           and the rightmost window one of the programs. The topmost window is a menu and index
           window, from which a number of aspects can be controlled." )

       (p (em "Elucidative programming") "is variant of" 
          (a-tag "http://www.loria.fr/services/tex/english/litte.html" "literate programming")
          _"," 
          "as coined by Knuth in the early eighties. In most literate programming tools (called
           WEB tools), fragments of programs are defined inside the program documentation. In
           literate programming, a tool (called tangle) can extract and assemble the program
           fragments according to  the rules of the programming language. Another tool (called
           weave) formats the documentation, generates indexes, and presents all of it in a
           nice-looking format." )

       (p "The main characteristics of Elucidative Programming in relation to Literate Programming are:")

       (ol 
          (an-entry "The program source files are not affected at all."
                 "It is not necessary to split the programs into fragments,
                  and to organize these in the context of the program explanations.
                  An existing program source file can be handled.")

          (an-entry "The program and the documentation are shown side by side."
                 "We do not go for an embedded presentation of the program inside its documentation.
                  Rather, we provide for mutual navigation
                  between program and documentation in a two-frame layout")

          (an-entry "The program units which we document, are whole abstractions."
                 "Things get simpler when we can settle on documentation of named abstractions
                  instead of arbitrary program fragments (sometimes called 'chunks' or 'scraps')")

          (an-entry "An elucidative program is presented on-line, in an Internet browser." 
                    "Literate programming tools are primary oriented towards presentation of
                     the weaved results on paper." )

          (an-entry "The elucidator tool use specific knowledge about the programming language."
                  (span "The language knowledge is used to identify the names in the program. Applied names are related to their definitions,
                  and the program is decorated with colors and extensive linking. Currently we support the programming language " 
                  (a-tag "http://www.schemers.org" "Scheme") " and Java (see " (a-tag "http://dopu.cs.auc.dk/portal.html" "the web pages about the Java elucidator") ").")
          )

          (an-entry "Program and documentation indexes are available."
                 "A table of contents, an index of the program definitions, and a cross reference index is available")

          (an-entry "The creation of an elucidative program
                     is supported by a special set of Emacs editor commands."
                 "In that way it is realistic to handle the practical aspect of documenting a program while it is written")
       )

       (p "A" (em "documentation bundle") 
          "consist of a single documentation file, a number of program files, and
           a setup file. The documentation file can be written in Scheme with LAML (using a mirror
           of a particular XML language for elucidative programming). In case the documentation
           is written in Scheme, the setup and the documentation are written in the same file.  Alternatively,
           the documentation can be written in simple, separate textual format, which allows
           the use of native HTML tags for formatting. As mentioned above, there are
           no special requirements to the program files. The setup file is an XML-in-LAML Scheme
           file, which describes the the constituents of the documentation bundle
           together with a number of processing parameters. Running the setup file
           through a Scheme processor generates the HTML pages shown in this browser." )

       (p "The icons in the menu and index frame (at the top) are now described:")
          

       (table-3
         1
         (list 100 600)
         (list
          (map b (list "Icon" "Explanation"))
          (list (help-image "three-frames.gif")
                "Reset the elucidator to vertical layout (the default layout). All frames are reverted to the 'start position'.") 

          (list (help-image "three-frames-horizontal.gif")  
                "Reset the elucidator to a horizontal layout. This is an alternative layout in which the documentation and
                 a selected program are shown under each other, in full width")

          (list (help-image "index.gif" )
                "Presents an index of all defined names in the menu and index frame, just below the icons at the top of the window.
                The index is pr. default broken into fragments according to starting letter of the defined name.")
 
          (list (help-image "cross-index.gif" )
                "Presents a cross reference index in the menu and index frame.
                A cross reference index relates all applied names to the definition, in which they occur.
                The index is pr. default broken into fragments according to starting letter of the applied name.")
 
          (list (help-image "xx.gif") 
                "Present an index of all named defined more than once in the documentation bundle.
                This is useful information in a Lisp program")

          (list (help-image "overall-contents.gif")  "Present an overall table of contents for the documentation in the menu and index frame.
                                                    This table of contents only covers the top-level section, but no subsections.")
 
          (list (help-image "contents.gif")  "Present a table of contents for the documentation in the menu and index frame.
                                            This table of contents convers both top-level sections and subsections (also called entries).")

          (list (help-image "question-left-arrow.gif")  "Present this Elucidator help page in the documentation frame to the left")
          (list (help-image "question-right-arrow.gif") "Present this Elucidator help page in the program frame to the right") 
         )
       )

       (cond  ((eq? program-menu-approach 'separate-frame)
                (p "The menu in the upper rightmost frame lists all source programs in the current documentation
                    bundle. If you select an item in the menu, the selected program will be shown in the
                    program frame. It is possible to switch between showing program source files and SchemeDoc
                    manual files." ))
              ((eq? program-menu-approach 'inline-table)
                 (p "The icons in the rightmost group allow navigation to each of the program files in a documentation bundle."))
              (else ""))

       (p "From the documentation frame (the large to the left) it is possible to adjust the program window, such
           that a given piece of  program is shown. Similarly, from the program frame (the large frame to the right),
           the yellow left arrows" (help-image "doc-left.gif") 
          "can be used to find the  section in the documentation, which" (em "explains") 
          "the particular program unit. The light yellow arrows" (help-image "doc-left-weak.gif") 
          "refer to a documentation section which" (em "mentions") 
          "the definition (as opposed to explaining it).  We talk about strong  and weak relations between the documentation
           and the program resp. Besides these means of navigation it is possible to navigate inside the documentation
           frame (from section to section), and inside the program frames (from applied name to the similar definitions,
           as well as to cross reference  indexes)." )

       (p "Inside the program and inside documentation sections you may find small color bullets like" 
          (help-image "source-mark-red.gif")_ "." "These are called" (em "source markers") _ "." 
          "The source markers are used to point out a particular place in a piece of program, which is discussed in a documentation
           section. You can click on a source marker in the documentation in order to navigate to the corresponding source
           marker in the program. Also navigation in the opposite direction is supported from most source markers. The popup
           text, which appears in most browsers when the cursor rests on a source marker, gives useful additional information
           about the source marker. Notice that a source marker in the documentation is associated with the closest preceding" 
          (em "strong")"documentation-program relation." )

       (if (and (eq? program-menu-approach 'separate-frame) (> (length manual-source-list) 0))
           (p "This instance of the Scheme Elucidator has " (as-string (length manual-source-list)) "different links to
               SchemeDoc manuals. You find these in the upper right corner, possibly by pressing
               " (string-it "Show Manual Menu") _ "." "The links to SchemeDoc manuals are dark green in the default CSS stylesheet.")
           "")

       (cond ((eq? 'r4rs the-scheme-report-version)
                (p "This Elucidator provides links the R4RS Scheme manual. Links to syntax items
                    are anchored in black bold words. Links to standard procedures are dark brown in the
                    default CSS stylesheet."))
             ((eq? 'r5rs the-scheme-report-version)
                (p "This Elucidator provides links the R5RS Scheme manual. Links to syntax items
                    are anchored in black bold words. Links to standard procedures are dark brown in the
                    default CSS stylesheet."))
             (else ""))
  
       (p "The source programs are, by default, shown using a fairly small font size. The small square symbols "
                  (help-image "small-square.gif") " can be used to toggle the program frames to use larger font.
                  Notice that the small square symbol is only shown in certain configurations 
       (when the variable " (kbd "make-large-source-files?") " is true or in the case that variable
        default-program-font-size is set to large)")

       (p "The icon " (help-image "small-green-up-triangle.gif") 
        " is an anchor of a link from a definition to an entry in the cross reference index. 
        This link is very convenient because it allows us to follow call chains via the cross reference index:
        Go from a definition of N to the cross reference entry N. Find via that entry a function F which calls N;
        Go the cross reference entry of F, and find a function G which calls F, etc.")

       (p "Many details of the presentation, such as colors, can be controlled via CSS stylesheets. You
           may have a" (kbd "stylesheets") 
           "directory in the directory with the LAML setup file. Within this directory two files," 
           (kbd "program.css")"and" (kbd "documentation.css") _ "," 
           "affect  the presentation details. Your own" (kbd "program.css") "and" 
           (kbd "documentation.css")
           "files are appended to the systems CSS files, thereby overruling the systems presentation preferences.
           You should take a look at the CSS stylesheets that come with the Scheme Elucidator. They are
           located in" (kbd "styles/xml-in-laml/elucidator/stylesheets/") "relative to you" 
           (kbd "laml")"directory." )

       (p "The Scheme Elucidator can handle a program source files in several different versions.
           The grey arrows," (help-image "gray-left-arrow.gif") "and" (help-image "gray-right-arrow.gif") _ "," "shown in the program frame,"
           "allow navigation from one version to another. In the documentation, links shown on a" 
           (span 'css:background (rgb-color-encoding 200 200 200) "grey background") _ "," "
            go to older versions of the program. - When versions are handled, the icons"
           (help-image "new.gif") _ "," (help-image "updated.gif") "," (help-image "renamed.gif") _ ","
           (help-image "moved.gif") _ 
           ", and" (help-image "no-pass-sign.gif")
           "are used to signal a new definition in this version, an existing but updated definition in this version,
            a definition renamed from relative to a similar defintion in an older version,
            an identical definition moved from another source file,
            and a definition which does not appear in the next version, repectively. - The icons" (help-image "doc-left-point.gif") "and" 
            (help-image "doc-left-weak-point.gif") "are used to refer to documention, in which we have asked for 
            " (em "specific versions") "of a program unit. Thus, these icons refer to documentation of specif
            versions of the software."
       )

       (p "The elucidator is written in Scheme, using the "
                  (a-tag "http://www.cs.aau.dk/~normark/laml/" "LAML") " software package.")

       (p (em "You can use the browser's back button to establish the original contents of this frame,
            or you can activate the reset elucidator icon in the top left corner
            to return to the standard layout."))


       (p "Kurt NÃ¸rmark" (br) "Aalborg University" (br) kn-email (br) (a-tag kn-www)) )))
   (html-destination "elucidator-help") ))))



; ---------------------------------------------------------------------------------------------------
; Source markers

; The association between marker characters and colors
(define marker-associations
  (list 
    (list #\a "red"      '(255 0 0))
    (list #\b "green"    '(0 128 0))
    (list #\c "blue"     '(0 0 255))
    (list #\d "black"    '(0 0 0))
    (list #\e "maroon"   '(128 0 0))
    (list #\f "grey"     '(128 128 128))
    (list #\g "purple"   '(128 0 128))
    (list #\h "silver"   '(192 192 192))
    (list #\i "tetal"    '(0 128 128))
    (list #\j "aqua"     '(0 255 255))
    (list #\k "lime"     '(0 255 0))
    (list #\l "olive"    '(128 128 0))
    (list #\m "yellow"   '(255 255 0))
    (list #\n "navy"     '(0 0 128))
    (list #\o "fuchsia"  '(255 0 255))
   ))

; Return a source marker for the character ch.
; A source marker is graphical image, which identifies a particular place in a source program.
(define (source-marker-image ch explanation)
  (let* ((ch1 (as-char ch))
         (ass-res (assv ch1 marker-associations))
         (color (if ass-res (cadr ass-res) "error")))
     (image (string-append "source-mark-" color ".gif") explanation)))

; Return the source marker glyph (text or image) depending on ch (a char) and the global variable source-marker-kind 
(define (source-marker-glyph ch explanation)
  (cond ((eq? source-marker-kind 'as-text) (source-marker-text ch #f))
        ((eq? source-marker-kind 'as-colored-text) (source-marker-text ch #t))
        ((eq? source-marker-kind 'as-image) (source-marker-image ch explanation))
        (else (error (string-append 
                        "source-marker-glyph: Problems determining the kind of source marker in the documentation: "
                        (as-string source-marker))))))

; return the string "c marker" where c is a color.
; if color? then color the string by means of font-color application
(define (source-marker-text ch color?)
  (let* ((ch1 (as-char ch))
         (ass-res (assv ch1 marker-associations))
         (color (if ass-res (cadr ass-res) "??"))
         (text (string-append color " " "marker"))
         (rgb-list (if ass-res (caddr ass-res) '(0 0 0))))
    (font-color (if color? rgb-list '(0 0 0)) (b text))))

; Return an anchor tag of the source marker mark-glyph.
; This is a link from the documentation to a source program location.
; The destination of the anchor is determined by the global variable previous-strong-program-word (a string),
; as encountered earlier in the documentation text.
; mark-glyph and mark-chaar are both strings.
(define (source-mark-anchor mark-glyph mark-char)
  (let ((link-targets (filter 
                        (lambda (dno-entry)
                          (and (equal? previous-strong-program-word (as-string (defined-name-of dno-entry)))
                               (= previous-strong-source-file-version-number (version-of dno-entry))))
                         defining-name-occurences))
        (size default-program-font-size)
       )
     (cond ((= (length link-targets) 0)
            (display-warning 
             "In documentation: Impossible linking from source marker" mark-char "via" previous-strong-program-word)
            mark-glyph)
           ((= (length link-targets) 1)
            (let ((source-key (source-key-of (car link-targets)))
                  (source-version (version-of (car link-targets)))
                 )
              (a-tag-target 
               (string-append (source-file-name-html-file source-key source-version size)
                              ".html" "#" previous-strong-program-word "-@" mark-char)
               mark-glyph
               "program-frame")))
           ((>= (length link-targets) 1)
            (let ((source-key (source-key-of (car link-targets)))
                  (source-version (version-of (car link-targets)))
                 )
              (display-warning "In documentation: Ambiguous linking from source marker "
                                mark-char " via" previous-strong-program-word ".("  (length link-targets) "possibilities )")
              (a-tag-target 
               (string-append (source-file-name-html-file source-key source-version size)
                              ".html" "#" previous-strong-program-word "-@" mark-char)
               mark-glyph
               "program-frame"))))))


; ---------------------------------------------------------------------------------------------------
; Making section navigation banners which allow us to navigate to parrent and sibling sections and entries.

; Return a banner which navigates to up, next and down URLs of
; doc-el, which is the elements of a section or entry.
(define (elucidator-section-navigation-banner doc-el)
  (let* ((cur-nums (get-value 'raw-numbering doc-el))
         (cur-sect (car cur-nums))
         (cur-subsect (cadr cur-nums)))
    (if (= 0 cur-subsect) ; a section
        (let ((up (documentation-url "START"))
              (prev (if (= 1 cur-sect)  ; @a
                        #f
                        (doc-section-url (- cur-sect 1) 0)))
              (next (doc-section-url (+ cur-sect 1) 0)))
          (elucidator-section-navigation-banner-1 doc-el up prev next))
        (let ((up (doc-section-url cur-sect 0)) ; an entry
              (prev (if (= 1 cur-subsect) ; @b
                        #f
                        (doc-section-url cur-sect (- cur-subsect 1))))
              (next (doc-section-url cur-sect (+ cur-subsect 1))))
          (elucidator-section-navigation-banner-1 doc-el up prev next)))))


; Return a banner which navigates to the URLs up, prev, and next in doc-el.
; up, prev, and next may be an URL (a string) or a cons-pair of (URL . section-title), or #f.
; If one of these are #f, present a blind navigation button.
; This function handles the presentation details given the URLs passed as parameters.
(define (elucidator-section-navigation-banner-1 doc-el up prev next)
 (letrec ((url-of (lambda (x) (cond ((pair? x) (car x))
                                     ((string? x) x)
                                     (else (error "url-of: unknown type of parameter")))))
           (title-of (lambda (x) (cond ((pair? x) (cdr x))
                                       ((string? x) "")
                                       (else (error "title-of: unknown type of parameter"))))))
  (con
    (if up (a-tag (url-of up) (image "small-up.gif" (title-of up))) (image "small-up-blind.gif" "")) 
    (if prev (a-tag  (url-of prev) (image "small-prev.gif" (title-of prev))) (image "small-prev-blind.gif" "")) 
    (if next (a-tag (url-of next) (image "small-next.gif" (title-of next))) (image "small-next-blind.gif" "")))))


; Return a cons pair of URL and section title of documentation entry n.m
; if m is 0, we mean section n.
; If no such entry/section exists, return #f
(define (doc-section-url n m)
  (let ((res (filter (section-subsection? n m) documentation-elements)))
    (cond ((= 1 (length res)) 
             (let* ((element (car res))
                    (id (get-value 'id element))
                    (ttl (get-value 'doc-title element))
                   )
               (cons (documentation-url id) ttl)))
          ((= 0 (length res)) #f)
          ((> (length res) 1) 
             (error (string-append "doc-subsection-url: multiple sections/entries cannot exists: "
                                   (as-string n) "." (as-string m)))))))

; ---------------------------------------------------------------------------------------------------
; Splitted cross reference index.

(define (split-defined-applied-names dan-list)
  (sublist-by-predicate
    dan-list
    (lambda (cur prev n) ;@a
       (not (eqv? (string-ref (as-string (car cur)) 0)
                  (string-ref (as-string (car prev)) 0))))))


(define (first-letter-of x)
  (as-string (string-ref (as-string x) 0)))

;; makes a cross reference index for a single letter
(define (make-cross-reference-index da-names letter alphabet)
  (write-html 'raw
   (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
	   (html 
            (head (title (string-append "Alphabetic cross reference index: letter " letter)))
            (body color-attributes
		  (icon-bar)  

		  (b (font-1 3 blue "Cross reference index: ")) (horizontal-space 2)
		  (alphabetic-link-array-1 "cross-reference-index" alphabet letter) ; at top
		  (present-cross-reference-index da-names) (p) ; fejl! Lav specialiseret udgave
		  (alphabetic-link-array-1 "cross-reference-index" alphabet letter) ; at bottom
		  (vertical-space 8)              
		  )))
   (html-destination (string-append "cross-reference-index" "-" (hygienic-file-character (downcase-string letter))))))

; Make the overall cross reference index, in terms of an alphabet array with links to smaller indexes.
(define (make-overall-cross-reference-index alphabet)
  (write-html 'raw
   (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
     (html 
      (head (title "Overall alphabetic cross reference index"))
      (body color-attributes
	    (icon-bar)  

	    (b (font-1 3 blue "Cross reference index: ")) (horizontal-space 2)
	    (alphabetic-link-array-1
             "cross-reference-index"
	     (map downcase-string alphabet)) (br)
            (font-size 2 (em "Navigate to subindexes via tha alphabet above"))              
            )))
   (html-destination "cross-reference-index")))


; ---------------------------------------------------------------------------------------------------
; Splitted defining name index.

(define (split-defining-name-occurences dno)
  (sublist-by-predicate
    dno
    (lambda (cur prev n)
       (not (eqv? (string-ref (as-string (defined-name-of cur)) 0)
                  (string-ref (as-string (defined-name-of prev)) 0))))))

(define (make-defining-name-index dno letter alphabet)
  (write-html 'raw
   (let ((color-attributes (bg-text-link-vlink-colors  (color-of-group "index") black black black)))
     (html 
      (head (title (string-append "Defining name index: letter " letter)))
      (body color-attributes
	    (icon-bar)  

	    (b (font-1 3 blue "Index of definitions: ")) (horizontal-space 2)
	    (alphabetic-link-array-1 "defining-name-index" alphabet letter) ; at top
	    (present-defined-name-index dno)              
            )))
   (html-destination (string-append "defining-name-index" "-"  (hygienic-file-character (downcase-string letter))))))

(define (make-overall-defining-name-index alphabet)
  (write-html 'raw
   (let ((color-attributes (bg-text-link-vlink-colors (color-of-group "index") black black black)))
     (html 
      (head (title "Overall defining name index"))
      (body color-attributes
	    (icon-bar)

	    (b (font-1 3 blue "Index of definitions: ")) (horizontal-space 2)
	    (alphabetic-link-array-1
	     "defining-name-index"
	     (map downcase-string alphabet)) (br)
	    (font-size 2 (em "Navigate to subindexes via tha alphabet above"))              
            )))
   (html-destination "defining-name-index")))

; ---------------------------------------------------------------------------------------------------------------

; A high level syntax function for definition of the color scheme
; Returns an association list that maps group strings to colors
(define (make-color-scheme . group-color-plist)
  (propertylist-to-alist group-color-plist))

; Return the color that is going to represent group.
; Group is the string given as group in program-source forms
(define (color-of-group group)
 (if elucidator-color-scheme
  (let ((group-color (assoc group elucidator-color-scheme)))
    (if (pair? group-color)
        (cdr group-color)
        default-background-color))
  default-background-color))




; A redefinition from laml.scm - due to use of the very old mirror in the elucidator.

;; Return the LAML POWER icon with link to the LAML home page.
;; Intended for the footer of LAML generated pages, from which the author wish to acknowledge the use of LAML.
;; The LAML icon is located in (string-append (laml-home-url-prefix extra-level) "images/laml-power-icon-1.gif"),
;; where extra-level is the optional parameter of the current function.
;; The optional parameter extra-level can be given if the generated HTML files are placed in a different directory than the startup directory.
;; The default value is 0.
;; The optional parameter icon-size can either be small or large. large is the default value.
;; The role of extra-level is the same as in the procedure laml-home-url-prefix.
;; .form (laml-power-icon [extra-level icon-size])
;; .internal-references "related procedure" "laml-home-url-prefix"
(define (laml-power-icon . optional-parameter-list)
 (let ((extra-level (optional-parameter 1 optional-parameter-list 0))
       (icon-size   (as-symbol (optional-parameter 2 optional-parameter-list 'large)))
      )
   (a-tag-target "http://www.cs.aau.dk/~normark/laml/"
      (img 'src (string-append 
                              (cond ((eq? icon-size 'large) "images/laml-power-icon-1.gif")
                                    ((eq? icon-size 'small) "images/laml-mini-icon-1.gif")
                                    (else (laml-error "laml-power-icon: third parameter must either be large or small"))))
            'alt "Program Oriented Web Engineering - using LAML") "_top")))


; Convert ch (character or sigular string) to a character that is legal in file names.
; The problems with '<' and '>' in Windows XP file names is the motivation behind this function.
; Returns a string.
(define (hygienic-file-character ch)
  (let ((ch-n (as-number (as-char ch))))
    (cond ((or (= ch-n 60) (= ch-n 62)) ;  '<' or '>' 
             (string-append "c" (as-string ch-n)))
          (else ch))))


; Redefined and specialized version of alphabetic-link-array-1 from html.scm
; Handle file hygienic letters appropriately.
(define (alphabetic-link-array-1 target-file-prefix alphabet . emphasis-letter)
  ;; Return an 'array' of letter links to (string-append target-file-prefix "-" letter ".html") for all letters in alphabet. 
  ;; This is a generalized version of alphabetic-link-array.
  ;; target-file-prefix is a prefix of the file names, in which the index files are located.
  ;; alphabet is a list of letters, for which to generate index links from the alphabet arrays. Some letters
  ;; may be missing from the alphabet compared with a complete alphabet.
  ;; emphasis-letter is an optional letter which we emphasize in the link array
  (let* ((em-let (if (not (null? emphasis-letter)) (as-string (car emphasis-letter)) #f))
         (alphabet-1 (map as-string alphabet)))
    (map 
     (lambda (letter) 
       (a-tag (string-append target-file-prefix "-" (hygienic-file-character letter) ".html")
	      (if (and em-let (equal? em-let letter))
		  (font-1 4 red (b (capitalize-string-nd letter)))
		  (capitalize-string-nd letter))
	      (horizontal-space 1))
       )
     alphabet-1)))


; ---------------------------------------------------------------------------------------------------------------
; Functions that help alleviate the conversion from lib/html.scm and lib/html-v1.scm stuff
; to XML-in-LAML and XHTML.

; Given the background color, text color, link color and visited link color (each a tripple r g b list)
; return a property list of these, ready for the body element.
(define (bg-text-link-vlink-colors bg-color text-color link-color vlink-color)
  (list 'bgcolor (rgb-color-encoding bg-color) 'text  (rgb-color-encoding text-color)
        'link (rgb-color-encoding link-color)    'vlink (rgb-color-encoding vlink-color)))

; ---------------------------------------------------------------------------------------------------------------------------------------
; Elucidator 2 handling of documentation, on LAML form.

; The program version documented in a given documentation entry or documentation section. 
; A number means a specific program version, typically lower than the newest version.
; The value #f means the newest version (default).
(define documented-program-version #f)

; Pass output stream and absolute output file path (the latter for for link checking purposes)
(define (laml-documentation-contents! op of)
  (render-to-output-port (a 'name "START") op)
  (render-to-output-port (h1 (font-color blue documentation-title)) op)
  (render-to-output-port (h3 (copyright-owner documentation-author) (horizontal-space 2) documentation-email (br)
                             documentation-affiliation ) op)
  (render-to-output-port (p) op)
  (render-to-output-port (present-documentation-abstract-laml-approach documentation-abstract) op)
  (render-to-output-port (div (vertical-space 1)) op)

  (for-each (lambda (doc-el) (present-documentation-element-laml-approach! doc-el op of)) documentation-elements)

  (render-to-output-port (div (vertical-space end-file-empty-lines)) op)
)

(define (present-documentation-abstract-laml-approach documentation-abstract-ast)
  (div 'class "elucidator-abstract" 
    (b "Abstract.") (ast-subtrees documentation-abstract-ast)))

(define (present-documentation-element-laml-approach! doc-el op of)
  (let ((kind (get-value 'kind doc-el))
        (program-version (get-value 'program-version doc-el))
       )
    (set! documented-program-version program-version)  ; if non-#f:  document an explicit program version
    (cond ((eq? kind 'section) (present-documentation-section-laml-approach! doc-el op of))
          ((eq? kind 'entry) (present-documentation-entry-laml-approach! doc-el op of))
          (else (laml-error "present-documentation-element-laml-approach!: unknown kind of documentation element:" kind)))
    (set! documented-program-version #f)  ; meaning: document newest program version
  )
)

(define (present-documentation-section-laml-approach! doc-el op of)
 (let* ((title (get-value 'doc-title doc-el))
        (section-numbering (get-value 'numbering doc-el))
        (section-number (car (get-value 'raw-numbering doc-el))) ; an integer
        (title-1 (span section-numbering (horizontal-space 2) title))
        (section-body-ast (get-value 'body-ast doc-el))
        (id (get-value 'id doc-el))
        (program-version (get-value 'program-version doc-el))
        (hidden-id-pres (font-1 2 documentation-entry-color (as-string id)))
        (subsection-elements (filter (subsections? section-number) documentation-elements))

        (surrounding-div (div 'class "elucidator-section"))
       )
   (write-string-to-port (start-tag-of surrounding-div) op)  
 
    ; Sectional front matters:
    (render-to-output-port (a 'name (internal-reference id)) op)
    (render-to-output-port
     (div
      (if program-version
          (left-right-banner 
           (elucidator-section-navigation-banner doc-el) 
           (span 'class "section-entry-version-mark" (string-append "Version" " " (as-string program-version)))
          )
          (elucidator-section-navigation-banner doc-el))) op)
    
    (render-to-output-port (div (b (font-size 5 title-1) )) op)

    ; The substantial sectional documentation is made here:
    (present-and-process-section-or-entry-body-ast! section-body-ast id op of)

    ; Subsection link table: 
    (render-to-output-port  (indent-pixels 10 (brl (map present-documentation-subsection-element subsection-elements))) op)

   (write-string-to-port (end-tag-of surrounding-div) op))
   (render-to-output-port (div (vertical-space 1)) op)  ; to ensure room between neighbor sections
)

(define (present-documentation-entry-laml-approach! doc-el op of)
 (let* ((title (get-value 'doc-title doc-el)) 
        (entry-numbering (get-value 'numbering doc-el))
        (title-1 (span entry-numbering (horizontal-space 2) title))
        (entry-body-ast (get-value 'body-ast doc-el))
        (id (as-string (get-value 'id doc-el)))
        (program-version (get-value 'program-version doc-el))
        (hidden-id-pres (font-1 2 documentation-entry-color (as-string id)))

        (surrounding-div (div 'class "elucidator-entry"))
       )
    (write-string-to-port (start-tag-of surrounding-div) op)

      ; Entry front matters:
      (render-to-output-port (a 'name (internal-reference id)) op)
      (render-to-output-port 
        (div  'class "elucidator-entry-head"
           (if program-version
             (left-right-banner 
               (elucidator-section-navigation-banner doc-el)
               (span 'class "section-entry-version-mark" (string-append "Version" " " (as-string program-version)))
             )
             (elucidator-section-navigation-banner doc-el)) 
           (if present-hidden-ids? hidden-id-pres "") (br)
	   (b (font-size 4 title-1))) op)

      ; The substantial entry documentation is made here: 
      (present-and-process-section-or-entry-body-ast! entry-body-ast id op of)

    (write-string-to-port (end-tag-of surrounding-div) op) ))


; Render a body of a documentation-section or documentation-entry, body-ast, on the open output port op.
; The doc-id of the section or entry is id.
; Similar to the textual counter part do-program-link-documentation
; Most important, doc-to-doc, doc-to-prog, and source markers must be handled in a similar way
; as in do-program-link-documentation, and its underlying procedures.
; Tricky: to catch previous-strong-program-word
; Idea: First transform body-ast from mixed ast to pure XHTML AST. In this process
; define documented-name-occurences, and keep and eye on previous-strong-program-word.
; Second, just render the XHTML AST to op.
(define (present-and-process-section-or-entry-body-ast! body-ast doc-id op of)
 (let* ((body-style (as-string (ast-attribute body-ast 'body-style "normal")))
        (elucidator-body-ast
          (transform-ast-shallow 
            body-ast

            (list (elucidator-ast? "strong-prog-ref")
                  (lambda (spr-ast) 
                   (let* ((file-qualification (ast-attribute spr-ast 'file ""))
                          (file-version (if documented-program-version documented-program-version (ast-attribute spr-ast 'vers #f)))
                          (link-word (ast-attribute spr-ast 'name ""))
                          (anchor-name (ast-text spr-ast))
                          (file-part (ast-attribute spr-ast 'file-part ""))
                         )
                     (destructured-linking-from-doc-to-prog 'strong file-qualification file-version
                                                            link-word doc-id file-part anchor-name))))

            (list (elucidator-ast? "weak-prog-ref")
                  (lambda (wpr-ast) 
                   (let* ((file-qualification (ast-attribute wpr-ast 'file ""))
                          (file-version (if documented-program-version documented-program-version (ast-attribute wpr-ast 'vers #f)))
                          (link-word (ast-attribute wpr-ast 'name ""))
                          (anchor-name (ast-text wpr-ast))
                          (file-part (ast-attribute wpr-ast 'file-part ""))
                         )                         
                     (destructured-linking-from-doc-to-prog 'weak file-qualification file-version
                                                            link-word doc-id file-part anchor-name))))

            (list (elucidator-ast? "doc-ref")
                  (lambda (dr-ast) 
                    (let ((link-word (ast-attribute dr-ast 'name)))
                     (linking-from-doc-to-doc link-word doc-id))))

            (list (elucidator-ast? "typographic-prog-ref")
                  (lambda (spr-ast) 
                    (let ((link-word (ast-attribute spr-ast 'name "")))
                     (span 'class "none-reference" link-word))))

            (list (elucidator-ast? "source-marker")
                  (lambda (sm-ast) 
                    (let ((marker-char (ast-attribute sm-ast 'name))
                          (expl (string-append "A link to a program source marker in " (as-string previous-strong-program-word)))
                         )
                     (source-mark-register previous-strong-program-word doc-id marker-char)
                     (span (source-mark-anchor (source-marker-glyph marker-char expl) marker-char) _
			   (a-name (string-append (as-string doc-id) "-" "@" (as-string marker-char))))
                     )))

          ))
       (xhtml-body-ast 
         (div 'class (string-append "body" "-" body-style)
              (ast-subtrees elucidator-body-ast)))
      )
    ; Specialized activation of the link checking - otherwise done in write-html
    (if (not (eq? xml-link-checking 'none))
        (collect-links-for-later-checking-in-ast! xhtml-body-ast of))

   (render-to-output-port xhtml-body-ast op) ))

; Transform ast by means of transform-specs.
; The parameter transform-specs is of the same kind as in the function transform-ast-list.
; During the transformation, AST nodes which are not matched by the transform-specs are copied.
; When some sub AST has been transformed, the transformed sub AST is not recursively transformed.
; .parameter ast The data to be transformed. Either an AST, textual contents, or a white space marker.
; .parameter transform-spec A list of transformation specifications, each of which is a list of length 2: (input-predicate transformation-function)
; .internal-references "similar function" "transform-ast-list"
; .internal-references "useful predicate generator" "ast-of-type?"
; .returns The transformed AST.
(define (transform-ast-shallow ast . transform-specs)
 (let ((transform-function (lookup-transform-spec ast transform-specs)))
    (if transform-function
        (transform-function ast)
        (cond ((ast? ast)
                (make-ast
                  (ast-element-name ast)
                  (map (lambda (x) (apply transform-ast-shallow x transform-specs)) (ast-subtrees ast))
                  (ast-attributes ast)
                  (ast-kind ast)
                  (ast-language ast)))
              ((cdata? ast) (string-copy ast))
              (else ast)))))


; Return an AST predicate that check if a given AST is an elucidator2 AST with element name el-name.
(define (elucidator-ast? el-name)
  (lambda (x)
    (and (ast? x)
         (eq? (ast-language x) 'elucidator2)
         (equal? (ast-element-name x) el-name))))

; Is is a unique documentation-entry or documentation-section id.
; If not, stop with a fatal error.
; We use the accumulated information in global variable documentation-key-title-alist to carry out the check.
(define (check-that-id-is-unique! id)
  (if (find-in-list            ; is there already an entry or section with id?
        (lambda (entry)
           (eq? (as-symbol (car entry)) (as-symbol id)))
        documentation-key-title-alist)
      (laml-error "Duplicate section or entry id encountered:" id)))

; --------------------------------------------------------------------------------------------------------------

; Should the source, as described by program-source-entry, be processed.
(define (must-process-source? program-source-entry)
  (car (get 'process program-source-entry)))


; ---------------------------------------------------------------------------------------------------------------
; Version stuff.

; The number assigned to the original version
(define starting-version 1)

; Return a predicate on program source list entries (an alist) which return if a given entry is of version n.
(define (program-version? n)
  (lambda (program-source-entry)
    (= n (car (get 'version program-source-entry)))))

; A program source list entry predicate.
(define (newest-version-source-program-entry? source-entry)
 (= (get-value 'version source-entry) (highest-version-number (get-value 'key source-entry))))

; A program source list entry predicate.
(define (older-version-source-program-entry? source-entry)
  (< (get-value 'version source-entry) (highest-version-number (get-value 'key source-entry))))

; Does n represent the newest version with respect to sources that nicknamed by source-key.
(define (newest-version-source-number? source-key n)
  (= n (highest-version-number source-key)))

; Does n represent an older version with respect to sources that nicknamed by source-key.
(define (older-version-source-number? source-key n)
  (< n (highest-version-number source-key)))

; A predicate that returns if a defining name occurrence entry is of the new version.
(define (new-version-name? dno-entry)
 (= (version-of dno-entry) (highest-version-number (source-key-of dno-entry))))

; A predicate that returns if a defining name occurrence entry is of an older version.
(define (old-version-name? dno-entry)
 (< (version-of dno-entry) (highest-version-number (source-key-of dno-entry))))


; ---------------------------------------------------------------------------------------------------------------

; Convert boolean values in the alist to either t or nil, such that Emacs can read them.
; Emacs errs on #t and #f.
(define (emacs-protect-alist alist)
 (letrec ((emacs-protect-bool 
            (lambda (val)
              (cond ((and(boolean? val) val) 't)
                    ((and(boolean? val) (not val)) 'nil)
                    (else val)))))
  (map 
   (lambda (pair)
     (let* ((key (car pair))
            (val (cdr pair)))
      (cons key
            (if (list? val) (map emacs-protect-bool val) (emacs-protect-bool val)))))
   alist)))
   
(define (emacs-protect-documented-name-entry dne)
  (let ((name (name-of-documented-name-entry dne))
        (doc-id (doc-id-of-documented-name-entry dne))
        (doc-kind (doc-kind-of-documented-name-entry dne))
        (vers (version-of-documented-name-entry dne)))
    (list name doc-id doc-kind
          (if (boolean? vers) 
              (if vers 't 'nil)
              vers)  ; number             
    )))

; -----------------------------------------------------------------------------------------------------------------------------------

; Return the source-destination-delta attribute of the manual given by manual-file-path.
; If manual-file-path is relative (and it is!) it is relative to ep-source-directory (which is the absolute path of the EP source directory).
(define (get-manual-source-destination-delta manual-file-path ep-source-directory)
  (let* ((abs-manual-path
           (if (absolute-file-path? manual-file-path)
               manual-file-path
               (string-append ep-source-directory manual-file-path)))
         (abs-manual-path-1 (string-append (file-name-initial-path abs-manual-path) (file-name-proper abs-manual-path) "." "manlsp")) 
         (manlsp-structure (file-read abs-manual-path-1))
         (meta-alist (car manlsp-structure))
         (sdd (defaulted-get 'source-destination-delta meta-alist 'not-provided))
        )
    (if (eq? sdd 'not-provided)
        ""
        sdd)))
         
(end-laml-loading)    
