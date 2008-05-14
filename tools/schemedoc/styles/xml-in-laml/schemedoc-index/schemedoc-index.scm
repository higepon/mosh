; The manual index. A frame-based index on top of a number of SchmeDoc manuals.

; The LAML library and programs are written by Kurt Normark, Aalborg University, Denmark.
; Copyright (C) 2004 Kurt Normark, normark@cs.auc.dk.
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
; CONSTANTS

(define manual-index-software-directory (string-append laml-dir "styles/xml-in-laml/schemedoc-index/"))

(define rnrs-version #f) ; either of the symbols r4rs or r5rs. Assigned later.

(define rnrs-url-prefix #f) ; assigned later

; Return the absolute path to the directory with scheme knowledge.
; rnrs is either the symbol r4rs or r5rs.
(define (rnrs-filepath-prefix rnrs)
        (if (directory-exists? (string-append laml-dir (as-string rnrs)))
            (string-append laml-dir (as-string rnrs) "/")
            #f))

(define source-destination-delta #f) ; assigned later

(define schemedoc-url "http://www.cs.auc.dk/~normark/schemedoc/")

; --------------------------------------------------------------------------------------------------
; SOFTWARE LOADING

(lib-load "xml-in-laml/xml-in-laml.scm")

; Top level action procedure definition.
(define (manual-index! ast)                    
   (do-manual-index! ast))

(load (string-append manual-index-software-directory "mirror/schemedoc-index.scm"))

(lib-load "xml-in-laml/mirrors/xhtml10-frameset-mirror.scm")
(lib-load "xml-in-laml/mirrors/xhtml10-transitional-mirror.scm")
(lib-load "xhtml1.0-convenience.scm")

(lib-load "time.scm")
(lib-load "color.scm")
(lib-load "file-read.scm")

; ---------------------------------------------------------------------------------------------------
; INITIAL SETUP

; To avoid excessive language overlap detection between XHTML transitional and frameset.
(set! xml-check-language-overlap? #f)

(define xhtml-frameset:frameset (xhtml10-frameset 'frameset))
(define xhtml-frameset:frame    (xhtml10-frameset 'frame))
(define xhtml-frameset:html     (xhtml10-frameset 'html))
(define xhtml-frameset:title    (xhtml10-frameset 'title))
(define xhtml-frameset:head    (xhtml10-frameset 'head))

(define menu-style-clause 
  (style 'type "text/css" "a:link, a:visited  {color: black}, a:hover {color: black; background-color: yellow; }"))

; ---------------------------------------------------------------------------------------------------

(define (do-manual-index! manual-index-ast)
  (let* ((front-matters-ast (find-first-ast manual-index-ast "manual-index-front-matters"))
         (source-destination-delta-attr (unique-ast-attribute front-matters-ast 'source-destination-delta "html/"))
         (browser-type-attr (as-symbol (unique-ast-attribute front-matters-ast 'browser-type "one-step")))
         (left-frame-width-attr (unique-ast-attribute front-matters-ast 'left-frame-width "30%")) 
         (top-frame-height-attr (unique-ast-attribute front-matters-ast 'top-frame-height "20%"))
         (initial-manual-frame-attr (as-symbol (unique-ast-attribute front-matters-ast 'initial-manual-frame "info")))
         (scheme-report-version-attr (as-symbol (unique-ast-attribute front-matters-ast 'scheme-report-version "r5rs")))
         (scheme-report-attr (as-symbol (unique-ast-attribute front-matters-ast 'scheme-report "none")))
        )

    (set! source-destination-delta source-destination-delta-attr)
    (set! rnrs-version (as-symbol scheme-report-version-attr))
    (set! rnrs-url-prefix 
          (string-append
             (laml-home-url-prefix 0 (normalize-file-path (string-append (startup-directory) source-destination-delta)))
             (as-string scheme-report-version-attr) "/"))

    (let* 
        ((index-title (find-first-ast front-matters-ast "manual-index-title" ast-subtrees))
         (index-contribution-ast-list (find-asts manual-index-ast "manual-index-contribution"))
         (abs-manlsp-path-list (map make-manual-path index-contribution-ast-list))
         (manual-title-list (map read-manual-title-from-manlsp-structure abs-manlsp-path-list))
         (name-list-0 (map file-name-proper abs-manlsp-path-list))
         (rnrs-string (upcase-string (as-string rnrs-version)))
         (name-list (if (or (eq? scheme-report-attr 'include) (eq? scheme-report-attr 'merge)) (cons rnrs-string name-list-0) name-list-0))
         (url-list-0 (map make-manual-url index-contribution-ast-list))
         (url-list (if (or (eq? scheme-report-attr 'include) (eq? scheme-report-attr 'merge))
                       (cons (rnrs-manual-top-url rnrs-version) url-list-0)
                       url-list-0))
         (info-name-list-0 (map (lambda (contr-ast manual-title) (ast-attribute contr-ast 'informative-name manual-title)) index-contribution-ast-list manual-title-list))
         (info-name-list (if (or (eq? scheme-report-attr 'include) (eq? scheme-report-attr 'merge))
                             (cons (string-append rnrs-string " " "Scheme Manual") info-name-list-0)
                             info-name-list-0))
	 )
					; (check-manlsp-file-existences! abs-manlsp-path-list)

      (if (not (directory-exists? (string-append (startup-directory) source-destination-delta-attr)))
	  (laml-error "YOU are supposed to create the directory/directories along the path:" source-destination-delta-attr))

      (let* ((rnrs-manual-list (if (or (eq? scheme-report-attr 'include) (eq? scheme-report-attr 'merge)) (make-rnrs-manual-list (as-symbol scheme-report-version-attr)) '()))
	     (manual-entry-list-list-0 (map read-and-augment-manlsp-file abs-manlsp-path-list url-list-0))
	     (manual-entry-list-list 
	      (if (or (eq? scheme-report-attr 'include) (eq? scheme-report-attr 'merge)) 
		  (cons rnrs-manual-list manual-entry-list-list-0)
		  manual-entry-list-list-0))


             (is-there-sections-list-0 (map is-there-sections? manual-entry-list-list-0))
	     (is-there-sections-list (map is-there-sections? manual-entry-list-list))
           
	     (manual-section-and-page-list-0 (flatten manual-entry-list-list-0))
	     (manual-section-and-page-list (flatten manual-entry-list-list))
	     (manual-page-list-0 (filter (lambda (entry) (equal? "manual-page" (get-val 'kind entry))) manual-section-and-page-list-0))
	     (manual-page-list (filter (lambda (entry) (equal? "manual-page" (get-val 'kind entry))) manual-section-and-page-list))
	     (sorted-manual-page-list-0 
	      (sort-list manual-page-list-0 manual-entry-leq?))
	     (sorted-manual-page-list 
	      (sort-list manual-page-list manual-entry-leq?))

	     (index-suffix 
	      (div
               (p)
               (font-1 2 red (when-generated)) (br)
               (font-1 2 red (span "A" (a 'href schemedoc-url 'target "frame-3" (font-color red "LAML SchemeDoc Index")) "."))))
	     )
  
					; Make frameset HTML file of the same name as the LAML source file:
	(write-html '(raw)
		    (xhtml-frameset:html
		     (xhtml-frameset:head (xhtml-frameset:title "")
					  (if (is-a-laml-directory? (in-startup-directory source-destination-delta))
					      (link 'rel "SHORTCUT ICON" 'href (string-append (laml-home-url-prefix 0 (in-startup-directory source-destination-delta)) "images/16-16-icon.ico"))
					      '())
					  )
		     (xhtml-frameset:frameset 
		      (xhtml-frameset:frameset
		       (xhtml-frameset:frame 'name "frame-1" 'src "manual-file-index.html" 'scrolling "yes")
		       (xhtml-frameset:frame 'name "frame-2" 'src "all-name-index.html" 'scrolling "yes")
		       'rows (string-append top-frame-height-attr "," "*"))
		      (xhtml-frameset:frame 'name "frame-3" 
					    'src (cond ((eq? initial-manual-frame-attr 'blank) "blank-initial-content.html")
						       ((eq? initial-manual-frame-attr 'info) "manual-index-initial-content.html")
						       ((eq? initial-manual-frame-attr 'first-manual-contribution) (car url-list))
						       (else (laml-error "Unknown value of initial-manual-frame")))
					    'scrolling "yes")
		      'cols (string-append left-frame-width-attr "," "*")))
		    (string-append (startup-directory) source-destination-delta (html-file (source-filename-without-extension))))

					; Make the manual file index:
	(write-html '(raw)
		    (html
		     (head menu-style-clause (title "Manual file index"))
		     (body 
		      (font 'size "5" 'color (rgb-color-encoding red) (b index-title)) _ ":" (br)

		      (if (eq? browser-type-attr 'two-steps)
			  (span
			   (a (b (if (eq? scheme-report-attr 'merge) (em "All defined names + Scheme names") (em "All defined names")))
			      'href "all-name-index.html"
			      'target "frame-2"
			      'css:text-decoration "none") (br))
			  "")
                
		      (table (map (if (eq? browser-type-attr 'one-step)
				      format-manual-index-entry-one-step
				      format-manual-index-entry-two-steps)
				  name-list info-name-list url-list is-there-sections-list))
            
		      index-suffix

		      ))
		    (in-startup-directory (string-append source-destination-delta "manual-file-index.html")))

					; Make the all name index:
	(write-html '(raw)
		    (html
		     (head menu-style-clause (title "Name index"))
		     (body 
            
		      (brl (map format-manual-entry (if (eq? scheme-report-attr 'include) sorted-manual-page-list-0 sorted-manual-page-list)))

		      index-suffix

		      ))
		    (in-startup-directory (string-append source-destination-delta "all-name-index.html")))

					; If two-steps browser, make the name indexes per manual:
	(for-each make-name-index-of-manual name-list info-name-list url-list manual-entry-list-list)

	(write-html '(raw)
		    (html
		     (head (title "Initial index page"))
		     (body 

		      (vertical-space 1)
		      (center (font-1 6 grey "LAML SchemeDoc"))

		      (vertical-space 1)
		      (center (narrow-with-pixels 100 
						  (div
                         
						   (p (font-1 4 grey "This frame is used for presentation of a selected SchemeDoc manual."))

						   (if (eq? browser-type-attr 'one-step)
						       (p (font-1 4 grey "You can select a SchemeDoc manual in the topmost frame to the left 
                                            or a name in the bottom frame to the left. In both cases the relevant
                                            SchemeDoc manual is shown in this frame."))
						       (p (font-1 4 grey "When you select a SchemeDoc manual in the topmost frame to the left, 
                                            details about this manual - including all names defined in the manual - appear in bottom frame to the left.
                                            Selecting some detail in this frame causes the relevant SchemeDoc manual to be shown in this frame.")))


						   (p (font-1 4 grey (span "For help on SchemeDoc consult the" 
									   (a 'href "http://www.cs.auc.dk/~normark/schemedoc" (font-color grey "SchemeDoc Home Page")) _ ".") )))
						  ))
		      ))
		    (in-startup-directory (string-append source-destination-delta "manual-index-initial-content.html")))

	(write-html '(raw)
		    (html
		     (head (title "Blank index page"))
		     (body ))
		    (in-startup-directory (string-append source-destination-delta "blank-initial-content.html")))



					; Write a manlsp file for the browser:
					;       (file-write
					;          manual-section-and-page-list
					;          (string-append (in-startup-directory (string-append (source-filename-without-extension) "." "manlsp"))))

	)

					; Image handling:
      (ensure-directory-existence! (string-append (startup-directory) source-destination-delta) "images")
      (copy-files 
       (list "table.gif" "toc.gif")
       (string-append manual-index-software-directory "images/")
       (string-append (startup-directory) source-destination-delta "images/"))

      (end-laml)
       
      )
  )
)

; ---------------------------------------------------------------------------------------------------        
; Formatting functions:

; Format a name entry:
(define (format-manual-entry manual-entry)
   (let ((name (get-val 'title manual-entry))
         (lib (get-val 'library manual-entry))
         (url (get-val 'url manual-entry))
        )
    (a name
       'href (if (or (equal? lib "R4RS") (equal? lib "R5RS")) url (string-append url "#" name))
       'target "frame-3"
       'css:text-decoration "none"
       'title (as-string lib)
       )))

; Format a manual file entry:
(define (format-manual-index-entry-one-step name info-name url sections?)
  (tr
    (td (a 'href url 'target "frame-3" 'css:text-decoration "none" info-name))
    (td (if sections? 
          (a 'href (string-append url "#" "MANUAL-TOC") 'target "frame-3"  'css:text-decoration "none" 
           (img 'border "0" 'src "images/toc.gif" 'alt "Table of Contents"))
        ""))
    (td (if (not (or (equal? name "R4RS") (equal? name "R5RS")))
        (a 'href (string-append url "#" "MANUAL-INDEX") 'target "frame-3"  'css:text-decoration "none"
           (img 'border "0" 'src "images/table.gif" 'alt "Defined names"))
        ""))))

(define (format-manual-index-entry-two-steps name info-name url sections?)
  (tr
    (td (a 'href (string-append (name-index-name name) ".html")  'target "frame-2" 'css:text-decoration "none" info-name))
))

; ---------------------------------------------------------------------------------------------------


(define (make-name-index-of-manual manual-name manual-info-name manual-url manual-entry-list)
 (let* ((manual-page-list (filter (lambda (entry) (equal? "manual-page" (get-val 'kind entry))) manual-entry-list))
        (sorted-manual-page-list (sort-list manual-page-list manual-entry-leq?)))
   (write-html '(raw)
        (html
          (head menu-style-clause (title "Name index" _ ":" (as-string manual-name) ))
          (body 
           (font 'size "1" manual-info-name _ ":") (br)

           (a (b (em "Top"))
              'href (if (or (equal? manual-name "R4RS") (equal? manual-name "R5RS")) (rnrs-manual-top-url rnrs-version) (string-append manual-url "#" "MANUAL-TOP"))   
              'target "frame-3"
              'css:text-decoration "none") (br)

           (a (b (em "Abstract"))
              'href (if (or (equal? manual-name "R4RS") (equal? manual-name "R5RS"))
                        (rnrs-abstract-url rnrs-version)  ; (string-append (file-name-initial-path manual-url) "r4rs_1.htm#SEC1")  ; !!!
                        (string-append manual-url "#" "MANUAL-ABSTRACT"))
              'target "frame-3"
              'css:text-decoration "none") (br)

           (a (b (em "Table of Content"))
              'href (if (or (equal? manual-name "R4RS") (equal? manual-name "R5RS")) 
                        (rnrs-table-of-content-url rnrs-version)
                        (string-append manual-url "#" "MANUAL-TOC"))
              'target "frame-3"
              'css:text-decoration "none") (br)

           (a (b (em "Definition Index"))
              'href (if (or (equal? manual-name "R4RS") (equal? manual-name "R5RS")) 
                        (rnrs-index-url rnrs-version) ; (string-append (file-name-initial-path manual-url) "r4rs_14.htm#SEC87")  ; !!!
                        (string-append manual-url "#" "MANUAL-INDEX"))
              'target "frame-3"
              'css:text-decoration "none") (br)

            (brl (map format-manual-entry sorted-manual-page-list))))
        (in-startup-directory (string-append source-destination-delta (html-file (name-index-name manual-name)))))))
       

; Does there exist one or more manual-section entries in manual-entry-list
(define (is-there-sections? manual-entry-list)
  (turn-into-boolean
   (find-in-list
     (lambda (entry)
        (equal? "manual-section" (get-val 'kind entry)))
     manual-entry-list)))

; Return an absolute path to the internal manlsp file. Full path, including extension.
; In the AST the information can either be given via path or manual-file-path
(define (make-manual-path manual-index-contribution-ast)
  (let* ((path-attr (ast-attribute manual-index-contribution-ast 'path #f))
         (manual-file-path-attr (ast-attribute manual-index-contribution-ast 'manual-file-path #f))
         (given-path (if path-attr path-attr manual-file-path-attr))
       )
    (if (not given-path)
        (laml-error "make-manual-path: You should either give path or manual-file-path of each manual-index-contribution."))
    (if (absolute-file-path? given-path)
        (string-append 
         (file-name-initial-path given-path)
         (file-name-proper given-path)
         "."
         "manlsp")
        (string-append 
         (startup-directory)
         (file-name-initial-path given-path)
         (file-name-proper given-path)
         "."
         "manlsp"))))

; Return an URL (absolute or relative) to a manual HTML page.
; Relative URLs go from the address of the directory in which the manual index LAML file is located.
(define (make-manual-url manual-index-contribution-ast)
  (let* ((path-attr (ast-attribute manual-index-contribution-ast 'path ""))  
         (referred-manual-source-destination-delta (get-manual-source-destination-delta path-attr (startup-directory)))          
       )

    (if (absolute-file-path? path-attr)
        (laml-error "The file path of manual-index-contribution elements must be relative"))

    (if #f    ;  (or (absolute-file-path? given-url) (absolute-url-path? given-url))  ; Not supported anymore
        (string-append 
         (file-name-initial-path given-url)
         (file-name-proper given-url)
         "."
         "html")
        (string-append 
         (inverse-return-path source-destination-delta (startup-directory))
         (file-name-initial-path path-attr)
         referred-manual-source-destination-delta
         (file-name-proper path-attr)
         "."
         "html"))))

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


(define (read-and-augment-manlsp-file manlsp-abs-path url)
  (let ((name (file-name-proper manlsp-abs-path))
        (path-only (file-name-initial-path manlsp-abs-path)))
   (let ((internal-list-repr (cdr (file-read manlsp-abs-path))))  ; cdr: Skip manual meta entry
    (map 
      (lambda (entry) (cons (list 'path path-only) (cons (list 'url url) (cons (list 'library name) entry))))
      internal-list-repr))))

; A little wasteful to re-read the manlsp file.
(define (read-manual-title-from-manlsp-structure manlsp-abs-path)
 (let ((manlsp-structure (file-read manlsp-abs-path)))
  (if (and (list? manlsp-structure) (>= (length manlsp-structure) 1))
      (let ((manual-meta-info (first manlsp-structure)))
        (defaulted-get 'title manual-meta-info "???"))
      "???")))

(define (check-manlsp-file-existences! abs-path-list)
  (for-each
    (lambda (path)
      (if (not (file-exists? path))
          (laml-error "Cannot find the absolute, internal manlsp file:" path)))
    abs-path-list))


; Is the string path an absoute URL. Just a primitive implementation.
(define (absolute-url-path? path)
  (and (string? path)
       (>= (string-length path) 5)
       (equal? (substring path 0 5) "http:")))
  
; Extracts information from an association list in which the entries are proper lists of length 2.
(define (get-val key alist)
 (let ((list-val (get key alist)))
   (car list-val)))

(define (manual-entry-leq? e1 e2)
  (string<=? (get-val 'title e1) (get-val 'title e2)))

; The name of a manual specific name index. Without extension and path.
(define (name-index-name name)
  (string-append "name-index-" (as-string name)))

(define (html-file name)
  (string-append name "." "html"))

; Convert a Scheme knowledge list (ala the elements in the file r4rs/scheme-knowledge.lsp and r5rs/scheme-knowledge.lsp) to a manual association list.
; scheme-version is a symbol, such as r4rs or r5rs.
(define (make-rnrs-manual-list scheme-version)
  (if (rnrs-filepath-prefix scheme-version)
      (let ((rnrs-knowledge-list (filter complete-rnrs-entry? (read-scheme-knowledge (as-symbol scheme-version)))))
         (map rnrs-entry-to-a-list rnrs-knowledge-list))
      (begin
         (display-warning "You can only use the r4rs/r5rs manual facility in full laml distributions")
         '())))

(define (complete-rnrs-entry? entry)
  (= 5 (length entry)))


(define (rnrs-entry-to-a-list rnrs-entry)
  (list
   (list 'title (as-string (symbol-of-scheme-knowledge rnrs-entry)))
   (list 'library (upcase-string (as-string rnrs-version)))
   (list 'kind "manual-page") ; faking...
   (list 'path (rnrs-filepath-prefix rnrs-version))
   (list 'url (string-append rnrs-url-prefix (url-suffix-of-scheme-knowledge rnrs-entry rnrs-version)))))
 

(define (rnrs-manual-top-url rnrs-version)
 (string-append rnrs-url-prefix
  (cond ((eq? rnrs-version 'r4rs) "index.htm")
        ((eq? rnrs-version 'r5rs) "r5rs_toc.html")
        (else (laml-error "rnrs-manual-top-url: Unknown rnrs-version:" rnrs-version)))))

(define (rnrs-abstract-url rnrs-version)
 (string-append rnrs-url-prefix
  (cond ((eq? rnrs-version 'r4rs) "r4rs_1.htm#SEC1")
        ((eq? rnrs-version 'r5rs) "r5rs_1.html#SEC1")
        (else (laml-error "rnrs-abstract-url: Unknown rnrs-version:" rnrs-version)))))

(define (rnrs-table-of-content-url rnrs-version)
 (string-append rnrs-url-prefix
  (cond ((eq? rnrs-version 'r4rs) "index.htm#TOC")
        ((eq? rnrs-version 'r5rs) "r5rs_toc.html#TOC1")
        (else (laml-error "rnrs-table-of-content-url: Unknown rnrs-version:" rnrs-version)))))

(define (rnrs-index-url rnrs-version)
 (string-append rnrs-url-prefix
  (cond ((eq? rnrs-version 'r4rs) "r4rs_14.htm#SEC87")
        ((eq? rnrs-version 'r5rs) "r5rs_14.html#SEC86")
        (else (laml-error "rnrs-index-url: Unknown rnrs-version:" rnrs-version)))))


(end-laml-loading)