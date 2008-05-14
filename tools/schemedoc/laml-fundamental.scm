;=>man/laml.sdoc

; The configuration independent part of the laml.scm.
; Loaded by laml.scm, and assumed to form the rear part of laml.scm.


; Yet another configuration feature:

;; Returns a full, absolute path to a temp directory, to which LAML software is assumed to have write access. 
;; In the default implementation, return temp in laml-dir.
;; You may redefine this function to return another path.
(define (laml-temp-file-path)
  (string-append laml-dir "temp/"))





; TIMING.
; In MzScheme we meassure the time used by LAML to process a document.
; The end-laml procedure reports on the elapsed time.
; We Start the timing here. 

; start-laml-time is curretly only valid in mzscheme and guile. Units: System dependent.
(define start-laml-time 
   (cond ((or (eq? scheme-system 'mzscheme) (eq? scheme-system 'mzscheme-200))
             (current-process-milliseconds))
         ((eq? scheme-system 'guile) (get-internal-run-time)) 
         (else 0)))

; ---------------------------------------------------------------------------------------------------


; Other variables.
; This part of the laml.scm file contains some other variables. Most LAML users can ignore these.
; Taken out of the manual interface May 14, 2003.


; An alias of laml-dir.
; For backward compatibility.
; Some LAML applications change this variable to a more local directory.
(define software-directory laml-dir)

; An alias of laml-library. 
; For backward compatibility
(define scheme-library laml-library)

; Full path to the scheme library.
; Normally the value of this variable is derived from laml-dir and laml-library. 
; Ends in a slash.
(define the-library (string-append laml-dir laml-library "/"))

; A global variable which signals some kind of variations in the loading of a LAML style or library.
; If no load variation is present, the value of the variable will be #f. The interpretation of a non-false value
; is entirely up to a style or a library.
; The value of this variable is assigned by the style and laml-style form based on the last optional parameter of style or laml-style
(define laml-load-variation #f)


; An association list of languages and language maps.
; The variable is related to the xml-in-laml library.
(define xml-in-laml-languages-in-use '())

; An association list of languages and XML navigator structures.
; Used for fast navigation in ASTs, guided by static information from the
; underlying DTD.
(define xml-in-laml-navigator-structures '())

; An association list of languages and XML validation structures.
; Used for access to XML validation procedures given an element name.
(define xml-in-laml-validator-structures '())

; An association list of languages and XML content model structures.
; Used for access to the XML content models at document generation or transformation time.
(define xml-in-laml-content-model-structures '())

; As association list of languages and action procedure structures.
; A single action procedure structure of an XML language maps XML elements to action procedures of the language. 
(define xml-in-laml-action-procedure-structures '())

; ---------------------------------------------------------------------------------------------------
; Variables related to link checking.
; Belongs naturally in lib/xml-in-laml/xml-in-laml.scm, but located here
; in case xml-in-laml.scm becomes reloaded before end-laml is called.

; A list of relative url entries for later checing. Each entry is of the form (rel-url surrounding-absolute-file).
(define relative-url-list-for-later-checking '())

; A list of absolute url entries for later checing. Each entry is a string (the absolute url).
(define absolute-url-list-for-later-checking '())

; Internal global variable used for counting relative url linking problems
(define relative-url-problem-count 0)

; Internal global variable used for counting absolute url linking problems
(define absolute-url-problem-count 0)

; ---------------------------------------------------------------------------------------------------


;;; LAML version information.
;;; The functions in this section return information about the version of LAML that you have installed.
;;; The functions basically return the same information as the string laml-version.
;;; We provide these function to make the LAML version information available on a more convenient form than in
;;; in the string laml-version. The version information is taken from the file distribution-version.lsp in the 
;;; root of the LAML distribution. Always use the functions in this section to access the version information.
;;; (Do not read and interpret the information in distribution-version.lsp directly).
;;; .section-id laml-version-functions

;; Return a list of two integers: the LAML major version number and the LAML minor version number.
;; .returns A list of two integers: (major-version-number minor-version-number)
(define (laml-version-numbers)
 (let ((laml-version-info (file-read (string-append laml-dir "distribution-version.lsp"))))
   (list (car laml-version-info) (cadr laml-version-info))))

;; Return the time stamp of this LAML distribution.
;; A number that represents the number of second elapsed since Jan 1, 1970.
;; Use the functions in the LAML time library to make good use of this number.
;; .reference "Useful time function" "time-decode" "../lib/man/time.html#time-decode"
;; .returns An integer.
(define (laml-version-time)
 (let ((laml-version-info (file-read (string-append laml-dir "distribution-version.lsp"))))
   (car (cddr laml-version-info))))

;; Return the kind of your current LAML distribution.
;; Currently we use the following kinds: full, slim, development.
;; .returns Either "full", "slim", "development" (a string).
(define (laml-version-kind)
 (let ((laml-version-info (file-read (string-append laml-dir "distribution-version.lsp"))))
   (as-string (car (cdr (cdr (cdr laml-version-info)))))))




; ---------------------------------------------------------------------------------------------------

;;; Library, style, tool, and local dir loading.
;;; The functions in this section loads LAML libraries and LAML styles.
;;; .section-id loading-section

;; Load file from the LAML library directory.
;; .parameter suffix-path The part of the library file name relative to the LAML library directory, including file extension.
(define (lib-load suffix-path)
  (load (string-append the-library suffix-path)))

;; Load a file from the LAML tool directory.
;; .parameter suffix-path The part of the tool file name relative to the LAML tool directory, including file extension.
(define (laml-tool-load suffix-path)
  (load (string-append laml-dir "tools/" suffix-path)))

;; Load file from the startup directory.
;; .parameter suffix-path The part of the file name relative to the LAML startup directory, including file extension.
;; .internal-references "related function " "startup-directory" 
(define (local-load suffix-path)
  (load (string-append (startup-directory) suffix-path)))

; Load a LAML style.
; .form (style style-spec [style-base load-variation])
; .parameter style-spec The name of the style to load. A style-spec is without extension. However, the style file must have the scm extension.
; .parameter style-base: The directory which contains the style. If style-base is given it must be a directory (a slash terminated string) from which to load your style. If style-base is omitted, the style is loaded from styles subdirectory of the LAML directory.
; .parameter load-variation: A load-variation assigned to the global LAML variable laml-load-variation.
; .example  (style "simple" #f 'xyz-variation)
; .example  (style "manual" "manual/")
; .internal-references "also relevant" "laml-style"
(define (style style-spec . optional-parameters)
 (let ((original-load-variation laml-load-variation))
  (let ((style-base (optional-parameter 1 optional-parameters))
        (load-variation (optional-parameter 2 optional-parameters))
       )
   (set! laml-load-variation load-variation)
   (if style-base
       (load (string-append style-base style-spec ".scm"))
       (load (string-append software-directory "styles/" style-spec ".scm")))
   (set! laml-load-variation original-load-variation))))

;; Load a LAML style.
;; .form (laml-style style-spec [style-base load-variation])
;; .parameter style-spec The name of the style to load. A style-spec is without extension. However, the style file must have the scm extension.
;; .parameter style-base: The directory which contains the style. If style-base is given it must be a full path directory (a slash terminated string) from which to load your style. If style-base is omitted, the style is loaded from styles subdirectory of the LAML directory.
;; .parameter load-variation: A load-variation assigned to the global LAML variable laml-load-variation.
;; .example  (laml-style "simple" #f 'xyz-variation)
;; .example  (laml-style "manual" "manual/")
(define laml-style style)

; ---------------------------------------------------------------------------------------------------

;;; LAML contextual information.
;;; The functions in this section deal with the necessary context information,
;;; which must be passed to Scheme when we use LAML.
;;; .section-id context-section

;; If possible return the name of the LAML source file (without extension).
;; This is only possible if the information somehow is passed to the Scheme execuctable.
;; In cases where it is not possible to know the source file name, return #f.
;; Notice: The parameter is not used, and should be avoided.
;; In order to be backward compatible, however, we allow a dummy parameter. 
;; .internal-references "similar function" "full-source-path-with-extension" 
(define (source-filename-without-extension . unused-parameter)
  (let ((cmd-line (laml-canonical-command-line)))
    (if cmd-line (cadr cmd-line) #f)))

;; Return the directory in which LAML is started up.
;; If this information is not available return #f.
;; Notice: The parameter is not used, and should be avoided.
;; In order to be backward compatible, however, we allow a dummy parameter. 
(define (startup-directory . unused-parameter)
  (let ((cmd-line (laml-canonical-command-line)))
    (if cmd-line (caddr cmd-line) #f)))

;; Return the list of program parameters passed to an activation of LAML.
;; If no program parameters are passed, the empty list is returned.
(define (laml-program-parameters)
  (let ((cmd-line (laml-canonical-command-line)))
    (if (and cmd-line (>= (length cmd-line) 3))
        (cadddr cmd-line)
        '())))
  

;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght three, or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (without extension and initial path).
;; Element number three must be a slash terminated directory in which the source file resides.
;; This function must be redefined in the scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (error "laml-canonical-command-line is not defined in scheme-system dependent compatibility file"))

;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; As an optional parameter, a list of program parameters can be passed.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in the scheme-system dependent compatibility file.
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (error "fake-startup-parameters is not defined in scheme-system dependent compatibility file"))


;; Set the LAML startup directory to dir. 
;; Dir can be a full path, "..", or a directory relative to the current laml startup directory.
;; This is specialized call to fake-startup-parameters with only directory information.
(define (set-laml-startup-directory dir)
 (let ((start-dir (startup-directory)))
  (let ((abs-dir
          (cond ((and (equal? ".." dir) start-dir (parent-directory start-dir)) (parent-directory start-dir))
                ((and (not (absolute-file-path? dir)) start-dir) (string-append start-dir (ensure-final-character dir #\/)))
                ((absolute-file-path? dir) (ensure-final-character dir #\/))
                (else (display-error (string-append "Use an absolute file path!!!"))))))
    (if (directory-exists? abs-dir)
        (begin 
          (fake-startup-parameters 
             (source-filename-without-extension) abs-dir (laml-program-parameters))
          (display-message (string-append "Using LAML in directory: " abs-dir)))
        (laml-error "Non-existing directory: " abs-dir)))))

;; Return a (full) path relative to the current startup-directory.
;; The directory and file contributions in suffixes are concatenated.
;; .internal-references "relevant function" "startup-directory"
(define (in-startup-directory . suffixes)
 (let ((suffix (accumulate-right string-append "" suffixes)))
  (string-append (startup-directory) suffix)))

;; Return the full path to the current source file (as returned by (source-filename-without-extension))
;; in the LAML startup directory (as returned by (startup-directory)).
;; .form (laml-source-file-path [extension])
;; .parameter extension The extension of the returned file path. A string (without initial dot). 
;; .returns A full file path to the current source file
(define (laml-source-file-path . optional-parameter-list)
  (let ((ext (optional-parameter 1 optional-parameter-list #f)))
    (in-startup-directory 
      (source-filename-without-extension) 
      (if ext (string-append "." ext) ""))))
  

;; Set the LAML startup directory to dir. 
;; Dir can be a full path, "..", or a directory relative to the current laml startup directory.
;; A convenient and easy to remember alias to set-laml-startup-directory.
(define (laml-cd dir)
  (set-laml-startup-directory dir))

;; Returns the working LAML directory.
;; Similar to the UNIX command pwd.
;; An alias of the function startup-directory.
(define (laml-pwd)
  (startup-directory))

;; Returns a list of files and directories of the LAML startup directory (the current directory).
;; Similar to the UNIX command ls
(define (laml-ls)
  (directory-list (startup-directory)))
  

;; Set the LAML source file name (without extension) to file.
;; This is specialized call to fake-startup-parameters with only source file information.
(define (set-laml-source-file file)
  (fake-startup-parameters 
    file (startup-directory) (laml-program-parameters)))

;; Set the LAML program parameters.
;; This is specialized call to fake-startup-parameters with only program parameters
(define (set-laml-program-parameters program-parameters)
  (fake-startup-parameters 
    (source-filename-without-extension) (startup-directory) program-parameters))

;; Return the full path to the current source file name in the startup directory, using the extension ext.
;; This function can be used conveniently to name the typical file for LAML to HTML transformations.
;; .internal-references "similar function" "source-filename-without-extension"
(define (full-source-path-with-extension ext)
  (string-append 
    (startup-directory)
    (source-filename-without-extension)
    "." ext))

; ---------------------------------------------------------------------------------------------------

;;; Programmatic loading of laml files. 
;;; Loading a LAML file invovles the setting of two pieces of context: The name of
;;; of the source file and the startup directory. The function laml-load sets these
;;; information and loads a file.
;;; .section-id prog-loading-section

;; Load and execute the LAML file on the file file-name (a string).
;; This procedure is a flexible and versatile alternative to laml-load.
;; .parameter file-name A file-name, with or without extension. The extension 'laml' will be added if not supplied. Takes file-name from the startup-directory. Can also be a full path.
;; .parameter program-parameters A list of program parameters
;; .misc Please notice that this procedure will not work in case you use directory or file names with dots ('.').
;; .internal-references "useful function" "laml-program-parameters"
(define (laml file-name . program-parameters)
 (let* ((init-path (file-name-initial-path file-name))
        (extension (file-name-extension file-name))
        (proper-name (file-name-proper file-name))
        (init-path-1 (if (empty-string? init-path) (startup-directory) init-path))
        (extension-1 (if (empty-string? extension) "laml" extension))
        (proper-name-1 proper-name))
 
   (if (and (empty-string? init-path) (not (startup-directory)))
       (error "Please use full file path or set the laml startup directory via set-laml-startup-directory"))

   (laml-load (string-append init-path-1 proper-name-1 "." extension-1) program-parameters)))

;; Load the laml file in full-file-path after faking the start up parameters.
;; full-file-path must be the full path of a laml file, including the laml extension.
;; This function is used by the function laml, which is recommended for most users.
;; .internal-references "similar function" "laml"
(define (laml-load full-file-path . optional-parameter-list)
 (let ((original-filename-without-extension (source-filename-without-extension))
       (original-startup-dir (startup-directory))
       (original-program-parameters (laml-program-parameters))
      )
  (let ((filename-wihtout-extension (file-name-proper full-file-path))
        (startup-dir (file-name-initial-path full-file-path))
        (program-parameter-list (optional-parameter 1 optional-parameter-list '()))
       )
    (fake-startup-parameters filename-wihtout-extension startup-dir program-parameter-list)
    (load full-file-path)
    ; restore originals:
    (if (and original-filename-without-extension original-startup-dir)
        (fake-startup-parameters original-filename-without-extension original-startup-dir original-program-parameters))
  )))


; ---------------------------------------------------------------------------------------------------
; It turns out that the loading stuff in the next section relies on case sensitive reading.
; Therefore the compatibility loading is placed here.

; Loads the scheme-system specific compatibility file and the LAML general library.
; Try most specifics first: 
;     scheme-system  and  platform  and  operating-system
;     scheme-system  and  platform  and  *
;     scheme-system  and  *         and  *


(let ((laml-lib-comp-file (lambda (nm) (string-append laml-dir "lib/compatibility/" nm)))
      (comp-file (lambda (nm) (string-append "compatibility/" nm)))
      (schemesys-platform-os (string-append (symbol->string laml-platform) "_" (symbol->string operating-system) "_" (symbol->string scheme-system) ".scm"))
      (schemesys-platform-star (string-append (symbol->string laml-platform) "_" "star" "_" (symbol->string scheme-system) ".scm"))
      (schemesys-star-star (string-append "star" "_" "star" "_" (symbol->string scheme-system) ".scm"))
     )
  (cond ((file-exists? (laml-lib-comp-file schemesys-platform-os))
             (lib-load (comp-file schemesys-platform-os)))
        ((file-exists? (laml-lib-comp-file schemesys-platform-star))
             (lib-load (comp-file schemesys-platform-star)))
        ((file-exists? (laml-lib-comp-file schemesys-star-star))
             (lib-load (comp-file schemesys-star-star)))
        (else (error (string-append "Compatibility loading: Cannot find compatibility file in lib/compatibility.")))))
        

; ---------------------------------------------------------------------------------------------------


;;; Interactive tool activation.
;;; The procedures in this section activate LAML tools. 
;;; It is recommended that you activate the commands from an interactive LAML (Scheme) prompt.
;;; From Emacs carry out the editor command <kbd> run-laml-interactively </kbd>.<p>
;;; All the commands below work relative to the LAML working directory, which is changed by the procedure
;;; <kbd> laml-cd </kbd>. Use the command <kbd> laml-pwd </kbd> to find out about the LAML working directory.
;;; .section-id interactive-tool-section

; Make documentation from a scheme source file.
; This procedure is meant to be called from a Scheme interpreter, in which LAML is loaded and available.
; The procedure utilizes the attributes, which are extracted from the introductory comment (the abstract comment) of the Scheme source file. 
; .form (schemedoc scheme-source-file [commenting-style])
; .parameter scheme-input-file The file name of the Scheme source file, including file extension.\
;    It can be an absolute file path.\
;    It can also be a simple file name (or relative file path) which is assumed to be relative to the so-called startup-directory of LAML.
; .parameter commenting-style   One of the symbols multi-semicolon or documentation-mark. The default value is multi-semicolon.
; .reference "Further info" "LAML Tutorial section" "../tutorial/schemedoc/schemedoc.html"
; (define (schemedoc scheme-input-file . optional-parameter-list)
;  (let* ((scheme-source-file (file-name-proper-and-extension scheme-input-file))
;         (commenting-style (as-symbol (optional-parameter 1 optional-parameter-list "multi-semicolon")))
;         (this-dir (if (absolute-file-path? scheme-input-file) (file-name-initial-path scheme-input-file) (startup-directory)))
;       )
;   (display-message "The LAML Schemedoc tool...")
;   (load (string-append software-directory "tools/schemedoc-extractor/schemedoc-extractor.scm"))
;   (set! scheme-documentation-commenting-style commenting-style)
; 
;   (let* (
;          (doc-list (reverse (extract-documentation-from-scheme-file (string-append this-dir scheme-source-file))))
;          (destination-dir (string-append this-dir extracted-source-destination-delta))
;          (manual-title extracted-manual-title)
;          (manual-author-info (list extracted-manual-author extracted-manual-affiliation ))
;          (manual-abstract (if (empty-string? extracted-manual-abstract) "-" extracted-manual-abstract))
;          (manual-name-from-file (file-name-proper scheme-source-file))
;          (extracted-laml-resource-info extracted-laml-resource)
;          (extracted-css-prestylesheet-info extracted-css-prestylesheet)
;          (extracted-css-stylesheet-info extracted-css-stylesheet)
;          (extracted-css-stylesheet-copying-info extracted-css-stylesheet-copying)
;          (extracted-source-linking-info extracted-scheme-source-linking)
;         )
; 
;     (laml-style "xml-in-laml/schemedoc-2/schemedoc" (string-append laml-dir "styles/") 'conservative-xhtml-loading)
; 
;     (set! laml-manual-stuff (as-boolean extracted-laml-resource-info))
;     (set! the-manual-prestylesheet extracted-css-prestylesheet-info)
;     (set! the-manual-stylesheet extracted-css-stylesheet-info)
;     (set! copy-css-stylesheet? (as-boolean extracted-css-stylesheet-copying-info))
; 
;     (set! css-stylesheet-schema 'local)
;     (set! the-manual-title manual-title) 
;     (set! the-manual-author manual-author-info)
;     (set! the-manual-abstract (if (not (empty-string? manual-abstract)) manual-abstract extracted-manual-abstract))
;     (set! manual-name manual-name-from-file)
; ;    (set! manual-index-width-list  (list 180 320 350))
;     (set! link-to-ep-source-program? (as-boolean extracted-source-linking-info))
; 
;     (set! end-remark "This documentation has been extracted automatically from the Scheme source file.")
; 
;     (make-manual doc-list 'manual-from-scheme-file destination-dir (string-append this-dir scheme-source-file))
; 
;     (display-message 
;       (string-append "DONE. The manual of " scheme-source-file " is located in "
;                      destination-dir manual-name ".html. "))
; ;    (display-message (string-append "The file "  manual-name ".manlsp" " contains a useful internal format."))
; 
;     )))

;; Extract documentation from a scheme source file and present the extracted information in an HTML file.
;; The HTML file is, per default, delivered as a sibling file to the Scheme source file.
;; If the .source-destination-delta attribute is present, the HTML file may be delivered in another directory. 
;; This procedure is supposed to be called from a Scheme interpreter, in which LAML is loaded and available.
;; The procedure takes into consideration the attributes, which are extracted from the introductory comment (the abstract comment) of the Scheme source file. 
;; .form (schemedoc scheme-input-file [commenting-style])
;; .parameter scheme-input-file The file name of the Scheme source file, including file extension.\
;;    It can be an absolute file path.\
;;    It can also be a simple file name (or relative file path) which is assumed to be relative to the so-called startup-directory of LAML.
;; .parameter commenting-style   One of the symbols multi-semicolon or documentation-mark. The default value is multi-semicolon.
;; .reference "Further info" "LAML Tutorial section" "../tutorial/schemedoc/schemedoc.html"
(define (schemedoc scheme-input-file . optional-parameter-list)
 (let* ((scheme-source-file (file-name-proper scheme-input-file))
        (scheme-source-file-plus (file-name-proper-and-extension scheme-input-file))
        (commenting-style (as-string (optional-parameter 1 optional-parameter-list "multi-semicolon")))
        (this-dir (if (absolute-file-path? scheme-input-file) (file-name-initial-path scheme-input-file) (startup-directory)))
        (temp-script-source-name "temp-script.sdoc")
        (loading-prefix (string-append "(load (string-append laml-dir \"laml.scm\")) (laml-style \"xml-in-laml/schemedoc-2/schemedoc\")"))
      )
  (display-message "The LAML Schemedoc tool...")

  ; Load SchemeDoc:
  (laml-style "xml-in-laml/schemedoc-2/schemedoc" (string-append laml-dir "styles/") 'conservative-xhtml-loading)

  ; Generate SchemeDoc LAML script:
  (let ((manual-ast 
          (manual 'internal:run-action-procedure "false"
	   (manual-front-matters
	    'documentation-commenting-style commenting-style
	    'manual-destination-name scheme-source-file
	    )

	   (manual-from-scheme-file 'src scheme-input-file)
	   )))
    (write-text-file
      (string-append loading-prefix (xml-render-as-laml manual-ast))
      (string-append this-dir temp-script-source-name)))

  ; Execute SchemeDoc LAML script:
  (laml (string-append this-dir temp-script-source-name))

  ; Delete SchemeDoc LAML script:
  (delete-file (string-append this-dir temp-script-source-name))

  (display-message 
    (string-append "DONE. The SchemeDoc manual of " scheme-source-file-plus " has been generated."))

))

;; Generate a LAML manual (in SchemeDoc style) of an XML DTD.
;; This procedure reads the parsed dtd file (from a file with extension lsp) and generates an HTML file that represents the manual.
;; .form (xml-dtd-manual dtd-path [target-path mirror-name-prefix])
;; .pre-condition It is assumed that the DTD file already is parsed, and that the parsed DTD file is located side by side the DTD source file. It is also assumed that lib/xml-in-laml/xml-in-laml.scm is already loaded. 
;; .parameter dtd-path the path to the dtd file, without any file extension.
;; .parameter target-path the path in which to write the manual target file. Defaults to the startup directory.
;; .parameter mirror-name-prefix The prefix name of the mirror of the XML language in LAML. (A string). Defaults to the empty string.
;; .example (dtd-manual "xhtml10-transitional")
;; .misc It is recommended that the XHTML1.0 transitional mirror is loaded before use of this procedure. The precondition and the recommendation is fulfilled when used via  M-x run-laml-interactively  in Emacs. 
;; .internal-references "preparatory function" "xml-dtd-parse"
(define (xml-dtd-manual dtd-path . optional-parameter-list)
 (let ((target-path (optional-parameter 1 optional-parameter-list (startup-directory)))
       (mirror-name-prefix (optional-parameter 2 optional-parameter-list ""))
      )
  (laml-style "manual/manual" (string-append laml-dir "styles/") 'conservative-xhtml-loading)
  (let* ((language-name (file-name-proper dtd-path))
         (doc-list 
           (map (manual-extend 'description (string-append "An XML element as defined in the " language-name " XML DTD."))
              (manual-from-parsed-dtd (file-read (string-append dtd-path "." "lsp")) mirror-name-prefix)))
       )
    (set-manual-abstract (string-append "An automatically generated LAML manual of the " language-name " XML DTD."))
    (set-manual-name language-name)
    (set-manual-title (string-append "The " language-name " XML DTD"))
    (make-manual (reverse doc-list) 'manual-from-xml-dtd target-path))))

;; Parse the XML DTD on dtd-file-name. If the input file is f, the parsed file will be 
;; located in f.lsp. The parsed DTD is represented as a Scheme list structure.
;; .parameter dtd-file-name The name of the XML DTD file name in the startup directory. Without any extension.
;; .misc As a side-effect, this procedure defines the variables element-list, attribute-list, and entity-list.
(define (xml-dtd-parse dtd-file-name)
  (load (string-append laml-dir "tools/dtd-parser/dtd-parser-4.scm"))
  (parse-dtd dtd-file-name)
)

;; Generate a mirror of an XML language in LAML and Scheme. This includes the generation of finite state automata for
;; XML validation purposes. If the parsed XML DTD file is named f.lsp, the generated mirror will be located in f.scm.
;; This procedure does not provide access to all 'parameters' of the mirror generation tool. If you need to control
;; the mirror generation in additional details, please write a small LAML script for this purpose.
;; .form (generate-xml-mirror parsed-dtd-file-name language-name [action-element-list])
;; .parameter parsed-dtd-file-name The name of the parsed XML DTD file in the startup directory. Without extension.
;; .parameter language-name The name allocated to the new XML language in LAML. A symbol of your choice.
;; .parameter action-element-list A list of names for which to generate action procedures (list of symbols).
;; .internal-references "preparatory procedure" "xml-dtd-parse"
;; .reference "Full tool support" "XML-in-LAML Mirror generation" "../tools/xml-in-laml/man/xml-in-laml.html"
;; .misc After the generation of the mirror you can move the Scheme mirror file (with extension scm) to a directory of your choice.
(define (generate-xml-mirror parsed-dtd-file-name language-name . optional-parameter-list)
 (let ((action-element-list (optional-parameter 1 optional-parameter-list '())))
  (load (string-append laml-dir "tools/xml-in-laml/xml-in-laml.scm"))
  (set! mirror-name (as-string language-name))
  (set! action-elements action-element-list)
  (let ((dtd-file (file-name-proper parsed-dtd-file-name)))
    (generate-mirror (string-append parsed-dtd-file-name ".lsp") (string-append (startup-directory) dtd-file "." "scm") language-name)
  ))) 


;; Parses an XML file, in-file-name, relative to the given xml-language.
;; Return the XML-in-LAML AST if the optional parameter out-file-name is not given.
;; The returned AST has positive white spacing (which means that white spaces are given as #t values in between constituents).
;; Handle white space preserving elements according the the value (xml-preformatted-text-elements-in xml-language).
;; If an out-file-name is given, write the resulting AST to the file and return a non-specified value.
;; Validate the resulting AST if the mirror library of xml-language is loaded on beforehand.
;; .form (xml-parse-file in-file-name xml-language [out-file-name])
;; .parameter in-file-name The name of an XML file, with or without the xml file extension.\
;;                         May be an absolute file name, or a file relative the the current directory (the value of the epression (startup-directory)).
;; .parameter xml-language The name of the XML language in LAML, to which the resulting AST belongs. A symbol or string.
;; .parameter out-file-name The name of the file on which the AST is written. A file relative to the current directory.
;; .returns The AST, but only if no out-file-name is given.
;; .internal-references "similar procedure" "xml-parse-string"
;; .pre-condition The XML-in-LAML library in /lib/xml-in-laml.xml-in-laml.scm must be loaded before this parsing procedure is called.
(define (xml-parse-file in-file-name xml-language . optional-parameters)
 (let* ((this-dir (startup-directory))
        (proper-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name))
        (out-file-name (optional-parameter 1 optional-parameters #f))
        (in-path (if (absolute-file-path? in-file-name) 
                     in-file-name
                     (string-append this-dir proper-file-name (if (empty-string? ext) "" (string-append "." ext)))))
        (out-path (if out-file-name (string-append this-dir out-file-name) #f))
      )

  (load (string-append laml-dir "tools/xml-html-support/xml-support.scm"))
  (set! white-space-preserving-tags 
        (if (memq (as-symbol xml-language) (languages-in-use))
            (xml-preformatted-text-elements-in (as-symbol xml-language))
            '()))

  (let ((ast (parse-xml-to-ast in-path xml-language)))
    (if (language-in-use? (as-symbol xml-language))
        (begin
           (display-message "Validating AST")
           (validate-ast! ast (as-symbol xml-language)))
        (display-message "Validation not posssible. (Mirror of" xml-language "is not loaded)."))
    (if out-path
        (begin
          (if (file-exists? out-path) (delete-file out-path))
          (let ((op (open-output-file out-path)))
	    (write ast op)
	    (close-output-port op)
	    (display-message "AST written to" out-path)))
        ast))))
      

;; Parses an XML string using the XML parser for LAML and delivers the corresponding XML-in-LAML AST as the result.
;; The returned AST has positive white spacing (which means that white spaces are given as #t values in between constituents).
;; Handle white space preserving elements according the the value (xml-preformatted-text-elements-in xml-language).
;; This function validates the resulting AST if that the mirror of the xml-language is loaded at the time xml-parse-string is called.
;; .form (xml-parse-string xml-string xml-language)
;; .parameter xml-string The string that holds the input to the parser.
;; .parameter xml-language The name of the XML language in LAML, to which the resulting AST belongs. A symbol or string.
;; .returns An XML-in-LAML AST.
;; .internal-references "similar procedure" "xml-parse-file"
(define (xml-parse-string xml-string xml-language)

  (load (string-append laml-dir "tools/xml-html-support/xml-support.scm"))
  (set! white-space-preserving-tags (xml-preformatted-text-elements-in (as-symbol xml-language)))

  (let ((res (parse-xml-string-to-ast xml-string (as-symbol xml-language))))
    (if (language-in-use? (as-symbol xml-language))
        (begin
          (display-message "Validating AST")
          (validate-ast! res (as-symbol xml-language))
        )
        (display-message "Validation not possible (mirror of" xml-language "is not loaded)"))
    res))

;; Parse the HTML file file-name (a file name with or without extension) using the HTML parser for LAML.
;; Writes the parse tree on the optional out-file-name.
;; This function delivers a low level parse tree, which is not the same as an XML-in-LAML AST.
;; If possible at all, use the function xml-parse-file instead.
;; .form (html-parse in-file-name [out-file-name])
;; .parameter in-file-name The name of an HTML file, with or without the html file extension.
;; .parameter out-file-name The name of the file on which the parse tree is written. Defaults to the proper name of the html file with and added lsp extension.
(define (html-parse in-file-name . optional-parameters)
 (let* ((this-dir (startup-directory))
        (proper-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name)) 
        (out-file-name (optional-parameter 1 optional-parameters (string-append proper-file-name "." "lsp"))) 
        (in-path (string-append this-dir proper-file-name (if (empty-string? ext) "" (string-append "." ext))))
        (out-path (string-append this-dir out-file-name))    
      )
  (load (string-append laml-dir "tools/xml-html-support/html-support.scm"))
  (parse-html-file in-path out-path)))

;; Pretty prints the XML file or XML parse tree in in-file-name and place the
;; pretty printed result in out-file-name. 
;; The input is assumed to be a parse tree if and only if the extension is lsp.
;; A XML file is parsed before pretty printing via use of the simple and
;; non-complete, non-validating XML parser from the LAML software package.
;; The optional file out-file-name defaults to in-file-name. 
;; In this case the original input file is overwritten.
;; If you care for your input file, it is strongly recommended that your output file does not overwrite your input file!
;; .form (xml-pp in-file-name [out-file-name single-lining indentation max-width])
;; .parameter in-file-name The file to pretty print
;; .parameter out-file-name The file on which to write the pretty printed result. Default value in-file-name.
;; .parameter single-lining A boolean variable that controls the line breaking; False means break consistently all forms. Default #t.
;; .parameter indentation The increment of indentation. Default value 3.
;; .parameter max-width The preferred maximum line width in the pretty printed file. Default value 80.
;; .misc The pretty printing done by this function is superseded by the LAML AST pretty printing, as implemented by pretty-render-to-output-port and pretty-xml-render.
;; .reference "Similar function" "pretty-render-to-output-port" "../lib/xml-in-laml/man/xml-in-laml.html#pretty-render-to-output-port"
;; .reference "Similar function" "pretty-xml-render" "../lib/xml-in-laml/man/xml-in-laml.html#pretty-xml-render"
(define (xml-pp in-file-name . optional-parameters)
 (let* ((out-file-name (optional-parameter 1 optional-parameters in-file-name))
        (single-lining (optional-parameter 2 optional-parameters #t))
        (indentation (optional-parameter 3 optional-parameters 3))
        (max-width (optional-parameter 4 optional-parameters 80))
        (this-dir (startup-directory))
        (proper-in-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name))
        (in-file-path (string-append this-dir in-file-name))
        (out-file-path (string-append this-dir out-file-name))
       )
  (load (string-append laml-dir "tools/xml-html-support/xml-support.scm"))

  (set! use-single-lining single-lining)
  (set! indentation-delta indentation)
  (set! prefered-maximum-width max-width)

  (write-text-file
    (pretty-print-xml-parse-tree 
      (if (equal? ext "lsp") (file-read in-file-path) (parse-xml in-file-path)))
    out-file-path)))

;; Pretty prints the HTML file or HTML parse tree in in-file-name and place the
;; pretty printed result in out-file-name. 
;; The input is assumed to be a parse tree if and only if the extension is lsp.
;; A HTML file is parsed before pretty printing via use of the non-validating HTML parser from the LAML software package.
;; The optional file out-file-name defaults to in-file-name. 
;; In this case the original input file is overwritten.
;; If you care for your input file, it is strongly recommended that your output file does not overwrite your input file!
;; .form (html-pp in-file-name [out-file-name single-lining indentation max-width])
;; .parameter in-file-name The file to pretty print
;; .parameter out-file-name The file on which to write the pretty printed result. Default value in-file-name.
;; .parameter single-lining A boolean variable that controls the line breaking; False means break consistently all forms. Default #t.
;; .parameter indentation The increment of indentation. Default value 3.
;; .parameter max-width The preferred maximum line width in the pretty printed file. Default value 80.
;; .misc The pretty printing done by this function is superseded by the LAML AST pretty printing, as implemented by pretty-render-to-output-port and pretty-xml-render.
;; .reference "Similar function" "pretty-render-to-output-port" "../lib/xml-in-laml/man/xml-in-laml.html#pretty-render-to-output-port"
;; .reference "Similar function" "pretty-xml-render" "../lib/xml-in-laml/man/xml-in-laml.html#pretty-xml-render"
(define (html-pp in-file-name . optional-parameters)
 (let* ((out-file-name (optional-parameter 1 optional-parameters in-file-name))
        (single-lining (optional-parameter 2 optional-parameters #t))
        (indentation (optional-parameter 3 optional-parameters 3))
        (max-width (optional-parameter 4 optional-parameters 80))
        (this-dir (startup-directory))
        (proper-in-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name))
        (in-file-path (string-append this-dir in-file-name))
        (out-file-path (string-append this-dir out-file-name))
       )
  (load (string-append laml-dir "tools/xml-html-support/html-support.scm"))

  (set! use-single-lining single-lining)
  (set! indentation-delta indentation)
  (set! prefered-maximum-width max-width)

  (write-text-file
    (pretty-print-html-parse-tree 
      (if (equal? ext "lsp") (file-read in-file-path) (parse-html in-file-path)))
    out-file-path)))



;; Parse the bibtex file, file-name, which is a bibtex file name without the bibtex extension.
;; Put the parsed result in file-name.lsp. In addition, deliver the result in the variable parse-result.
;; Finally, present the parsed file as HTML in file-name.html.
(define (bibtex file-name)
 (let ((this-dir (startup-directory))
       (proper-file-name (file-name-proper file-name))
       (ext (file-name-extension file-name))       
      )
  (lib-load "collect-skip.scm")
  (lib-load "file-read.scm")
  (load (string-append laml-dir "tools/bibtex/bibtex.scm"))
  (lib-load "time.scm")
  (lib-load "color.scm")
  (lib-load "html4.0-loose/basis.scm")
  (lib-load "html4.0-loose/surface.scm")
  (lib-load "html4.0-loose/convenience.scm")

  (parse-bibtex-file (string-append this-dir proper-file-name))
  (set! parse-result (reverse parse-result))

  (write-text-file
    (page 
      "Bibtex"
      (present-bibtex-entries parse-result (p)))
    (string-append this-dir proper-file-name ".html"))

  (display-message (string-append "The HTML output is in the file " (string-append this-dir proper-file-name ".html")))))

;; Pretty prints the Scheme or Lisp file - including comments -  in in-file-name and write the result to out-file-name.
;; Conventional comments (prefixed with semicolon) are converted with the Schemedoc procedure
;; lexical-to-syntactical-comments! before the pretty printing. In case you 
;; don't care about comments, you should probably use lisp-pp instead.
;; The optional file out-file-name defaults to in-file-name. 
;; In this case the original input file is overwritten.
;; It is strongly recommended that your output file does not overwrite your input file!
;; This function assumes that the general LAML library is loaded in advance.
;; .form (scheme-pp in-file-name [out-file-name single-lining indentation max-width])
;; .parameter in-file-name The file to pretty print
;; .parameter out-file-name The file on which to write the pretty printed result. Default value in-file-name.
;; .parameter single-lining A boolean variable that controls the line breaking; False means break consistently all forms. Default #t.
;; .parameter indentation The increment of indentation. Default value 3.
;; .parameter max-width The preferred maximum line width in the pretty printed file. Default value 80.
;; .internal-references "similar function" "scheme-pp-simple" 
(define (scheme-pp in-file-name . optional-parameters)
 (let* ((out-file-name (optional-parameter 1 optional-parameters in-file-name))
        (single-lining (optional-parameter 2 optional-parameters #t))
        (indentation (optional-parameter 3 optional-parameters 3))
        (max-width (optional-parameter 4 optional-parameters 80))
        (this-dir (startup-directory))
        (proper-in-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name))
        (in-file-path (string-append this-dir in-file-name))
        (out-file-path (string-append this-dir out-file-name))
        (in-file-path-temp (string-append (laml-temp-file-path) proper-in-file-name "-" "temp!!!" "." ext))
       )
  (lib-load "file-read.scm")
  (load (string-append laml-dir "tools/schemedoc-extractor/schemedoc-extractor.scm"))
  (set! COMMENT-FORM-START (string-append "(comment!!! "))
  (lib-load "scheme-pretty-printing.scm") 
  (set! use-single-lining single-lining)
  (set! indentation-delta indentation)
  (set! prefered-maximum-width max-width)
  (lexical-to-syntactical-comments! in-file-path in-file-path-temp) 
  (pretty-print-lisp-file in-file-path-temp out-file-path)
  (delete-file in-file-path-temp)
 )
)

;; Pretty prints the Scheme or Lisp file - without comment preservation - 
;; in in-file-name and write the result to out-file-name.
;; The pretty printing is simple because the conventional semicolon comments are lost.
;; The similar function scheme-pp preserves the comments during pretty printing.
;; The optional file out-file-name defaults to in-file-name. 
;; In this case the original input file is overwritten.
;; It is strongly recommended that your output file does not overwrite your input file!
;; This function assumes that the general LAML library is loaded in advance.
;; .form (scheme-pp-simple in-file-name [out-file-name single-lining indentation max-width])
;; .parameter in-file-name The file to pretty print
;; .parameter out-file-name The file on which to write the pretty printed result. Default value in-file-name.
;; .parameter single-lining A boolean variable that controls the line breaking; False means break consistently all forms. Default #t.
;; .parameter indentation The increment of indentation. Default value 3.
;; .parameter max-width The preferred maximum line width in the pretty printed file. Default value 80.
;; .internal-references "similar function" "scheme-pp" 
(define (scheme-pp-simple in-file-name . optional-parameters)
 (let* ((out-file-name (optional-parameter 1 optional-parameters in-file-name))
        (single-lining (optional-parameter 2 optional-parameters #t))
        (indentation (optional-parameter 3 optional-parameters 3))
        (max-width (optional-parameter 4 optional-parameters 80))
        (this-dir (startup-directory))
        (proper-in-file-name (file-name-proper in-file-name))
        (ext (file-name-extension in-file-name))
        (in-file-path (string-append this-dir in-file-name))
        (out-file-path (string-append this-dir out-file-name))
       )
  (lib-load "file-read.scm")
  (lib-load "scheme-pretty-printing.scm") 
  (set! use-single-lining single-lining)
  (set! indentation-delta indentation)
  (set! prefered-maximum-width max-width)
  (pretty-print-lisp-file in-file-path out-file-path)
 )
)

;; Convert the HTML file on in-file-name to a LAML file on out-file-name.
;; The conversion is done by parsing in-file-name, transforming the parse tree to LAML,
;; and by pretty printing the resulting LAML program.
;; .misc Exprimental
(define (html-to-laml in-file-name out-file-name)
 (let* ((this-dir (startup-directory))
        (in-file-path (string-append this-dir in-file-name))
        (out-file-path (string-append this-dir out-file-name))
       )
  (load (string-append laml-dir "tools/xml-html-support/html-support.scm"))
  (lib-load "scheme-pretty-printing.scm") 
  (let* ((html-parse-tree (parse-html in-file-path)))
    (parse-tree-to-laml html-parse-tree out-file-path)
    (pretty-print-lisp-file out-file-path))))


;; Process a LENO xml file.
;; .misc Experimental
(define (leno-xml leno-xml-file)
  (set-laml-source-file (file-name-proper leno-xml-file)) 
  (laml-tool-load "xml-html-support/xml-support.scm")
  (display "Parsing XML file") (newline)
  (let* ((parse-tr (parse-xml (string-append (startup-directory) leno-xml-file)))
         (element-str (parse-tree-to-element-structure parse-tr)))
    (display "Parsing OK.  LENO Processing starts.") (newline)
    (laml-style "lecture-notes/leno")  
    (leno-xml-process element-str)))



; ---------------------------------------------------------------------------------------------------
;;; Language settings.
;;; .section-id language-section

;; A variable which determines which language to use in selected parts of the LAML software.
;; The value of the variable must be a symbol.
;; Currently we only support danish and english. english is the default value.
(define language-preference 'english) 

;; Return either danish or english, depending on the value of the global variable language-preference.
(define (text-choice danish english)
  (cond ((equal? language-preference 'english) english)
        ((equal? language-preference 'danish) danish)
        (else (error "Text: Problems in chosing language. Only 'english and 'danish are supported"))))

; ---------------------------------------------------------------------------------------------------
;;; LAML home URL and directories.
;;; The home directory of LAML is always the value of the variable laml-dir, which is defined a LAML installation time.
;;; In this directories there are useful URL and directory functions related to the LAML home directory.
;;; .section-id home-url-section


;; The URL prefix of the LAML software home page at Aalborg University's WWW server.
;; An absolute URL to the latest distributed version of LAML.
(define laml-absolute-url-prefix "http://www.cs.aau.dk/~normark/scheme/distribution/laml/")


;; Return a relative or absolute url prefix to the LAML home directory.
;; If start-dir is given, and if start-dir is a subdirectory of laml-dir, a relative directory path is returned.
;; In other cases, an absolute URL is returned, namely the value of the variable laml-absolute-url-prefix.
;; The parameter extra-level is an extra level (an integer) wich extends a relative path.
;; As an example, extra-level should be 1 in case HTML files are organized in a sub-directory.
;; Normally, extra-level is 0 (zero).
;; If a boolean extra-level is passed we explicitly ask for an absolute URL result.
;; If a string extra-level is passed, we use this string as a relative path to the home.
;; The parameter start-dir is optional. It defaults to the value of (startup-directory).
;; .form (laml-home-url-prefix [extra-level start-dir])
;; .parameter extra-level The extra level as explained above (either an integer, a boolean, or a string). Defaults to the integer 0.
;; .parameter start-dir The directory from which we attempt to establish a relative path to the LAML home directory. Defaults to the value of the expression (startup-directory).
;; .internal-references "applied function" "startup-directory"
(define (laml-home-url-prefix . optional-parameter-list)
 (let ((extra-level (optional-parameter 1 optional-parameter-list 0))
       (start-dir (optional-parameter 2 optional-parameter-list (startup-directory))))
  (cond ((boolean? extra-level) laml-absolute-url-prefix)
	((string? extra-level) extra-level)
	((number? extra-level) 
	   (if start-dir
	       (let ((dir-diff (directory-level-difference start-dir laml-dir)))
		 (cond ((and dir-diff (number? dir-diff) (>= dir-diff 0)) 
			(string-append (repeat-string "../" (+ dir-diff extra-level))))
		       (else laml-absolute-url-prefix)))
	       laml-absolute-url-prefix))
	(else (laml-error "laml-home-url: Problems with the type of extra-level parameter" extra-level)))))


;; Return the relative or absolute prefix file path from dir to the root directory of the LAML software.
;; If dir is a subdirectory of laml-dir, return the relative path from dir to laml-dir.
;; If not, return the absolute path laml-dir
;; .form (laml-dir-prefix [dir])
;; .parameter dir An absolute directory path, inside or outside laml-dir. Defaults to the value of (startup-directory).
;; .returns If possible, the relative directory path from dir to laml-dir. Else laml-dir
;; .internal-references "similar function" "laml-local-url-prefix"
(define (laml-dir-prefix . optional-parameter-list)
 (let ((dir (optional-parameter 1 optional-parameter-list (startup-directory))))
  (let* ((normalized-dir (normalize-file-path dir))
         (diff (directory-level-difference normalized-dir laml-dir))
        )
    (if diff 
        (repeat-string "../" diff)
        laml-dir))))

;; Return the relative or absolute url to the local laml dir.
;; The URL function corresponding to laml-dir-prefix.
;; If dir is a subdirectory of laml-dir, return the relative path from dir to laml-dir.
;; If not, return the absolute file:// prefixed URL to the laml directory.
;; .form (laml-local-url-prefix [dir])
;; .parameter dir An absolute directory path, inside or outside laml-dir. Defaults to the value of (startup-directory).
;; .returns If possible, the relative url from dir to laml-dir. Else laml-dir prefixed with "file://"
;; .internal-references "similar function" "laml-dir-prefix"
(define (laml-local-url-prefix . optional-parameter-list)
 (let ((dir (optional-parameter 1 optional-parameter-list (startup-directory))))
  (let* ((normalized-dir (normalize-file-path dir))
         (diff (directory-level-difference normalized-dir laml-dir))
        )
    (if diff 
        (repeat-string "../" diff)
        (string-append "file://" laml-dir)))))


;; Is the directory dir a (potential) subdirectory of laml-dir.
;; It is not necessary for dir to actually exist within laml-dir.
;; laml-dir is the path to the directory, in which your LAML system is installed.
(define (is-a-laml-directory? dir)
  (let ((dir-diff (directory-level-difference dir laml-dir)))
    (cond ((and (boolean? dir-diff) (not dir-diff)) #f)
          ((and (number? dir-diff) (< dir-diff 0)) #f)
          ((and (number? dir-diff) (>= dir-diff 0)) #t)
          (else (laml-error "is-a-laml-directory?: Should not happen:" dir-diff)))))

; ---------------------------------------------------------------------------------------------------
;;; Document prolog and epilog functions.
;;; This section contains definitions of document prolog and epilog functions.
;;; In addition, there are a number of more basic functions which return information about 
;;; the document. Several of these return empty strings, and they intended to be redefined in other contexts.
;;; .section-id prolog-epilog-section

;; Return a standard document prolog - front matters - inserted before any document elements.
;; If requested, the rendering function can insert the standard prolog.
;; In some contexts, the standard prolog may depend on the optional language parameter.
;; .returns The document type declaration and the copyright-clause.
;; .form (standard-prolog [language])
(define (standard-prolog . optional-parameter-list)
 (let ((language (optional-parameter 1 optional-parameter-list #f)))
  (string-append
   (document-type-declaration)
   (if (not (empty-string? (document-type-declaration))) (as-string #\newline) "")
   (copyright-clause)
   (if (not (empty-string? (copyright-clause))) (as-string #\newline) ""))))

;; Returns a standard document epilog - end matters - inserted after the document elements.
;; If requested, the rendering function can insert the standard epilog.
;; In some contexts, the standard epilog may depend on the optional language parameter.
;; .returns the laml standard comment and the tracing comment.
;; .form (standard-epilog [language])
(define (standard-epilog . optional-parameter-list)
 (let ((language (optional-parameter 1 optional-parameter-list #f)))
  (string-append 
    (as-string #\newline)
    (laml-standard-comment) (as-string #\newline)
    (tracing-comment)))) 


;; Return a document type declaration. This function is redefined in the individual mirrors.
;; Called by standard-prolog.
;; In some contexts, the document type declaration may depend on the optional language parameter.
;; .form (document-type-declaration [language])
;; .returns the empty string (if not redefined)
(define (document-type-declaration . optional-parameter-list)
 (let ((language (optional-parameter 1 optional-parameter-list #f)))
   ""))


;; Return an HTML comment with a copyright notice, or an empty string.
;; You can redefine this function if you need a copyright message as part of your document.
;; If you redefine this function, it must return an HTML/XML comment.
;; Called by standard-prolog.
;; .returns the empty string (if not redefined)
(define (copyright-clause)
  "")

;; Return a standard comment about LAML. Depends on the function html-comment.
;; Called by standard-epilog.
;; .returns an HTML comment about LAML.
(define  (laml-standard-comment)
  (html-comment 
    (string-append 
      "Generated from a LAML source file. " 
      laml-version ". "
      "LAML is designed and implemented by Kurt Nørmark, normark@cs.aau.dk. "
    )))

(define (html-comment comment)
  (string-append "<!-- " comment "-->"))

;; Return a HTML comment which somehow traces this document.
;; Typical information includes source file, time of generation, operating system, Scheme systemt, etc.
;; Redefine this function if you need tracing information in your document.
;; .returns the empty string (if not redefined)
(define (tracing-comment) "")

    

; ---------------------------------------------------------------------------------------------------
;;; Cosmetic welcome, ending and copyright functions. 
;;; .section-id welcome-section


;; Initiating welcome and info text for interactive LAM tools. 
;; As of now this is entirely cosmetic.
(define (laml-welcome)
  (let ((vers (read-text-file (string-append laml-dir "distribution-version"))))
    (display (string-append "Welcome to LAML " vers ".")) (newline)
    (display "(C) Kurt Normark, Aalborg University, Denmark.") (newline) ))

;; This function is intended to end a LAML file.
;; It is strongly recommended that any LAML file - in particular XML-in-LAML file - calls end-laml as the last action.
;; Reports on elapsed processing time (currently only in MzScheme and Guile).
;; Checks ID attributes and links (only from XML-in-LAML contexts, via redefine version of this function).
;; A redefined version of end-laml in xml-in-laml.scm - used for xml-in-laml processing - calls this function (in addition to xml-in-laml relevant stuff).
;; .internal-references "related function" "begin-laml"
;; .reference "redefinition in XML-in-LAML" "end-laml" "../lib/xml-in-laml/man/xml-in-laml.html#end-laml"
(define (end-laml)
 (let ((time-diff 
          (cond ((or (eq? scheme-system 'mzscheme) (eq? scheme-system 'mzscheme-200))
                    (- (current-process-milliseconds) start-laml-time))
                ((eq? scheme-system 'guile)
                    (inexact->exact (round (* (/ (- (get-internal-run-time) start-laml-time) internal-time-units-per-second) 1000))))
                (else #f))))
  (if time-diff
      (begin
       (display (string-append "LAML processing time: " (as-string time-diff) " milliseconds."))
       (newline)))

  (display "End of LAML processing") (newline)))

; The original end-laml function. 
; Used by other parts of LAML to get access to the original end-laml in laml.scm, for instance as part of redefining end-laml.
(define original-end-laml end-laml)


;; Return a credit message to Kurt Nørmark about system-dk (the Danish name) and system-eng (the English name).
;; As an optional parameter, an URL can be supplied with a link to the credited system.
;; .form (credits system-dk system-eng [system-url])
;; .parameter system-dk The system name in Danish
;; .parameter system-eng The system name in English
;; .parameter system-url A URL referring to a WWW description of the system
(define (credits system-dk system-eng . optional-parameter-list)
 (let* ((url (optional-parameter 1 optional-parameter-list #f))
        (anchor-text (text-choice system-dk system-eng))
        (anchor-clause (if url (a-tag url anchor-text) anchor-text))
       )
   (string-append 
     (text-choice 
         (con anchor-clause " er designet og programmeret af Kurt Nørmark (c), Aalborg Universitet, med brug af "
                       (a-tag "http://www.cs.aau.dk/~normark/laml/" (font-color black "LAML")) " teknologi.")
         (con anchor-clause " is designed and programmed by Kurt Nørmark (c), Aalborg University, Denmark using " 
                      (a-tag "http://www.cs.aau.dk/~normark/laml/" (font-color black "LAML")) " technology.")
                  ))))


;; Return the LAML POWER icon with link to the LAML home page.
;; Intended for the footer of LAML generated pages, from which the author wish to acknowledge the use of LAML.
;; The LAML icon is located in the directory (string-append (laml-home-url-prefix extra-level) "images/"),
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
   (a 'href "http://www.cs.aau.dk/~normark/laml/"
      (img 'border "0"
           'src (string-append (laml-home-url-prefix extra-level)
                              (cond ((eq? icon-size 'large) "images/laml-power-icon-4.gif")
                                    ((eq? icon-size 'small) "images/laml-mini-icon-1.gif")
                                    (else (laml-error "laml-power-icon: third parameter must either be large or small"))))
           'alt "Program Oriented Web Engineering - using LAML"))))


;; Generate a LAML shortcut icon from the current directory (the startup-directory).
;; Shortcut icons are in some browsers shown as a tiny picture in the navigation tool bar, and together with bookmarks.
;; .parameter laml-home-url-dir A path from the current directory to the LAML home directory. Typically the value of (laml-home-url-prefix).
;; .pre-condition The HTML link mirror function must be defined for this function to work.
(define (laml-shortcut-icon laml-home-url-dir)
  (link 'rel "SHORTCUT ICON" 'href (string-append laml-home-url-dir "images/16-16-icon.ico")))



;;; XML file writing procedures.
;;; In this section we have a convenient and versatile function which can be used to write an XML expression in LAML to a text file.
;;; .section-id html-file-writing-section


;; Write xml-clause (an XML-in-LAML ast) to a text file.
;; Use the character transformation table assigned to the XML language of xml-clause, as availble via the expression (xml-char-transformation-table-in language).
;; This procedure also collects links for later checking, and it expands procedural content items.
;; The full path to the text file can be given by the third, optional parameter,
;; the default value of which is (full-source-path-with-extension "html").
;; Mode may be a symbol (raw or pp), or a list of symbols including one of raw/pp and the symbols prolog and epilog.
;; The latter determines the rendering of the standard prolog and the standard epilog,
;; as defined by the functions standard-prolog and standard-epilog (in this file).
;; If mode is the symbol pp, do pretty print the XML fragment before writing.
;; If mode is raw, just write the xml clause without any kind of pretty printing.
;; As the last action, activate a post processor function on the full file path to the XML target file name.
;; If the file extension of the target file is e, the name of the post processor is (string-append e "-" "process").
;; (The post processor may, for instance, be used to transform an XSL-FO to PDF.)
;; .form (write-xml mode xml-clause [file-path-with-extension])
;; .parameter mode a list with one or more of the symbols raw, pp, prolog, and epilog. Alternatively just one of the symbols pp or raw.
;; .parameter xml-clause the ast to be written.
;; .parameter file-path-with-extension the path of the file on which to write. Must include the file extension, typically html. Defaults to the name of the current source file with extension '.html'.
;; .internal-references "default target file" "full-source-path-with-extension"
;; .internal-references "prolog and epilog" "standard-prolog" "standard-epilog"
;; .misc This procedure is a renamed, XML-in-LAML only version of the procedure write-html
(define (write-xml mode-0 xml-clause . optional-parameter-list)
 (let ((file-path-with-extension (optional-parameter 1 optional-parameter-list (full-source-path-with-extension "html")))
       (mode (cond ((symbol? mode-0) mode-0)
                   ((list? mode-0) (cond ((memq 'raw mode-0) 'raw)
                                         ((memq 'pp mode-0) 'pp)
                                         (else raw)))))
       (prolog? (cond ((list? mode-0) (cond ((memq 'prolog mode-0) #t)
                                           (else #f)))
                      (else #f)))
       (epilog? (cond ((list? mode-0) (cond ((memq 'epilog mode-0) #t)
                                           (else #f)))
                      (else #f)))
       
      )
  (cond 
        ((and (ast? xml-clause) (is-xml-ast? xml-clause) (eq? mode 'pp))
	 (if (file-exists? file-path-with-extension) (delete-file file-path-with-extension))
         (if (not (eq? xml-link-checking 'none)) (collect-links-for-later-checking-in-ast! xml-clause file-path-with-extension))
	 (let* ((op (open-output-file file-path-with-extension)))
           (pretty-render-to-output-port (expand-procedural-content-items-in-ast xml-clause) op (if prolog? 'prolog #f) (if epilog? 'epilog #f))
           (close-output-port op))
         (write-xml-post-process! file-path-with-extension)
         'done
        )

        ((and (ast? xml-clause) (is-xml-ast? xml-clause) (eq? mode 'raw))
	 (if (file-exists? file-path-with-extension) (delete-file file-path-with-extension))
         (if (not (eq? xml-link-checking 'none)) (collect-links-for-later-checking-in-ast! xml-clause file-path-with-extension))
	 (let* ((op (open-output-file file-path-with-extension)))
           (render-to-output-port (expand-procedural-content-items-in-ast xml-clause) op (if prolog? 'prolog #f) (if epilog? 'epilog #f))
           (close-output-port op))
         (write-xml-post-process! file-path-with-extension)
         'done
        )
	(else (laml-error "write-xml: Unsupported combination of xml-clause and writing mode" mode "Consider the procedure write-html.")))))

; Activate the XML post processor, as determined by the file extension of full-target-file-path-with-extension.
; Pass full-target-file-path-with-extension to the processor.
; Given an extension e, activate the function (string-append e "-" "process") if this name is bound in the current environment.
(define (write-xml-post-process! full-target-file-path-with-extension)
  (let* ((ext (file-name-extension full-target-file-path-with-extension))
         (processor-symbol (as-symbol (string-append ext "-" "process")))
        )
    (cond ((bound? processor-symbol)
             ((eval-cur-env processor-symbol) full-target-file-path-with-extension))
          (else 'do-nothing))))

;; Process the ast according to the processing-specs and write the resulting XML files to files determined by file-path.
;; More specifically, the AST is processed once for each processing-spec, by calling write-xml for each processing specification in processing-specs.
;; As part of calling write-xml, the post processing facility may be activated (for instance for processing an XSL-FO file).
;; A processing spec is a list (ext transformer . mode-symbols), where ext is the file extension, transformer is an AST transformation function, 
;; and mode-symbols is a list of symbols in the set {pp raw prolog epilog}.
;; .internal-references "called function" "write-xml"
;; .parameter ast The XML-in-LAML AST to be processed.
;; .parameter processing-specs A list of processing specifications.
;; .parameter file-path An absolute file path with or without file extension.\ 
;;            The final file extension is determined by the first element of the processing-spec
(define (process-xml processing-specs file-path ast)
  (let ((init-path (file-name-initial-path file-path))
        (proper-name (file-name-proper file-path)))
  (for-each
    (lambda (spec)
      (let ((ext (first spec))
            (transformer (second spec))
            (mode-symbols (cddr spec)))
        (write-xml mode-symbols (transformer ast) (string-append init-path proper-name "." ext))))
    processing-specs
  )))
             

;; Write html-clause (a string or an ast) to a text file.
;; When used on XML-in-LAML asts, this procedure also collects links (for later checking), 
;; and it expands procedural content items.
;; The full path to the text file can be given by the third, optional parameter,
;; the default value of which is (full-source-path-with-extension "html").
;; Mode may be a symbol (raw or pp), or a list of symbols including one of raw/pp and the symbols prolog and epilog.
;; The latter determines the rendering of the standard prolog and the standard epilog,
;; as defined by the functions standard-prolog and standard-epilog (in this file).
;; If mode is the symbol pp, do pretty print the HTML fragment before writing.
;; If mode is raw, just write the html clause without any kind of pretty printing.
;; This procedure loads the LAML xml-html-support pretty printing stuff if needed.
;; This procedure works on both the ast based (including XML-in-LAML) and the text based mirrors.
;; In case html-clause is an AST, the tree is processed by an AST rendering function before the file writing takes place. 
;; In case hmtl-clause is an XML-in-LAML AST, it is expanded with respect to procedural content items before the writing takes place.
;; There are still a few minor problems with the HTML pretty printer.
;; .form (write-html mode html-clause [file-path-with-extension])
;; .parameter mode a list with one or more of the symbols raw, pp, prolog, and epilog. Alternatively just one of the symbols pp or raw.
;; .parameter html-clause the string or ast to be written
;; .parameter file-path-with-extension the path of the file on which to write. Must include the file extension, typically html. Defaults to the name of the current source file with extension '.html'.
;; .internal-references "default target file" "full-source-path-with-extension"
;; .internal-references "prolog and epilog" "standard-prolog" "standard-epilog" 
;; .misc This procedure is the original, broader version of write-xml.
(define (write-html mode-0 html-clause . optional-parameter-list)
 (let ((file-path-with-extension (optional-parameter 1 optional-parameter-list (full-source-path-with-extension "html")))
       (mode (cond ((symbol? mode-0) mode-0)
                   ((list? mode-0) (cond ((memq 'raw mode-0) 'raw)
                                         ((memq 'pp mode-0) 'pp)
                                         (else raw)))))
       (prolog? (cond ((list? mode-0) (cond ((memq 'prolog mode-0) #t)
                                           (else #f)))
                      (else #f)))
       (epilog? (cond ((list? mode-0) (cond ((memq 'epilog mode-0) #t)
                                           (else #f)))
                      (else #f)))
       
      )
  (cond 
        ((and (ast? html-clause) (is-xml-ast? html-clause) (eq? mode 'pp))
	 (if (file-exists? file-path-with-extension) (delete-file file-path-with-extension))
         (if (not (eq? xml-link-checking 'none)) (collect-links-for-later-checking-in-ast! html-clause file-path-with-extension))
	 (let* ((op (open-output-file file-path-with-extension)))
           (pretty-render-to-output-port (expand-procedural-content-items-in-ast html-clause) op (if prolog? 'prolog #f) (if epilog? 'epilog #f))
           (close-output-port op))
        )

        ((and (ast? html-clause) (is-xml-ast? html-clause) (eq? mode 'raw))
	 (if (file-exists? file-path-with-extension) (delete-file file-path-with-extension))
         (if (not (eq? xml-link-checking 'none)) (collect-links-for-later-checking-in-ast! html-clause file-path-with-extension))
	 (let* ((op (open-output-file file-path-with-extension)))
           (render-to-output-port (expand-procedural-content-items-in-ast html-clause) op (if prolog? 'prolog #f) (if epilog? 'epilog #f))
           (close-output-port op))
        )

        ((and (ast? html-clause) (eq? mode 'pp))  ; non-xml ast - html4.01 presumably
         (load (string-append laml-dir "tools/xml-html-support/html-support.scm"))
         (let ((transformer (compose pretty-print-html-parse-tree ast-to-parse-tree)))
	   (write-text-file
	    (prolog-epilog-envelope (transformer html-clause) prolog? epilog?)
	    file-path-with-extension)))

	((and (ast? html-clause) (eq? mode 'raw))
	 (if (file-exists? file-path-with-extension) (delete-file file-path-with-extension))
	 (let* ((op (open-output-file file-path-with-extension)))
           (render-to-output-port html-clause op (if prolog? 'prolog #f) (if epilog? 'epilog #f))
           (close-output-port op))
        )

	((and (string? html-clause) (eq? mode 'pp))
         (load (string-append laml-dir "tools/xml-html-support/html-support.scm"))
	 (let ((transformer (compose pretty-print-html-parse-tree parse-html-string)))
	   (write-text-file
	    (prolog-epilog-envelope (transformer html-clause) prolog? epilog?)
	    file-path-with-extension)))

	((and (string? html-clause) (eq? mode 'raw))
         (write-text-file
	  (prolog-epilog-envelope html-clause prolog? epilog?)
	  file-path-with-extension))

	(else (laml-error "write-html: Unsupported combination of html-clause and writing mode" mode)))))



; Is x an XML AST, such as an XHTML AST.
; Non-XML ASTs do not have a language indication as last elements. Therefore
; we can distinguish XML asts from older HTML asts by the number of elements in the
; list AST representation.
(define (is-xml-ast? x)
  (and (ast? x)            ; hereby a proper list
       (>= (length x) 6))) 


; Surround html-text with the standard prolog and epilog, if signalled by the two boolean parameters.
; This function depends on the two parameterless functions standard-prolog and standard-epilog.
; .form (prolog-epilog-envelope html-text prolog? epilog? [language])
(define (prolog-epilog-envelope html-text prolog? epilog? . optional-parameter-list)
 (let ((language (optional-parameter 1 optional-parameter-list #f)))
  (let ((prolog-text (cond (prolog? (standard-prolog language))
                           (else "")))
	(epilog-text (cond (epilog? (standard-epilog language))
                           (else "")))
	)
    (string-append prolog-text html-text epilog-text))))




;;; The HTML character transformation table.
;;; This table is used by the HTML rendering function to transliterate char data to
;;; textual contents, as to be shown in a browser. You can use this table to perform
;;; transformation of national characters to HTML character entities, and to perform
;;; other character transliterations.
;;; .section-id char-trans-section

; Depends on make-list from general.scm

;; A vector of length 256 which transforms character number i to a string. 
;; Position number i determines how the (extended) ASCII character i is transformed.
;; Boolean entry #t means 'do not transform'.
;; Boolean entry #f means 'ignore char'.
;; A string entry describes a proper transformation.
;; A char entry describes a proper transformation.
;; An integer entry describes a transformation to the corresponding character number.
;; All other entries are illegal.
;; The table represent the identity transformation (all characters are mapped to the value #t).
;; Use the function set-html-char-transformation-entry! for mutation of individual entries.
;; .internal-references "mutation function" "set-html-char-transformation-entry!"
(define html-char-transformation-table
    (list->vector (make-list 256 #t)))

; The html-char-transformation-table is initialized in lib/xml-in-laml/xml-in-laml.scm

;; Mutate a html character transformation table at position index. 
;; More specifically, put new-entry at position index in the table. 
;; The first entry in the table has index 0.
;; .parameter transformation-table Typically the vector html-char-transformation-table
;; .parameter index a number between 0 and 255
;; .parameter new-entry The new entry, which can be boolean, a string, a character, or an integer.\
;;                      Boolean true means 'Do not transform character'.\
;;                      Boolean false means 'Ignore character' (outputs the empty string).\
;;                      A string means: 'Transform char to the given string'.\
;;                      An integer number means: 'Transform char to the char with the given number'.\
;;                      A character means: 'Transform to the given char'.
;; .internal-references "info about table" "html-char-transformation-table"
(define (set-html-char-transformation-entry! transformation-table index new-entry)
  (vector-set! transformation-table index new-entry))

; The actual mutations of the html character transformation table is done in the actual
; mirrors. The reason is that the HTML4 mirrors are less mature than the XHTML mirror with
; respect to character references. (The '&' character is not allowed to be character transformed
; in HTML4, but it need to be transformed in XHTML. See lib/xml-in-laml/xml-in-laml.scm and
; tools/validating-html-mirror-from-dtd/runtime/basic.scm).


; ---------------------------------------------------------------------------------------------------
; HTML char and text transformation using the html-char-transformation-table.
; html-char-transformation-table is defined in laml.scm, and possibly redefined in the .laml setup file.

;; Transform each character in the string str, using the HTML char transformation table, html-char-transformation-table.
;; Very inefficient in memory usage - therefore eliminated.

; (define (html-text-transform str)
;  (html-text-transform-1 str (string-length str) 0 '())
; )
; 
; 
; (define (html-text-transform-1 str str-lgt i res)
;   (cond ((= i str-lgt) (list-to-string (reverse res) ""))
;         (else (html-text-transform-1 str str-lgt (+ i 1) (cons (html-char-transform (string-ref str i)) res)))))

;; Return the transformation of char via a character transformation table.
;; Chararcters outside the range [0..255] are just passed through, in case an extended character set is used.
;; .form (html-char-transform char [transformation-table])
;; .parameter char A character to be transformed.
;; .parameter transformation-table A character transformation table, which defaults to html-char-transformation-table defined in laml.scm
;; .returns A string (the transformation of character)
(define (html-char-transform char . optional-parameter-list)
 (let ((transformation-table (optional-parameter 1 optional-parameter-list html-char-transformation-table)))
  (let* ((n (char->integer char))
         (res (if (and (>= n 0) (<= n 255))
	          (vector-ref transformation-table n)
                  (char->string char)))
        )
    (cond ((and (boolean? res) res) (char->string char))
          ((string? res) res)
          ((and (boolean? res) (not res)) "")
          ((char? res) (char->string res))
          ((and (integer? res) (>= res 0) (<= res 255)) (char->string (integer->char res)))
          (else (laml-error "html-char-transform: Unable to transform character: " char))))))

; ---------------------------------------------------------------------------------------------------------------

;;; R4RS and R5RS Scheme knowledge.
;;; The section contains accessor and loading functions to R4RS and R5RS Scheme knowledge files.
;;; The Scheme knowledge files are located in the r4rs and the r5rs directories of the full LAML distribution.
;;; The r4rs and r5rs directories each hold a HTML version of the Scheme Report.
;;; Overall, a Scheme knowledge file is a mapping from syntax/procedure name to an URL in the Scheme Report.
;;; More precisely, a Scheme knowledge file is a list of entries, each of which
;;; contains the name of a Scheme form, the categorization of the form, and the URL of place, where form
;;; is described (in a compact format). Scheme knowledge files have extensions lsp.
;;; .section-id scheme-knowledge

;; Read and the return the list structure for Scheme knowledge of RnRS, where n
;; corresponds to scheme-version (a number).
;; .parameter scheme-version either 4 or 5 (integer numbers). Alternatively r4rs or r5rs (symbols).
;; .returns The list structure of Scheme knowledge 
(define (read-scheme-knowledge scheme-version)
  (let* ((scheme-version-number (cond ((number? scheme-version)
				       scheme-version)
                                      ((and (symbol? scheme-version) (eq? scheme-version 'r4rs)) 4)
                                      ((and (symbol? scheme-version) (eq? scheme-version 'r5rs)) 5)
                                      (else (laml-error "read-scheme-knowledge: scheme-version must be an integer (4 or 5) or one of the symbols r4rs or r5rs:" scheme-version))))
        )
     (cond ((= scheme-version-number 4)
            (file-read (string-append laml-dir "r4rs/" "scheme-knowledge.lsp")))
	   ((= scheme-version-number 5)
            (file-read (string-append laml-dir "r5rs/" "scheme-knowledge.lsp")))
	   (else (laml-error (string-append "R" (as-string scheme-version-number) "RS") "is not supported.")))))

;; Selects the name of a scheme knowledge entry.
(define symbol-of-scheme-knowledge (make-selector-function 1 'symbol-of-scheme-knowledge))

;; Selects the category of a scheme knowledge entry.
(define category-of-scheme-knowledge (make-selector-function 2 'category-of-scheme-knowledge))

;; Selects the essentiality of a scheme knowledge entry.
(define essentiality-of-scheme-knowledge (make-selector-function 3 'essentiality-of-scheme-knowledge))

;; Selects the HTML file number of a scheme knowledge entry.
(define file-number-of-scheme-knowledge (make-selector-function 4 'file-number-of-scheme-knowledge))

;; Selects the anchor name of a scheme knowledge entry.
(define anchor-name-of-scheme-knowledge (make-selector-function 5 'anchor-name-of-scheme-knowledge))


;; Return the suffix part of an URL to an RnRS Scheme HTML file. 
;; .parameter an entry in a Scheme knowledge file.
;; .parameter scheme-version either 4 or 5 (integer numbers). Alternatively r4rs or r5rs (symbols).
;; .pre-condition The Scheme knowledge of RnRS corresponding to n = scheme-version must be read on beforehand, and the entry must be Scheme knowledge of an RnRS entry.
(define (url-suffix-of-scheme-knowledge entry scheme-version)
  (let* ((scheme-version-number (cond ((number? scheme-version)
                                         scheme-version)
                                      ((and (symbol? scheme-version) (eq? scheme-version 'r4rs)) 4)
                                      ((and (symbol? scheme-version) (eq? scheme-version 'r5rs)) 5)
                                      (else (laml-error "url-suffix-of-scheme-knowledge: scheme-version must be an integer (4 or 5) or one of the symbols r4rs or r5rs:" scheme-version))))
         (rnrs (cond ((= scheme-version-number 4) "r4rs")
                     ((= scheme-version-number 5) "r5rs")
                     (else (laml-error (string-append "r" (as-string scheme-version-number) "rs") "is not supported.")))))
   (if (>= (length entry) 5)
       (string-append rnrs "_" 
                      (as-string (file-number-of-scheme-knowledge entry))
                      (cond ((= scheme-version-number 4)  ".htm")
                            ((= scheme-version-number 5)  ".html")
                            (else (laml-error (string-append "r" (as-string scheme-version-number) "rs") "is not supported.")))
                      "#" 
                      (anchor-name-of-scheme-knowledge entry))
       #f)))


;;; Miscellaneous.

;; The LAML Manual/SchemeDoc standard settings.
;; The list returned is to be a constituent of laml-front-matters element of a LAML SchemeDoc manual.
;; .form (kn-manual-settings [abstract-clause])
;; .parameter abstract-clause A manual abstract form.
;; .reference "manual mirror function" "laml-front-matters" "../styles/xml-in-laml/manual/man/manual.html#laml-front-matters"
;; .internal-references "category" "ref1" "ref2" ...
;; .misc I usually keep this and similar functions in my .laml file. A version in the .laml file will overwrite this function.
;; .returns A list of attributes and constituents of laml-front-matters.
(define (kn-manual-settings . optional-parameter-list)
 (let ((abstract-clause (optional-parameter 1 optional-parameter-list #f)))
  (list
    (manual-author (copyright-owner "Kurt Nørmark") "normark@cs.aau.dk" )
    (manual-affiliation "Department of Computer Science," "Aalborg University," "Denmark.")
    (if abstract-clause abstract-clause '())
    (laml-library-source-linking)
    'css-prestylesheet "compact"
    'css-stylesheet "argentina"
    'css-stylesheet-copying "true"
  )))

;; A collection of scheme-source-linking-manual clauses which provides for 
;; linking from Scheme Sources to LAML manuals.
;; The boolean SchemeDoc attribute named scheme-source-linking controls the generation of Scheme source linking
;; from SchemeDoc manuals.
;; .reference "SchemeDoc" "scheme-source-linking-manual" "../styles/xml-in-laml/manual/man/manual.html#scheme-source-linking-manual"
;; .internal-references "Used by" "kn-manual-settings"
(define (laml-library-source-linking)
  (append
   (map                                 ; the LAML lib/ manuals 
    (lambda (key) 
      (scheme-source-linking-manual 
       (list 'key key)
       (list 'file-path (string-append  (laml-dir-prefix) "lib/man/" key))  
       )
      )
    (list "cgi" "collect-skip" "color" "crypt" "encode-decode" "file-read" "final-state-automaton" "general" "time" 
          "xhtml10-convenience")
    )
   (map                                 ; the LAML core library
    (lambda (key) 
      (scheme-source-linking-manual 
       (list 'key key)
       (list 'file-path (string-append (laml-dir-prefix) "man/" key))
       )
      )
    (list "laml")
    )
   (map                                 ; the XML-in-LAML library
    (lambda (key) 
      (scheme-source-linking-manual 
       (list 'key key)
       (list 'file-path (string-append (laml-dir-prefix) "lib/xml-in-laml/man/" key))
       )
      )
    (list "xml-in-laml")
    )
   (map                                 ; the XML-in-LAML library
    (lambda (key) 
      (scheme-source-linking-manual 
       (list 'key key)
       (list 'file-path (string-append (laml-dir-prefix) "lib/xml-in-laml/mirrors/man/" key))
       )
      )
    (list "xhtml10-transitional-mirror" "xhtml10-strict-mirror" "xhtml10-frameset-mirror")
    )

   ))


