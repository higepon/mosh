;; The editor part of the (Scheme) Elucidator
;; Requires the LAML emacs-stuff, in particular emacs-support/laml-template.el

(provide 'scheme-elucidator-support)

; ---------------------------------------------------------------------------------------------------
; Variables keeping track of important information in the editor part of the elucidator

(defvar elucidator-directory nil 
  "The directory in which the elucidator file (-.laml, html, internal) resides. Ends in a slash")

(defvar software-directory (concat laml-dir "styles/xml-in-laml/elucidator-2/")
  "The directory in which the elucidator software resides")

(defvar educidator-name nil
  "The name of the elucidator, without extension")

(defvar program-source-files '()
  "The list of source file descriptions in the documentation bundle.
In this context, a source file description is a tripple of source-key, file-location (full path), and language.")

(defvar source-keys '()
  "The list of source keys of the files in this documentation bundle. Parallel to source-versions.")

(defvar source-versions '()
  "The list of source versions numbers of the files in this documentation bundle. Parallel to source-keys. ")

(defvar source-key-buffer-map '() 
 "An association list from source-key to buffer, in which the source is represented in the editor")

(defvar documentation-buffer nil
 "The emacs buffer which contains textual documentation file")

(defvar documentation-window nil "The top most documentation window")
(defvar program-window nil "The bottom most program window")

(defvar current-program-buffer nil
  "One of the program buffers, which is considered as the current one for the moment")

(defvar setup-buffer nil
 "The emacs buffer which contains the laml documentation file")

(defvar defined-names '()
  "An alist of defined names. The pairs in the a-list represent (name-symbol . source-key-string).
A source-key is an abbreviation of a source file name (and path).")

(defvar defined-name-strings
  '()
  "A trivial alist of the defined names, as strings")

(defvar documentation-ids '()
  "A list of symbols, representing the ids of sections and entries in the documentation part")

(defvar documented-name-occurences '()
  "A list of (program-name doc-id weak/strong) triples. All constituents are symbols.
Transferred from a similar variable of the Elucidator (of the same name)")

(defvar documentation-id-strings '()
  "A trivial alist of documentation-ids, as strings")

;; The prefix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(defvar p-link-prefix-char "{")  

;; The suffix character of links from documentation to program: p for program. 
;; Must be an absolute unique character in the documentation
(defvar p-link-suffix-char "}")

(defvar p-link-strong-rel-char "*" "The character which is used for strong relations between program and documentation")

(defvar p-link-weak-rel-char "+" "The character which is used for weak relations between program and documentation")

(defvar p-link-none-rel-char "-" "The character which is used for program words in the documentation (non relations, just typographic)")

(defvar qualification-char "$" "The character which is used between the source key qualification and the program name")

(defvar sectional-comment-char ":" "The character used (double) in both ends of a sectional comment name. A singular string.")

(defvar source-marker-char "@" "The character used as prefix of source markers.")


; repeated documentation navigation from program definition.
(defvar doc-word-list-repeated-navigation nil "A list of documentation words to visit repeatedly.")

(defvar doc-word-repeated-navigation nil "A documentation word to visit repeatedly via ??")

(defvar position-repeated-navigation -1 "An integer position in doc-word-list-repeated-navigation, where to go next. 
The first element is number 0. -1 means no next navigation possible")


;; The prefix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(defvar d-link-prefix-char "[")  

;; The suffix character of links from documentation to documentation: d for documentation. 
;; Must be an absolute unique character in the documentation
(defvar d-link-suffix-char "]")

(defvar locate-documentation-exactly t "If true locates the documentation relation exactly when
  going from program to documentation by means of goto. If false, just locate the relevant documentation section")

(defvar always-qualify-program-names t "If true, always qualify a program name with the source key")

(defvar laml-style-documentation nil 
  "If true, write LAML style documentation directly in the setup buffer of the Scheme Elucidator.
If true, the textual documentation format is not used. Use M-x use-laml-style-documentation to enable LAML style documentation.")

(defvar program-selection-method 'by-name 
  "Determines the way to select a program name in a program buffer.  A symbol.
If by-form, the name of the scheme definition form under point is returned (locate the surrounding definition and return its name).
If by-name, the the name under the point is returned.
By using by-form, it is necessary to locate the definition to select a name.
By using by-name, you can chose an arbitrary occurence of the name.")
   


; Directory functions:

(defun in-internal-directory (file-name)
  "Return the full path to a file in the internal directory"
  (concat elucidator-directory "internal/" file-name))


; ---------------------------------------------------------------------------------------------------------------
; Minor elucidator mode:

(defvar elucidator-mode nil 
  "The initial value of elucidator mode")

(defvar elucidator-screen-indication " Elucidator" 
 "The string in the mode line that indicates elucidator minor mode")
    
(defvar original-minor-mode-alist minor-mode-alist
  "The value of the variable minor-mode-alist at the time the elucidator minor mode is loaded")

(setq Ctl-E-keymap (make-keymap))	; allocate elucidator, Ctl-E keymap table
(global-set-key (if (eq laml-keybinding-mode 'original) "" "") Ctl-E-keymap)
(define-key Ctl-E-keymap "" 'end-of-line) ; make C-E C-E equal old C-E


; (setq elucidator-keymap (make-sparse-keymap))	
; (global-set-key "" elucidator-keymap)
; (laml-define-key elucidator-keymap "C-e" 'end-of-line (eq laml-keybinding-mode 'original)) ; make C-E C-E equal old C-E


(defun elucidator-mode (arg)  ;; Inspiration: abbrev-mode
  "Toggle elucidator-mode, a minor mode.
With arg, turn elucidator-mode on if arg is positive.  In elucidator-mode some
characters on the keyboard are redefined, to make it simpler to activate
the special elucidator commands."
  (interactive "P")
  (if t ; appropriate major mode
      (progn (make-local-variable 'elucidator-mode)
             (setq elucidator-mode
                   (if (null arg) (not elucidator-mode)
	                          (> (prefix-numeric-value arg) 0)))

             (make-local-variable 'minor-mode-alist)
             (setq minor-mode-alist (cons (list 'elucidator-mode elucidator-screen-indication)
                               original-minor-mode-alist))

             ; define key bindings here.
             (laml-define-key Ctl-E-keymap "\C-p" 'elucidator-prog-ref)
             (laml-define-key Ctl-E-keymap "p"  'elucidator-show-program)
             (laml-define-key Ctl-E-keymap "\C-d" 'elucidator-doc-ref)
             (laml-define-key Ctl-E-keymap "\C-h" 'elucidator-goto)  ; C-g is used to quit a command
             (laml-define-key Ctl-E-keymap "\C-g" 'goto-repeated)  
             (laml-define-key Ctl-E-keymap "g"  'elucidator-goto)  ; an mnemonic alternative to C-E C-H
             (laml-define-key Ctl-E-keymap "\C-b" 'elucidator-back)
             (laml-define-key Ctl-E-keymap "r"  'elucidator-refresh-elucidator)
             (laml-define-key Ctl-E-keymap "\C-r" 'elucidator-reset-elucidator)
             (laml-define-key Ctl-E-keymap "\C-o" 'elucidator-elucidate)
             (laml-define-key Ctl-E-keymap "\C-u" 'elucidator-show-setup)

             (laml-define-key Ctl-E-keymap "\C-l" 'elucidator-large-documentation-window)
             (laml-define-key Ctl-E-keymap "\C-s" 'elucidator-large-program-window)

             (laml-define-key Ctl-E-keymap "\C-x" 'elucidator-insert-documentation-section)
             (laml-define-key Ctl-E-keymap "\C-c" 'elucidator-insert-documentation-entry)

             (laml-define-key Ctl-E-keymap "\C-m" 'elucidator-show-processing)

             (laml-define-key Ctl-E-keymap "?" 'elucidator-help)

;              (laml-define-key Ctl-E-keymap [menu-bar]
;                 (cons "Elucidator" (make-sparse-keymap "Elucidator")))
; 
;              (laml-define-key Ctl-E-keymap [menu-bar elucidator]
;                '("insert sectional comment" . insert-sectional-comment))

             
             (set-buffer-modified-p (buffer-modified-p)))
                                             ;No-op, but updates mode line.
      (progn (setq elucidator-mode nil)
             (beep)
             (message "Elucidator mode can only be used in the current major mode"))))


; wrapped elucidator commands
(defun elucidator-prog-ref ()
  "Calls the elucidator command prog-ref"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'prog-ref)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-show-program ()
  "Calls the elucidator command show-program"
  (interactive)
  (if elucidator-buffer-info ; if in an eludciator buffer
      (call-interactively 'show-program)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-doc-ref ()
  "Calls the elucidator command doc-ref"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'doc-ref)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-goto ()
  "Calls the elucidator command goto"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'goto)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-back ()
  "Calls the elucidator command back"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'back)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-refresh-elucidator ()
  "Calls the elucidator command refresh-elucidator"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'refresh-elucidator)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-reset-elucidator ()
  "Calls the elucidator command reset-elucidator"
  (interactive)
  (if t  ; always possible
      (call-interactively 'reset-elucidator)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-elucidate ()
  "Calls the elucidator command elucidate, which processes the current elucidative program. Before that, save all elucidator buffers."
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'elucidate)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-insert-documentation-section ()
  "Calls an appropriate version of the elucidator command insert-documentation-section"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (if laml-style-documentation
          (progn (call-interactively 'insert-lispstyle-documentation-section) (indent-sexp))
          (call-interactively 'insert-textual-documentation-section))
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun insert-documentation-section ()
  "Insert the a documentation section template"
  (interactive)
  (elucidator-insert-documentation-section))


(defun elucidator-insert-documentation-entry ()
  "Calls an appropriate version of the elucidator command insert-documentation-entry"
  (interactive)
  (if elucidator-buffer-info  ; if in an elucidator buffer
      (if laml-style-documentation
          (progn (call-interactively 'insert-lispstyle-documentation-entry) (indent-sexp))
          (call-interactively 'insert-textual-documentation-entry))
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun insert-documentation-entry ()
  "Insert the a documentation entry template"
  (interactive)
  (elucidator-insert-documentation-entry))


(defun elucidator-show-setup ()
  "Calls the elucidator command show-setup"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'show-setup)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


(defun elucidator-large-documentation-window ()
  "Calls the elucidator command large-documentation-window"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'large-documentation-window)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-large-program-window ()
  "Calls the elucidator command large-program-window"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'large-program-window)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-surround-prog-ref ()
  "Calls the elucidator command surround-prog-ref"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'surround-prog-ref)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))

(defun elucidator-surround-none-ref ()
  "Calls the elucidator command surround-none-ref"
  (interactive)
  (if elucidator-buffer-info  ; if in an eludciator buffer
      (call-interactively 'surround-none-ref)
      (error "You can only use elucidator commands when you are in elucidator mode. Consider M-x elucidator-let-be")))


; ---------------------------------------------------------------------------------------------------------------

(defun source-file-info-ok ()
  "Return whether the first source key location exist in the program-source-list file in the internal directory"
  (if (and (file-exists-p elucidator-directory) (file-exists-p (in-internal-directory "program-source-list")))
    (let ((source-list (file-read (in-internal-directory "program-source-list"))))
      (if (>= (length source-list) 1)
          (let ((first-source-path (source-path-select (car source-list))))
            (file-exists-p first-source-path))
          nil))
    nil))

(defun source-path-select (source-record)
  "Return the source path of a source file record triple"
  (cadr (assq 'file-location source-record)))


; ---------------------------------------------------------------------------------------------------



; Top level interactive commands of the elucidator.

(defun setup-elucidator (laml-file-path)
  "Set up the elucidator on an existing laml setup file, which must be an laml file.
If only a single laml file exists in a directory, you do not have to select it explicitely; it is enough to select the directory.
If f.laml is selected, the textual documentation is assumed to in file f.txt.
Both files are brought up in editor buffers."
 (interactive "fName of Elucidator LAML setup file: ")

 (let* ((path (file-name-directory laml-file-path))
        (fname (file-name-proper (file-name-nondirectory laml-file-path)))
        (e-name (if (equal "" fname) ; file name without path and extension
                    (file-name-proper (lonely-file "laml" path))
                    fname))
       )

   (setq setup-buffer (find-file-noselect (concat path e-name ".laml")))
   (setq laml-style-documentation (is-laml-style-documentation setup-buffer))
   
   (let* ((ext (if (equal "" fname)
               (file-name-extension (lonely-file "laml" path))
               (file-name-extension (file-name-nondirectory laml-file-path))))
          (documentation-path
	   (if laml-style-documentation
	       (concat path e-name ".laml")
	       (concat path e-name ".txt")))
         )

     ; Warn if the setup file is a non-laml file
     (if (not (and e-name (equal "laml" ext)))
         (error "You are supposed to activate the elucidator on a setup file with extension laml"))
   
     ; Bring up laml file, which controls the elucidator, and remember the buffer in 
     ; the variable documentation-buffer
     (setq documentation-buffer (find-file-noselect documentation-path))
   
     ; Define the directory and name of the elucidators in the variables elucidator-directory and elucidator-name
     (setq elucidator-directory (file-name-directory laml-file-path))
     (setq elucidator-name e-name)
   
   
     ; buffer local variable telling whether the buffer shows program or documentation
     (make-variable-buffer-local 'elucidator-buffer-info)  
   
     ; buffer local variable giving the source key string of the current buffer
     (make-variable-buffer-local 'source-key-of-program-buffer)  
   
     ; Referesh from saved information:
     (if (source-file-info-ok)
         (progn
           (refresh-elucidator)
           (message "OK"))
         (progn
           (beep)
           (message "The source files cannot be located. Try process the LAML setup file and then M-x refresh-elucidator")))
)))

(defun is-laml-style-documentation (laml-setup-buffer)
  "Does the laml-setup-buffer call for LAML documentation."
  (save-excursion
   (set-buffer laml-setup-buffer)
   (if (search-forward "(documentation-from" nil t)
       (or (is-inside-string) (is-inside-comment))
       t)))

(defun refresh-elucidator ()
  "Refresh the program knowledge established by the laml processing
part of the elucidator. This affect the cross referencing capabilities
of the editor part, and it (re)defines the source files on which the elucidator
works. The information is taken from the internal directory."
  (interactive)

  (if (and documentation-buffer setup-buffer elucidator-directory elucidator-name)
    (progn
  
    ;; extract the triples information about program source files
    (setq program-source-files (file-read (in-internal-directory "editor-program-source-list")))
  
     ;; and futher on, extract the pure source key list
    (setq source-keys (mapcar (function (lambda (triple) (cadr (assq 'key triple)))) program-source-files))
    (setq source-versions (mapcar (function (lambda (triple) (cadr (assq 'version triple)))) program-source-files))
  
    ;; read the defined names in all source files, and append reduce it into one list
    (let ((name-files (mapcar2 
                        (function (lambda (key vers) (in-internal-directory (concat key "-" (number-to-string vers) ".names"))))
                        source-keys source-versions)))
      (setq defined-names
         (mapcar (function entry-as-pair) (accumulate-right (function append) nil (mapcar (function file-read) name-files))))
      (setq defined-name-strings   (mapcar (function (lambda (pair) (list (car pair)))) defined-names))
    )
  
    (setq documentation-ids (file-read (in-internal-directory "documentation-ids")))
    (setq documentation-id-strings (mapcar (function (lambda (id) (list (symbol-to-string id)))) documentation-ids))
  
    (setq documented-name-occurences (file-read (in-internal-directory "documented-names")))
  
    ;; bring up all source files in buffers
    ;; be careful not to destroy existing buffers.
    ;; create a mapping from source-key to buffer  
    (let* ((source-files (mapcar (function (lambda (triple) (cadr (assq 'file-location triple)))) program-source-files))
           (source-buffers (mapcar (function find-file-noselect) source-files)))
      (setq source-key-buffer-map 
         (mapcar2 (function (lambda (source-key buffer) (cons source-key buffer))) source-keys source-buffers))
      )
  
    ; mark the documentation buffer as such: 
    ; the value of the buffer local variable in the documentation buffer will by 'documentation
    (save-excursion
      (set-buffer documentation-buffer)
      (if (eq documentation-buffer setup-buffer) (laml-mode) (text-mode)) ; only set HTML mode when textual documentation is used.
      (modify-syntax-entries-according-to-scheme)
      (elucidator-mode 1)
      (setq elucidator-buffer-info 'documentation))
  
    ; mark the setup buffer as such: 
    ; the value of the buffer local variable in the documentation buffer will by 'setup
    (save-excursion
      (set-buffer setup-buffer)
      (setq elucidator-buffer-info 'setup))
  
    ; mark the program buffers as such
    ; the value of the buffer local variable in the program buffers will by 'program
    (save-excursion
      (mapcar 
        (function 
          (lambda (sourcekey-buffer-pair)
             (set-buffer (cdr sourcekey-buffer-pair))
             (modify-syntax-entries-according-to-scheme)
             (elucidator-mode 1)
             (setq elucidator-buffer-info 'program)
             (setq source-key-of-program-buffer (car sourcekey-buffer-pair))
          )
        )
        source-key-buffer-map))
  
    ; Set the current-program-buffer to the first source program:
    (setq current-program-buffer (cdr (car source-key-buffer-map)))
  
    (reset-elucidator)
    (message "Refreshing done"))
  (progn
    (beep)
    (message "The elucidator has not been setup yet. Use M-x setup-elucidator"))))

; Convert an entry in defined names to a pair, hereby disregarding version information
(defun entry-as-pair (def-name-entry)
 (let ((def-name (symbol-to-string (car def-name-entry)))
       (source-key (cadr def-name-entry)))
   (cons def-name source-key)))


(defun modify-syntax-entries-according-to-scheme ()
  "Modify the current buffers syntax tables, such that question and exclamation marks
become word constituents"
  (modify-syntax-entry ?? "word")
  (modify-syntax-entry ?! "word")
  (modify-syntax-entry ?- "word")
)



(defun reset-elucidator ()
  "Reestablish a situation where the documentation and program is shown in a two window
   configuration, the documentation above the program"
  (interactive)
   (if (and documentation-buffer setup-buffer elucidator-directory elucidator-name program-source-files)
    (progn
      (setq go-back-stack nil)
      (delete-other-windows)
      (split-window)
      (show-buffer (selected-window) documentation-buffer)
      (setq documentation-window (selected-window))
      (other-window 1)
      (show-buffer (selected-window) current-program-buffer)
      (setq program-window (selected-window))
      (other-window 1)
      (setq last-doc-word nil))
    (progn
      (beep)
      (message "The elucidator has not yet been setup or refreshed properly. Use M-x setup-elucidator or M-x refresh to do so."))))

(defun elucidator-status-programs ()
  (mapcar
    (function
      (lambda (sk)
        (insert (concat "  " (buffer-name (cdr sk))))))
    source-key-buffer-map))

(defun elucidator-status ()
  "Show information about the current elucidator in a help window"
  (interactive)
   (if (and documentation-buffer setup-buffer elucidator-directory elucidator-name program-source-files)
       (let ((help-buffer (if (get-buffer "*Elucidator-status*") (get-buffer "*Elucidator-status*") (generate-new-buffer "*Elucidator-status*"))))
                (show-buffer (other-window 1) help-buffer)
                (set-buffer help-buffer)
                (erase-buffer)
                (insert (concat "Setup buffer: " (buffer-name setup-buffer))) (insert CR)
                (insert (concat "documentation buffer: " (buffer-name documentation-buffer))) (insert CR)
                (insert "Program buffers: ")  (elucidator-status-programs) (insert CR) (insert CR)
                (insert "To return to elucidator:  M-x reset-elucidator")
       )
                       
       (progn
         (beep)
         (message "There is no current setup of an elucidator"))))

(defun quit-elucidator ()
  "Kill the elucidator buffers"
  (interactive)
  (kill-buffer (buffer-name documentation-buffer))
  (kill-buffer (buffer-name setup-buffer))
  (mapcar 
   (function
    (lambda (b) (kill-buffer (buffer-name b))))
   (mapcar (function cdr) source-key-buffer-map))

  (setq documentation-buffer nil)
  (setq source-keys '())
  (setq source-key-buffer-map '())
  (setq setup-buffer nil)
  (setq elucidator-directory nil)
  (setq elucidator-name nil)
  (setq documentation-window nil)
  (setq program-window nil)
  (setq current-program-buffer nil)
  (setq defined-names '())
  (setq defined-name-strings '())
  (setq documentation-ids '())
  (setq documented-name-occurences '())
  (setq documentation-id-strings '())
  (setq last-doc-word nil)
  (setq go-back-stack nil)
)

(defun make-elucidator (dir0 elucidator-file-name kind-n source-file-path0)
  "Organize an elucidator in the directory dir0, which is assumed to be
a \"doc\" directory"
  (interactive "DMake Elucidator 2 in which directory: 
sName of this elucidator (plain name without extension): 
nKind of documentation: (1:xml-in-laml or 2:textual). Type either 1 or 2: 
FName of first source file: ")
  (let ((dir (ensure-trailing-slash (expand-file-name dir0)))
        (source-file-path (expand-file-name source-file-path0))
        (elucidator-kind (cond ((= kind-n 1) 'xml-in-laml-documentation)
                               ((= kind-n 2) 'textual-documentation)
                               (t (error "Type either 1 or 2 to give the kind of documentation. Try active M-x make-elucidator again."))))
       )
   (setq laml-style-documentation (if (eq elucidator-kind 'xml-in-laml-documentation) t nil))

   (if dir
       (if (or (file-exists-p (concat dir "html")) (file-exists-p (concat dir "internal")))
          (progn 
            (beep) 
            (message "An elucidator already seems to be set up in this directory. Nothing done!"))
          (progn
            (make-directory (concat dir "html/"))
            (make-directory (concat dir "html/images/"))
            (make-directory (concat dir "internal/"))
            (if (not (file-exists-p source-file-path))
                (progn
                  (message "Non-existing source file, making an empty one")
                  (make-empty-file source-file-path)
                ))
            (let* ((proper-source-file-name (file-name-proper (file-name-nondirectory source-file-path)))
                   (source-key proper-source-file-name)
                   (setup-buf nil))

              ; Make the setup file
              (setq setup-buf
                    (make-a-file-from-laml-template 
                     (concat elucidator-file-name ".laml")
                     dir
                     "elucidator-2-setup"
                     'laml-mode
                     (list
                      (list "SOURCE-KEY" source-key)
                      (list "SOURCE-PATH" source-file-path)
              )))

              ; Make an initial documentation file:
;               (make-a-file-from-laml-template 
;                (concat elucidator-file-name ".txt")
;                dir
;                "elucidator-documentation"
;                'text-mode
;                '())

              (show-buffer (selected-window) setup-buf)

              (message (concat "Done. Now process the laml setup file with " (if (eq laml-keybinding-mode 'original) "C-o" "C-c C-o") ". Next do M-x setup-elucidator."))

              )))

    (progn
      (beep)
      (message "Cannot determine the current directory. Sorry...")))))


(defun make-empty-file (file-path)
  "Make an empty file with file-path and bring it up in a buffer in Emacs. Return the buffer."
  (let ((new-buffer (create-file-buffer file-path)))
    (set-buffer new-buffer)
    (write-file (concat file-path))
    new-buffer))

; make-a-file-from-laml-template and dependents are now in the emacs-support/laml-template.el file of the LAML system

; ---------------------------------------------------------------------------------------------------

; Navigation:

; Construct and entry to be pushed onto the go back stack
(defun make-go-back-info (buf pt inf)
  (list buf pt inf))

; The go back stack - just a list
(defvar go-back-stack nil)

(defvar max-go-back-stack 100)

; Push entry on the go back stack
(defun push-go-back-info (entry)
  (if (> (length go-back-stack) max-go-back-stack)
      (progn
         (beep)
         (message "The stack of go back information is very long. Consider M-x reset-goback-stack or C-e C-r")))
  (setq go-back-stack (cons entry go-back-stack)))

; Pop the go back stack
(defun pop-go-back-stack ()
  (if (not (null go-back-stack))
      (setq go-back-stack (cdr go-back-stack))
      (error "pop-go-back-stack: Trying to pop empty stack")))

; Return top of stack, or nil if stack is emtpy. Do not pop.
(defun top-of-go-back-stack ()
  (if (not (null go-back-stack))
      (car go-back-stack)
      nil))

(defun reset-goback-stack ()
  (interactive)
  (setq go-back-stack nil))


; Return the substring of word in which initial stars etc are taken away
(defun disregard-possible-initial-word-modifier (word)
  (if (initial-word-modifier-p (substring word 0 1))
      (substring word 1 (length word))
      word))

(defun initial-word-modifier-p (unit-string)
  (or (equal unit-string p-link-strong-rel-char)
      (equal unit-string p-link-weak-rel-char)
      (equal unit-string p-link-none-rel-char)))

(defvar last-doc-word nil "The last documentation word not located exactly. Can be assigned by the goto command.")

(defun goto ()
  "Goto the name under point. Determine from the context which kind of goto to apply.
Can be applied both from the documentation buffer and from program buffers."
  (interactive)

  (let ((qualification (qualification-of-selection))
        (word (name-of-selection))
       )

   (if (or qualification word) (setq last-doc-word nil))

   (push-mark (point) t)

   ; Set go back information
   (push-go-back-info (make-go-back-info (current-buffer) (point) elucidator-buffer-info))

   (setq go-back-buffer (current-buffer))
   (setq go-back-pos    (point))
   (setq go-back-kind   elucidator-buffer-info)

   (cond (last-doc-word                                             ; For precise location of a program reference within the documentation
             (progn

               (goto-message "A")
               (search-forward-reference-form 'prog-ref last-doc-word nil nil)     ; not using qualification
               (recenter 0)
               (setq last-doc-word nil)))

         ((and (in-program-buffer-p) (known-program-name word) (definition-context-p))               ; Program to documentation navigation
             (goto-message "B")
             (let* ((doc-words (documentation-units-from-program-name word))
                    (doc-words-unique (remove-duplicates doc-words))
                   )

              (setq dwu doc-words-unique) (setq www word) (setq qqq qualification)
              
              ; administration of repeated navigation
              (setq doc-word-list-repeated-navigation doc-words-unique)
              (setq doc-word-repeated-navigation word)
              (setq position-repeated-navigation (- (length doc-words-unique) 1))

              (if (> position-repeated-navigation 0)                 ; there are two or more documentation places where word is documented 
                  (message (concat "Use  M-x goto-repeated  to locate " 
                                   (number-to-string position-repeated-navigation)
                                   " other documentations of " word)))

              (if (not (null doc-words-unique))
               (progn
                (goto-documentation-definition (car doc-words-unique)) 
                (if locate-documentation-exactly
                 (progn
                    (search-forward-reference-form 'prog-ref word qualification (bound-of-current-documentation-section-or-entry))
                    (recenter 0)
                    (setq last-doc-word nil))
                 (progn
                    (setq last-doc-word word)
                    (beep)
                    (message (concat "Next goto will locate the exact place of " last-doc-word)))))
               (progn
                  (beep)
                  (message (concat word " is not documented."))))))

         ((and (in-program-buffer-p) (known-program-name word))                         ; Applied to defined name occurence in the program
             (goto-message "C")
             (goto-program-definition qualification word))
         ((and (in-documentation-buffer-p)                                                        ; Documentation to program file location
               (is-program-link-context)
               (is-empty word) (not (is-empty qualification)))
             (goto-message "D")
             (goto-program qualification))
         ((and (in-documentation-buffer-p) (is-program-link-context) (known-program-name word))                 ; Documentation to program 
             (goto-message "E")
             (goto-program-definition qualification word))
         ((and (in-documentation-buffer-p)                                                                ; Documentation to documentation
               (is-documentation-link-context)
               (known-documentation-name word))
             (goto-message "F")
             (goto-documentation-definition word))
         ((and (in-documentation-buffer-p) (known-program-name word))                                     ; ??? - drop
              (goto-message "G") (error "goto: Case G!")
              (goto-program-definition qualification word))
         ((and (in-documentation-buffer-p) (known-documentation-name word))                               ; ??? - drop
             (goto-message "H") (error "goto: Case H!")
             (goto-documentation-definition word))
         ((and (in-documentation-buffer-p) (is-program-link-context))
             (goto-message "I")
             (goto-program-definition-in-user-selected-buffer word))
         (t (progn (beep) (message (concat "Cannot goto " word)))))
))

(defvar mmm nil)

(defun goto-message (m)
  (setq mmm m))

(defun goto-repeated ()
  "Cycle through the places in the documentation where a given program word is described"
  (interactive)
  (cond ((= position-repeated-navigation -1) (progn (beep) (message (concat "No repeated goto possible " word))))
        ((= position-repeated-navigation (- (length doc-word-list-repeated-navigation) 1))
           (if (= position-repeated-navigation 0)
               (message "Now repeating the visiting of documentation places..."))
           (let ((section-entry-id (nth position-repeated-navigation doc-word-list-repeated-navigation)))
              (goto-documentation-definition section-entry-id)
              (search-forward-reference-form 'prog-ref doc-word-repeated-navigation nil nil)
              (recenter 0))
           (setq position-repeated-navigation 0))
        ((< position-repeated-navigation (- (length doc-word-list-repeated-navigation) 1))
           (if (= position-repeated-navigation 0)
               (message "Now repeating the visiting of documentation places..."))
           (let ((section-entry-id (nth position-repeated-navigation doc-word-list-repeated-navigation)))
              (goto-documentation-definition section-entry-id)
              (search-forward-reference-form 'prog-ref doc-word-repeated-navigation nil nil)
              (recenter 0))           
           (setq position-repeated-navigation (+ position-repeated-navigation 1)))))


(defun back ()
  "Go back to the place located before the previous goto"
  (interactive)

  (let ((top  (top-of-go-back-stack)))
   (if top
  
  ; locate window
     (let  ((buf (car top))
	    (pt (cadr top))
	    (go-back-kind (caddr top))) 

       (cond ((and (in-program-buffer-p) (eq go-back-kind 'documentation)) (other-window 1))
	     ((and (in-documentation-buffer-p) (eq go-back-kind 'program)) (other-window 1)))

       (set-buffer (car top))
       (show-buffer (selected-window) (car top))
       (goto-char (cadr top))

       (pop-go-back-stack))
     (error "No more go back possibilities"))))

(defun in-program-buffer-p ()
  (eq elucidator-buffer-info 'program))

(defun in-documentation-buffer-p ()
  (if laml-style-documentation
      (eq elucidator-buffer-info 'setup)
      (eq elucidator-buffer-info 'documentation)))


(defun known-program-name (name)
  (assoc name defined-names))

(defun known-documentation-name (name)
  (assoc name documentation-id-strings))

(defun program-link-context-p (c1 c2)
  (and (equal c1 p-link-prefix-char) (equal c2 p-link-suffix-char)))

(defun documentation-link-context-p (c1 c2)
  (and (equal c1 d-link-prefix-char) (equal c2 d-link-suffix-char)))


(defun goto-program-definition (qualification name)
  "Goto the definition of NAME in one of the program files. 
If QUALIFICATION is a non-empty string it determines the program buffer. 
This function may change the program window to show another program buffer.
Assume that this function is called in situation where the upper window is documention, and the lower is program"
  (interactive 
     (list (completing-read "Locate program identifier: " 
                   defined-name-strings nil nil)))
    (let* ((name-sourckey-pair (assoc name defined-names))
           (source-key 
             (if (and (not (equal "" qualification)) (member qualification source-keys))
                 qualification
                 (cdr name-sourckey-pair)))
           (prog-buffer (cdr (assoc source-key source-key-buffer-map)))
          )

      (if (and (not (equal "" qualification)) (not (member qualification source-keys)))
          (progn  ; warn if qualification is not a known source key
            (beep)
            (message (concat "Unknown qualification: " qualification ". Using " (cdr name-sourckey-pair)))))

      (cond ((in-program-buffer-p) ; current buffer is program buffer
                (set-buffer prog-buffer)
                (goto-char (point-min))
                (show-buffer (selected-window) prog-buffer)
                (goto-program-place-in-current-buffer 'scheme name))
            ((in-documentation-buffer-p) ; current buffer is documentation buffer
                (other-window 1)
                (set-buffer prog-buffer)
                (goto-char (point-min))
                (show-buffer (selected-window) prog-buffer)
                (goto-program-place-in-current-buffer 'scheme name))
            (t (message "You must be located in an elucidator documentation or program buffer to use this function")))
      (recenter 0)
      (setq last-doc-word nil) ))

(defun goto-program-definition-in-user-selected-buffer (name)
  (let* ((source-key  (completing-read (concat "Look for " name " in which program: ") source-key-buffer-map nil t)) ; prompt
         (prog-buffer (cdr (assoc source-key source-key-buffer-map))))
      (cond ((in-program-buffer-p) ; current buffer is program buffer
                (set-buffer prog-buffer)
                (goto-char (point-min))
                (show-buffer (selected-window) prog-buffer)
                (goto-program-place-in-current-buffer 'scheme name))
            ((in-documentation-buffer-p) ; current buffer is documentation buffer
                (other-window 1)
                (set-buffer prog-buffer)
                (goto-char (point-min))
                (show-buffer (selected-window) prog-buffer)
                (goto-program-place-in-current-buffer 'scheme name))
            (t (message "You must be located in an elucidator documentation or program buffer to use this function")))
      (recenter 0)))



(defun goto-documentation-definition (name)
  "Goto a documentation entry/section in the documentation file.
Assume that this function is called in situation where the upper window is documention, and the lower is program"
  (interactive 
     (list (completing-read "Locate documentation identifier: " 
                   documentation-id-strings nil nil)))
      (cond ((in-program-buffer-p) ; current buffer is program buffer
                (other-window 1)
                (set-buffer documentation-buffer)
                (goto-char (point-min))
                (goto-documentation-place-in-current-buffer name))
            ((in-documentation-buffer-p) ; current buffer is documentation buffer
                (goto-char (point-min))
                (goto-documentation-place-in-current-buffer name))
            (t (message "You must be located in an elucidator documentation or program buffer to use this function")))
      (recenter 0) 
      (setq last-doc-word nil))


; ---------------------------------------------------------------------------------------------------
; Insertion of program references and documentation references into the documentation.
; Secured by completion of name based on the last refresh from the LAML (Schme) processing.



(defun prog-ref ()
  "Insert a program reference in the documentation text and leave the point in the documentation buffer.
   If necessary, qualify the program reference with a source key.
   The variable always-qualify-program-names can force a qualification, even if it is not strictly necessary.
   The variable program-selection-method affects the way program names are selected in program buffers.
   The variable laml-style-documentation controls the kind of references to be inserted in the documentation buffer."
  (interactive)
  (cond ((in-program-buffer-p)
           (let* ((prog-name 
                     (cond ((eq program-selection-method 'by-name) (prog-name-under-point-strict))
                           ((eq program-selection-method 'by-form) (name-of-surrounding-scheme-form))
                           (t (error "program-selection-method must be one of the symbols by-name or by-form"))))
                  (possible-source-keys (if prog-name (find-source-keys-of-name prog-name) '()))
                  (qualification source-key-of-program-buffer)
                 )
              (other-window 1)

              (if laml-style-documentation
                  (progn (laml-style-insert-program-ref prog-name possible-source-keys qualification)
                         (backward-sexp 1) ; now at the start-parenthesis of the program reference form
                         (normalize-spaces-around-form 1)
                  )
                  (text-style-insert-program-ref prog-name possible-source-keys qualification))
              
;              (other-window 1)
              (if (not laml-style-documentation) (forward-char 1)) ; at * when using textual documentation
            )
          (message "Redo command to toggle between strong and weak program references")
        )

        ((and (in-documentation-buffer-p)   
              laml-style-documentation 
              (or (looking-at "(strong-prog-ref") (looking-at "(weak-prog-ref")))     ; toggle strong to weak and weak to strong
           (toggle-program-reference))

        ((in-documentation-buffer-p)
            (let* ((ref (completing-read "Program reference: " 
                                         defined-name-strings nil nil))
                   (possible-source-keys (find-source-keys-of-name ref))
                   (lgt-qualifications (length possible-source-keys))
                   (qualification (cond ((= lgt-qualifications 1) (cdr (car possible-source-keys)))
                                        ((> lgt-qualifications 1)
                                         (completing-read 
                                          "Source key qualification: "
                                          (trivial-alist (mapcar (function cdr) possible-source-keys))
                                          nil t))
                                        ((is-empty ref) 
                                         (completing-read 
                                          "Source key qualification (without particular program reference): "
                                          (trivial-alist source-keys)
                                          nil nil))
                                        (t nil)))
                   )
              (if laml-style-documentation
                  (progn 
                    (laml-style-insert-program-ref ref possible-source-keys qualification)
                    (backward-sexp 1) ; now at the start-parenthesis of the program reference form
                    (normalize-spaces-around-form 1))
                  (text-style-insert-program-ref ref possible-source-keys qualification))
              )
            (message "Redo command to toggle between strong and weak program references")
           )
       (t (message "You must be located in an elucidator documentation or program buffer to use this function"))))


(defun toggle-program-reference ()
  "Assume as a precondition that point is located a the start parenthesis just before a strong-prog-ref or a weak-prog-ref.
Toggle strong to weak or weak to strong."
  (forward-char 1)
  (cond ((looking-at "strong") (progn (delete-char 6) (insert "weak")))
        ((looking-at "weak") (progn (delete-char 4) (insert "strong")))
        (t (error "toggle-program-reference: toggle only works on strong or weak program references.")))
  (backward-up-list 1))

; Precondition: Current buffer is the documentation buffer.
(defun laml-style-insert-program-ref (prog-name possible-source-keys qualification)
 (let ((inside-string (is-inside-string)))
  (if inside-string 
      (progn 
         (if (equal (char-under (point)) "\"")   ; due to is-inside-string: end string delimiter
             (forward-char 1)
             (progn (split) (backward-char 1)))))
  (if (and (not always-qualify-program-names) (= 1 (length possible-source-keys)))
      (insert (make-strong-program-reference nil prog-name))
      (insert (make-strong-program-reference qualification prog-name))
  )
 )
)

; Precondition: Current buffer is the documentation buffer.
(defun laml-style-insert-source-marker (marker-char)
 (let ((inside-string (is-inside-string)))
  (if inside-string 
      (progn 
         (if (equal (char-under (point)) "\"")   ; due to is-inside-string: end string delimiter
             (forward-char 1)
             (progn (split) (backward-char 1)))))
  (insert (make-source-marker marker-char))
 )
)

(defun make-source-marker (char)
 (let ((SP " "))
  (concat "(source-marker" SP "'name" SP (string-it char) ")")))  

; Precondition: Current buffer is the documentation buffer.
(defun laml-style-insert-documentation-ref (doc-name)
 (let ((inside-string (is-inside-string)))
  (if inside-string 
      (progn 
         (if (equal (char-under (point)) "\"")   ; due to is-inside-string: end string delimiter
             (forward-char 1)
             (progn (split) (backward-char 1)))))
  (insert (make-documentation-reference doc-name))
 )
)

(defun make-documentation-reference (doc-name)
 (let ((SP " "))
  (concat "(doc-ref" SP "'name" SP (string-it doc-name) ")")))  


(defun make-strong-program-reference (qual pname)
 (let ((SP " "))
  (concat "(strong-prog-ref" SP
          (if qual
             (concat "'file" SP (string-it qualification) SP)
             "")
          (if pname 
             (concat "'name" SP (string-it prog-name))
             "")
           ")")))

; Precondition: Current buffer is the documentation buffer.
(defun text-style-insert-program-ref (prog-name possible-source-keys qualification)
 (let ((start (point))
       (the-prefix (concat p-link-prefix-char p-link-weak-rel-char))
       )
   (put-in the-prefix p-link-suffix-char)
   (if (and (not always-qualify-program-names) (= 1 (length possible-source-keys)))
       (insert prog-name)
       (insert (concat qualification qualification-char prog-name)))
   (forward-char 1)
   (copy-to-register ?r start (point) nil)
   (search-backward the-prefix nil t) (forward-char 1) ; cursor (point) now at *
   ))


(defun prog-name-under-point ()
  "Return the word under point and disregard sectional comment desinators (double colons) if necessary.
Is also able to return list structure strings."
  (let* ((wd (sexp-delimitor))
	 (first (car wd))
	 (last (+ 1 (cadr wd)))
	 (word (buffer-substring-no-properties first last)))
    (disregard-sectional-comment-chars word)))

(defun prog-name-under-point-strict ()
  "As prog-name-under-point, but can only return a proper name. 
In case of problems, return nil."
  (cond ((looking-at-white-space (point)) nil)
        ((or (looking-at "(") (looking-at ")")) nil)
        (t (prog-name-under-point))))
   

(defun sexp-under-point ()
  "Return the word under point."
  (let* ((wd (sexp-delimitor))
	 (first (car wd))
	 (last (+ 1 (cadr wd))))
    (buffer-substring-no-properties first last)))

(defun name-of-surrounding-scheme-form ()
  "Return the name of the surrounding scheme form.
If it is definition (top-level or local) return its name.
If it is a top-level form with an initial sectional comment, return the name of the sectional comment.
Else return nil."
  (save-excursion
    (if (is-inside-string) 
        (progn
           (backward-char 1)
           (while (not (looking-at "\"")) (backward-char))))   ; goto beginning of string
    (while (not (or (looking-at-top-level-form) (looking-at "(define"))) (backward-up-list 1))  ; goto surrounding define form
    (cond ((looking-at "(define")
            (forward-char 1)
            (forward-sexp 1)                                           ; proceed through define
            (while (looking-at-white-space (point)) (forward-char 1))  ; proceed through white space after define
            (if (looking-at "(")
                (progn
                  (forward-char 1) (prog-name-under-point))
                (prog-name-under-point))
           )
          ((is-there-initial-comment-p)
            (let ((comment-end-point (point)))
             (while (forward-comment -1) 'do-nothing)    ; proceed backward through comment lines
             (search-forward ";" nil t)                  ; now at start of comment
             (let ((sectional-comment (locate-sectional-comment-name)))
               sectional-comment)))
         (t nil))))

; Not used. Instead we use name-of-surrounding-scheme-form.
(defun  name-of-surrounding-scheme-definition ()
  "Assume as a precondition that point is located in a Scheme define form.  Return the name of the define form"   
  (save-excursion
    (if (is-inside-string) 
        (progn
           (backward-char 1)
           (while (not (looking-at "\"")) (backward-char))))   ; goto beginning of string
    (while (not (looking-at "(define")) (backward-up-list 1))  ; goto surrounding define form
    (forward-char 1)
    (forward-sexp 1)                                           ; proceed through define
    (while (looking-at-white-space (point)) (forward-char 1))  ; proceed through white space after define
    (if (looking-at "(")
        (progn
          (forward-char 1) (prog-name-under-point))
        (prog-name-under-point))
  )
)

(defun strong-relation (name)
 (concat p-link-prefix-char p-link-strong-rel-char name p-link-suffix-char))

(defun weak-relation (name)
 (concat p-link-prefix-char  name p-link-suffix-char))

(defun surround-basic-prog-ref (kind)
  "Surround the current word with a program reference. Put the current word into register r for convenience.
Kind is a one-letter string to use as 'modifier' (*, -, +)"
  (interactive)
  (let* ((wd (sexp-delimitor))
         (first (car wd))
         (last (+ 1 (cadr wd)))
         (raw-word (buffer-substring-no-properties first last))
         (current-word (disregard-possible-initial-word-modifier raw-word)))
    (set-register ?r (concat p-link-prefix-char kind current-word p-link-suffix-char))
    (put-around-word 
       (concat p-link-prefix-char kind)
       p-link-suffix-char
       t)
    (search-forward kind nil t) (backward-char 1) ; now at *
))

(defun surround-prog-ref ()
  "Surround the current word with a weak program reference. Put the current word into register r for convenience."
  (interactive)
  (surround-basic-prog-ref p-link-weak-rel-char))

(defun surround-none-ref ()
  "Surround the current word with a non-link program reference. Put the current word into register r for convenience."
  (interactive)
  (surround-basic-prog-ref p-link-none-rel-char))

(defun doc-ref ()
  "Insert a documentation reference in the documentation text"
  (interactive)
  (let ((ref (completing-read "Documentation reference: " 
                                     documentation-id-strings nil nil)))
    (cond 
      ((in-program-buffer-p)
         (error "You cannot insert a documentation reference in a program buffer"))
      ((in-documentation-buffer-p)
    
         (if laml-style-documentation 
             (progn 
                (laml-style-insert-documentation-ref ref)
                (backward-sexp 1) ; now at the start-parenthesis of the documentation reference form
                (normalize-spaces-around-form 1)
               )
             (let ((start (point)))
               (put-in d-link-prefix-char d-link-suffix-char)
               (insert ref)
               (forward-char 1)
               (copy-to-register ?r start (point) nil)
               (message 
                (concat "The reference "
                        (buffer-substring-no-properties start (point))
                        " has been copied to register r.")))))
      (t (message "You must be located in an elucidator documentation or program buffer to use this function")))))

(defun source-marker (marker-char)
  "Insert a source marker form in a documentation buffer"
  (interactive "sSource marker char (a single letter): ")
  (cond 
      ((in-program-buffer-p)
         (error "You cannot insert a documentation reference in a program buffer"))
      ((in-documentation-buffer-p)
         (if laml-style-documentation 
             (progn 
                (laml-style-insert-source-marker marker-char)
                (backward-sexp 1) ; now at the start-parenthesis of the documentation reference form
                (normalize-spaces-around-form 1)
               )
             (progn
                (insert (concat source-marker-char marker))  ; marker??
             )))

      (t (message "You must be located in an elucidator documentation or program buffer to use this function"))))

; ---------------------------------------------------------------------------------------------------
; Generic navigation functions (goto...)

(defun goto-program-place-in-current-buffer (language name)
  "Goto the definition of name in the current buffer. Language is a symbol telling which programming language we work in.
Assume point is at the beginning of the buffer"
  (cond ((eq language 'scheme) (goto-program-place-in-scheme-program name))
        (t (error "goto-program-place-in-current-buffer: Unknown programming language"))))


(defun goto-documentation-place-in-current-buffer (name)
  "Goto a documentation section or entry with id NAME in the current buffer, which must be a documentation buffer. 
Supports both textual and laml style documentation.
Assume point is at the beginning of the documentation buffer.
If found, located point just before the documentation section or entry. "
  (if laml-style-documentation
      (progn

        ; get to point where the elucidator front matters form start:
        (if (search-forward "(elucidator-front-matters" nil t)
            (search-backward "(elucidator-front-matters" nil t)
            (error "goto-documentation-place-in-current-buffer: Cannot locate elucidator-front-matters form. Confused!"))

        (forward-sexp-robust 1) (skip-white-space-and-comments)

        ; Traverse top-level forms and look for appropriate documentation sections and entries:
        (let ((found nil)
              (done nil))
          (while (not (or found done))
            (if (or (looking-at "(documentation-section") (looking-at "(documentation-entry"))
                (let* ((delimitors (sexp-delimitor))
                       (attributes (get-attributes-by-delimitors (car delimitors) (cadr delimitors))))
                  (setq found (match-attribute "id" attributes name))
                  (if (not found)
                      (progn  (setq done (not (forward-sexp-robust 1)))
                              (skip-white-space))))
                (progn
                  (setq done (not (forward-sexp-robust 1)))
                  (skip-white-space))))))
     (let ((res  (search-forward (concat ".SECTION " name) nil t)))
       (if (not res)
           (search-forward (concat ".ENTRY " name) nil t))
       (beginning-of-line 1))))


(defun match-attribute (attr-name attributes-alist value)
  "Does the attribute attr-name (a string) have the value value (a string) in attributes-alist."
  (let ((name-pair (assoc attr-name attributes-alist)))
    (if name-pair
        (equal (cdr name-pair) value) 
        nil)))


;; ---------------------------------------------------------------------------------------------------
;; Elucidator functions for the programming language Scheme

; Scheme dependent goto function:
(defun goto-program-place-in-scheme-program (name)
  "Goto the place where name is Scheme defined in the current buffer"
  (let* (;(WS "[\\s ]*")
         ;(WSP "[\\s ]+")
         (WS "[ \\n\\t]*")
         (WSP "[ \\n\\t]+")
         (res1  (re-search-forward (concat "(" WS "define" WSP "(" name WS) nil t))           ; function definition with more than one par
         (res2  (if res1 res1 (re-search-forward (concat "(" WS "define" WSP "(" WS name WS ")" ) nil t))) ; function def without par
         (res3  (if res2 res2 (re-search-forward (concat "(" WS "define" WSP name WS) nil t)))     ; plain name definition 
         (res4  (if res3 res3 (re-search-forward (concat "(" WS "define" WSP "(" WS name ) nil t)))       ; another function definition 
        ) 
    (if res4 (locate-beginning-of-scheme-definition)
        (let ((res5  (search-forward (concat (concat sectional-comment-char sectional-comment-char) name ) nil t)))
          (if res5 
              (beginning-of-line 1)
              (message (concat "Cannot locate the Scheme define form or sectional commment " name)))))))

(defun locate-beginning-of-scheme-definition ()
  (beginning-of-defun 1)
  (while (previous-line-comment-line-p)
     (previous-line 1)))

(defvar scheme-comment-char-string ";")

(defun previous-line-comment-line-p ()
  (save-excursion
     (beginning-of-line 1) (previous-line 1)
     (equal scheme-comment-char-string (buffer-substring-no-properties (point) (+ 1 (point))))))


(defun show-setup ()
  (interactive)
  (show-buffer (selected-window) setup-buffer))

(defun show-program ()
  "The the programmer select a program from the documentation bundle. This becomes the
current program buffer, which is shown in the editors program (bottom) window. Assumes that
the editor works in splitted documentation/program mode."
  (interactive)
  (let* ((source-key  (completing-read "Show which program: " source-key-buffer-map nil t))
         (buffer (cdr (assoc source-key source-key-buffer-map))))
    (set-buffer documentation-buffer)
    (setq current-program-buffer buffer)
    (show-buffer 
      program-window 
      buffer)))
    

(defun s ()
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (goto-char (point-max)))

(defun elucidate ()
  "Run the elucidator on the setup file of the current documentation bundle. This command can
be activated from an arbitrary buffer. Before processing, save all elucidator buffers."
  (interactive)
  (save-elucidator-buffers)
  (if setup-buffer 
      (progn
        (laml-process-buffer setup-buffer)
         (message "Consider M-x refresh-elucidator when the processing is finished"))
      (message "This command requires that the setup buffer has been defined via setup-elucidator")))

(defun save-elucidator-buffers ()
  "Save all elucidator buffers"
  (interactive)
  (save-particular-buffer setup-buffer)
  (save-particular-buffer documentation-buffer)
  (mapcar 
     (function
        (lambda (sourcekey-buffer-pair) (save-particular-buffer (cdr sourcekey-buffer-pair))))
     source-key-buffer-map))

; move to general
(defun save-particular-buffer (buffer)
  "Save a named buffer. Emacs' save-buffer saves the current buffer"
  (save-excursion
    (set-buffer buffer) (save-buffer)))


(defun large-program-window ()
  "Make the program window large. Assume that the screen is splitted with the documentation buffer as the top buffer"
  (interactive)
  (reset-elucidator)
  (select-window program-window)
  (enlarge-window 10))


(defun large-documentation-window ()
  "Make the documentation window large. Assume that the screen is splitted with the documentation buffer as the top buffer"
  (interactive)
  (reset-elucidator)
  (select-window documentation-window)
  (enlarge-window 10)
)


(defun definition-context-p ()
  (or (within-sectional-comment-word)
      (define-just-before-point)
      (plain-definition-context-p)
      (sugared-definition-context-p)))

(defun within-sectional-comment-word() 
  (save-excursion 
    (while (and (> (point) 1) (not (looking-at-chars (point) (list 9 10 13 32)))) 
           (backward-char 1))
    (forward-char 1)
    (looking-at (concat sectional-comment-char sectional-comment-char))))

(defun plain-definition-context-p ()
  (save-excursion 
    (while (and (> (point) 1) (not (looking-at-chars (point) (list 9 10 13 32 40))))  ; 40 = '('
           (backward-char 1))
    (define-just-before-point)))

(defun sugared-definition-context-p ()
  (save-excursion
    (while (and (> (point) 1) (not (looking-at-chars (point) (list 40 41))))  ; 40 = '('
           (backward-char 1))
    (backward-char 1)
    (define-just-before-point))) 

(defun define-just-before-point()
  (save-excursion
    (while (and (> (point) 1) (looking-at-chars (point) (list 9 10 13 32 ?d ?e ?f ?i ?n)))
           (backward-char 1))
    (looking-at "(def")))
    

(defun documentation-unit-from-program-name (name)
  (let ((doc-units 
          (filter
           (function
            (lambda (triple)
               (and (eq (string-to-symbol name) (car triple)) (eq 'strong (caddr triple)))))
           documented-name-occurences)))
    (cond ((= (length doc-units) 1 ) (symbol-to-string (cadr (car doc-units))))
          ((> (length doc-units) 1) 
             (progn  (beep) (message "Taking the first out of several sections") (symbol-to-string (cadr (car doc-units)))))
          ((= (length doc-units) 0) 
             (progn (error (concat "Program name is not found in a strong reference: " name)))))))

(defun documentation-units-from-program-name (name)
  "Return a list of ids of those documentation units which document name strongly or weakly"
  (let ((doc-units 
          (filter
           (function
            (lambda (triple)
               (and (eq (string-to-symbol name) (car triple)) (or (eq 'strong (caddr triple)) (eq 'weak (caddr triple))))))
           documented-name-occurences)))
    (mapcar 
      (function (lambda (e) (symbol-to-string (cadr e))))
      doc-units)))

(defun elucidator-help ()
  (interactive)
  (let ((help-buffer (if (get-buffer "*ElucidatorHelp*") (get-buffer "*ElucidatorHelp*") (generate-new-buffer "*ElucidatorHelp*"))))
    (show-buffer (other-window 1) help-buffer)
    (set-buffer help-buffer)
    (toggle-read-only -1)
    (erase-buffer)
    (insert-file (concat software-directory (if (eq laml-keybinding-mode 'original) 
                                                "help/elucidator-original.help"
                                                "help/elucidator-hygienic.help")))
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (other-window 1)))


(defun elucidator-let-be (kind)
  (interactive 
     (list (string-to-symbol 
            (completing-read "Force this buffer to an elucidator buffer of kind: "
                   (trivial-alist '("documentation" "program" "setup")) nil t))))
  (cond ((eq kind 'program) 
           (elucidator-mode 1)
           (setq elucidator-buffer-info kind) )
        ((eq kind 'documentation) 
           (elucidator-mode 1)
           (setq elucidator-buffer-info kind) )
        ((eq kind 'setup) 
           (elucidator-mode 1)
           (setq elucidator-buffer-info kind) )))

(defun use-laml-style-documentation ()
  "Write LAML style 'inline' documentation in the laml setup buffer."
  (interactive)
  (setq laml-style-documentation t)
  (setq documentation-buffer setup-buffer))

(defun use-textual-style-documentation (text-buffer-name)
  "Write textual documentation in a distinguished documentation buffer."
  (interactive "bName of existing textual style documentation buffer: ")
  (setq laml-style-documentation nil)
  (setq documentation-buffer (get-buffer text-buffer-name)))
           

(defun find-source-keys-of-name (name)
  "Return a list of source-keys strings in which the program name NAME is defined"
  (filter 
    (function 
     (lambda (sourcekey-name-pair)
       (equal (car sourcekey-name-pair) name)))
    defined-names))

(defun qualification-of-program-reference (word)
  "Return the qualification of word. If not qualified, return the empty string"
  (let ((qual-division-point (find-in-string word ?$)))
    (if qual-division-point
        (substring word 0 qual-division-point)
        "")))

(defun proper-program-reference (word)
  "Return the suffix of the word without qualification"
  (let ((qual-division-point (find-in-string word ?$)))
    (if qual-division-point
        (substring word (+ qual-division-point 1) (length word))
        word)))

; Now in laml-general.el:
; (defun find-in-string (str char)
;   "Return the index of the first occurrence of CHAR (a character) in STR (a string).
; If CHAR does not occur in STR, return nil."
;   (let ((idx 0)
;         (lgt (length str))
;        )
;     (while (and (< idx lgt) (not (eq (aref str idx) char)))
;       (setq idx (+ idx 1)))
; 
;     (if (and (< idx lgt) (eq (aref str idx) char)) 
;         idx
;         nil)))

(defun is-empty (str)
  "Is str considered string"
  (or (equal "" str) (null str)))

(defun goto-program (source-key)
  "Assume as a precondition that this function is called from a documentation context."
  (if (member source-key source-keys)
      (let ((prog-buffer (cdr (assoc source-key source-key-buffer-map))))
	(set-buffer documentation-buffer)
	(setq current-program-buffer prog-buffer)
	(show-buffer 
	 program-window 
	 prog-buffer)
        (other-window 1)
        )
    (progn
      (beep)
      (message (concat "Cannot goto program buffer. Unknown source key: " source-key)))))

(defun disregard-sectional-comment-chars (word)
  "Return word without sectional comment chars (double colons)."
  (if (< (length word) 4)
      word
      (let* ((lgt (length word))
             (ch1 (substring word 0 1))
             (ch2 (substring word 1 2))
             (ch3 (substring word (- lgt 2) (- lgt 1)))
             (ch4 (substring word (- lgt 1) lgt))
            )
         (if (and (equal sectional-comment-char ch1) (equal sectional-comment-char ch2)
                  (equal sectional-comment-char ch3) (equal sectional-comment-char ch4))
             (substring word 2 (- lgt 2))
             word))))

(defun insert-sectional-comment (sectional-comment-word)
  "Insert the sectional comment word, passed as parameter, surrounded by the appropriate markup."
  (interactive "sThe sectional comment word: ")
  (if (in-program-buffer-p)
      (insert (concat "; " sectional-comment-char sectional-comment-char 
                      sectional-comment-word sectional-comment-char sectional-comment-char))
      (error "Sectional comments belong to program buffers.")))


(defun elucidator-show-processing ()
  "Show the Scheme Output buffer, scm-output or laml-output, in the other window" 
  (interactive)
  (show-buffer (other-window 1) 
     (cond ((get-buffer "scm-output") (get-buffer "scm-output"))
           ((get-buffer "laml-output") (get-buffer "laml-output"))
           (t (error "Cannot find processing buffer")))                      
 ))



; ---------------------------------------------------------------------------------------------------
; Elucidator tempaltes
; Currently just simple insertions from the template directory.
; We should consider to use the LAML template stuff.


(defun insert-lispstyle-documentation-entry ()
  "Insert an Elucidator 2 XML-in-LAML documentation entry"
  (interactive)
  (laml-insert-template "documentation-xml-in-laml-entry"))

(defun insert-lispstyle-documentation-section ()
  "Insert an Elucidator 2 XML-in-LAML documentation section"
  (interactive)
  (laml-insert-template "documentation-xml-in-laml-section"))


(defun insert-textual-documentation-entry ()
  "Insert an Elucidator documentation entry"
  (interactive)
  (laml-insert-template "documentation-entry"))

(defun insert-textual-documentation-intro ()
  "Insert an Elucidator documentation entry"
  (interactive)
  (laml-insert-template "documentation-intro"))

(defun insert-textual-documentation-section ()
  "Insert an Elucidator documentation section"
  (interactive)
  (laml-insert-template "documentation-section"))


; ---------------------------------------------------------------------------------------------------
; Elucidator Menu support.
; Menu items in tools>laml>elucidator-commands

(laml-define-key global-map [menu-bar tools elucidator]
     (cons "Scheme Elucidator" (make-sparse-keymap "Elucidator")))

(laml-define-key global-map [menu-bar tools elucidator quit-elucidator]
    '("Quit Elucidator" . quit-elucidator))

(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-5]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator elucidator-help]
    '("Elucidator help" . elucidator-help))

(laml-define-key global-map [menu-bar tools elucidator elucidator-status]
    '("Elucidator status" . elucidator-status))

(laml-define-key global-map [menu-bar tools elucidator show-setup]
    '("Show setup" . show-setup))

(laml-define-key global-map [menu-bar tools elucidator show-program]
    '("Show program..." . show-program))

(laml-define-key global-map [menu-bar tools elucidator elucidator-show-processing]
    '("Elucidator show processing" . elucidator-show-processing))


(laml-define-key global-map [menu-bar tools laml elucidator menu-forms-sep-tools-elu-3]
    '("----"))


(laml-define-key global-map [menu-bar tools elucidator elucidator-let-be]
    '("Elucidator let be..." . elucidator-let-be))

(laml-define-key global-map [menu-bar tools elucidator large-program-window]
    '("Large program window" . large-program-window))

(laml-define-key global-map [menu-bar tools elucidator large-documentation-window]
    '("Large documentation window" . large-documentation-window))

(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-4]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator back]
    '("Go back" . back))

(laml-define-key global-map [menu-bar tools elucidator goto-repeated]
    '("Go repeated" . goto-repeated))

(laml-define-key global-map [menu-bar tools elucidator goto]
    '("Goto" . goto))






(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-3]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator insert-sectional-comment]
    '("Insert sectional comment..." . insert-sectional-comment))



(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-2]
    '("----"))


(laml-define-key global-map [menu-bar tools elucidator reset-elucidator]
    '("Reset elucidator" . reset-elucidator))

(laml-define-key global-map [menu-bar tools elucidator refresh-elucidator]
    '("Refresh elucidator" . refresh-elucidator))

(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-1]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator doc-ref]
    '("Insert doc ref" . doc-ref))

(laml-define-key global-map [menu-bar tools elucidator prog-ref]
    '("Insert prog ref" . prog-ref))

(laml-define-key global-map [menu-bar tools elucidator elucidator-insert-documentation-entry]
    '("Insert documentation entry" . elucidator-insert-documentation-entry))

(laml-define-key global-map [menu-bar tools elucidator elucidator-insert-documentation-section]
    '("Insert documentation section" . elucidator-insert-documentation-section))


(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-0]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator elucidate]
   '("Elucidative processing" . elucidate))


; ----------------------------------------------------------
; Tools > LAML > elucidator stuff

(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-4]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator setup-elucidator]
    '("Setup existing elucidator..." . setup-elucidator))

(laml-define-key global-map [menu-bar tools elucidator menu-forms-sep-tools-elu-6]
    '("----"))

(laml-define-key global-map [menu-bar tools elucidator make-elucidator]
    '("Make new elucidator..." . make-elucidator))


; End Elucidator bindings
; ---------------------------------------------------------------------------------------------------

(defun looking-at-top-level-form ()
  "Does the point select a top-level form?" 
  (save-excursion
    (let* ((parse-result (parse-partial-sexp (point) (point-max)))
           (depth (car parse-result)))
      (= depth  0))))

(defun is-there-initial-comment-p ()
 "Is there a comment just before the point?"
 (save-excursion
  (let ((initial-comment (forward-comment -1)))
    initial-comment)))

(defun locate-sectional-comment-name ()
 "Assume as a precondition that the initial semicolon of a Lisp comment is selected.
  Search for a sectional comment line in this comment line.
  If found, return the name. If not found, return nil."
  (save-excursion
    (let ((cur-point (point))
          (eol-point (progn (end-of-line) (point)))
         )
     (goto-char cur-point)
     
     ; Could be improved - problem if for instance:  ::xxx yyy::
     (if (and (search-forward (concat sectional-comment-char sectional-comment-char) eol-point t)   
              (search-forward (concat sectional-comment-char sectional-comment-char) eol-point t))
       (progn
          (search-backward (concat sectional-comment-char sectional-comment-char) cur-point t)
          (search-backward (concat sectional-comment-char sectional-comment-char) cur-point t)
          (prog-name-under-point))
       nil))))
  

(defun normalize-spaces-around-form (n)
  "Ensure n spaces before and after form. The first character of form is assumed to be selected
such as the initial parenthesis"
  (let ((space-char 32))
   (forward-sexp 1)
   (space-remove (point))
   (insert (make-string n space-char))
   (backward-sexp 1)
   (backward-char 1)
   (if (not (looking-at " ")) (forward-char))  
   (space-remove (point)) 
   (insert (make-string n space-char))
   ))


(defun name-of-selection ()
  "Return the name of the selection. Return nil if no name can be selected. 
Can handle both textual and laml-style documentation.
Returns a string."
 (save-excursion
  (cond ((in-program-buffer-p) 
            (let* ((name-candidate (prog-name-under-point)))
              (if (equal "(" (substring name-candidate 0 1))
                  (name-of-surrounding-scheme-form)
                  name-candidate)))
              
        ((and (in-documentation-buffer-p) (not laml-style-documentation))
            (if (looking-at "{") (forward-char 1))
            (if (looking-at "}") (backward-char 1))
            (let* ((wd (sexp-delimitor))
                   (first (car wd))
                   (last (+ 1 (cadr wd)))
                   (raw-word (buffer-substring-no-properties first last)) ; !!!
                   (possible-qualified-word (disregard-possible-initial-word-modifier raw-word))
                   (word (proper-program-reference possible-qualified-word)))
               word))
        ((and (in-documentation-buffer-p) laml-style-documentation) 
          (if (is-inside-string)                                                    ; goto beginning of string
              (progn (backward-char 1)                                    
                     (while (not (looking-at "\"")) (backward-char))))   
          (while (not (or (looking-at-top-level-form) (looking-at-reference-form))) ; goto surrounding program reference form
                 (backward-up-list 1))
          (if (looking-at-reference-form)  
              (let ((cur-point (point))
                    (end-form-point (progn (forward-sexp 1) (point))))
                (goto-char cur-point)        
                (down-list 1)
                (forward-sexp 1)                                                        ; pass through name of reference form
                (let ((name-search-res (search-forward "'name" end-form-point t)))
                  (if name-search-res
                      (progn
                         (while (looking-at-white-space (point)) (forward-char 1))               ; proceed through white space
                         (if (looking-at "\"") (forward-char 1) (error "name-of-selection: Cannot locate name."))
                         (sexp-under-point))
                      nil))
              )
              nil))
        (t (error "name-of-selection: Unknown buffer context.")))))

(defun qualification-of-selection ()
  "Return the qualification of the selection. Return nil if no qualification can be located.
Can handle both textual and laml-style documentation."
  (save-excursion
  (cond ((in-program-buffer-p) 
            (qualification-of-buffer (current-buffer)))
        ((and (in-documentation-buffer-p) (not laml-style-documentation))
              (if (looking-at "{") (forward-char 1))
              (if (looking-at "}") (backward-char 1))
              (let* ((wd (sexp-delimitor))
                     (first (car wd))
                     (last (+ 1 (cadr wd)))
                     (raw-word (buffer-substring-no-properties first last)) 
                     (possible-qualified-word (disregard-possible-initial-word-modifier raw-word))
                     (qualification (qualification-of-program-reference possible-qualified-word))
                     )
                (if (equal qualification "") nil qualification)))
        ((and (in-documentation-buffer-p) laml-style-documentation) 
          (if (is-inside-string)                                                    ; goto beginning of possible string
              (progn (backward-char 1)                                    
                     (while (not (looking-at "\"")) (backward-char))))   
          (while (not (or (looking-at-top-level-form) (looking-at-reference-form))) ; goto surrounding program reference form
                 (backward-up-list 1))
          (if (looking-at-reference-form)  
              (let ((cur-point (point))
                    (end-form-point (progn (forward-sexp 1) (point))))
                (goto-char cur-point)        
                (down-list 1)
                (forward-sexp 1)                                                        ; pass through name of reference form
                (let ((file-search-res (search-forward "'file" end-form-point t)))
                  (if file-search-res
                      (progn
                         (while (looking-at-white-space (point)) (forward-char 1))               ; proceed through white space
                         (if (looking-at "\"") (forward-char 1) (error "name-of-selection: Cannot locate file attribute."))
                         (sexp-under-point))
                      nil))
              )
              nil))
        (t (error "qualification-of-selection: Unknown buffer context.")))))

(defun looking-at-reference-form ()
  "Does point locate the start parenthesis of a xml-in-laml style elucidator 2 reference form"
  (save-excursion
   (if (looking-at "(")
       (progn (down-list 1)
              (if (member (sexp-under-point) (list "strong-prog-ref" "weak-prog-ref" "doc-ref" "typographic-prog-ref"))
                  t nil))
       nil)))

(defun qualification-of-buffer (buffer)
  (linear-search 
     source-key-buffer-map
     (function (lambda (ass) (eq (cdr ass) buffer)))
     (function car)))

(defun is-program-link-context()
  "Is point identifying a program link context in a documentation buffer."
  (save-excursion
   (cond ((and (in-documentation-buffer-p) (not laml-style-documentation))
              (if (or (looking-at "{") (looking-at "}")) 
                  t
                  (progn
                    (while (not (looking-at-chars (point) (list 9 10 13 32 123))) (backward-char 1))  ; 123 = {
                    (looking-at "{"))))
         ((and (in-documentation-buffer-p) laml-style-documentation) 
          (if (is-inside-string)                                                    ; goto beginning of possible string
              (progn (backward-char 1)                                    
                     (while (not (looking-at "\"")) (backward-char))))   
          (while (not (or (looking-at-top-level-form) (looking-at-reference-form))) ; goto surrounding program reference form
                 (backward-up-list 1))
          (or (looking-at "(strong-prog-ref") (looking-at "(weak-prog-ref"))
         )
        (t nil))))

(defun is-documentation-link-context ()
  "Is point identifying a documentation link context in a documentation buffer."
  (save-excursion
   (cond ((and (in-documentation-buffer-p) (not laml-style-documentation))
            (if (or (looking-at "\\[") (looking-at "\\]")) 
                t
                (progn 
                  (while (not (looking-at-chars (point) (list 9 10 13 32 91))) (backward-char 1))   ; 91 = [
                  (looking-at "\\["))))
         ((and (in-documentation-buffer-p) laml-style-documentation) 
            (if (is-inside-string)                                                    ; goto beginning of possible string
                (progn (backward-char 1)                                    
                       (while (not (looking-at "\"")) (backward-char))))   
            (while (not (or (looking-at-top-level-form) (looking-at-reference-form))) ; goto surrounding program reference form
                   (backward-up-list 1))
            (looking-at "(doc-ref")
         )
        (t nil))))

(defun search-forward-reference-form (kind name file bound) 
  "In a documentation buffer, search forward for a reference form of kind.
bound limits the search to an upper bound. nil means unbounded search.
kind may be prog-ref (covering both strong-prog-ref and weak-prog-ref) or doc-ref. A symbol.
name is the value of the attribute called name. If passing nil, the name attribute is not matched.
file is the value of the attribute called file (the qualification). If passing nil, the file attribute is not matched.
As the effect of this function, set the possition of point to the initial char of the reference form.
"

 (let ((found nil)
       (done nil))
  (cond ((and (in-documentation-buffer-p) (not laml-style-documentation) (eq kind 'prog-ref))
            (cond ((and name file)
                    (search-forward-3 (concat "{" file "$" name "}") (concat "{*" file "$" name "}")
                                      (concat "{+" file "$" name "}") bound))
                  (name 
                    (search-forward-3 (concat "{" name "}") (concat "{*" name "}")  (concat "{+" name "}") bound))
                  (file 
                    (search-forward-3 (concat "{" file "$}") (concat "{*" file "$}")  (concat "{+" file "$}") bound))
                  (t nil)))
        
        ((and (in-documentation-buffer-p) (not laml-style-documentation) (eq kind 'doc-ref))  ; ignore file parameter
            (search-forward (concat "\\[" name "\\]")  bound t))

        ((and (in-documentation-buffer-p) laml-style-documentation (eq kind 'prog-ref))  
            (while (not (or found done))
              (let* ((sr (search-forward-2 "(strong-prog-ref" "(weak-prog-ref" bound))
                     (is-done (not sr))
                     (delimitors (if (and sr (not (is-inside-string))) (progn (backward-up-list 1) (sexp-delimitor)) nil))
                     (prog-ref-attributes (if delimitors (get-attributes-by-delimitors (car delimitors) (cadr delimitors)) '())))
                (setq found (if sr 
                                (match-name-file-attributes
                                     name
                                     (if (assoc "file" prog-ref-attributes) file nil)  ; only match file if found in prog-ref-attributes
                                     prog-ref-attributes)
                                nil))
                (setq done is-done)
                (if (not (or found done)) (forward-sexp-robust 1))
                ))

            (if (and done (not found)) 
                (progn (beep) (message (concat "Could not locate " name))))

        )

        ((and (in-documentation-buffer-p) laml-style-documentation (eq kind 'doc-ref)) 
            (error "Still Missing") )

        (t (error "search-forward-reference-form: Cannot search.")))))

(defun match-name-file-attributes (name file att-alist)
  "Does name and file match similar named attributes att-alist (an a-list).
If name or file is nil, they are not matched agaist any attribute."
  (let* ((name-pair (assoc "name" att-alist))
         (name-val (if name-pair (cdr name-pair) nil))
         (file-pair (assoc "file" att-alist))
         (file-val (if file-pair (cdr file-pair) nil)))
   (and
      (if name (equal name name-val) t)
      (if file (equal file file-val) t))))

(defun search-forward-2 (s1 s2 bound)
  "Search forward for s1 and s2, and use the nearest match as result - for effect as well as return value.
Uses search-forward twice. bound is like the second parameter of search-forward.
Use search-forward in noerror mode with count 1"
  (let* ((p0 (point))
         (r1 (search-forward s1 bound t))
         (r2 (progn (goto-char p0) (search-forward s2 bound t))))
   (cond ((and r1 r2)
            (goto-char (min r1 r2)) (min r1 r2))
         (r1
            (goto-char r1) r1)
         (r2
            (goto-char r2) r2)
         (t 
            (goto-char p0) nil))))

(defun search-forward-3 (s1 s2 s3 bound)
  "Search forward for s1, s2, and s3 and use the nearest match as result - for effect as well as return value.
Uses search-forward three times. bound is like the second parameter of search-forward.
Use search-forward in noerror mode with count 1"
  (let* ((p0 (point))
         (r1 (search-forward s1 bound t))
         (r2 (progn (goto-char p0) (search-forward s2 bound t)))
         (r3 (progn (goto-char p0) (search-forward s3 bound t)))
        )
   (cond ((and r1 r2 r1)
            (goto-char (min r1 r2 r3)) (min r1 r2 r3))
         ((and r1 r2)
            (goto-char (min r1 r2)) (min r1 r2))
         ((and r1 r3)
            (goto-char (min r1 r3)) (min r1 r3))
         ((and r2 r3)
            (goto-char (min r2 r3)) (min r2 r3))
         (r1
            (goto-char r1) r1)
         (r2
            (goto-char r2) r2)
         (r3
            (goto-char r3) r3)
         (t 
            (goto-char p0) nil))))
  
           
(defun get-attributes-by-delimitors (from to)
  "Access and return the xml-in-laml attribute value pairs within the character range (starting at) f and (ending at) t.
Return an association list. Leave point at possition from."
  (let ((more t)
        (attr-name nil)
        (attr-val nil)
        (res nil)
       )
    (goto-char from)
    (down-list 1) 
    (forward-sexp 1) ; proceed through the element name 
    (skip-white-space)  ; ... and white space

    (setq more (< (point) to))
    (while more
           (if (looking-at "\'")
               (progn
                 (setq attr-name (disregard-quote (sexp-under-point)))
                 (forward-sexp 1)
                 (skip-white-space)
                 (if (looking-at "\"")
                     (progn 
                       (forward-char 1)
                       (setq attr-val (sexp-under-point))
                       (forward-sexp 1) (forward-char 1))
                     (progn
                       (setq attr-val (sexp-under-point))
                       (forward-sexp 1)))
                 (skip-white-space)
    
                 (setq more (< (point) to))
                 (if (and attr-name attr-val) (setq res (cons (cons attr-name attr-val) res)))
                 (setq attr-name nil) (setq attr-val nil) 
               )
               (progn
                 (forward-sexp 1) (skip-white-space)
                 (setq more (< (point) to)))
           ))
    (goto-char from)
    (reverse res)))

(defun disregard-quote (str)
  (if (equal "'" (substring str 0 1))
      (substring str 1 (length str))
      str))

(defun skip-white-space ()
  (while (looking-at-white-space (point)) (forward-char 1)))

(defun skip-white-space-and-comments ()
  (while (looking-at-chars (point) (list 9 10 13 32 59))  ; 59 = semicolon = comment char
    (cond ((looking-at-white-space (point)) (forward-char 1))
          (t (forward-comment 1)))))
    
             
(defun bound-of-current-documentation-section-or-entry()
  "Return the upper bound the current documentation section or entry, or nil (meaning unbounded).
This information can be returned if the point is at the beginning of a documentation-entry
or a documentation-section."
 (save-excursion
  (if (or (looking-at "(documentation-entry") (looking-at "(documentation-section"))
      (progn
         (end-of-defun 1)
         (point))
      nil)))


; Specialized versions of fundamental LAML editing commands

(defun eembed ()
  "A specialized embedding of a word phrase in a elucidator element mirror functions."
  (interactive)
  (let* (
         (embed-require-match nil)
         (html-laml-embed-tag-list '(strong-prog-ref weak-prog-ref doc-ref source-marker))
         (nest-embed-attribute-insertion-map '(
            (weak-prog-ref file vers name)
            (strong-prog-ref file vers name)
            (doc-ref name)
            (source-marker name)
          )
         )
        )
    (call-interactively 'embed)))

(defun eopen-laml-form ()
  "A specialized open-laml-form for the Scheme elucidator."
  (interactive)
  (let* ((laml-open-form-tag-list '(weak-prog-ref strong-prog-ref doc-ref source-marker))
         (nest-embed-attribute-insertion-map '(
            (weak-prog-ref file vers name)
            (strong-prog-ref name file vers)
            (doc-ref name)
            (source-marker name)
          )
         )
        )
    (call-interactively 'open-laml-form)))