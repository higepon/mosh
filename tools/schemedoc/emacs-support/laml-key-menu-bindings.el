; General LAML keybindings.
; The LENO keybindings are located in the file styles/lecture-notes/leno.el
; The Scheme Elucidator keybindings are located in the file styles/elucidator/elucidator.el 

; For LAML keybindings we use the emacs lisp function laml-define-key, which is a generalization
; of define-key. Two different key binding modes are distinguished: original 
; and hygienic. If none is passed, no keybindings will be made. 
; The original is the first in the list, and the hyginic is the second, as 
; passed as second parameter to laml-define-key.
; The keybinding mode is set in the LAML configuration file laml-config/configuration. 
; It may also be controlled via the Emacs Lisp variable laml-keybinding-mode.
; If a string or key is passed as second parameter to laml-define-key, this value is
; common for both keybinding modes.

; Arrange that C-o starts asynchronous laml processing
(laml-define-key laml-mode-map (list "\C-o" "\C-c\C-o")  'laml-process-current-buffer )

; Arrange that C-x C-o starts synchronous laml processing
(laml-define-key laml-mode-map (list "\C-x\C-o" "\C-c\C-x\C-o") 'laml-sync-process-current-buffer)

; Arrange that C-o and C-x C-0 start laml processing commands from dired. Done via dired-mode-hook
(defun laml-enable-dired-mode ()
  (laml-define-key dired-mode-map (list "\C-o" "\C-c\C-o")  'laml-process-selected-file-from-dired )
  (laml-define-key dired-mode-map (list "\C-x\C-o" "\C-c\C-x\C-o")  'laml-process-selected-multiple-files-from-dired)
  (laml-define-key dired-mode-map (list "\C-x\C-u" "\C-c\C-x\C-u")  'laml-process-unique-laml-file-from-dired)

  (laml-define-key dired-mode-map [menu-bar operate separator-1]
    '("----"))

  (laml-define-key dired-mode-map [menu-bar operate midi-to-laml]
      '("Midi to XML-in-LAML source file..." . midi-to-laml))

  (laml-define-key dired-mode-map [menu-bar operate laml-process-unique-laml-file-from-dired]
      '("LAML process unique file" . laml-process-unique-laml-file-from-dired))

  (laml-define-key dired-mode-map [menu-bar operate laml-process-selected-multiple-files-from-dired]
      '("LAML process multiple files" . laml-process-selected-multiple-files-from-dired))

  (laml-define-key dired-mode-map [menu-bar operate laml-process-selected-file-from-dired]
      '("LAML process single file" . laml-process-selected-file-from-dired))

)

(setq dired-mode-hook (list 'laml-enable-dired-mode))  
; looses other dired load hooks. How can it be avoided? dired-mode-hook is auto loaded.

(defun schemedoc-insert-definition-comment() (interactive) (laml-insert-template "schemedoc-definition-comment"))
(defun schemedoc-insert-section-comment () (interactive) (laml-insert-template "schemedoc-section-comment"))
(defun schemedoc-insert-introduction-comment () (interactive) (laml-insert-template "schemedoc-introduction-comment"))


(defun laml-enable-scheme-mode ()
  (laml-define-key scheme-mode-map [menu-bar scheme  menu-scheme-forms-sep]
    '("----"))

  ; SchemeDoc Menu entries:

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc]
    (cons "SchemeDoc" (make-sparse-keymap "SchemeDoc")))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc schemedoc-help]
      '("SchemeDoc Help" . schemedoc-help))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc menu-scheme-forms-sep-2]
    '("----"))


  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-misc-clause]
      '("Insert miscellaneous clause" . (lambda () (interactive (laml-insert-template "schemedoc-misc-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-comment-clause]
      '("Insert internal comment clause" . (lambda () (interactive (laml-insert-template "schemedoc-comment-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-post-condition-clause]
      '("Insert postcondition clause" . (lambda () (interactive (laml-insert-template "schemedoc-post-condition-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-returns-clause]
      '("Insert returns clause" . (lambda () (interactive (laml-insert-template "schemedoc-returns-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-reference-clause]
      '("Insert external reference clause" . (lambda () (interactive (laml-insert-template "schemedoc-reference-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-internal-references-clause]
      '("Insert internal references clause" . (lambda () (interactive (laml-insert-template "schemedoc-internal-references-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-example-clause]
      '("Insert example clause" . (lambda () (interactive (laml-insert-template "schemedoc-example-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-attribute-clause]
      '("Insert attribute clause" . (lambda () (interactive (laml-insert-template "schemedoc-attribute-clause")))))
  
  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-parameter-clause]
      '("Insert parameter clause" . (lambda () (interactive (laml-insert-template "schemedoc-parameter-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-pre-condition-clause]
      '("Insert precondition clause" . (lambda () (interactive (laml-insert-template "schemedoc-pre-condition-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-form-clause]
      '("Insert form clause" . (lambda () (interactive (laml-insert-template "schemedoc-form-clause")))))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc insert-dependencies-clause]
      '("Insert schemedoc dependencies clause" . (lambda () (interactive (laml-insert-template "schemedoc-dependencies-clause")))))



  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc menu-scheme-forms-sep-4]
    '("----"))


  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc definition-comment]
      '("Insert definition comment" . schemedoc-insert-definition-comment))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc section-comment]
      '("Insert section comment" . schemedoc-insert-section-comment))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc introduction-comment]
      '("Insert abstract comment" . schemedoc-insert-introduction-comment))







  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc menu-scheme-forms-sep-3]
    '("----"))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc reset-schemedoc-information]
      '("Reset Emacs SchemeDoc support" . reset-schemedoc-information))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc info-emacs-schemedoc]
      '("Info about Emacs SchemeDoc support" . info-emacs-schemedoc))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc display-schemedoc-information]
      '("Show documentation of..." . display-schemedoc-information))


  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc menu-scheme-forms-sep-1]
    '("----"))

  (laml-define-key scheme-mode-map [menu-bar scheme schemedoc run-schemedoc]
      '("Run SchemeDoc" . schemedoc))

  




  ; Scheme Eval Menu entries:

  (laml-define-key scheme-mode-map [menu-bar scheme schemeeval]
    (cons "Evaluate Scheme form(s)" (make-sparse-keymap "SchemeEval")))

  (laml-define-key scheme-mode-map [menu-bar scheme schemeeval eval-buffer]
      '("Current Buffer" . lisp-eval-buffer))

  (laml-define-key scheme-mode-map [menu-bar scheme schemeeval eval-region]
      '("Current Region" . lisp-eval-region))

  (laml-define-key scheme-mode-map [menu-bar scheme schemeeval eval-def]
      '("Current Form" . lisp-eval-defun))

  ; SchemeDoc key bindings:
  (laml-define-key scheme-mode-map (list "\C-c\C-s" "\C-c\C-s")  'schemedoc )
  (laml-define-key scheme-mode-map "\C-c\C-c" 'schemedoc-insert-definition-comment)
  (laml-define-key scheme-mode-map "\C-c\C-b" 'schemedoc-insert-section-comment)
  (laml-define-key scheme-mode-map "\C-c\C-a" 'schemedoc-insert-introduction-comment)

  (laml-define-key scheme-mode-map (list "\C-hf" "\C-hf") 'display-schemedoc-information)

  ; SchemeEval key bindings:
  (laml-define-key scheme-mode-map "\C-\M-x" 'lisp-eval-defun)
  (laml-define-key scheme-mode-map "\C-\M-y" 'lisp-eval-region)
  (laml-define-key scheme-mode-map "\C-\M-z" 'lisp-eval-buffer)

)

(setq scheme-mode-hook (list 'laml-enable-scheme-mode))



; Arrange that C-x C-p makes laml-output visible.
(laml-define-key laml-mode-map (list "\C-x\C-p" "\C-c\C-p") 'see-laml-output)

(laml-define-key laml-mode-map (list "\C-x\C-t" "\C-c\C-x\C-t") 'tidy-laml-form)
(laml-define-key laml-mode-map (list "\C-x\C-y" "\C-c\C-x\C-y") 'tidy-laml-form-again)

(laml-define-key laml-mode-map (list "\C-x\C-v" "\C-c\C-x\C-v") 'one-line-form)

(laml-define-key laml-mode-map (list "\C-x\C-l" "\C-c\C-x\C-l") 'open-laml-form)

; Arrange that C-x C-n calls the LAML nest command:
(laml-define-key laml-mode-map (list "\C-x\C-n" "\C-c\C-n") 'nest)

; Arrange that C-x C-m calls the LAML unnest command:
(laml-define-key laml-mode-map (list "\C-x\C-m" "\C-c\C-m") 'unnest)

; Arrange that C-x C-e calls the LAML embed command:
(laml-define-key laml-mode-map (list "\C-x\C-e" "\C-c\C-x\C-e") 'embed)

; Arrange that C-x C-r calls the LAML unembed command:
(laml-define-key laml-mode-map (list "\C-x\C-r" "\C-c\C-r") 'unembed)

; Arrange that C-x C-q calls the LAML split command:
(laml-define-key laml-mode-map (list "\C-x\C-q" "\C-c\C-q") 'split)

; Arrange that C-x C-a calls the LAML unsplit command:
(laml-define-key laml-mode-map (list "\C-x\C-a" "\C-c\C-a") 'unsplit)

; Arrange that C-M-q indents the currently selected S-expression
(laml-define-key laml-mode-map "\C-\M-q" 'indent-sexp)

; Arrange that C-M-x activates lisp-eval-defun (definition af Lisp expression in inferior Lisp job).
(laml-define-key laml-mode-map "\C-\M-x" 'lisp-eval-defun)


; -----------------------------------------------------------------------------
; Laml mode menu bindings.

(laml-define-key laml-mode-map [menu-bar laml]
    (cons "LAML" (make-sparse-keymap "LAML")))


; -----------------------------------------------------------------------------
; Templates - menu items.

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template]
    (cons "Insert LAML template" (make-sparse-keymap "laml-templates")))

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template other]
      '("Prompt me..." . laml-insert-template))


(laml-define-key laml-mode-map [menu-bar laml laml-insert-template processing-options]
      '("Processing options" . (lambda () (interactive) (laml-insert-template "laml-processing-options"))))

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template xhtml1-frameset-page]
      '("XHTML1.0 frameset page" . (lambda () (interactive) (laml-insert-template "xhtml1-frameset-page"))))

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template xhtml1-transitional-page]
      '("XHTML1.0 transitional page" . (lambda () (interactive) (laml-insert-template "xhtml1-transitional-page"))))

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template xhtml1-strict-page]
      '("XHTML1.0 strict page" . (lambda () (interactive) (laml-insert-template "xhtml1-strict-page"))))

(laml-define-key laml-mode-map [menu-bar laml laml-insert-template html401-page]
      '("HTML4.01 transitional page" . (lambda () (interactive) (laml-insert-template "html401-page"))))



(laml-define-key laml-mode-map [menu-bar laml menu-forms-sep3]
    '("----"))

; -----------------------------------------------------------------------------
; Scheme evaluation - menu items

(laml-define-key laml-mode-map [menu-bar laml scheme-eval]
     (cons "Evaluate Scheme form(s)" (make-sparse-keymap "Scheme evaluation")))

(laml-define-key laml-mode-map [menu-bar laml scheme-eval lisp-eval-buffer]
      '("Entire buffer" . (lambda () (interactive) (lisp-eval-region (point-min) (point-max)))))

(laml-define-key laml-mode-map [menu-bar laml scheme-eval lisp-eval-region]
      '("Current selection" . lisp-eval-region))

(laml-define-key laml-mode-map [menu-bar laml scheme-eval lisp-eval-defun]
      '("Current form" . lisp-eval-defun))

; -----------------------------------------------------------------------------
; Laml mode menu items

(laml-define-key laml-mode-map [menu-bar laml indent-sexp]
      '("Indent symbolic expression" . indent-sexp))


(laml-define-key laml-mode-map [menu-bar laml menu-forms-sep2]
    '("----"))

(laml-define-key laml-mode-map [menu-bar laml one-line-form]
      '("One line form" . one-line-form))

(laml-define-key laml-mode-map [menu-bar laml tidy-laml-form-again]
      '("Tidy Laml form - same width" . tidy-laml-form-again))

(laml-define-key laml-mode-map [menu-bar laml tidy-laml-form]
      '("Tidy Laml form" . tidy-laml-form))

(laml-define-key laml-mode-map [menu-bar laml open-laml-form]
      '("Open new form" . open-laml-form))

(laml-define-key laml-mode-map [menu-bar laml unnest]
      '("Unnest" . unnest))

(laml-define-key laml-mode-map [menu-bar laml nest]
      '("Nest form in form" . nest))

(laml-define-key laml-mode-map [menu-bar laml unsplit]
      '("Unsplit" . unsplit))

(laml-define-key laml-mode-map [menu-bar laml split]
      '("Split string" . split))

(laml-define-key laml-mode-map [menu-bar laml unembed]
      '("Unembed" . unembed))

(laml-define-key laml-mode-map [menu-bar laml embed]
      '("Embed string in form" . embed))

(laml-define-key laml-mode-map [menu-bar laml menu-forms-sep1]
    '("----"))

(laml-define-key laml-mode-map [menu-bar laml process-sync]
      '("Process synchronously" . laml-sync-process-current-buffer))

(laml-define-key laml-mode-map [menu-bar laml process-async]
      '("Process asynchronously" . laml-process-current-buffer))

; End main LAML menu entries



; ---------------------------------------------------------------------------------------------------

(laml-define-key global-map [menu-bar tools laml menu-forms-sep2]
    '("----"))

(laml-define-key global-map [menu-bar tools laml laml-customize]
      '("Customize LAML..." . laml-customize))

(laml-define-key global-map [menu-bar tools laml menu-forms-sep1]
    '("----"))


(laml-define-key global-map [menu-bar tools laml set-interactive-laml-mirror-library]
      '("Set mirror of LAML interaction..." . set-interactive-laml-mirror-library))

(laml-define-key global-map [menu-bar tools laml run-laml-interactively]
      '("Run LAML interactively" . run-laml-interactively))



; ---------------------------------------------------------------------------------------------------

(laml-define-key global-map [menu-bar tools schemedoc]
     (cons "SchemeDoc" (make-sparse-keymap "SchemeDoc")))

(laml-define-key global-map [menu-bar tools schemedoc schemedoc-help]
      '("SchemeDoc Help" . schemedoc-help))

(laml-define-key global-map [menu-bar tools schemedoc menu-forms-sep1]
    '("----"))

(laml-define-key global-map [menu-bar tools schemedoc make-laml-schemedoc-manual-index]
    '("Make SchemeDoc Index..." . make-laml-schemedoc-manual-index))

(laml-define-key global-map [menu-bar tools schemedoc make-laml-schemedoc-manual]
    '("Make SchemeDoc Manual..." . make-laml-schemedoc-manual))

; ---------------------------------------------------------------------------------------------------



