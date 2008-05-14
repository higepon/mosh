(defgroup laml nil
  "LAML CUSTOMIZATIONS. LAML means Lisp Abstracted Markup Language.

With LAML it is possible to write HTML documents and CGI programs in Scheme.
For more informations see http://www.cs.auc.dk/~normark/laml/."
  :group 'emacs
  )

(defcustom silent-laml-processing nil
  "With silent laml processing, no scheme buffer - scm-output - will appear during asynchronous processing.

If true (t),  LAML processing is reported in a buffer scm-output which isn't shown.
Go to the buffer with switch-to-buffer to consult it. Only supported in MzScheme based configurations.
If false (nil), LAML processing will cause a buffer, scm-output, to appear at the bottom of the frame with processing information."

  :type 'boolean
  :group 'laml)

(defcustom interactive-laml-mirror-library "xhtml-1.0-transitional"
  "Controls which HTML mirror to use in an interactive LAML session with Emacs - in run-laml-interactively.
Can also be set by the Emacs command set-interactive-laml-mirror-library.
The possible values are the following strings:
html-4.01: A fully validating HTML 4.01 mirror.
html-4.00: HTML4.0 with only attribute validation.
xhtml-1.0-strict: A fully validating XHTML1.0 strict mirror.
xhtml-1.0-transitional: A fully validating XHTML1.0 strict mirror.
xhtml-1.0-frameset: A fully validating XHTML1.0 strict mirror.
xhtml-1.1: A fully validating XHTML1.1 mirror.
svg-1.1: A fully validating SVG XML mirror."
  :type 'string
  :group 'laml
)


(defcustom interactive-laml-load-convenience-library t
  "Controls the loading of a convenience library in an interactive LAML session with Emacs - in run-laml-interactively.
If true, load a mirror library dependent convenience library.
A LAML convenience library contains a lot of helpful (X)HTML related functions. For people that are new to LAML, it 
is recommeded to built and acculated your own convenience collections. If the value is true, you load Kurt Nørmark's 
convenience collections."

  :type 'boolean
  :group 'laml
)

(defcustom laml-pp-helpful t
  "Show useful role names of constituents in selected LAML templates. 

LAML templates are activated by M-x insert-... in the various LAML styles. In LENO, use M-x leno-insert-..."
  :type 'boolean
  :group 'laml)


(defvar laml-single-string-approach nil)

(defcustom smart-word-determination t
  "With smart word determination a word is implicitely selected if the point is at the first character of a word.

If you do not use smart word determination you must explicitly select a string to embed. The variable is important for the embed editor command."
  :type 'boolean
  :group 'laml)

(defvar concatenate-form 'con "The Scheme string concatenate function to be used with LAML. Only relevant for text-based mirrors (the very old mirrors).

The very old mirrors are simple and simple-html4.0-loose. With use of all other mirrors, just ignore this variable.
") 

(defvar leno-version 'xml-in-laml 
 "Determines which generation of the LENO templates to apply. 
With the symbol original, the first generation of the templates is applied.
With the symbol xml-in-laml, the second generation is applied.")


(defcustom scheme-documentation-style 'auto
  "Defines the kind of documentation commenting style of LAML SchemeDoc; one of the symbols auto, multi-semicolon or documentation-mark.

The value of this variable is relevant when SchemeDoc is activated from Emacs.
The value auto: Guess the documentation style based on a scan through the Scheme source file.
The value multi-semicolon: Two semicolons used for definitions, three semicolons 
used for sections, and four semicolons used for introductory abstract.
The value documentation-mark: One exclamation mark is used for definitions, 
two exclamation marks are used for sections, and three exclamation marks are used for
the introductory abstract."
  :type 'symbol
  :group 'laml)

