; This file is made during the LAML configuration process - DO NOT EDIT!


; In Windows the .emacs file is normally in c:/
; In Unix the .emacs file is in normally located in the root of your home directory.

; Loads the LAML emacs support via the file laml.el

(load "/home/taro/vm/monar/tools/schemedoc/laml.el") 

; -----------------------------------------------------------------------------------
; Top level contribution to the Emacs tools menu:

(define-key global-map [menu-bar tools menu-forms-sep-tools]
    '("----"))

(define-key global-map [menu-bar tools laml]
    (cons "Laml" (make-sparse-keymap "Laml")))

; ------------------------------------------------------------------------------------
; Loading of Emacs support of LAML subsystems and tools.
; You can eliminate the Emacs support by putting a semicolon comment in front of
; one or several of the following load commands.


; Loads the Scheme Elucidator 2 Emacs support.
; Establishes the Elucidator keyboard shortcuts (C-e ...) and the elucidator menu items
; in Tools > LAML > Elucidator.
;(load "/home/taro/vm/monar/tools/schemedoc/styles/xml-in-laml/elucidator-2/elucidator.el")  

; Load the Emacs support for LAML SchemeDoc
;(load "/home/taro/vm/monar/tools/schemedoc/styles/xml-in-laml/schemedoc-2/schemedoc.el") 

; Loads the LENO emacs support.
; Establishes the LENO keyboard shortcuts (C-l ...) and the LENO menu item.
;(load "/home/taro/vm/monar/tools/schemedoc/styles/lecture-notes/leno.el")

; Load the LAML photo show emacs support.
;(load "/home/taro/vm/monar/tools/schemedoc/styles/xml-in-laml/photo-show/photo-show.el")

; Load the LAML chords emacs support. This supports a song format with chords.
;(load "/home/taro/vm/monar/tools/schemedoc/styles/xml-in-laml/chords/chords.el")

; Load the LAML keybinding.
(load "/home/taro/vm/monar/tools/schemedoc/emacs-support/laml-key-menu-bindings.el") 