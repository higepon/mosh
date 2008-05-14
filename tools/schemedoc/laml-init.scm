; Load this convenient file to help you use LAML from a Scheme Prompt, such as DrScheme.
; This requires that you have already configured and installed LAML on your computer.
; We assume as a precondition that laml-dir already has been defined on beforehand.
; This file loads the fundamental LAML setup file, called laml.scm,  the XHTML1.0
; transitional mirror, and with that a number of other useful LAML library files
; (color, time, and file-read). Your LAML startup directory is set to
; the root of the LAML distribution. Use (laml-cd "dir") to relocate the
; startup directory.


(load (string-append laml-dir "laml.scm"))
(laml-style "simple-xhtml1.0-transitional-validating")

(fake-startup-parameters "some-file" laml-dir)   ; It is crucial that both startup parameters have been initialized.

(laml-cd laml-dir)  ; Actually redundant, but this gives a message about the current directory.
