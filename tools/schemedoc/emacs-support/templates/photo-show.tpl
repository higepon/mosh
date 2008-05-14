(load (string-append laml-dir "laml.scm"))
(laml-style "xml-in-laml/photo-show/photo-show")

(define photo-show-title *PHOTO-TITLE*)

(photo-show
  'language "danish"                                       ; danish or english
  'time "3"
  'background-color (rgb-color-encoding black)             ; color string, like "#123456"
  'text-color (rgb-color-encoding white)                   ; color string, like "#123456"
  'home-path "../"
  'transition "none"                                       ; none or blend
  'tabular-overview-dimension "3"                          ; The dimension of tabular overviews. An integer, such as 2, 3, or 4
  'index-linking "stop"                                    ; run, stop

  'previous-film "../*PREVIOUS-FILM*/"
  'before-first "previous-film"                            ; last-picture, previous-film, none
  'next-film "../*NEXT-FILM*/"
  'after-last "next-film"                                  ; first-picture, next-film, stop-page, none

  'default-size "auto"                                     ; original, auto, explicit
  'default-orientation "landscape"                         ; portrait or landscape
  'portrait-percentage "90"                                ; Applies for auto default-size, portrait only. Image height in percent.
  'landscape-percentage "92"                               ; Applies for auto default-size, landscape only. Image width in percent.
  'photo-displacement-left "18"                            ; Fine tuning. Compensates for invisible scroll bar.

  'verbosity-level "2"                                     ; 0, 1, 2

  'copy-film "false"                                       ; true, false
  'copy-destination "DIR"                                  ; abs. path to destination directory.  Used if copy-film is true.

  'camera "None"                                           ; The name of the camara used. "Canon-G3" supported.

  (title-of-show photo-show-title)
  (upper-caption 'size "6" photo-show-title)
  (lower-caption 'size "4" "")

  (photos-in-current-directory)                            ; or alternatively (photo-list (photo ...) ...)

)
