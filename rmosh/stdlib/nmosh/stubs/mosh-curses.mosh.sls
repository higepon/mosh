;; generated from src/ext/curses/Library.scm DO NOT EDIT!!
(library (nmosh stubs mosh-curses)
(export
  mcur_getkeyloop
  mcur_resize_term
  mcur_key_mouse
  mcur_key_resize
  mcur_set_title
  mcur_pdcurses
  mcur_print
  mcur_attrset
  mcur_attroff
  mcur_attron
  mcur_attrib_color_pair
  mcur_attrib_altcharset
  mcur_attrib_invis
  mcur_attrib_protect
  mcur_attrib_bold
  mcur_attrib_dim
  mcur_attrib_blink
  mcur_attrib_reverse
  mcur_attrib_underline
  mcur_attrib_standout
  mcur_attrib_normal
  mcur_init_color
  mcur_init_pair
  mcur_pair_content
  mcur_color_content
  mcur_color_configurable_p
  mcur_color_pairs
  mcur_colors
  mcur_cls
  mcur_locate
  mcur_cols
  mcur_lines
  mcur_mouse_disable
  mcur_mouse_enable
  mcur_refresh
  mcur_release
  mcur_acquire
  mcur_getch)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi-plugin)
  (nmosh ffi stublib))


(define %library (make-pffi-ref/plugin 'mosh_curses))


(define
  mcur_getch
  (pffi-c-function %library int mcur_getch))
(define
  mcur_acquire
  (pffi-c-function %library void mcur_acquire))
(define
  mcur_release
  (pffi-c-function %library void mcur_release))
(define
  mcur_refresh
  (pffi-c-function %library void mcur_refresh))
(define
  mcur_mouse_enable
  (pffi-c-function %library void mcur_mouse_enable))
(define
  mcur_mouse_disable
  (pffi-c-function
    %library
    void
    mcur_mouse_disable))
(define
  mcur_lines
  (pffi-c-function %library int mcur_lines))
(define
  mcur_cols
  (pffi-c-function %library int mcur_cols))
(define
  mcur_locate
  (pffi-c-function
    %library
    void
    mcur_locate
    int
    int))
(define
  mcur_cls
  (pffi-c-function %library void mcur_cls))
(define
  mcur_colors
  (pffi-c-function %library int mcur_colors))
(define
  mcur_color_pairs
  (pffi-c-function %library int mcur_color_pairs))
(define
  mcur_color_configurable_p
  (pffi-c-function
    %library
    int
    mcur_color_configurable_p))
(define
  mcur_color_content
  (pffi-c-function
    %library
    void
    mcur_color_content
    int
    void*
    void*
    void*))
(define
  mcur_pair_content
  (pffi-c-function
    %library
    void
    mcur_pair_content
    int
    void*
    void*))
(define
  mcur_init_pair
  (pffi-c-function
    %library
    void
    mcur_init_pair
    int
    int
    int))
(define
  mcur_init_color
  (pffi-c-function
    %library
    void
    mcur_init_color
    int
    int
    int
    int))
(define
  mcur_attrib_normal
  (pffi-c-function %library int mcur_attrib_normal))
(define
  mcur_attrib_standout
  (pffi-c-function
    %library
    int
    mcur_attrib_standout))
(define
  mcur_attrib_underline
  (pffi-c-function
    %library
    int
    mcur_attrib_underline))
(define
  mcur_attrib_reverse
  (pffi-c-function
    %library
    int
    mcur_attrib_reverse))
(define
  mcur_attrib_blink
  (pffi-c-function %library int mcur_attrib_blink))
(define
  mcur_attrib_dim
  (pffi-c-function %library int mcur_attrib_dim))
(define
  mcur_attrib_bold
  (pffi-c-function %library int mcur_attrib_bold))
(define
  mcur_attrib_protect
  (pffi-c-function
    %library
    int
    mcur_attrib_protect))
(define
  mcur_attrib_invis
  (pffi-c-function %library int mcur_attrib_invis))
(define
  mcur_attrib_altcharset
  (pffi-c-function
    %library
    int
    mcur_attrib_altcharset))
(define
  mcur_attrib_color_pair
  (pffi-c-function
    %library
    int
    mcur_attrib_color_pair
    int))
(define
  mcur_attron
  (pffi-c-function %library void mcur_attron int))
(define
  mcur_attroff
  (pffi-c-function %library void mcur_attroff int))
(define
  mcur_attrset
  (pffi-c-function %library void mcur_attrset int))
(define
  mcur_print
  (pffi-c-function %library void mcur_print char*))
(define
  mcur_pdcurses
  (pffi-c-function %library int mcur_pdcurses))
(define
  mcur_set_title
  (pffi-c-function
    %library
    void
    mcur_set_title
    char*))
(define
  mcur_key_resize
  (pffi-c-function %library int mcur_key_resize))
(define
  mcur_key_mouse
  (pffi-c-function %library int mcur_key_mouse))
(define
  mcur_resize_term
  (pffi-c-function %library void mcur_resize_term))
(define
  mcur_getkeyloop
  (pffi-c-function %library void* mcur_getkeyloop))
)
