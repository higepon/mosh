(library (nmosh pffi ext curses)
         (export
           mcur_getch
           mcur_acquire
           mcur_release
           mcur_refresh
           mcur_mouse_enable
           mcur_mouse_disable
           mcur_lines
           mcur_cols
           mcur_locate
           mcur_cls
           mcur_colors
           mcur_color_pairs
           mcur_color_configurable?
           mcur_color_content
           mcur_pair_content
           mcur_init_pair
           mcur_init_color
           mcur_attrib_normal
           mcur_attrib_standout
           mcur_attrib_underline
           mcur_attrib_reverse
           mcur_attrib_blink
           mcur_attrib_dim
           mcur_attrib_bold
           mcur_attrib_protect
           mcur_attrib_invis
           ;mcur_attrib_altcharset
           mcur_attrib_color_pair
           mcur_attron
           mcur_attroff
           mcur_attrset
           mcur_print
           mcur_pdcurses
           mcur_set_title
           mcur_key_resize
           mcur_key_mouse
           mcur_resize_term
           mcur_getkeyloop
           )
         (import
           (rnrs)
           (nmosh ffi box)
           (prefix (nmosh stubs mosh-curses) stub:))

(define mcur_getch stub:mcur_getch)
(define mcur_acquire stub:mcur_acquire)
(define mcur_release stub:mcur_release)
(define mcur_refresh stub:mcur_refresh)
(define mcur_mouse_enable stub:mcur_mouse_enable)
(define mcur_mouse_disable stub:mcur_mouse_disable)
(define mcur_lines stub:mcur_lines)
(define mcur_cols stub:mcur_cols)
(define mcur_locate stub:mcur_locate)
(define mcur_cls stub:mcur_cls)
(define mcur_colors stub:mcur_colors)
(define mcur_color_pairs stub:mcur_color_pairs)
(define mcur_color_configurable? stub:mcur_color_configurable_p)
(define (mcur_color_content c) ;; => r g b
  (let ((r (make-int-box))
        (g (make-int-box))
        (b (make-int-box)))
    (stub:mcur_color_content c r g b)
    (values (int-box-ref r)
            (int-box-ref g)
            (int-box-ref b))))
(define (mcur_pair_content c) ;; => fg bg
  (let ((fg (make-int-box))
        (bg (make-int-box)))
    (stub:mcur_pair_content c fg bg)
    (values (int-box-ref fg)
            (int-box-ref bg))))
(define mcur_init_pair stub:mcur_init_pair)
(define mcur_init_color stub:mcur_init_color)
(define mcur_attrib_normal stub:mcur_attrib_normal)
(define mcur_attrib_standout stub:mcur_attrib_standout)
(define mcur_attrib_underline stub:mcur_attrib_underline)
(define mcur_attrib_reverse stub:mcur_attrib_reverse)
(define mcur_attrib_blink stub:mcur_attrib_blink)
(define mcur_attrib_dim stub:mcur_attrib_dim)
(define mcur_attrib_bold stub:mcur_attrib_bold)
(define mcur_attrib_protect stub:mcur_attrib_protect)
(define mcur_attrib_invis stub:mcur_attrib_invis)
;(define mcur_attrib_altcharset stub:mcur_attrib_altcharset)
(define mcur_attrib_color_pair stub:mcur_attrib_color_pair)

(define mcur_attron stub:mcur_attron)
(define mcur_attroff stub:mcur_attroff)
(define mcur_attrset stub:mcur_attrset)
(define mcur_print stub:mcur_print)

(define (mcur_pdcurses)
  (let ((r (stub:mcur_pdcurses)))
    (case
      ((0) #f)
      ((1) #t)
      (else (assertion-violation
              'mcur_pdcurses
              "invalid status value"
              r)))))

(define mcur_set_title stub:mcur_set_title)
(define mcur_key_resize stub:mcur_key_resize)
(define mcur_key_mouse stub:mcur_key_mouse)
(define mcur_resize_term stub:mcur_resize_term)
(define mcur_getkeyloop stub:mcur_getkeyloop)

)
