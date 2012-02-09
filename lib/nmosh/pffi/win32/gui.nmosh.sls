(library (nmosh pffi win32 gui)
         (export win32_messagebox
                 win32_window_move
                 win32_window_show
                 win32_window_hide
                 win32_window_settitle
                 win32_window_close
                 win32_window_destroy
                 win32_registerwindowclass
                 win32_window_alloc
                 win32_window_create
                 win32_window_fitbuffer
                 win32_getmonitorinfo

                 win32_window_updaterects
                 win32_window_createbitmap
                 win32_window_getclientrect
                 win32_window_getwindowrect
                 win32_window_clienttoscreen
                 win32_dc_create
                 win32_dc_dispose
                 win32_dc_selectobject
                 ;win32_dc_transform
                 ;win32_dc_settransform
                 win32_gdi_deleteobject
                 win32_pen_create
                 win32_brush_create
                 win32_font_create
                 win32_dc_draw
                 win32_dc_measure_text

                 ;integer-hwnd
                 make-hwnd
                 dc->pointer
                 )
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi win32 aio) ;; for win32-handle
                 (nmosh pffi win32 util)
                 (prefix (nmosh stubs win32-gui) stub:))

(define* HWND (handle ovl))

(define (make-hwnd hwnd ovl)
  (make HWND
        (handle hwnd)
        (ovl ovl)))

(define* (hwnd->pointer (x HWND))
  (let-with x (handle) handle))

(define (win32_messagebox caption msg dlgtype icontype)
  (stub:win32_messagebox 
    (string->utf16-bv caption)
    (string->utf16-bv msg)
    dlgtype
    icontype))

(define* (win32_window_move (HWND) x y w h)
  (stub:win32_window_move (hwnd->pointer HWND)
                          x y w h))

;; cmd 0 = not activate, 1 = activate
(define* (win32_window_show (HWND) cmd)
  (stub:win32_window_show (hwnd->pointer HWND)
                          cmd))

(define* (win32_window_hide (HWND))
  (stub:win32_window_hide (hwnd->pointer HWND)))

(define* (win32_window_settitle (HWND) text)
  (stub:win32_window_settitle (hwnd->pointer HWND)
                              (string->utf16-bv text)))

(define* (win32_window_close (HWND))
  (stub:win32_window_close (hwnd->pointer HWND)))

(define* (win32_window_destroy (HWND))
  (stub:win32_window_destroy (hwnd->pointer HWND))
  (win32_overlapped_free (~ HWND 'ovl)))

;; you will need some sync on this.
(define* (win32_window_fitbuffer p)
  (stub:win32_window_fitbuffer p))

(define (win32_registerwindowclass)
  (stub:win32_registerwindowclass))

(define (win32_window_alloc) ;; => pointer
  (stub:win32_window_alloc))

(define* (win32_window_create (iocp win32-handle) overlapped)
  (make-hwnd
    (stub:win32_window_create (handle->pointer iocp)
                              overlapped)
    overlapped))

;; 
(define (win32_getmonitorinfo id cmd) ;; => x0 y0 x1 y1 / false
  (let ((valid? (make-int-box))
        (x0 (make-int-box))
        (y0 (make-int-box))
        (x1 (make-int-box))
        (y1 (make-int-box)))
    (stub:win32_getmonitorinfo id cmd
                               valid? x0 y0 x1 y1)
    (let ((valid? (int-box-ref valid?))
          (x0 (int-box-ref x0))
          (y0 (int-box-ref y0))
          (x1 (int-box-ref x1))
          (y1 (int-box-ref y1)))
      (if (= valid? 1)
        (values x0 y0 x1 y1)
        (values #f #f #f #f)))))

(define* DC (pointer))
(define (pointer->dc p)
  (make DC (pointer p)))
(define* (dc->pointer (DC))
  (let-with DC (pointer) pointer))
(define* GDIOBJ (pointer))

(define (pointer->gdiobj p)
  (make GDIOBJ (pointer p)))
(define* (gdiobj->pointer (GDIOBJ))
  (let-with GDIOBJ (pointer) pointer))

(define* (win32_window_updaterects wnd (DC) rects count)
  (stub:win32_window_updaterects wnd (dc->pointer DC) rects count))
(define (win32_window_createbitmap wnd x y)
  (pointer->gdiobj
    (stub:win32_window_createbitmap wnd x y)))

(define* (win32_window_getclientrect (HWND))
  (let ((x0 (make-int-box))
        (y0 (make-int-box))
        (x1 (make-int-box))
        (y1 (make-int-box)))
    (stub:win32_window_getclientrect (hwnd->pointer HWND)
                                     x0 y0 x1 y1)
    (values (int-box-ref x1)
            (int-box-ref y1))))

(define* (win32_window_getwindowrect (HWND))
  (let ((x0 (make-int-box))
        (y0 (make-int-box))
        (x1 (make-int-box))
        (y1 (make-int-box)))
    (stub:win32_window_getwindowrect (hwnd->pointer HWND)
                                     x0 y0 x1 y1)
    (values (int-box-ref x0)
            (int-box-ref y0)
            (int-box-ref x1)
            (int-box-ref y1))))

(define* (win32_window_clienttoscreen (HWND) x y) ;; => x y
  (let ((x0 (make-int-box))
        (y0 (make-int-box)))
    (stub:win32_window_clienttoscreen (hwnd->pointer HWND)
                                      x y
                                      x0 y0)
    (values (int-box-ref x0)
            (int-box-ref y0))))

(define (win32_dc_create)
  (pointer->dc (stub:win32_dc_create)))
(define* (win32_dc_dispose (DC))
  (stub:win32_dc_dispose (dc->pointer DC)))
(define* (win32_dc_selectobject (DC) (GDIOBJ))
  (stub:win32_dc_selectobject (dc->pointer DC)
                              (gdiobj->pointer GDIOBJ)))
(define* (win32_gdi_deleteobject (GDIOBJ))
  (stub:win32_gdi_deleteobject (gdiobj->pointer GDIOBJ)))
(define (win32_pen_create width r g b)
  (pointer->gdiobj
    (stub:win32_pen_create width r g b)))
(define (win32_brush_create r g b)
  (pointer->gdiobj
    (stub:win32_brush_create r g b)))
(define (win32_font_create height weight italic? face)
  (define (convert-weight-sym w)
    (cond 
      ((symbol? w)
       (case w
         ((thin) 100)
         ((extralight) 200)
         ((light) 300)
         ((normal) 400)
         ((regular) 400)
         ((medium) 500)
         ((semibold) 600)
         ((bold) 700)
         ((ultrabold) 800)
         ((black) 900)
         (else 400)))
      ((number? w) w)
      (else 400)))
  (let ((facename (string->utf16-bv face))
        (weight (convert-weight-sym weight))
        (italicp (if italic? 1 0)))
    (let ((p (stub:win32_font_create height weight italicp facename)))
      (and (not (= 0 (pointer->integer p)))
           (pointer->gdiobj p)))))
(define* (win32_dc_draw (DC) bmpdc? ops count)
  (define bmpdc (if bmpdc?
                  (let-with bmpdc? (pointer) pointer)
                  (integer->pointer 0)))
  (stub:win32_dc_draw (dc->pointer DC)
                      bmpdc
                      ops
                      count))

(define* (win32_dc_measure_text (DC) str)
  (let ((x (make-int-box))
        (y (make-int-box))
        (str-bv (string->utf16-bv str)))
    (let ((b (stub:win32_dc_measure_text (dc->pointer DC)
                                         str-bv (string-length str) x y)))
      (if (= b 0)
        (values #f #f)
        (values (int-box-ref x) (int-box-ref y))))))

)
