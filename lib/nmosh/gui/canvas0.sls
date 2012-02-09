(library (nmosh gui canvas0)
         (export 
           canvas-create
           canvas-resize
           canvas-destroy
           canvas-show
           canvas-update
           canvas-size
           canvas-move
           canvas-client->screen
           surface->png
           png->surface
           )
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (nmosh stubs mosh-cairo)
                 (nmosh pffi interface)
                 (nmosh pffi win32 gui)
                 (nmosh aio platform win32)
                 (nmosh io master-queue))

(define* canvas (hwnd window bitmap bitmapDC surface))

(define (param->x+y param)
  (let ((y (bitwise-and param #xffff))
        (x (bitwise-arithmetic-shift-right param 16)))
    (list x y)))

;; FIXME: 32bit only
(define (pointer->signed-integer p)
  (define bv (make-bytevector 4))
  (bytevector-u32-native-set! bv 0 (pointer->integer p))
  (bytevector-s32-native-ref bv 0))

(define (canvas-create cb create-cb)
  ;; cb = ^[canvas evt . opt]
  ;; create-cb = ^[canvas]
  (define c)
  (define w (win32_window_alloc))
  (define (callback err bytes ovl key)
    (case (pointer->integer key)
      ((0) ;; [create hWnd]
       (set! c (make canvas 
                     (hwnd (make-hwnd bytes w))
                     (window w)
                     (bitmap #f)
                     (bitmapDC #f)
                     (surface #f)))
       (create-cb c))
      ((1)
       (cb c 'destroy))
      ((2)
       (cb c 'close))
      ((3)
       (cb c 'char (pointer->integer bytes)))
      ((4)
       (apply cb c 'mouse-move (param->x+y (pointer->integer bytes))))
      ((6)
       (cb c 'mouse-wheel-h (pointer->signed-integer bytes)))
      ((20)
       (apply cb c 'resize (param->x+y (pointer->integer bytes))))
      ((30)
       (cb c 'active))
      ((31)
       (cb c 'inactive))
      ((40)
       (apply cb c 'mouse-down         0 (param->x+y (pointer->integer bytes))))
      ((41)
       (apply cb c 'mouse-up           0 (param->x+y (pointer->integer bytes))))
      ((42)
       (apply cb c 'mouse-double-click 0 (param->x+y (pointer->integer bytes))))
      ((43)
       (apply cb c 'mouse-down         1 (param->x+y (pointer->integer bytes))))
      ((44)
       (apply cb c 'mouse-up           1 (param->x+y (pointer->integer bytes))))
      ((45)
       (apply cb c 'mouse-double-click 1 (param->x+y (pointer->integer bytes))))
      ((46)
       (apply cb c 'mouse-down         2 (param->x+y (pointer->integer bytes))))
      ((47)
       (apply cb c 'mouse-up           2 (param->x+y (pointer->integer bytes))))
      ((48)
       (apply cb c 'mouse-double-click 2 (param->x+y (pointer->integer bytes))))
      (else
    (write (list 'message err bytes ovl key))(newline)
        ))
    )
  (queue-window-register nmosh-io-master-queue
                         w
                         callback)
  #t)

(define* (canvas-size (canvas))
  (win32_window_getclientrect (~ canvas 'hwnd)))

(define* (canvas-resize (canvas) x y)
  (let-with canvas (hwnd window bitmap bitmapDC surface)
    (let-values (((w h) (canvas-size canvas))
                 ((x0 y0 x1 y1) (win32_window_getwindowrect hwnd)))
                (win32_window_move hwnd
                                   x0 y0
                                   (+ x1 (- x w))
                                   (+ y1 (- y h)))
                ;; Clear backbuffer
                (win32_window_fitbuffer window)
                (when surface
                  (mc_surface_destroy surface))
                (when bitmapDC
                  (win32_dc_dispose bitmapDC))
                (when bitmap
                  (win32_gdi_deleteobject bitmap))

                ;; Allocate new backbuffer
                (let* ((dc (win32_dc_create))
                       (bitmap (win32_window_createbitmap window x y))) 
                  (win32_dc_selectobject dc bitmap) 
                  (~ canvas 'bitmap := bitmap) 
                  (~ canvas 'bitmapDC := dc) 
                  (~ canvas 'surface := 
                     (mc_win32_create (dc->pointer dc))))))
  )

(define* (canvas-move (canvas) x y)
  (let-with canvas (hwnd)
    (receive (x0 y0 x1 y1) (win32_window_getwindowrect hwnd)
      (let ((off-x (- x x0))
            (off-y (- y y0)))
        (win32_window_move hwnd
                           x y
                           ;; w
                           (- x1 x0)
                           ;; h
                           (- y1 y0))))))

(define* (canvas-client->screen (canvas) x y)
  (let-with canvas (hwnd)
    (win32_window_clienttoscreen hwnd x y)))

(define* (canvas-show (canvas))
  (win32_window_show (~ canvas 'hwnd) 1))

(define* (canvas-destroy (canvas))
  (queue-window-destroy nmosh-io-master-queue
                        (~ canvas 'hwnd)))

(define* (canvas-update (canvas))
  (define bv (make-bytevector (* 4 4)))
  (bytevector-u32-native-set! bv 0 0)
  (bytevector-u32-native-set! bv 4 0)
  (let-with canvas (window bitmapDC)
    (receive (x y) (canvas-size canvas)
      (bytevector-u32-native-set! bv 8 x)
      (bytevector-u32-native-set! bv 12 y)
      (win32_window_updaterects window bitmapDC bv 1))))

(define (surface->png s file)
  (mc_mem_png_save s file))

(define (png->surface file)
  (mc_mem_png_load file))

)
