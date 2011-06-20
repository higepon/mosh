(library (nmosh canvas platform win32)
         (export 
           window-show
           window-surface-create
           window-create0
           window-hide
           window-resetbuffer
           window-move
           window-title
           window-dispose
           window-client-size
           param->x+y
           )
         (import
           (rnrs)
           (match)
           (yuni core)
           (nmosh io master-queue)
           (nmosh aio platform win32)
           (nmosh win32 aio)
           (nmosh win32 gui))

(define* surface (x y gdiobj))

(define* window (win hwnd io-object handler))

(define Q nmosh-io-master-queue)

(define (param->x+y param)
  (let ((y (bitwise-and param #xffff))
        (x (bitwise-arithmetic-shift-right param 16)))
    (values x y)))

(define* (make-handler (window) proc) ;; => lambda
  (lambda (ovl key param)
    (case key
      ((0) ;; INIT
       (touch! window
         (win ovl)
         (hwnd (integer->hwnd param)))
       (proc window key param))
      ((1) ;; DESTROY
       (let-with window (io-object)
         (dispose-io-object io-object))
       (proc window key param))
      (else (proc window key param)))))


(define (window-create0 handler) ;; FIXME: TEST ONLY, implement some API
  (let ((me (make window
                  (handler handler)
                  (win #f) 
                  (hwnd #f))))
    (touch! me (io-object (queue-create-window
                            Q (make-handler me handler))))
    me))

(define* (window-surface-create (window) x y)
  (let-with window (win)
    (make surface 
          (x x)
          (y y)
          (gdiobj (win32_window_createbitmap win x y)))))

(define* (window-show (window))
  (let-with window (hwnd)
    (win32_window_show hwnd 1)))

(define* (window-hide (window))
  (let-with window (hwnd)
    (win32_window_hide hwnd)))

(define* (window-resetbuffer (window))
  (let-with window (win)
    (win32_window_fitbuffer win)))

(define* (window-move (window) x y w h)
  (let-with window (hwnd)
    (win32_window_move hwnd x y w h)))

(define* (window-set-title (window) text)
  (let-with window (hwnd)
    (win32_window_settitle hwnd text)))

(define* (window-client-size (window)) ;; => w h
  (let-with window (hwnd)
    (win32_window_getclientrect hwnd)))

(define window-title
  (case-lambda
    ((wnd) "") ;; FIXME: implement it
    ((wnd text) (window-set-title wnd text))))

(define* (window-dispose (window))
  (let-with window (hwnd)
    (win32_window_destroy hwnd)))

(define* drawop (obj* buf))


)
