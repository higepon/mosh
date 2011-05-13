(library (nmosh win32 gui)
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

                 integer->hwnd)
         (import (rnrs)
                 (yuni core)
                 (mosh ffi)
                 (nmosh ffi box)
                 (nmosh win32 aio) ;; for win32-handle
                 (nmosh win32 util)
                 (prefix (nmosh stubs win32-gui) stub:))

(define* HWND (handle))

(define* (hwnd->pointer (x HWND))
  (let-with x (handle) handle))

(define (pointer->hwnd x)
  (make HWND (handle x)))

(define (integer->hwnd x)
  (pointer->hwnd (integer->pointer x)))

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
  (stub:win32_window_destroy (hwnd->pointer HWND)))

;; you will need some sync on this.
(define* (win32_window_fitbuffer (HWND) p)
  (stub:win32_window_fitbuffer (hwnd->pointer HWND)
                               p))

(define (win32_registerwindowclass)
  (stub:win32_registerwindowclass))

(define (win32_window_alloc) ;; => pointer
  (stub:win32_window_alloc))

(define* (win32_window_create (iocp win32-handle) overlapped)
  (stub:win32_window_create (handle->pointer iocp)
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



)
