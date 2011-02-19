(import (rnrs) (srfi :64) (yuni binary packunpack octet))

(define-syntax render-unit
  (syntax-rules ()
    ((_ "unpack" (spec dat pak))
     (test-equal "orig" (octet-unpack pak 0 spec) (list->vector 'dat)))
    ((_ "pack" (spec dat pak))
     (test-equal "orig" (octet-pack spec dat) pak))))

(define-syntax render-units
  (syntax-rules ()
    ((_ mode unit)
     (render-unit mode unit))
    ((_ mode unit0 unit1 ...)
     (begin
       (render-unit mode unit0)
       (render-units mode unit1 ...)))))

(define-syntax testcase
  (syntax-rules ()
    ((_ group unit0 ...)
     (begin 
       (test-begin group)
       (test-begin "pack")
       (render-units "pack" unit0 ...)
       (test-end "pack")
       (test-begin "unpack")
       (render-units "unpack" unit0 ...)
       (test-end "unpack")
       (test-end group)))))

(test-begin "packunpack(octet)")
(testcase "basic"
	  (((unsigned 1 little) (unsigned 2 little))
	   (1 2)
	   #vu8(1 2 0))
	  (((unsigned 1 big) (unsigned 2 big))
	   (1 2)
	   #vu8(1 0 2)))
(testcase "pad(by 0)"
	  (((pad 1) (unsigned 2 little))
	   (2)
	   #vu8(0 2 0))
	  (((unsigned 2 little) (pad 1))
	   (2)
	   #vu8(2 0 0)))

(test-end "packunpack(octet)")
