(import (mosh test)
        (shorten) (json)
        (yuni util lists)
        (yuni binary codec msgpack)
        (rnrs))

(define jsondata ;; test data from MessagePack
  "[false,true,null,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,127,127,255,65535,4294967295,-32,-32,-128,-32768,-2147483648,0.0,-0.0,1.0,-1.0,\"a\",\"a\",\"a\",\"\",\"\",\"\",[0],[0],[0],[],[],[],{},{},{},{\"a\":97},{\"a\":97},{\"a\":97},[[]],[[\"a\"]]]" 
)
(define jsondata-ans '(#f #t () 0 0 0 0 0 0 0 0 0 -1 -1 -1 -1 -1 127 127 255 65535 4294967295 -32 -32 -128 -32768 -2147483648 0.0 -0.0 1.0 -1.0 #vu8(97) #vu8(97) #vu8(97) #vu8() #vu8() #vu8() (0) (0) (0) () () () #() #() #() #((#vu8(97) . 97)) #((#vu8(97) . 97)) #((#vu8(97) . 97)) (()) ((#vu8(97)))))



(define json-obj (sexp-map (^[obj] (if (string? obj)
                                     (string->utf8 obj)
                                     obj)) 
                           (json-read (open-string-input-port jsondata))))

(define (obj->msgpack obj) (generate-msgpack-buffer obj))
(define (msgpack->obj m)
  (define ret)
  (define deser (make-msgpack-deserializer (^[obj] (set! ret obj))))
  (for-each (^e (deser e)) m)
  ret)

(test-equal json-obj jsondata-ans)
(test-equal json-obj (msgpack->obj (obj->msgpack json-obj)))

(test-results)
