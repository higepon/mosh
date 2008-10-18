(display (bytevector->string #vu8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86)
                             (make-transcoder (utf-8-codec))))

(newline)
(display (string->bytevector "あいう" (make-transcoder (utf-8-codec))) )
