(import (rnrs))

(display (bytevector-u32-ref #vu8(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253) 12 'little))
