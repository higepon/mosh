(library (nmosh debugger)
         (export debugger)
         (import 
           (rnrs)
           (primitives %nmosh-fail-trace
                       %nmosh-fail-condition)
           (rename (nmosh debugger core)
                   (debugger debugger/core)))
(define (debugger)
  (debugger/core %nmosh-fail-condition %nmosh-fail-trace))

)

