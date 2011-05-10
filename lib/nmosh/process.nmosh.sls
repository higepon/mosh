(library (nmosh process)
         (export process->stdout+stderr
                 process->stdout
                 process->stderr
                 process->string)
         (import (nmosh io core)
                 (srfi :8)
                 (rnrs)
                 (shorten))

(define (process->stdout+stderr . args)
  (io-dispatch-sync return
                    (launch! (exec . args)
                             ((finish stdout stderr)
                              return))))

(define (process->stdout . args)
  (receive (status stdout stderr) (apply process->stdout+stderr args)
    (values status stdout)))

(define (process->stderr . args)
  (receive (status stdout stderr) (apply process->stdout+stderr args)
    (values status stderr)))

(define (process->string . args)
  (receive (status stdout) (apply process->stdout args)
    (if (= status 0)
      stdout
      #f)))

)
