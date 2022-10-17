; loader.sld - MNIST data loader.
;
;   Copyright (c) 2022  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
(define-library (mnist loader)
  (export load-mnist)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme file))
  (import (mnist matrix))

(begin

;; Read train/test images
(define (load-images path num-images)
  (call-with-port
    (open-binary-input-file path)
    (lambda (port)
      (let ([image-width 28]
            [image-height 28])
        ;; Skip the header
        (read-bytevector 16 port)
        ;; Read images as bytevector
        (bytevector->matrix (read-bytevector (* num-images image-width image-height) port) num-images)))))

;; Read train/test labels
(define (load-labels path num-labels)
  (call-with-port
    (open-binary-input-file path)
    (lambda (port)
      (read-bytevector 8 port)
       ;; Read labels as bytevector
     (bytevector->matrix (read-bytevector num-labels port) 1))))

(define load-mnist
  (case-lambda
    [(data-dir num-train num-test)
        (values
          (matrix-divide (load-images (string-append data-dir "train-images-idx3-ubyte") num-train) 255.0)
          (one-hot (load-labels (string-append data-dir "train-labels-idx1-ubyte") num-train) 10)
          (matrix-divide (load-images (string-append data-dir "t10k-images-idx3-ubyte") num-test) 255.0)
          (one-hot (load-labels (string-append data-dir "t10k-labels-idx1-ubyte") num-test) 10))]
    [(data-dir)
      (load-mnist data-dir 60000 10000)]))
))