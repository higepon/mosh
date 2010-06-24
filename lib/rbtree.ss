; rbtree.ss - Red-Black tree
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

#|
    Title: Red-Black tree

    Example:
    (start code)
    (end code)

    library: (rbtree)

|#
(library (rbtree)
  (export
   rbtree?
   check-rbtree
   rbtree-set!
   rbtree-keys
   rbtree-delete!
   rbtree-get
   rbtree-size
   rbtree-contains?
   make-rbtree
   rbtree->dot
   )
  (import (rnrs)
          (srfi :48))

(define-record-type rbtree
  (fields
   (mutable root)
   (mutable size)
   (immutable  key=?)
   (immutable key<?))
  (protocol
   (lambda (c)
     (lambda (key=? key<?)
       (c #f 0 key=? key<?)))))

(define-record-type node
  (fields
   (mutable left)
   (mutable right)
   (mutable parent)
   (mutable key)
   (mutable value)
   (mutable color)))

(define (rbtree-keys rb)
  (reverse
   (node-fold '()
              (lambda (accum node)
                (cons (node-key node) accum))
              (rbtree-root rb))))

(define (rbtree-contains? rb key)
  (if (eq? (rbtree-get rb key '$$$not-found)
           '$$$not-found)
      #f
      #t))

(define (rbtree-get rb key . fallback)
  (let ([node (rbtree-get-node (rbtree-key=? rb) (rbtree-key<? rb) (rbtree-root rb) key)])
    (cond
     [node (node-value node)]
     [else
      (if (pair? fallback)
          (car fallback)
          (error 'rbtree-get (format "key = ~a not found" key) key))])))

(define (rbtree-get-node key=? key<? node key)
  (cond
   [(not node) #f]
   [(key=? key (node-key node))
    node]
   [(key<? key (node-key node))
    (rbtree-get-node key=? key<? (node-left node) key)]
   [else
    (rbtree-get-node key=? key<? (node-right node) key)]))

(define (rbtree-delete! rb key)
  (let ([node (rbtree-get-node (rbtree-key=? rb) (rbtree-key<? rb) (rbtree-root rb) key)])
    (cond
     [node
      (rbtree-size-set! rb (- (rbtree-size rb) 1))
      (node-delete! rb node)
      #t]
     [else #f])))

(define (tree-successor x)
  (cond
   [(node-right x)
    (tree-minimum (node-right x))]
   [else
    (let ([y (node-parent x)])
      (let loop ([x x]
                 [y y])
        (cond
         [(or y (not (eq? x (node-right y))))
          y]
         [else
          (loop y (node-parent y))])))]))

(define (tree-minimum x)
  (if (node-left x)
      (tree-minimum (node-left x))
      x))

(define (rbtree-delete-fixup-rec rb x node-next1 node-next2)
  (let ([w (node-next2 (node-parent x))])
    (when (red? w)
      (node-color-set! w 'black)
      (node-color-set! (node-parent x))
      (left-rotate rb (node-parent x))
      (set! w (node-next2 (node-parent x))))
    (cond
     [(and (black? (node-next1 w)) (black? (node-next2 w)))
      (node-color-set! w 'red)
      (set! x (node-parent x))]
     [(black? (node-next2 w))
      (node-color-set! (node-next1 w) 'black)
      (node-color-set! w 'red)
      (right-rotate rb w)
      (set! w (node-next2 (node-parent x)))]
     [else '()])
    (node-color-set! w (node-color (node-parent x)))
    (node-color-set! (node-parent x) 'black)
    (node-color-set! (node-next2 w) 'black)
    (left-rotate rb (node-parent x))))

(define (rbtree-delete-fixup rb x)
  (let loop ([x x])
    (cond
     [(or (eq? x (rbtree-root rb)) (red? x))
      (node-color-set! x 'black)]
     [else
      (cond
       [(eq? x (node-left (node-parent x)))
        (rbtree-delete-fixup-rec rb x node-left node-right)]
       [else
        (rbtree-delete-fixup-rec rb x node-right node-left)])
      (loop (rbtree-root rb))])))

(define (node-delete! rb z)
  (let* ([y (if (or (not (node-left z)) (not (node-right z)))
                z
                (tree-successor z))]
         [x (if (node-left y) (node-left y) (node-right y))])
    (when x
      (node-parent-set! x (node-parent y)))
    (cond
     [(not (node-parent y))
      (rbtree-root-set! rb x)]
     [(eq? y (node-left (node-parent y)))
      (node-left-set! (node-parent y) x)]
     [else
      (node-right-set! (node-parent y) x)])
    (when (not (eq? y z))
      (node-key-set! z (node-key y))
      (node-value-set! z (node-value y))
      (node-color-set! z (node-color y)))
    (when (and (black? y) x)
      (rbtree-delete-fixup rb x))
    y))

(define (rbtree-set! rb key value)
  (let loop ([x (rbtree-root rb)]
             [y #f])
    (cond
     [(not x)
      (let ([z (make-node #f #f y key value 'black)])
;        (format #t "set key=~a y=~a\n" key y)
        (cond
         [(not y)
          (node-color-set! z 'black)
          (rbtree-size-set! rb (+ (rbtree-size rb) 1))
          (rbtree-root-set! rb z)]
         [((rbtree-key<? rb) key (node-key y))
          (node-color-set! z 'red)
          (rbtree-size-set! rb (+ (rbtree-size rb) 1))
          (node-left-set! y z)]
         [else
          (rbtree-size-set! rb (+ (rbtree-size rb) 1))
          (node-color-set! z 'red)
          (node-right-set! y z)])
        (insert-fixup rb z))]
     [else
      (cond
       [((rbtree-key=? rb) key (node-key x))
        (node-value-set! x value)]
       [((rbtree-key<? rb) key (node-key x))
          (loop (node-left x) x)]
       [else
          (loop (node-right x) x)])])))

(define (node-fold init proc node)
  (cond
   [(not node)
    init]
   [else
    (if (node-left node)
        (let ([accum (node-fold init proc (node-left node))])
          (if (node-right node)
              (node-fold (proc accum node) proc (node-right node))
              (proc accum node)))
        (if (node-right node)
            (node-fold (proc init node) proc (node-right node))
            (proc init node)))]))

(define (check-rbtree rb)
  (define (raise-error reason)
    (error 'check-rbtree reason))
  (assert (rbtree? rb))
  (cond
   [(not (rbtree-root rb)) #t]
   [else
    (and (or (binary-search-tree? rb)
             (raise-error "not binary-search-tree"))
         (or (black? (rbtree-root rb))
             (raise-error "root is not black"))
         (or (red-has-two-black? (rbtree-root rb))
             (raise-error "red should have black childlen"))
         (or (node-fold #t (lambda (accum node) (and accum (black-hight-same? node))) (rbtree-root rb))
             (raise-error "black height should be same")))]))

(define (leaf? node)
  (and (not (node-left node))
       (not (node-right node))))

(define (black? node)
  (eq? 'black (node-color node)))

(define (red? node)
  (eq? 'red (node-color node)))

(define (left-rotate rb x)
  (let ([y (node-right x)])
    (node-right-set! x (node-left y))
    (when (node-left y)
      (node-parent-set! (node-left y) x))
    (node-parent-set! y (node-parent x))
    (if (not (node-parent x))
        (rbtree-root-set! rb y)
        (if (eq? x (node-left (node-parent x)))
            (node-left-set! (node-parent x) y)
            (node-right-set! (node-parent x) y)))
    (node-left-set! y x)
    (node-parent-set! x y)))

(define (right-rotate rb x)
  (let ([y (node-left x)])
    (node-left-set! x (node-right y))
    (when (node-right y)
      (node-parent-set! (node-right y) x))
    (node-parent-set! y (node-parent x))
    (if (not (node-parent x))
        (rbtree-root-set! rb y)
        (if (eq? x (node-right (node-parent x)))
            (node-right-set! (node-parent x) y)
            (node-left-set! (node-parent x) y)))
    (node-right-set! y x)
    (node-parent-set! x y)))

(define (insert-fixup-rec rb z node-fetch node-rotate1 node-rotate2)
  (let ([y (node-fetch (node-parent (node-parent z)))])
    (cond
     [(and y (red? y))
      (node-color-set! (node-parent z) 'black)
      (node-color-set! y 'black)
      (node-color-set! (node-parent (node-parent z)) 'red)
      (insert-fixup rb (node-parent (node-parent z)))]
     [else
      (when (eq? z (node-fetch (node-parent z)))
        (set! z (node-parent z))
        (node-rotate1 rb z))
      (node-color-set! (node-parent z) 'black)
      (node-color-set! (node-parent (node-parent z)) 'red)
      (node-rotate2 rb (node-parent (node-parent z)))
      (insert-fixup rb z)])))

(define (insert-fixup rb z)
  (cond
   [(and (node-parent z) (red? (node-parent z)))
    (cond
     [(eq? (node-parent z) (node-left (node-parent (node-parent z))))
      (insert-fixup-rec rb z node-right left-rotate right-rotate)]
     [else
      (insert-fixup-rec rb z node-left right-rotate left-rotate)])]
   [else '()])
  (node-color-set! (rbtree-root rb) 'black))

(define (red-has-two-black? node)
  (cond
   [(not node) #t]
   [(red? node)
    (and (or (not (node-left node)) (black? (node-left node)))
         (or (not (node-right node)) (black? (node-right node)))
         (red-has-two-black? (node-left node))
         (red-has-two-black? (node-right node)))]
   [else
    (and (red-has-two-black? (node-left node))
         (red-has-two-black? (node-right node)))]))

(define (black-hight-same? node)
  (define height* '())
  (define (add-height! h)
    (set! height* (cons h height*)))
  (define (rec h node)
    (let ([h (if (black? node) (+ h 1) h)])
      (cond
       [(and (not (node-left node)) (not (node-right node)))
        (add-height! h)]
       [else
        (when (node-left node)
          (rec h (node-left node)))
        (when (node-right node)
          (rec h (node-right node)))])))
  (rec 0 node)
  (let ([height (car height*)])
    (for-all (lambda (x) (= height x)) height*)))

(define nil-index 0)
(define (gen-nil)
  (let ([x (format "Nil~d" nil-index)])
    (set! nil-index (+ nil-index 1))
    x))

(define (rbtree->dot rb . port)
  (define (print-node-color node port)
    (if (black? node)
        (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" (node-key node))
        (format port "    ~s [style = filled, color = \"#336666\", fillcolor = \"#CC9999\"];\n" (node-key node))))
  (let ([port (if (pair? port) (car port) (current-output-port))])
    (format port "digraph rbtrees {\n")
    (node-fold '() (lambda (accum node)
                     (let ([left (node-left node)]
                           [right (node-right node)])
                       (print-node-color node port)
                       (cond
                        [(not left)
                         (let ([nil (gen-nil)])
                           (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" nil)
                           (format port "    ~s -> ~s;\n" (node-key node) nil))]
                        [else
                         (print-node-color left port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key left))])
                       (cond
                        [(not right)
                         (let ([nil (gen-nil)])
                           (format port "    ~s [style = filled, fillcolor = \"#cccccc\"];\n" nil)
                           (format port "    ~s -> ~s;\n" (node-key node) nil))]
                        [else
                         (print-node-color right port)
                         (format port "    ~s -> ~s;\n" (node-key node) (node-key right))])
                     ))
               (rbtree-root rb))
    (display "}\n" port)))

;; internal procedures
(define (binary-search-tree? rb)
  (call/cc (lambda (break)
             (node-fold #f
                        (lambda (prev-key node)
                          (cond
                           [(and prev-key (or ((rbtree-key=? rb) prev-key (node-key node)) ((rbtree-key<? rb) prev-key (node-key node))))
                            (node-key node)]
                           [(not prev-key)
                            (node-key node)]
                           [else
                            (break #f)])) (rbtree-root rb)))))

)

